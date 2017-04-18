{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanBoard(Terrain(..),

                  Token(..),
                  tokenOrder,

                  Tile(..),
                  Tiles,
                  TileLocation,
                  getTile,
                  desert,
                  makeTileLocation,

                  Reward,
                  rewardTiles,
                  rewardLocs,
                  Harbor(..),
                  Neighbors(..),

                  Corner,
                  Corners,
                  CornerLocation,
                  adjacentCorners,
                  getCorner,
                  makeCornerLocation,
                  cornerIndices,

                  Board(..),
                  setupBoard,

                  -- This stuff is only in ehre to avoid circular dependencies
                  -- TODO: put this all back in catanTypes
                  defaultBuildings,
                  Building(..),
                  Color(..),
                  Roads,
                  defaultRoads,
                  Resource(..))
                  where


import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromJust)

data Color = Blue | Red | Orange | White
    deriving (Enum, Read, Show, Eq, Ord)


data Resource = Brick | Lumber | Ore | Grain | Wool
    deriving (Enum, Read, Show, Eq, Ord)

-- No Seven because 7 is the robber
data Token = Two   | Three | Four | Five   | Six
           | Eight | Nine  | Ten  | Eleven | Twelve
    deriving (Enum, Read, Show, Eq)

--default token order, starting from top left and circling clockwise inwards
tokenOrder :: [Token]
tokenOrder = [Five, Two, Six, Three, Eight, Ten, Nine, Twelve, Eleven, Four,
              Eight, Ten, Nine, Four, Five, Six, Three, Eleven]

tileIndices :: [TileLocation]
tileIndices = ([8..11] ++ [0..7]) `zip` repeat 2 ++
              ([4,  5] ++ [0..3]) `zip` repeat 1 ++
              [(0, 0)]

--default tile order, starting from top left and circling clockwise inwards
terrainOrder :: [Terrain]
terrainOrder = [Mountains, Pasture, Forest, Hills, Mountains, Pasture, Pasture,
                Fields, Hills, Forest, Fields, Fields, Hills, Pasture, Forest,
                Fields, Mountains, Forest]

type Roads = [(CornerLocation, CornerLocation, Color)]

data Terrain = Hills     -- produce brick
             | Forest    -- produce lumber
             | Mountains -- produce ore
             | Fields    -- produce grain
             | Pasture   -- produces Wool
    deriving (Enum, Read, Show, Eq)

data Neighbors = OneTile TileLocation
               | TwoTiles TileLocation TileLocation
               | ThreeTiles TileLocation TileLocation TileLocation
    deriving (Read, Show, Eq)

data Harbor = GenericHarbor
            | SpecialHarbor Resource
    deriving (Read, Show, Eq)

type Reward = (Neighbors, Maybe Harbor)


type Corner = Reward
type CornerLocation = (Int, Int)

makeCornerLocation :: Int -> Int -> Maybe CornerLocation
makeCornerLocation x y | x < 0 || y < 0 = Nothing
makeCornerLocation x 0 | x < 6          = Just (x, 0)
makeCornerLocation x 1 | x < 18         = Just (x, 1)
makeCornerLocation x 2 | x < 30         = Just (x, 2)
makeCornerLocation _ _                  = Nothing

-- do we need TileLocation???
type TileLocation = (Int, Int)
data Tile = Paying Terrain Token
          | Desert
        deriving (Eq, Show, Read)

makeTileLocation :: Int -> Int -> Maybe CornerLocation
makeTileLocation x y | x < 0 || y < 0 = Nothing
makeTileLocation 0 0                  = Just (0, 0)
makeTileLocation x 1 | x < 6          = Just (x, 1)
makeTileLocation x 2 | x < 12         = Just (x, 2)
makeTileLocation _ _                  = Nothing

data Building = Settlement Color CornerLocation
              | City       Color CornerLocation
    deriving (Read, Show, Eq)

-- 30 around outside, 18 around inside, 6 around center
-- corners indexed with radial coordinates where 0 is the far right of the hex
-- and the coordinates go clockwise
type Corners = Map CornerLocation Corner

-- 1 then 6 then 12, indexed clockwise
type Tiles = Map TileLocation Tile

cornerIndices :: [CornerLocation]
cornerIndices = [0.. 5] `zip` repeat 0 ++
                [0..17] `zip` repeat 1 ++
                [0..29] `zip` repeat 2

innerNeighbors :: [Neighbors]
innerNeighbors =
  zipWith3 ThreeTiles n1 n2 n3 where
    n1 = repeat 0     `zip` repeat 0
    n2 = (5 : [0..4]) `zip` repeat 0
    n3 = [0..5]       `zip` repeat 0

middleNeighbors :: [Neighbors]
middleNeighbors =
  zipWith3 ThreeTiles n1 n2 n3 where
    n1 = concatMap (replicate 3) (zip [0..5] $ repeat 1)
    n2 = take 18 $ outOutIn 0 0
    n3 = (11 : oneThenTwo 0) `zip` repeat 2
    outOutIn o i = (o, 2) : (o + 1, 2) : (i, 1) : outOutIn (o + 2) (i + 1)
    oneThenTwo i = i : i + 1 : i + 1 : oneThenTwo (i + 2)

outerNeighbors :: [Neighbors]
outerNeighbors = TwoTiles (11, 2) (0, 2) : take 29 (pat 0) where
  pat x = one x : one x : two x (x + 1) : one (x + 1) : two (x + 1) (x + 2) : pat (x + 2)
  one y = OneTile (y, 2)
  two y1 y2 = TwoTiles (y1, 2) (y2, 2)

neighbors :: [Neighbors]
neighbors = innerNeighbors ++ middleNeighbors ++ outerNeighbors

allCorners :: [Corner]
allCorners = zip neighbors harbors

harbors :: [Maybe Harbor]
harbors = replicate 54 Nothing

data Board = Board {tiles :: Tiles, corners :: Corners}
  deriving(Read, Show, Eq)

rewardLocs :: Reward -> [TileLocation]
rewardLocs (OneTile t, _)           = [t]
rewardLocs (TwoTiles t1 t2, _)      = [t1, t2]
rewardLocs (ThreeTiles t1 t2 t3, _) = [t1, t2, t3]

rewardTiles :: Board -> Reward -> [Tile]
rewardTiles b r = map (getTile b) (rewardLocs r)

allTiles :: [Tile]
allTiles = zipWith Paying terrainOrder tokenOrder ++ [Desert]

setupBoard :: IO Board
setupBoard = let ts = Map.fromList (zip tileIndices allTiles)
                 cs = Map.fromList (zip cornerIndices allCorners)
             in return $ Board ts cs


getCorner :: Board -> CornerLocation -> Corner
getCorner (Board _ cs) c = fromJust $ Map.lookup c cs

-- always x +- 1 but also sometimes need to look inwards or outwards
adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners (0, 0) = [(17, 1), (5, 0), (1,0)]
adjacentCorners (0, 1) = [(29, 2), (5, 0), (1,0)]
adjacentCorners (0, 2) = [(29, 2), (1, 2)]
adjacentCorners (x, 0) = [(3 * x, 1), (x - 1, 0), (x + 1, 0)]
adjacentCorners (x, 1) = [(3 * x, 1), (x - 1, 0), (x + 1, 0)]
adjacentCorners (x, 2)
  | (x `mod` 5) `elem` [0, 1, 3] = [(x - 1, 2), (x + 1, 2)]
  | otherwise = [(x - 1, 2), (x + 1, 2), (x `quot` 5, 1)]
adjacentCorners _ = error "invalid corner"

getTile :: Board -> TileLocation -> Tile
getTile (Board ts _) l = fromJust $ Map.lookup l ts

desert :: Board -> TileLocation
desert = foldr des err . Map.toList . tiles
           where des (l, Desert) _ = l
                 des _ acc = acc
                 err = error "desert definitely exists"

defaultBuildings :: [Building]
defaultBuildings = map (uncurry Settlement)
   [(White, (1,1)), (Blue, (3, 1)),  (Orange, (5, 1)), (Blue, (7,1)),
    (Red, (9,1)),   (White, (11,1)), (Red, (13,1)),    (Orange, (16,1))]

defaultRoads :: Roads
defaultRoads = map mkRoad
          [(0,1, White), (2, 3, Blue),   (4, 5, Orange), (6, 7, Blue),
           (8,9, Red),    (10,11, White), (13,14, Red),   (15, 16, Orange)]
      where mkRoad (x1, x2, c) = ((x1, 1), (x2, 1), c)
