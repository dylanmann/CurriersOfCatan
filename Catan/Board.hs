{-|
Module      : Catan.Board
Description : Board Types for Catan
Copyright   : (c) Dylan Mann, David Cao 2017
License     : GPL-3
Maintainer  : mannd@seas.upenn.edu
Stability   : experimental
Portability : POSIX

Contains the primitive board types and wraps unsafe operations to provide a total
mapping from CornerLocations and Tile Locations, and so no other code has to
touch the Board and invariants are maintained.  Also contains some setup methods.

-}

{-# OPTIONS_HADDOCK not-home, show-extensions #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module Board(Terrain(..),

             Token(..),
             tokenOrder,

             Tile(..),
             Tiles,
             TileLocation,
             tileToAxial,
             axialToTile,
             getTile,
             desert,
             tileIndices,
             makeTileLocation,

             Resource(..),
             Reward,
             rewardTiles,
             rewardLocs,
             Harbor(..),
             Neighbors(..),

             Corner,
             Corners,
             CornerLocation,
             cornerToAxial,
             adjacentCorners,
             getCorner,
             makeCornerLocation,
             cornerIndices,

             Board(..),
             setupBoard,

             defaultBuildingLocations,
             defaultRoadLocations)
             where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromJust)
import System.Random.Shuffle(shuffleM)

-- | Resources that are held by players and used as currency
data Resource = Brick | Lumber | Ore | Grain | Wool
    deriving (Enum, Read, Show, Eq, Ord)

-- | Tokens that represent when tile payouts happen
data Token = Two   | Three | Four | Five   | Six
           | Eight | Nine  | Ten  | Eleven | Twelve
    deriving (Enum, Read, Show, Eq)

-- | default token order, starting from top left and circling clockwise inwards
tokenOrder :: [Token]
tokenOrder = [Five, Two, Six, Three, Eight, Ten, Nine, Twelve, Eleven, Four,
              Eight, Ten, Nine, Four, Five, Six, Three, Eleven]

-- | indices of all tiles, starting from top left and going inwards clockwise
tileIndices :: [TileLocation]
tileIndices = map TileLocation $
              ([8..11] ++ [0..7]) `zip` repeat 2 ++
              ([4,  5] ++ [0..3]) `zip` repeat 1 ++
              [(0, 0)]

-- | default tile order, starting from top left and circling clockwise inwards
terrainOrder :: [Terrain]
terrainOrder = [Mountains, Pasture, Forest, Hills, Mountains, Pasture, Pasture,
                Fields, Hills, Forest, Fields, Fields, Hills, Pasture, Forest,
                Fields, Mountains, Forest]

-- | represents the types of paying tiles
data Terrain = Hills     -- produce brick
             | Forest    -- produce lumber
             | Mountains -- produce ore
             | Fields    -- produce grain
             | Pasture   -- produces Wool
    deriving (Enum, Read, Show, Eq)

-- | Corner on a board, it has the neighboring tiles and whether it is adacent
--   to any harbors
type Corner = Reward
type Reward = (Neighbors, Maybe Harbor)

-- | Protected type can only be instantiated by makeCornerLocation outside the
--   module
data CornerLocation = CornerLocation (Int, Int)
    deriving(Ord, Show, Read, Eq)

-- | neighboring tiles of the corner
data Neighbors = OneTile TileLocation
               | TwoTiles TileLocation TileLocation
               | ThreeTiles TileLocation TileLocation TileLocation
    deriving (Read, Show, Eq)

-- | what type of harbor that corner has access to
data Harbor = GenericHarbor
            | SpecialHarbor Resource
    deriving (Read, Show, Eq)


-- | safe method for creating corner locations
makeCornerLocation :: Int -> Int -> Maybe CornerLocation
makeCornerLocation x y | x < 0 || y < 0 = Nothing
makeCornerLocation x 0 | x < 6          = Just $ CornerLocation (x, 0)
makeCornerLocation x 1 | x < 18         = Just $ CornerLocation (x, 1)
makeCornerLocation x 2 | x < 30         = Just $ CornerLocation (x, 2)
makeCornerLocation _ _                  = Nothing

-- | Protected type can only be instantiated by makeCornerLocation outside the
--   module
data TileLocation = TileLocation (Int, Int)
    deriving(Ord, Read, Show, Eq)

-- | Tile is represented either by a paying tile or the desert.  Paying tiles
--   yield resources to neighboring players when the dice roll the token
data Tile = Paying Terrain Token
          | Desert
        deriving (Eq, Show, Read)

-- | safe method for creating tile locations
makeTileLocation :: Int -> Int -> Maybe TileLocation
makeTileLocation x y | x < 0 || y < 0 = Nothing
makeTileLocation 0 0                  = Just $ TileLocation (0, 0)
makeTileLocation x 1 | x < 6          = Just $ TileLocation (x, 1)
makeTileLocation x 2 | x < 12         = Just $ TileLocation (x, 2)
makeTileLocation _ _                  = Nothing

-- 30 around outside, 18 around inside, 6 around center
-- corners indexed with radial coordinates where 0 is the far right of the hex
-- and the coordinates go clockwise
type Corners = Map CornerLocation Corner

-- 1 then 6 then 12, indexed clockwise
type Tiles = Map TileLocation Tile

cornerIndices :: [CornerLocation]
cornerIndices = map CornerLocation $
                [0.. 5] `zip` repeat 0 ++
                [0..17] `zip` repeat 1 ++
                [0..29] `zip` repeat 2

innerNeighbors :: [Neighbors]
innerNeighbors =
  zipWith3 ThreeTiles n1 n2 n3 where
    n1 = map TileLocation $ repeat 0     `zip` repeat 0
    n2 = map TileLocation $ (5 : [0..4]) `zip` repeat 0
    n3 = map TileLocation $ [0..5]       `zip` repeat 0

middleNeighbors :: [Neighbors]
middleNeighbors =
  zipWith3 ThreeTiles n1 n2 n3 where
    n1 = map TileLocation $ concatMap (replicate 3) (zip [0..5] $ repeat 1)
    n2 = map TileLocation $ take 18 $ outOutIn 0 0
    n3 = map TileLocation $ (11 : oneThenTwo 0) `zip` repeat 2
    outOutIn o i = (o, 2) : (o + 1, 2) : (i, 1) : outOutIn (o + 2) (i + 1)
    oneThenTwo i = i : i + 1 : i + 1 : oneThenTwo (i + 2)

outerNeighbors :: [Neighbors]
outerNeighbors = two 11 0 : take 29 (pat 0) where
  pat x = one x : one x : two x (x + 1) : one (x + 1) : two (x + 1) (x + 2) : pat (x + 2)
  one y = OneTile (TileLocation (y, 2))
  two y1 y2 = TwoTiles (TileLocation (y1, 2)) (TileLocation (y2, 2))

neighbors :: [Neighbors]
neighbors = innerNeighbors ++ middleNeighbors ++ outerNeighbors

allCorners :: IO [Corner]
allCorners = do h <- harbors
                return $ zip neighbors $ makeHarbors h

makeHarbors :: [Harbor] -> [Maybe Harbor]
makeHarbors (h1:h2:tl) =
  Just h1: Just h1: Nothing: Nothing: Just h2: Just h2: Nothing: makeHarbors tl
makeHarbors (h:[]) = [Just h, Just h]
makeHarbors [] = []

harbors :: IO [Harbor]
harbors = shuffleM $ map SpecialHarbor [Wool, Lumber, Brick, Ore, Grain] ++
          replicate 4 GenericHarbor

data Board = Board {tiles :: Tiles, corners :: Corners}
  deriving(Read, Show, Eq)

rewardLocs :: Reward -> [TileLocation]
rewardLocs (OneTile t, _)           = [t]
rewardLocs (TwoTiles t1 t2, _)      = [t1, t2]
rewardLocs (ThreeTiles t1 t2 t3, _) = [t1, t2, t3]

rewardTiles :: Board -> Reward -> [Tile]
rewardTiles b r = map (getTile b) (rewardLocs r)

allTiles :: IO [Tile]
allTiles = do terrs <- shuffleM terrainOrder
              toks <- shuffleM tokenOrder
              shuffleM $ zipWith Paying terrs toks ++ [Desert]

setupBoard :: IO Board
setupBoard = do allCs <- allCorners
                allTs <- allTiles
                let ts = Map.fromList (zip tileIndices allTs)
                    cs = Map.fromList (zip cornerIndices allCs)
                return $ Board ts cs


getCorner :: Board -> CornerLocation -> Corner
getCorner (Board _ cs) c = fromJust $ Map.lookup c cs

-- always x +- 1 but also sometimes need to look inwards or outwards
adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners (CornerLocation c) = map CornerLocation $ adj c where
  adj (0, 0) = [(17, 1), (5, 0), (1,0)]
  adj (0, 1) = [(29, 2), (5, 0), (1,0)]
  adj (0, 2) = [(29, 2), (1, 2)]
  adj (x, 0) = [(3 * x, 1), (x - 1, 0), (x + 1, 0)]
  adj (x, 1) = [(3 * x, 1), (x - 1, 0), (x + 1, 0)]
  adj (x, 2)
    | (x `mod` 5) `elem` [0, 1, 3] = [(x - 1, 2), (x + 1, 2)]
    | otherwise = [(x - 1, 2), (x + 1, 2), (x `quot` 5, 1)]
  adj _ = error "invalid corner"

getTile :: Board -> TileLocation -> Tile
getTile (Board ts _) l = fromJust $ Map.lookup l ts

desert :: Board -> TileLocation
desert = foldr des err . Map.toList . tiles
           where des (l, Desert) _ = l
                 des _ acc = acc
                 err = error "desert definitely exists"

defaultBuildingLocations :: [CornerLocation]
defaultBuildingLocations = map CornerLocation
  [(16,1), (14, 1), (12, 1), (10,1), (8,1), (6,1), (4,1), (1,1)]


defaultRoadLocations :: [(CornerLocation, CornerLocation)]
defaultRoadLocations = map mkLoc
          [(0, 1), (2, 3), (4, 5), (6, 7), (8, 9), (10, 11), (13, 14), (15, 16)]
      where mkLoc (x1, x2) = (CornerLocation (x1, 1), CornerLocation (x2, 1))


axialToTile :: (Int, Int) -> Maybe TileLocation
axialToTile loc = uncurry makeTileLocation $ case loc of
  (0,0)   -> (0,0)
  (1,0)   -> (0,1)
  (1,-1)  -> (1,1)
  (0,-1)  -> (2,1)
  (-1,0)  -> (3,1)
  (-1,1)  -> (4,1)
  (0, 1)  -> (5,1)
  (2, 0)  -> (0,2)
  (2,-1)  -> (1,2)
  (2,-2)  -> (2,2)
  (1,-2)  -> (3,2)
  (0,-2)  -> (4,2)
  (-1,-1) -> (5,2)
  (-2,0)  -> (6,2)
  (-2,1)  -> (7,2)
  (-2,2)  -> (8,2)
  (-1,2)  -> (9,2)
  (0,2)   -> (10,2)
  (1,1)   -> (11,2)
  l       -> l

tileToAxial :: TileLocation -> (Int, Int)
tileToAxial (TileLocation l) = case l of
  (0,0)  -> (0,0)
  (0,1)  -> (1,0)
  (1,1)  -> (1,-1)
  (2,1)  -> (0,-1)
  (3,1)  -> (-1,0)
  (4,1)  -> (-1,1)
  (5,1)  -> (0, 1)
  (0,2)  -> (2, 0)
  (1,2)  -> (2,-1)
  (2,2)  -> (2,-2)
  (3,2)  -> (1,-2)
  (4,2)  -> (0,-2)
  (5,2)  -> (-1,-1)
  (6,2)  -> (-2,0)
  (7,2)  -> (-2,1)
  (8,2)  -> (-2,2)
  (9,2)  -> (-1,2)
  (10,2) -> (0,2)
  (11,2) -> (1,1)
  _      -> error "No other tile location can exist"


cornerToAxial :: CornerLocation -> (Int, Int, Bool)
cornerToAxial (CornerLocation c) = case c of
  (0,0)   -> ( 1, -1, False)
  (1,0)   -> ( 0,  0, True)
  (2,0)   -> ( 0, -1, False)
  (3,0)   -> (-1,  1, True)
  (4,0)   -> ( 0,  0, False)
  (5,0)   -> ( 0,  1, True)
  (0,1)   -> ( 2, -1, False)
  (1,1)   -> ( 1,  0, True)
  (2,1)   -> ( 2, -2, False)
  (3,1)   -> ( 1, -1, True)
  (4,1)   -> ( 1, -2, False)
  (5,1)   -> ( 0, -1, True)
  (6,1)   -> ( 0, -2, False)
  (7,1)   -> (-1,  0, True)
  (8,1)   -> ( 1, -1, False)
  (9,1)   -> (-2,  1, True)
  (10,1)  -> (-1,  0, False)
  (11,1)  -> (-2,  2, True)
  (12,1)  -> (-1,  1, False)
  (13,1)  -> (-1,  2, True)
  (14,1)  -> ( 0,  1, False)
  (15,1)  -> ( 0,  2, True)
  (16,1)  -> ( 1,  0, False)
  (17,1)  -> ( 1,  1, True)
  (0,2)   -> ( 3, -1, False)
  (1,2)   -> ( 2,  0, True)
  (2,2)   -> ( 3, -2, False)
  (3,2)   -> ( 2, -1, True)
  (4,2)   -> ( 4, -3, False)
  (5,2)   -> ( 2, -2, True)
  (6,2)   -> ( 3, -3, False)
  (7,2)   -> ( 1, -2, True)
  (8,2)   -> ( 2, -3, False)
  (9,2)   -> ( 0, -2, True)
  (10,2)  -> ( 1, -3, False)
  (11,2)  -> (-1, -1, True)
  (12,2)  -> (-1, -2, False)
  (13,2)  -> (-2,  0, True)
  (14,2)  -> (-2, -1, False)
  (15,2)  -> (-3,  1, True)
  (16,2)  -> (-2,  0, False)
  (17,2)  -> (-3,  2, True)
  (18,2)  -> (-2,  1, False)
  (19,2)  -> (-3,  3, True)
  (20,2)  -> (-2,  2, False)
  (21,2)  -> (-2,  3, True)
  (22,2)  -> (-1,  2, False)
  (23,2)  -> (-1,  3, True)
  (24,2)  -> ( 0,  2, False)
  (25,2)  -> ( 0,  3, True)
  (26,2)  -> ( 1,  1, False)
  (27,2)  -> ( 1,  2, True)
  (28,2)  -> ( 2,  0, False)
  (29,2)  -> ( 2,  1, True)
  _      -> error "No other corner locations exist"
