{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanBoard(Board,Corner,CornerLocation,Tile(..),TileLocation,
                  getTile,setupBoard,adjacentCorners,tokenOrder,Reward,
                  Resource(..),Terrain(..),Token,desert,getCorner,getAllTiles,
                  getAllCorners,Neighbors(..),Harbor(..),rewardTiles,rewardLocs)
                  where

-- import Math.Geometry.GridMap.Lazy(LGridMap,lazyGridMapIndexed)
-- import Math.Geometry.Grid.Hexagonal(HexHexGrid,hexHexGrid)
-- import qualified Math.Geometry.GridMap as GM
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromJust)


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
tileIndices = [8..11] `zip` repeat 2 ++ [0..7] `zip` repeat 2 ++
              [4, 5] `zip` repeat 1 ++ [0..3] `zip` repeat 1 ++
              [(0, 0)]

--default tile order, starting from top left and circling clockwise inwards
terrainOrder :: [Terrain]
terrainOrder = [Mountains, Pasture, Forest, Hills, Mountains, Pasture, Pasture,
        Fields, Hills, Forest, Fields, Fields, Hills, Pasture, Forest,Fields,
        Mountains, Forest]

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

-- need to store corner location because it keeps corners distinct
type Corner = Reward
type CornerLocation = (Int, Int)

-- do we need TileLocation???
type TileLocation = (Int, Int)
data Tile = Paying Terrain Token
          | Desert
        deriving (Eq, Show, Read)

-- 30 around outside, 18 around inside, 6 around center
-- corners indexed with radial coordinates where 0 is the far right of the hex
-- and n-1 is the bottom far right hex
type Corners = Map CornerLocation Corner
type Tiles   = Map TileLocation Tile

cornerIndices :: [CornerLocation]
cornerIndices = [0.. 5] `zip` repeat 0 ++
                [0..17] `zip` repeat 1 ++
                [0..29] `zip` repeat 2

-- neighbors :: Tiles -> [Neighbors]
-- neighbors ts = zipWith3 ThreeTiles (repeat (get (0,0))) (map get (zip [0..5] repeat 0)) (zip
--     where get l = Map.lookup l ts

-- TODO
harbors :: [Maybe Harbor]
harbors = replicate 54 Nothing



-- type Tiles = ((Tile, Tile, Tile),
--               (Tile, Tile, Tile, Tile),
--               (Tile, Tile, Tile, Tile, Tile),
--               (Tile, Tile, Tile, Tile),
--               (Tile, Tile, Tile))

-- type Corners = ((Corner, Corner, Corner, Corner, Corner, Corner, Corner),
--                 (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
--                 (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
--                 (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
--                 (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
--                 (Corner, Corner, Corner, Corner, Corner, Corner, Corner))

data Board = Board {tiles :: Tiles, corners :: Corners}
  deriving(Read, Show, Eq)

rewardLocs :: Reward -> [TileLocation]
rewardLocs (OneTile t, _) =  [t]
rewardLocs (TwoTiles t1 t2, _) = [t1, t2]
rewardLocs (ThreeTiles t1 t2 t3, _) = [t1, t2, t3]

rewardTiles :: Board -> Reward -> [Tile]
rewardTiles b r = map (getTile b) (rewardLocs r)

allTiles :: [Tile]
allTiles = zipWith Paying terrainOrder tokenOrder ++
        [Desert]

setupBoard :: IO Board
setupBoard = let ts = Map.fromList (zip tileIndices allTiles)
                 cs = Map.empty
             in return $ Board ts cs


getCorner :: Board -> CornerLocation -> Corner
getCorner = undefined

getAllTiles :: Board -> [Tile]
getAllTiles (Board ts _) = Map.elems ts

getAllCorners :: Board -> [Corner]
getAllCorners = undefined

-- always x +- 1 but also sometimes need to look inwards or outwards
adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners = undefined

getTile :: Board -> TileLocation -> Tile
getTile (Board ts _) l = fromJust $ Map.lookup l ts

desert :: Board -> TileLocation
desert = foldr des err . Map.toList . tiles
           where des (l, Desert) _ = l
                 des _ acc = acc
                 err = error "desert definitely exists"
