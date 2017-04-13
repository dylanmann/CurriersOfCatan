{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanBoard(Board,Corner,CornerLocation,Tile(..),TileLocation,
                  getTile,setupBoard,adjacentCorners,tokenOrder,Reward,
                  Resource(..),Terrain(..),Token,desert,getCorner,getAllTiles,
                  getAllCorners,Neighbors(..),Harbor(..),rewardTiles) where

import Math.Geometry.GridMap.Lazy(LGridMap,lazyGridMapIndexed)
import Math.Geometry.Grid.Hexagonal(HexHexGrid,hexHexGrid)
import qualified Math.Geometry.GridMap as GM
import Data.Map(Map)
import qualified Data.Map as Map


data Resource = Brick | Lumber | Ore | Grain | Wool
    deriving (Enum, Read, Show, Eq, Ord)

-- No Seven because 7 is the robber
data Token = Two|Three|Four|Five|Six|Eight|Nine|Ten|Eleven|Twelve
    deriving (Enum, Read, Show, Eq)

--default token order, starting from top left and circling clockwise inwards
tokenOrder :: [Token]
tokenOrder = [Five, Two, Six, Three, Eight, Ten, Nine, Twelve, Eleven, Four,
              Eight, Ten, Nine, Four, Five, Six, Three, Eleven]

tileIndices :: [TileLocation]
tileIndices = [(-2,2), (-1,2), (0,2), (1,1), (2,0), (2,-1), (2,-2), (1,-2), (0,-2),
    (-1,-1), (-2,0), (-2,1), (-1,1), (0,1), (1,0), (1,-1), (0,-1), (-1,0), (0,0)]

--default tile order, starting from top left and circling clockwise inwards
terrainOrder :: [Terrain]
terrainOrder = [Mountains, Pasture, Forest, Hills, Mountains, Pasture, Pasture,
        Fields, Hills, Forest, Fields, Fields,Hills, Pasture, Forest,Fields,
        Mountains, Forest]

data Terrain = Hills     -- produce brick
             | Forest    -- produce lumber
             | Mountains -- produce ore
             | Fields    -- produce grain
             | Pasture   -- produces Wool
    deriving (Enum, Read, Show, Eq)

data Neighbors = OneTile Tile
               | TwoTiles Tile Tile
               | ThreeTiles Tile Tile Tile
    deriving (Read, Show, Eq)

data Harbor = GenericHarbor
            | SpecialHarbor Resource
    deriving (Read, Show, Eq)

type Reward = (Neighbors, Maybe Harbor)

-- need to store corner location because it keeps corners distinct
type Corner = (Reward, CornerLocation)
type CornerLocation = (Int, Int)

-- do we need TileLocation???
type TileLocation = (Int, Int)
data Tile = Paying Terrain Token TileLocation
          | Desert TileLocation
        deriving (Eq, Show, Read)

-- 30 around outside, 18 around inside, 6 around center
-- corners indexed with radial coordinates where 0 is the far right of the hex
-- and n-1 is the bottom far right hex
type Corners = Map CornerLocation Corner
type Tiles   = LGridMap HexHexGrid Tile -- 3 sided hexhexgrid

cornerIndices :: [CornerLocation]
cornerIndices = [0..29] `zip` replicate 30 2 ++
                [0..17] `zip` replicate 18 1 ++
                [0.. 5] `zip` replicate 6 0


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

data Board = Board Tiles Corners

rewardTiles :: Reward -> [Tile]
rewardTiles (OneTile t,_) = [t]
rewardTiles (TwoTiles t1 t2,_) = [t1, t2]
rewardTiles (ThreeTiles t1 t2 t3,_) = [t1, t2, t3]

tiles :: [Tile]
tiles = zipWith3 Paying terrainOrder tokenOrder tileIndices ++
        [Desert (last tileIndices)]

setupBoard :: IO Board
setupBoard = let ts = lazyGridMapIndexed (hexHexGrid 3) (zip tileIndices tiles)
                 cs = Map.empty
             in return $ Board ts cs


getCorner :: CornerLocation -> Board -> Corner
getCorner = undefined

getAllTiles :: Board -> [Tile]
getAllTiles (Board ts _) = GM.elems ts

getAllCorners :: Board -> [Corner]
getAllCorners = undefined

-- always x +- 1 but also sometimes need to look inwards or outwards
adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners = undefined

getTile :: TileLocation -> Board -> Maybe Tile
getTile l (Board ts _) = GM.lookup l ts

desert :: Board -> TileLocation
desert = foldr des (error "desert definitely exists") . getAllTiles
           where des (Desert l) _ = l
                 des _ acc = acc
