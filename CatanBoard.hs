{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanBoard(Board,Corner,CornerLocation,Tile(..),TileLocation,
                  getTile,setupBoard,adjacentCorners,tokenOrder,Reward,
                  Resource(..),Terrain(..),Token,desert,getCorner,getAllTiles,
                  getAllCorners,Neighbors(..),Harbor(..),rewardTiles) where

import Data.Map(Map)


data Resource = Brick | Lumber | Ore | Grain | Wool
    deriving (Enum, Read, Show, Eq, Ord)

-- No Seven because 7 is the robber
data Token = Two|Three|Four|Five|Six|Eight|Nine|Ten|Eleven|Twelve
    deriving (Enum, Read, Show, Eq)

--default token order, starting from top left and circling clockwise inwards
tokenOrder :: [Token]
tokenOrder = [Five, Two, Six, Three, Eight, Ten, Nine, Twelve, Eleven, Four,
              Eight, Ten, Nine, Four, Five, Six, Three, Eleven]


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

type Corners = Map CornerLocation Corner
type Tiles =   Map TileLocation Tile


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

type Board = (Tiles, Corners)

rewardTiles :: Reward -> [Tile]
rewardTiles (OneTile t,_) = [t]
rewardTiles (TwoTiles t1 t2,_) = [t1, t2]
rewardTiles (ThreeTiles t1 t2 t3,_) = [t1, t2, t3]

setupBoard :: IO Board
setupBoard = undefined

getCorner :: CornerLocation -> Board -> Corner
getCorner = undefined

getAllTiles :: Board -> [Tile]
getAllTiles = undefined

getAllCorners :: Board -> [Corner]
getAllCorners = undefined

adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners = undefined

getTile :: TileLocation -> Board -> Tile
getTile = undefined

desert :: Board -> TileLocation
desert = foldr des (error "desert definitely exists") . getAllTiles
           where des (Desert l) _ = l
                 des _ acc = acc
