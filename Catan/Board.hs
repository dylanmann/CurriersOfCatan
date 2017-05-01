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
import Data.Maybe(fromJust, mapMaybe)
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
data CornerLocation = CornerLocation (Int, Int, Bool)
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
makeCornerLocation :: Int -> Int -> Bool -> Maybe CornerLocation
makeCornerLocation x y t = let c = CornerLocation (x, y, t) in
  if c `elem` cornerIndices then Just c else Nothing


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

type Corners = Map CornerLocation Corner

type Tiles = Map TileLocation Tile

cornerIndices :: [CornerLocation]
cornerIndices = map CornerLocation [(1,-1,False),(0,0,True),(0,-1,False),(-1,1,True),(0,0,False),(0,1,True),(2,-1,False),(1,0,True),(2,-2,False),(1,-1,True),(1,-2,False),(0,-1,True),(0,-2,False),(-1,0,True),(-1,-1,False),(-2,1,True),(-1,0,False),(-2,2,True),(-1,1,False),(-1,2,True),(0,1,False),(0,2,True),(1,0,False),(1,1,True),(3,-1,False),(2,0,True),(3,-2,False),(2,-1,True),(3,-3,False),(2,-2,True),(2,-3,False),(1,-2,True),(1,-3,False),(0,-2,True),(0,-3,False),(-1,-1,True),(-1,-2,False),(-2,0,True),(-2,-1,False),(-3,1,True),(-2,0,False),(-3,2,True),(-2,1,False),(-3,3,True),(-2,2,False),(-2,3,True),(-1,2,False),(-1,3,True),(0,2,False),(0,3,True),(1,1,False),(1,2,True),(2,0,False),(2,1,True)]

getNeighbors :: CornerLocation -> Neighbors
getNeighbors (CornerLocation (x, y, True))  = mkNeighbors $ mapMaybe axialToTile [(x, y), (x, y-1), (x+1, y-1)]
getNeighbors (CornerLocation (x, y, False)) = mkNeighbors $ mapMaybe axialToTile [(x, y), (x, y+1), (x-1, y+1)]

mkNeighbors :: [TileLocation] -> Neighbors
mkNeighbors [tl] = OneTile tl
mkNeighbors [tl, tl2] = TwoTiles tl tl2
mkNeighbors [tl, tl2, tl3] = ThreeTiles tl tl2 tl3
mkNeighbors _ = error "shouldnt happen"

neighbors :: [Neighbors]
neighbors = map getNeighbors cornerIndices


allCorners :: IO [Corner]
allCorners = do h <- harbors
                return $ zip neighbors $ (replicate 24 Nothing) ++ makeHarbors h
                -- return $ zip neighbors $ (replicate 30 Nothing ) ++ makeHarbors h

makeHarbors :: [Harbor] -> [Maybe Harbor]
makeHarbors [] = []
makeHarbors (h:tl) = [Just h] ++ (makeHarborsAux tl) ++ [Just h]
  where
    makeHarborsAux (h1:h2:h3:h4:h5:h6:h7:h8:_) = 
      Nothing: Just h1: Just h1: Nothing: Nothing: Just h2: Just h2:
      Nothing: Just h3: Just h3: Nothing: Just h4: Just h4: Nothing:
      Nothing: Just h5: Just h5: Nothing: Just h6: Just h6: Nothing:
      Just h7: Just h7: Nothing: Nothing: Just h8: Just h8: [Nothing]
    -- these are just to make the compiler happy
    makeHarborsAux _ = []

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

adjacentCorners :: CornerLocation -> [CornerLocation]
adjacentCorners (CornerLocation (x, y, True)) =
  mapMaybe mk [(x, y-1, False), (x+1, y-1, False), (x+1, y-2, False)]
    where mk (a, b, c) = makeCornerLocation a b c
adjacentCorners (CornerLocation (x, y, False)) =
  mapMaybe mk [(x, y+1, True), (x-1, y+1, True), (x-1, y+2, True)]
    where mk (a, b, c) = makeCornerLocation a b c

getTile :: Board -> TileLocation -> Tile
getTile (Board ts _) l = fromJust $ Map.lookup l ts

desert :: Board -> TileLocation
desert = foldr des err . Map.toList . tiles
           where des (l, Desert) _ = l
                 des _ acc = acc
                 err = error "desert definitely exists"

defaultBuildingLocations :: [CornerLocation]
defaultBuildingLocations = map CornerLocation
   [(1,1,True),(0,2,True),(-1,2,True),(-2,2,True),(-2,1,True),(-1,0,True),(0,-1,True),(2,-2,False)]


defaultRoadLocations :: [(CornerLocation, CornerLocation)]
defaultRoadLocations = map mkLoc [((2,-1,False),(1,1,True)),((1,0,False),(0,2,True)),((0,1,False),(-1,2,True)),((-1,1,False),(-2,2,True)),((-1,0,False),(-2,1,True)),((-1,-1,False),(-1,0,True)),((0,-1,True),(1,-2,False)),((1,-1,True),(2,-2,False))]
  where mkLoc (x1, x2) = (CornerLocation x1, CornerLocation x2)


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
cornerToAxial (CornerLocation c) = c
