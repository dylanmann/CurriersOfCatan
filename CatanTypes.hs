{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanTypes where

import CatanBoard
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromMaybe, fromJust)

data Color = Blue | Red | Orange | White
    deriving (Enum, Read, Show, Eq, Ord)

allColors :: [Color]
allColors = [Blue, Red, Orange, White]

data ProgressCard = RoadBuilding
                  | YearOfPlenty
                  | Monopoly
    deriving (Eq, Read, Show)

data DevCard = VictoryPoint
             | Knight
             | Progress ProgressCard
    deriving (Read, Show, Eq)

devCards :: [DevCard]
devCards = replicate 14 Knight ++ replicate 5 VictoryPoint ++ replicate 2 (Progress RoadBuilding) ++ replicate 2 (Progress YearOfPlenty) ++ replicate 2 (Progress Monopoly)


buildingColor :: Building -> Color
buildingColor (City c _) = c
buildingColor (Settlement c _) = c

buildingLoc :: Building -> CornerLocation
buildingLoc (City _ l) = l
buildingLoc (Settlement _ l) = l

data Building = Settlement Color CornerLocation
              | City       Color CornerLocation
    deriving (Read, Show, Eq)

buildingTileLocs :: Board -> Building -> [TileLocation]
buildingTileLocs b (City _ l)       = rewardLocs $ getCorner b l
buildingTileLocs b (Settlement _ l) = rewardLocs $ getCorner b l

buildingTiles :: Board -> Building -> [Tile]
buildingTiles b (City _ l)       = rewardTiles b $ getCorner b l
buildingTiles b (Settlement _ l) = rewardTiles b $ getCorner b l

type Name = String

type Resources = Map Resource Int
emptyResources :: Resources
emptyResources = Map.empty

addResources :: Resources -> Player -> Player
addResources r p = p {resources = Map.union r $ resources p}


-- Players can change with the state
data Player = Player {name::Name,
                        resources::Resources,
                        cards::[DevCard]}
    deriving (Read, Show, Eq)

newPlayer :: Name -> Player
newPlayer n = Player n emptyResources []

makePlayers :: [(Color,Name)] -> Players
makePlayers = foldr add Map.empty
    where add (c, n) = Map.insert c (newPlayer n)

type Players = Map Color Player

-- Roads can change with state
type Roads = [(CornerLocation, CornerLocation, Color)]

-- Game is going to be updated via the State monad throughout the
-- execution of the program
data Game = Game {board         :: Board,
                  players       :: Players,
                  roads         :: Roads,
                  buildings     :: [Building],
                  robberTile    :: TileLocation,
                  longestRoad   :: Maybe Color,
                  largestArmy   :: Maybe Color,
                  deck          :: [DevCard],
                  currentPlayer :: Color}
    deriving (Read, Show, Eq)


-- lol these ended up working out well
getResource :: Resource -> Resources -> Int
getResource r = fromMaybe 0 . Map.lookup r

setResource :: Resource -> Int -> Resources -> Resources
setResource = Map.insert

updResource :: (Int -> Int) -> Resource -> Resources -> Resources
updResource = Map.adjust

updPlayer :: (Player -> Player) -> Color -> Players -> Players
updPlayer = Map.adjust

getPlayer :: Color -> Players -> Player
getPlayer c = fromJust . Map.lookup c

allPlayers :: Players -> [(Color, Player)]
allPlayers = Map.toList

allResources :: Player -> [Resource]
allResources p = concatMap comb (Map.toList rs)
  where rs = resources p
        comb (r, amount) = replicate amount r

produces :: Token -> Tile -> Maybe Resource
produces roll (Paying t token) | roll == token =
  (Just . toEnum . fromEnum) t
produces _ _ = Nothing
