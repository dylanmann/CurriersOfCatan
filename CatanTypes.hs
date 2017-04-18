{-# LANGUAGE ConstraintKinds, RecordWildCards #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanTypes(ProgressCard(..),
                  DevCard(..),
                  devCards,

                  Building(..),
                  buildingColor,
                  buildingLoc,
                  buildingTileLocs,
                  buildingTiles,
                  Name,

                  Resources,
                  getResource,
                  updResource,
                  allResources,
                  emptyResources,

                  Players,
                  Player(..),
                  Color(..),
                  getPlayer,
                  allPlayers,
                  updPlayer,
                  makePlayers,
                  nextPlayer,
                  validPlayer,

                  Paths,
                  --pathsOfColor,

                  Roads,
                  newLongestRoad,
                  Game(..),
                  produces)
                  where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromMaybe, fromJust)
import Data.List.Split
import Data.List as List
import Data.Function(on)
import Data.Ord
-- import Text.PrettyPrint (Doc)
-- import qualified Text.PrettyPrint as PP
import CatanBoard

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

validPlayer :: Player -> Bool
validPlayer p = all (>= 0) rs where
  Resources rs = resources p

buildingColor :: Building -> Color
buildingColor (City c _) = c
buildingColor (Settlement c _) = c

buildingLoc :: Building -> CornerLocation
buildingLoc (City _ l) = l
buildingLoc (Settlement _ l) = l

buildingTileLocs :: Board -> Building -> [TileLocation]
buildingTileLocs b (City _ l)       = rewardLocs $ getCorner b l
buildingTileLocs b (Settlement _ l) = rewardLocs $ getCorner b l

buildingTiles :: Board -> Building -> [Tile]
buildingTiles b (City _ l)       = rewardTiles b $ getCorner b l
buildingTiles b (Settlement _ l) = rewardTiles b $ getCorner b l

type Name = String

data Resources = Resources (Map Resource Int)
      deriving (Read, Eq)
emptyResources :: Resources
emptyResources = Resources $ foldr (\x -> Map.insert (toEnum x) 0) Map.empty [0..4]


-- Players can change with the state
data Player = Player {name::Name,
                      resources::Resources,
                      cards::[DevCard]}
    deriving (Read, Eq)

newPlayer :: Name -> Player
newPlayer n = Player n emptyResources []

makePlayers :: [(Color,Name)] -> Players
makePlayers = Players . foldr add Map.empty
    where add (c, n) = Map.insert c (newPlayer n)

data Players = Players (Map Color Player)
  deriving (Read, Eq)

type Paths = Map Color [[CornerLocation]]

edges :: Color -> CornerLocation -> Roads -> [CornerLocation]
edges c l = foldr step []
  where step :: (CornerLocation, CornerLocation, Color) -> [CornerLocation] -> [CornerLocation]
        step (l1, l2, c1) acc | c == c1 && l1 == l = l2 : acc
        step (l1, l2, c1) acc | c == c1 && l2 == l = l1 : acc
        step _ acc                                 = acc

uninterruptedLength :: Color -> Roads -> [CornerLocation] -> Int
uninterruptedLength _ _ [] = 0
uninterruptedLength c r (x:xs) | stop x = 1
                               | otherwise = 1 + uninterruptedLength c r xs
  where stop hd = any (\(l1, l2, c1) -> c1 /= c && (l1 == hd || l2 == hd)) r


longestPathFrom :: Roads -> Color -> CornerLocation -> CornerLocation -> [CornerLocation]
longestPathFrom = aux [] where
  aux seen roads c start end
    | start == end = [start]
    | notIn = []
    | otherwise =
      maximumBy (comparing $ uninterruptedLength c roads) $
                []:[start : (aux (start:seen) roads c e end) |
                      e <- edges c start roads, e `notElem` seen]
      where notIn = all (\(l1, l2, c1) -> c1 /= c || (l1 /= start && l2 /= start
                                                      && l1 /= end && l2 /= end)) roads


longestPath :: Color -> Roads -> [CornerLocation]
longestPath c r = maximumBy (comparing $ uninterruptedLength c r)
              [longestPathFrom r c l1 l2 | l1 <- cornerIndices, l2 <- cornerIndices]

newLongestRoad :: Maybe Color -> Roads -> Maybe Color
newLongestRoad oldWinner roads =
  let ps = foldr (\c -> Map.insert c (longestPath c roads)) Map.empty colors
      lengths = Map.map length ps
      longest = Map.findMax lengths
      in
  case oldWinner of
    _       | snd longest < 5  -> Nothing
    Nothing | snd longest >= 5 -> Just $ fst longest
    Just c  | snd longest > Map.findWithDefault 0 c lengths -> Just $ fst longest
    m -> m
  where colors = [Blue, Red, White, Orange]


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
    deriving (Read)

-- lol these ended up working out well
getResource :: Resource -> Resources -> Int
getResource r (Resources rs) = fromMaybe 0 $ Map.lookup r rs

updResource :: (Int -> Int) -> Resource -> Resources -> Resources
updResource f r (Resources rs) = Resources $ Map.adjust f r rs

updPlayer :: (Player -> Player) -> Color -> Players -> Players
updPlayer f c (Players ps) = Players $ Map.adjust f c ps

getPlayer :: Color -> Players -> Player
getPlayer c (Players ps) = fromJust $ Map.lookup c ps

allPlayers :: Players -> [(Color, Player)]
allPlayers (Players ps) = Map.toList ps

allResources :: Player -> [Resource]
allResources p = concatMap comb (Map.toList rs)
  where (Resources rs) = resources p
        comb (r, amount) = replicate amount r

produces :: Token -> Tile -> Maybe Resource
produces roll (Paying t token) | roll == token =
  (Just . toEnum . fromEnum) t
produces _ _ = Nothing

nextPlayer :: Color -> Color
nextPlayer White = Blue
nextPlayer c = succ c


instance Show Game where
  show Game{..} = unlines
                ["Game { board = " ++ "board" ++ ",",
                 "       players = " ++ show players ++ ",",
                 "       roads = " ++ show roads ++ ",",
                 "       buildings = " ++ show buildings ++ ",",
                 "       robberTile = " ++ show robberTile ++ ",",
                 "       longestRoad = " ++ show longestRoad ++ ",",
                 "       largestArmy = " ++ show largestArmy ++ ",",
                 "       deck = " ++ "deck" ++ ",",
                 "       currentPlayer = " ++ show currentPlayer,
                 "}"]

instance Show Players where
  show (Players ps) = unlines $ map show $ Map.toList ps

instance Show Resources where
  show (Resources rs) = show $ Map.toList rs

instance Show Player where
  show Player{..} = unlines ["Player { name = " ++ show name,
                    "         resources = " ++ show resources,
                    "         cards = " ++ show cards,
                    "}"]
