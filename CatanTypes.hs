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

edges :: [CornerLocation] -> Color -> CornerLocation -> Roads -> [CornerLocation]
edges excluded c l = foldr step []
  where step :: (CornerLocation, CornerLocation, Color) -> [CornerLocation] -> [CornerLocation]
        step (l1, l2, c1) _   | c /= c1 && (l1 == l || l2 == l) = []
        step (l1, l2, c1) acc | l2 == l && l1 `elem` excluded = acc
        step (l1, l2, c1) acc | l1 == l && l2 `elem` excluded = acc
        step (l1, l2, c1) acc | c == c1 && l1 == l = l2 : acc
        step (l1, l2, c1) acc | c == c1 && l2 == l = l1 : acc
        step _ acc                                 = acc


longestPathFrom :: Roads -> Color -> CornerLocation -> CornerLocation -> [CornerLocation]
longestPathFrom = aux [] where
  aux excluded roads c start end
    | start == end = [start]
    | notIn = []
    | otherwise =
      maximumBy (comparing length) $
                []:[ start : (aux (start:excluded) roads c e end) |
                      e <- edges excluded c start roads, not$null e]
      where notIn = all (\(l1, l2, c1) -> c1 /= c || (l1 /= start && l2 /= start
                                                      && l1 /= end && l2 /= end)) roads


longestPath :: Color -> Roads -> [CornerLocation]
longestPath c r = maximumBy (comparing length)
              [longestPathFrom r c l1 l2 | l1 <- cornerIndices, l2 <- cornerIndices]

newLongestPath :: Maybe Color -> Roads -> Maybe Color
newLongestPath oldWinner roads =
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




-- pathsOfColor :: Color -> Paths -> [[CornerLocation]]
-- pathsOfColor c = fromJust . Map.lookup c

-- pathsNotOfColor :: Color -> Paths -> [(Color, [[CornerLocation]])]
-- pathsNotOfColor c p = filter ((c /=) . fst) (Map.toList p)

-- splitOnLocation :: CornerLocation -> [CornerLocation] ->
--                       [[CornerLocation]]
-- splitOnLocation l path = case (split . keepDelimsL . oneOf) [l] path of
--                               hd:tl -> hd : map (l:) (filter (not.null) tl)
--                               p     -> p

-- addToLocation l1 l2 (hd:path) | hd == l1 = l2 : hd : path

-- addRoadToPaths :: (CornerLocation, CornerLocation, Color) -> Paths -> Paths
-- addRoadToPaths (l1, l2, c) paths =
--   let sameC = pathsOfColor c paths
--       splitAllOn l = concatMap (splitOnLocation l)
--       newSameC = List.nub (splitAllOn l1 sameC ++ splitAllOn l2 sameC ++ sameC)
--       otherC = map (splitAllOn l1 . snd) $
--                map (splitAllOn l2 . snd) $
--                pathsNotOfColor c paths
--   in
--   undefined




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
                  currentPlayer :: Color,
                  paths         :: Paths}
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
