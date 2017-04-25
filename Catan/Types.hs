{-|
Module      : Catan.Board
Description : Contains all types used by the game and their methods
Copyright   : (c) Dylan Mann, David Cao 2017
License     : GPL-3
Maintainer  : mannd@seas.upenn.edu
Stability   : experimental
Portability : POSIX

This is a module where all game primitives are defined.  The game is based on a
record that will be maintained and updated by a StateT.

-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# LANGUAGE ConstraintKinds, RecordWildCards #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module Types(ProgressCard(..),
             DevCard(..),
             devCards,

             Building(..),
             defaultBuildings,
             buildingColor,
             buildingLoc,
             buildingTileLocs,
             buildingTiles,
             Name,

             Resources,
             produces,
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

             Road,
             Roads,
             mkRoad,
             getRoad,
             defaultRoads,
             newLongestRoad,

             Game(..),

             CatanMVars(..),
             PlayerAction(..),

             module Board)
             where

import qualified Data.Map as Map
import Data.Map(Map)
import Data.Maybe(fromMaybe, fromJust)
import Data.List as List
import Data.Ord
import Board
import Control.Concurrent.MVar.Lifted

data Color = Blue | Red | Orange | White
    deriving (Enum, Read, Show, Eq, Ord)

data Road = Road (CornerLocation, CornerLocation, Color)
  deriving(Read, Show, Eq)

mkRoad :: (CornerLocation, CornerLocation, Color) -> Maybe Road
mkRoad r@(cl1, cl2, _) = if cl1 `elem` adjacentCorners cl2 then
  Just (Road r) else Nothing

getRoad :: Road -> (CornerLocation, CornerLocation, Color)
getRoad (Road r) = r

type Roads = [Road]

data Building = Settlement Color CornerLocation
              | City       Color CornerLocation
    deriving (Read, Show, Eq)

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


data Player = Player {name::Name,
                      resources::Resources,
                      cards    ::[DevCard],
                      knights  :: Int}
    deriving (Read)

newPlayer :: Name -> Player
newPlayer n = Player n emptyResources [] 0

makePlayers :: [(Color,Name)] -> Players
makePlayers l = Players (foldr add Map.empty l)
    where add (c, n) m = let p = newPlayer n in
                         Map.insert c p m

data Players = Players (Map Color Player)
  deriving (Read)

edges :: Color -> CornerLocation -> Roads -> [CornerLocation]
edges c l = foldr step []
  where step :: Road -> [CornerLocation] -> [CornerLocation]
        step (Road (l1, l2, c1)) acc | c == c1 && l1 == l = l2 : acc
        step (Road (l1, l2, c1)) acc | c == c1 && l2 == l = l1 : acc
        step _ acc                                 = acc

uninterruptedLength :: Color -> Roads -> [CornerLocation] -> Int
uninterruptedLength _ _ [] = 0
uninterruptedLength c r (x:xs) | stop x = 1
                               | otherwise = 1 + uninterruptedLength c r xs
  where stop hd = any (\(Road (l1, l2, c1)) -> c1 /= c && (l1 == hd || l2 == hd)) r


longestPathFrom :: Roads -> Color -> CornerLocation -> CornerLocation -> [CornerLocation]
longestPathFrom = aux [] where
  aux seen roads c start end
    | start == end = [start]
    | notIn = []
    | otherwise =
      maximumBy (comparing $ uninterruptedLength c roads) $
                []:[start : aux (start : seen) roads c e end |
                      e <- edges c start roads, e `notElem` seen]
      where notIn = all (\(Road (l1, l2, c1)) -> c1 /= c || (l1 /= start && l2 /= start
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
    Nothing | snd longest >= 5 -> Just $ fst longest
    Just c  | snd longest > Map.findWithDefault 0 c lengths -> Just $ fst longest
    m -> m
  where colors = List.nub (map (\(Road (_,_,c)) -> c) roads)


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
                  errorMessage  :: Maybe String,
                  pendingCards  :: [DevCard],
                  mvars         :: CatanMVars}
    deriving (Read)

-- | methods for getting and setting resources and players, in case the
-- | implementation changes, so that the API doesn't have to
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


-- TODO: refactor so that we can play with 3 colors
nextPlayer :: Color -> Color
nextPlayer White = Blue
nextPlayer c = succ c

defaultColorOrder :: [Color]
defaultColorOrder = [White, Blue, Orange, Blue, Red, White, Red, Orange]

defaultBuildings :: [Building]
defaultBuildings = zipWith Settlement defaultColorOrder defaultBuildingLocations

defaultRoads :: Roads
defaultRoads = map Road $ (uncurry zip3) (unzip defaultRoadLocations) defaultColorOrder

instance Show Game where
  show Game{..} = unlines
                ["Game { board = " ++ "board" ++ ",",
                 "       players = " ++ show players ++ ",",
                 "       roads = " ++ show roads ++ ",",
                 "       buildings = " ++ show buildings ++ ",",
                 "       robberTile = " ++ show robberTile ++ ",",
                 "       longestRoad = " ++ show longestRoad ++ ",",
                 "       largestArmy = " ++ show largestArmy ++ ",",
                 "       deck = " ++ show deck ++ ",",
                 "       currentPlayer = " ++ show currentPlayer ++ ",",
                 "       error = " ++ show errorMessage,
                 "}"]

instance Show Players where
  show (Players ps) = unlines $ map show $ Map.toList ps

instance Show Resources where
  show (Resources rs) = show $ Map.toList rs

instance Show Player where
  show Player{..} = unlines ["Player { name = " ++ show name,
                    "         resources = " ++ show resources,
                    "         cards = " ++ show cards,
                    "         army = " ++ show knights,
                    "}"]

data CatanMVars = CatanMVars{nameVar     :: MVar Name,
                             actionVar   :: MVar PlayerAction,
                             robberVar   :: MVar TileLocation,
                             colorVar    :: MVar Color,
                             gameVar     :: MVar Game,
                             rollVar     :: MVar Int,
                             stealVar    :: MVar [(Name, Color)]}

-- | show and read instances so that the game can be shown
instance Show CatanMVars where
  show _ = "CatanMVars"

instance Read CatanMVars where
  readsPrec _ = undefined

-- | Player Actions that can be recieved by the server from the UI thread
-- | after a `NextMove` `Request`
data PlayerAction = BuildRoad CornerLocation CornerLocation
                  | BuildCity CornerLocation
                  | BuildSettlement CornerLocation
                  | PlayMonopoly Resource
                  | PlayYearOfPlenty Resource Resource
                  | PlayRoadBuilding Road Road
                  | PlayKnight
                  | BuyCard
                  | TradeWithBank Resource Resource Int
                  | TradeWithPlayer [Resource] Color [Resource]
                  | EndTurn
                  | EndGame
                  | Cheat [Resource]
                  deriving(Read, Show, Eq)
