{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanTypes where

import Data.Maybe(mapMaybe)
import qualified Data.List as List
import qualified Control.Monad.State as S
import Control.Monad.State(State)

data Color = Blue | Red | Orange | White
    deriving (Enum, Read, Show, Eq)

data ProgressCard = NotImplemented
    deriving (Eq, Read, Show)

data DevCard = VictoryPoint
             | Knight
             | Progress ProgressCard
    deriving (Read, Show, Eq)

data Token = Two
           | Three
           | Four
           | Five
           | Six
           | Eight
           | Nine
           | Ten
           | Eleven
           | Twelve
    deriving (Enum, Read, Show, Eq)

data Resource = Brick
              | Lumber
              | Ore
              | Grain
              | Wool
    deriving (Enum, Read, Show, Eq, Ord)

data Terrain = Hills     -- produce brick
             | Forest    -- produce lumber
             | Mountains -- produce ore
             | Fields    -- produce grain
             | Pasture   -- produces Wool
             | Desert    -- produces nothing
    deriving (Enum, Read, Show, Eq)

type TileLocation = (Int, Int)
type Tile = (Terrain, Token, TileLocation)

data Reward = TwoTiles Tile Tile
            | ThreeTiles Tile Tile Tile
            | GenericHarbor Tile Tile
            | SpecialHarbor Tile Tile Resource
    deriving (Read, Show, Eq)

type CornerLocation = (Int, Int)
type Corner = (Reward, CornerLocation)

data Building = Settlement Color Corner
              | City       Color Corner
    deriving (Read, Show, Eq)

type Name = String

type Resources = (Int, Int, Int, Int, Int)

-- instance Functor ((,,,,) Int Int Int Int) where
--     fmap f (q, w, e, r, t) = (f q, f w, f e, f r, f t)

mapR :: (Int -> Int) -> Resources -> Resources
mapR f (q, w, e, r, t) = (f q, f w, f e, f r, f t)

-- Players can change with the state
data Player = MkPlayer {id::Color,
                        name::Name,
                        resources::Resources,
                        cards::[DevCard]}
    deriving (Read, Show, Eq)

data Players = MkPlayers {redPlayer    :: Player,
                          bluePlayer   :: Player,
                          orangePlayer :: Player,
                          whitePlayer  :: Player}
    deriving (Eq, Show, Read)

-- Roads can change with state
type Roads = [(Corner, Corner, Color)]
type Tiles = ((Tile, Tile, Tile),
              (Tile, Tile, Tile, Tile),
              (Tile, Tile, Tile, Tile, Tile),
              (Tile, Tile, Tile, Tile),
              (Tile, Tile, Tile))

type Corners = ((Corner, Corner, Corner, Corner, Corner, Corner, Corner),
                (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
                (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
                (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
                (Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner, Corner),
                (Corner, Corner, Corner, Corner, Corner, Corner, Corner))

type Board = (Tiles, Corners)
-- Game is going to be updated via the State monad throughout the
-- execution of the program
data Game = MkGame {board     :: Board,
                    players   :: Players,
                    roads     :: Roads,
                    buildings :: [Building],
                    robber    :: TileLocation}
    deriving (Read, Show, Eq)


{- ======================== HELPER FUNCTIONS =========================== -}


allResources :: [Resource]
allResources = [Brick, Lumber, Ore, Grain, Wool]

produces :: Tile -> Maybe Resource
produces (Hills, _, _)     = Just Brick
produces (Forest, _, _)    = Just Lumber
produces (Mountains, _, _) = Just Ore
produces (Fields, _, _)    = Just Grain
produces (Pasture, _, _)   = Just Wool
produces (Desert, _, _)    = Nothing

-- TODO is there a nicer way
reduceResources :: [Resource] -> Resources
reduceResources l = case (map (\x -> List.length x - 1)) .
              List.group .
              List.sort $
              (++ allResources) l of
    [q,w,e,r,t] -> (q, w, e, r, t)
    _           -> error "impossible because invariants"

cornerRewards :: Token -> Corner -> Resources
cornerRewards tok (r,_) = reduceResources $ mapMaybe produces (ts r) where
    ts (TwoTiles t1 t2)        = filter payingTile [t1, t2]
    ts (ThreeTiles t1 t2 t3)   = filter payingTile [t1, t2, t3]
    ts (GenericHarbor t1 t2)   = filter payingTile [t1, t2]
    ts (SpecialHarbor t1 t2 _) = filter payingTile [t1, t2]
    payingTile (_, t, _)       = tok == t

buildingRewards :: Token -> Building -> (Color, Resources)
buildingRewards tok (Settlement c loc) = (c, cornerRewards tok loc)
buildingRewards tok (City c loc)       = (c, mapR (* 2) $ cornerRewards tok loc)

addResources :: Resources -> Player -> Player
addResources r p = p {resources = combineResources (resources p) r} where
    combineResources (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
        (a1+a2, b1+b2, c1+c2, d1+d2, e1+e2)

updatePlayer :: (Player -> Player) -> Color -> Players -> Players
updatePlayer f Blue ps   = ps { bluePlayer = f $ bluePlayer ps }
updatePlayer f Red ps    = ps { redPlayer = f $ redPlayer ps }
updatePlayer f Orange ps = ps { orangePlayer = f $ orangePlayer ps }
updatePlayer f White ps  = ps { whitePlayer = f $ whitePlayer ps }

allocateRewards :: Token -> State Game ()
allocateRewards tok = do
    game <- S.get
    let bs = buildings game
        rewards = map (buildingRewards tok) bs
    S.put (foldr step game rewards)
    where step (c, r) g =
            let newPlayers = updatePlayer (addResources r) c (players g) in
            g { players = newPlayers }
