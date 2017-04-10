{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}

module CatanTypes where

data Color = Blue | Red | Orange | White
    deriving (Enum, Read, Show, Eq)

data ProgressCard = RoadBuilding
                  | YearOfPlenty
                  | Monopoly
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

tokenOrder :: [Token]
tokenOrder = [Five, Two, Six, Three, Eight, Ten, Nine, Twelve, Eleven, Four,
              Eight, Ten, Nine, Four, Five, Six, Three, Eleven]

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
data Player = MkPlayer {name::Name,
                        resources::Resources,
                        cards::[DevCard]}
    deriving (Read, Show, Eq)

data Players = MkPlayers {redPlayer    :: Player,
                          bluePlayer   :: Player,
                          orangePlayer :: Player,
                          whitePlayer  :: Player}
    deriving (Eq, Show, Read)

-- Roads can change with state
type Roads = [(CornerLocation, CornerLocation, Color)]
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
data Game = MkGame {board       :: Board,
                    players     :: Players,
                    roads       :: Roads,
                    buildings   :: [Building],
                    robber      :: TileLocation,
                    longestRoad :: Maybe Color,
                    largestArmy :: Maybe Color,
                    deck        :: [DevCard] }
    deriving (Read, Show, Eq)
