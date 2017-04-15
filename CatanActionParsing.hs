module CatanActionParsing (getNextAction, handleAction) where

import Control.Applicative
import Data.Char(isSpace)
import Data.Maybe(isJust, fromJust)
import Control.Monad(when, unless)
import Control.Monad.IO.Class(liftIO, MonadIO)
import qualified Parser as P
import qualified ParserCombinators as P
import CatanActions
import CatanTypes
import CatanBoard
constP :: String -> a -> P.Parser a
constP s v = const v <$> P.string s

wsP :: P.Parser a -> P.Parser a
wsP p =  many (P.satisfy isSpace) *> p <* many (P.satisfy isSpace)

cornerP :: P.Parser CornerLocation
cornerP = wsP $ maybeP $ makeCornerLocation <$> wsP P.int <*> wsP P.int

maybeP :: P.Parser (Maybe a) -> P.Parser a
maybeP = fmap fromJust . P.ensure isJust

tileP :: P.Parser TileLocation
tileP = wsP $ maybeP $ makeTileLocation <$> wsP P.int <*> wsP P.int

roadP :: P.Parser PlayerAction
roadP = P.string "road" *> (BuildRoad <$> cornerP <*> cornerP)

cityP :: P.Parser PlayerAction
cityP = P.string "city" *> (BuildCity <$> cornerP)

settP :: P.Parser PlayerAction
settP = P.string "sett" *> (BuildSettlement <$> cornerP)

cardP :: P.Parser PlayerAction
cardP = constP "card" BuyCard

progressP :: P.Parser ProgressCard
progressP = P.choice [constP "Monopoly" Monopoly,
                     constP "YearOfPlenty" YearOfPlenty,
                     constP "RoadBuilding" RoadBuilding]

playCardP :: P.Parser PlayerAction
playCardP =  P.string "progress" *> (PlayCard <$> progressP)

resourceP :: P.Parser Resource
resourceP = P.choice [constP "Wool" Wool,
                      constP "Ore" Ore,
                      constP "Lumber" Lumber,
                      constP "Grain" Grain,
                      constP "Brick" Brick]

colorP :: P.Parser Color
colorP = P.choice [constP "Red" Red,
                   constP "Blue" Blue,
                   constP "White" White,
                   constP "Orange" Orange]

bankP :: P.Parser PlayerAction
bankP = P.string "bank" *> (TradeWithBank <$> wsP resourceP <*> wsP resourceP <*> wsP P.int)

resourcesP :: P.Parser [Resource]
resourcesP = resourceP `P.sepBy` many (P.satisfy isSpace)

tradeP :: P.Parser PlayerAction
tradeP = P.string "trade" *> (TradeWithPlayer <$> resourcesP <*> colorP <*> resourcesP)

actionP :: P.Parser PlayerAction
actionP = P.choice [roadP, cityP, settP, cardP, playCardP, bankP, tradeP, constP "n" EndTurn, constP "q" EndGame]

help :: String
help = unlines
       ["BuildRoad: road <corner1 x> <corner1 r> <corner2 x> <corner2 r>",
        "Build City: city <corner x> <corner r>",
        "Build Settlement: sett <corner x> <corner r>",
        "Buy Card: card",
        "Play Card: progress <card>",
        "Trade with Bank or harbor: bank <Resource> <Resource> <Int>",
        "Trade with player: trade <Resources> <Color> <Resources>",
        "Next Turn: n",
        "End Game: q"]

getNextAction :: IO PlayerAction
getNextAction = do
    action <- getLine
    case P.parse actionP action of
        Left _ -> putStrLn help >> getNextAction
        Right act -> return act

ignoreI :: MonadIO m => m Int -> m Bool
ignoreI m = do
  res <- m
  when (res == 0) $ liftIO $ putStrLn "not successful"
  return False

ignoreB :: MonadIO m => m Bool -> m Bool
ignoreB m = do
  res <- m
  unless res $ liftIO $ putStrLn "not successful"
  return False

handleAction :: PlayerAction -> MyState Bool
handleAction (BuildRoad l1 l2) = ignoreB $ buildRoad l1 l2
handleAction (BuildCity l) = ignoreB $ buildCity l
handleAction (BuildSettlement l) = ignoreB $ buildSett l
handleAction (PlayCard c) = ignoreB $ playCard c
handleAction BuyCard = ignoreB buyCard
handleAction (TradeWithBank r1 r2 i) = ignoreI $ genericTrade r1 r2 i
handleAction (TradeWithPlayer rs1 c rs2) = ignoreB $ playerTrade rs1 c rs2
handleAction EndTurn = return True
handleAction EndGame = error "over"
