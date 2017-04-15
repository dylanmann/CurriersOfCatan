module CatanActionParsing (getNextAction, handleAction) where

import Control.Applicative
import Data.Char(isSpace)
import Data.Maybe(isJust, fromJust)

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

getNextAction :: IO PlayerAction
getNextAction = do
    action <- getLine
    case P.parse actionP action of
        Left _ -> putStrLn "not a command" >> getNextAction
        Right act -> return act

ignore ::Monad m => m a -> m Bool
ignore m = m >> return False

handleAction :: PlayerAction -> MyState Bool
handleAction (BuildRoad l1 l2) = ignore $ buildRoad l1 l2
handleAction (BuildCity l) = ignore $ buildCity l
handleAction (BuildSettlement l) = ignore $ buildSett l
handleAction (PlayCard c) = ignore $ playCard c
handleAction BuyCard = ignore buyCard
handleAction (TradeWithBank r1 r2 i) = ignore $ genericTrade r1 r2 i
handleAction (TradeWithPlayer rs1 c rs2) = ignore $ playerTrade rs1 c rs2
handleAction EndTurn = return True
handleAction EndGame = error "over"
