{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module CatanActionParsing (ioThread) where

import Control.Applicative
import Data.Char(isSpace)
import Data.Maybe(isJust, fromJust)
import Control.Concurrent.MVar

import qualified Parser as P
import qualified ParserCombinators as P
import CatanTypes


constP :: String -> a -> P.Parser a
constP s v = const v <$> P.string s

wsP :: P.Parser a -> P.Parser a
wsP p = many (P.satisfy isSpace) *> p <* many (P.satisfy isSpace)

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
       ["BuildRoad: road <Corner1 x> <Corner1 r> <Corner2 x> <Corner2 r>",
        "Build City: city <Corner x> <Corner r>",
        "Build Settlement: sett <Corner x> <Corner r>",
        "Buy Card: card",
        "Play Card: progress <Card>",
        "Trade with Bank or harbor: bank <Resource> <Resource> <Int>",
        "Trade with player: trade <Resources> <Color> <Resources>",
        "Next Turn: n",
        "End Game: q",
        "Color: Red Blue Orange White",
        "Resource: Wool Lumber Ore Grain Brick",
        "Resources is a space separated list of Resources"]

prompt :: String
prompt = " >> "

getNextAction :: Name -> IO PlayerAction
getNextAction n = do
    putStr $ n ++ prompt
    action <- getLine
    case P.parse actionP action of
        Left _ -> putStrLn help >> getNextAction n
        Right act -> return act

ioThread :: CatanVars -> IO ()
ioThread CatanVars{..} = --do
    -- print "What is your name?: "
    -- name <- getLine
    -- putMVar nameVar name
    go where go = do r <- takeMVar requestVar
                     case r of
                        NextMove -> takeMVar nameVar >>= getNextAction >>= putMVar actionVar
                        MoveRobber -> promptForRobber >>= putMVar robberVar
                        StealFrom ps -> getChoiceFrom ps >>= putMVar colorVar
                     go

promptForRobber :: IO TileLocation
promptForRobber = do
    putStr "where do you want to put the robber?"
    action <- getLine
    case P.parse tileP action of
        Left _ -> putStrLn "invalid location" >> promptForRobber
        Right tile -> return tile

getChoiceFrom :: [(Name, Color)] -> IO Color
getChoiceFrom l = do let p = P.choice $ map (uncurry constP) l
                     choice <- getLine
                     case P.parse p choice of
                         Left _ -> putStrLn "invalid name" >>
                                   getChoiceFrom l
                         Right c -> return c
