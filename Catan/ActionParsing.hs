{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module ActionParsing (commandLineInput) where

import Control.Applicative
import Data.Char(isSpace)
import Data.Maybe(isJust, fromJust)
import Control.Concurrent.MVar
import Control.Concurrent(threadDelay)

import qualified ParserCombinators as P
import Types


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
devP :: P.Parser DevCard
devP = wsP $ P.choice [Progress <$> wsP progressP,
                       constP "Knight" Knight]

playCardP :: P.Parser PlayerAction
playCardP = wsP $ P.string "play" *> (PlayCard <$> devP)

resourceP :: P.Parser Resource
resourceP = wsP $ P.choice [constP "Wool" Wool,
                      constP "Ore" Ore,
                      constP "Lumber" Lumber,
                      constP "Grain" Grain,
                      constP "Brick" Brick]

colorP :: P.Parser Color
colorP = wsP $ P.choice [constP "Red" Red,
                   constP "Blue" Blue,
                   constP "White" White,
                   constP "Orange" Orange]

rbP :: Color -> P.Parser (Road, Road)
rbP c = (,) <$> (mkRoad <$> cornerP <*> cornerP) <*> (mkRoad <$> cornerP <*> cornerP)
    where mkRoad l1 l2 = (l1, l2, c)

bankP :: P.Parser PlayerAction
bankP = P.string "bank" *> (TradeWithBank <$> wsP resourceP <*> wsP resourceP <*> wsP P.int)

resourcesP :: P.Parser [Resource]
resourcesP = resourceP `P.sepBy` many (P.satisfy isSpace)

tradeP :: P.Parser PlayerAction
tradeP = P.string "trade" *> (TradeWithPlayer <$> resourcesP <*> colorP <*> resourcesP)

actionP :: P.Parser PlayerAction
actionP = P.choice [roadP, cityP, settP, cardP, playCardP, bankP, tradeP,
 constP "n" EndTurn, constP "q" EndGame, cheatP]

cheatP :: P.Parser PlayerAction
cheatP = P.string "$$$" *> (Cheat <$> wsP resourcesP)

help :: String
help = unlines
       ["BuildRoad: road <Corner1 x> <Corner1 r> <Corner2 x> <Corner2 r>",
        "Build City: city <Corner x> <Corner r>",
        "Build Settlement: sett <Corner x> <Corner r>",
        "Buy Card: card",
        "Play Card: play <Card>",
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

commandLineInput :: Color -> CatanMVars -> IO ()
commandLineInput c CatanMVars{..} = --do
    -- print "What is your name?: "
    -- name <- getLine
    -- putMVar nameVar name
    go where go = do
              threadDelay 500
              r <- takeMVar requestVar
              roll <- tryTakeMVar rollVar
              case roll of
                Just i -> putStr "roll: " >> print i
                Nothing -> return ()
              case r of
                 NextMove ->           do game <- takeMVar gameVar
                                          print game
                                          n <- takeMVar nameVar
                                          a <- getNextAction n
                                          putMVar actionVar a

                 MoveRobber ->         do tile <- promptForRobber
                                          putMVar robberVar tile
                                          takeMVar gameVar >>= print

                 StealFrom ps ->       do color <- getChoiceFrom ps
                                          putMVar colorVar color

                 RoadBuildingChoice -> do roads <- promptForRoadBuilder c
                                          putMVar roadVar roads

                 YearOfPlentyChoice -> do rs <- promptForYOP
                                          putMVar yopVar rs

                 MonopolyChoice ->     do res <- promptForMonopoly
                                          putMVar monopolyVar res
              go

promptForRobber :: IO TileLocation
promptForRobber = do
    putStrLn "where do you want to put the robber?"
    action <- getLine
    case P.parse tileP action of
        Left _ -> putStrLn "invalid location" >> promptForRobber
        Right tile -> return tile

promptForMonopoly :: IO Resource
promptForMonopoly = do
    putStrLn "What resource do you want to monopolize?"
    putStr prompt
    action <- getLine
    case P.parse resourceP action of
        Left _ -> putStrLn "invalid resource" >> promptForMonopoly
        Right r -> return r

promptForYOP :: IO (Resource, Resource)
promptForYOP = do
    putStrLn "Which two resources do you want?"
    putStr prompt
    action <- getLine
    case P.parse resourcesP action of
        Right [r1, r2] -> return (r1, r2)
        _ -> putStrLn "must be two resources" >> promptForYOP

promptForRoadBuilder :: Color -> IO (Road, Road)
promptForRoadBuilder c = do
    putStrLn "where do you want to build two roads?"
    putStr prompt
    action <- getLine
    case P.parse (rbP c) action of
        Left _ -> putStrLn "invalid roads" >> promptForRoadBuilder c
        Right rs -> return rs

getChoiceFrom :: [(Name, Color)] -> IO Color
getChoiceFrom l = do putStr "options to steal from: "
                     print (unwords (map fst l))
                     let p = P.choice $ map (uncurry constP) l
                     choice <- getLine
                     case P.parse p choice of
                         Left _ -> putStrLn "invalid name" >>
                                   getChoiceFrom l
                         Right c -> return c
