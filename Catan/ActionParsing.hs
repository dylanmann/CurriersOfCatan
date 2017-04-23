{-|
Module      : Catan.ActionParsing
Description : Command line based user input thread for a game
Copyright   : (c) Dylan Mann, David Cao 2017
License     : GPL-3
Maintainer  : mannd@seas.upenn.edu
Stability   : experimental
Portability : POSIX

Module that exposes commandLineInput, a method that can be run as a thread
alongside the playGame method in GamePlay, which takes user actions as input and
returns them to the server thread to be processed with game logic.  Simply
prints the Game State after each request recieved.
-}

{-# OPTIONS_HADDOCK prune, ignore-exports, show-extensions #-}
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

devP :: Color -> P.Parser PlayerAction
devP c = wsP $ P.choice [constP "Monopoly" PlayMonopoly <*> resourceP,
                       constP "YearOfPlenty" PlayYearOfPlenty <*> wsP resourceP <*> wsP resourceP,
                       constP "Monopoly" (uncurry PlayRoadBuilding) <*> wsP (rbP c),
                       constP "Knight" PlayKnight]

playCardP :: Color -> P.Parser PlayerAction
playCardP c = wsP $ P.string "play " *> devP c

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
rbP c = liftA2 (,)
        (maybeP $ mr <$> cornerP <*> cornerP)
        (maybeP $ mr <$> cornerP <*> cornerP)
    where mr l1 l2 = mkRoad (l1, l2, c)

bankP :: P.Parser PlayerAction
bankP = P.string "bank" *> (TradeWithBank <$> wsP resourceP <*> wsP resourceP <*> wsP P.int)

resourcesP :: P.Parser [Resource]
resourcesP = resourceP `P.sepBy` many (P.satisfy isSpace)

tradeP :: P.Parser PlayerAction
tradeP = P.string "trade" *> (TradeWithPlayer <$> resourcesP <*> colorP <*> resourcesP)

actionP :: Color -> P.Parser PlayerAction
actionP c = P.choice [roadP, cityP, settP, cardP, playCardP c, bankP, tradeP,
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

-- | Main UI thread for the command line version of the UI
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
                 NextMove ->     do game <- takeMVar gameVar
                                    print game
                                    n <- takeMVar nameVar
                                    a <- getNextAction n c
                                    putMVar actionVar a

                 MoveRobber ->   do tile <- promptForRobber
                                    putMVar robberVar tile
                                    takeMVar gameVar >>= print

                 StealFrom ps -> do color <- getChoiceFrom ps
                                    putMVar colorVar color
              go

-- | Returns the next action a user takes on their turn
getNextAction :: Name -> Color -> IO PlayerAction
getNextAction n c = do
    putStr $ n ++ prompt
    action <- getLine
    case P.parse (actionP c) action of
        Left _ -> putStrLn help >> getNextAction n c
        Right act -> return act

promptForRobber :: IO TileLocation
promptForRobber = do
    putStrLn "where do you want to put the robber?"
    action <- getLine
    case P.parse tileP action of
        Left _ -> putStrLn "invalid location" >> promptForRobber
        Right tile -> return tile

-- promptForMonopoly :: IO Resource
-- promptForMonopoly = do
--     putStrLn "What resource do you want to monopolize?"
--     putStr prompt
--     action <- getLine
--     case P.parse resourceP action of
--         Left _ -> putStrLn "invalid resource" >> promptForMonopoly
--         Right r -> return r

-- promptForYOP :: IO (Resource, Resource)
-- promptForYOP = do
--     putStrLn "Which two resources do you want?"
--     putStr prompt
--     action <- getLine
--     case P.parse resourcesP action of
--         Right [r1, r2] -> return (r1, r2)
--         _ -> putStrLn "must be two resources" >> promptForYOP

-- promptForRoadBuilder :: Color -> IO (Road, Road)
-- promptForRoadBuilder c = do
--     putStrLn "where do you want to build two roads?"
--     putStr prompt
--     action <- getLine
--     case P.parse (rbP c) action of
--         Left _ -> putStrLn "invalid roads" >> promptForRoadBuilder c
--         Right rs -> return rs

getChoiceFrom :: [(Name, Color)] -> IO Color
getChoiceFrom l = do putStr "options to steal from: "
                     print (unwords (map fst l))
                     let p = P.choice $ map (uncurry constP) l
                     choice <- getLine
                     case P.parse p choice of
                         Left _ -> putStrLn "invalid name" >>
                                   getChoiceFrom l
                         Right c -> return c
