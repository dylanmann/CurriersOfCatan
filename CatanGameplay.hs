{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, NamedFieldPuns #-}

module CatanGamePlay where

import Data.Maybe(mapMaybe)
import Control.Monad (liftM2, unless,mapM)
import qualified Control.Monad.Random.Class as Random
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.State as S
import Control.Concurrent.MVar
import Control.Concurrent(forkIO)
import CatanBoard
import CatanTypes
import CatanActions
import CatanActionParsing
import qualified Data.Map as Map

main :: IO Name
main = playGame

defaultPlayers :: Players
defaultPlayers = makePlayers
    [(Blue,"blue"),(Red,"red"),(White,"white"),(Orange,"orange")]

getName :: Color -> [Name] -> IO (Color,Name)
getName c used = do
    putStr $ show c
    putStrLn " player, What is your name?"
    name <- getLine
    if name `notElem` used then return (c, name) else getName c used

initialize :: Map.Map Color Name -> IO Game
initialize names = do
    b <- setupBoard
    d <- shuffleM devCards
    let p = makePlayers $ Map.toList names
    return $
     Game b p defaultRoads defaultBuildings (desert b) Nothing Nothing d White

rollSeven :: MyState ()
rollSeven = do liftIO $ putStr "penalty victims: "
               victims <- rollSevenPenalty
               liftIO $ print victims
               newRobber <- liftIO promptForRobber
               moveRobber newRobber

stealFromOneOf :: [(Name, Color)] -> MyState()
stealFromOneOf l = do
    game@Game{..} <- S.get
    liftIO $ putStr "options to steal from: " >> print (unwords (map fst l))
    c <- liftIO $ getChoiceFrom l
    let resChoices = allResources $ getPlayer c players
    case resChoices of
        [] -> liftIO $ putStrLn "no resources"
        hd:_ -> S.put (game {players = recieve [hd] currentPlayer (spend [hd] c players)}) >>
                liftIO (putStr "stole 1 " >> print hd)

moveRobber :: TileLocation -> MyState ()
moveRobber t = do
    game@Game{..} <- S.get
    let options = mapMaybe (playerAtCorner board) buildings
    S.put(game{robberTile = t})
    case options of
        []  -> liftIO $ putStrLn "no adjacent settlements"
        l   -> stealFromOneOf (zip (map (name . flip getPlayer players) l) l)
    where playerAtCorner board b =
           let corner = getCorner board (buildingLoc b) in
           if t `elem` rewardLocs corner
            then Just $ buildingColor b
            else Nothing


advancePlayer :: MyState ()
advancePlayer = do
    roll <- rollDice
    case roll of
        2  -> allocateRewards Two
        3  -> allocateRewards Three
        4  -> allocateRewards Four
        5  -> allocateRewards Five
        6  -> allocateRewards Six
        7  -> rollSeven
        8  -> allocateRewards Eight
        9  -> allocateRewards Nine
        10 -> allocateRewards Ten
        11 -> allocateRewards Eleven
        12 -> allocateRewards Twelve
        _  -> error "impossible"
    game@Game{..} <- S.get
    let next = nextPlayer currentPlayer
    S.put(game{currentPlayer = next})

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM

takeTurn :: (MVar Name, MVar PlayerAction) -> MyState ()
takeTurn (nameVar, actionVar) = do
    g@Game{..} <- S.get
    liftIO $ print g
    liftIO $ putMVar nameVar (name (getPlayer currentPlayer players))
    action <- liftIO $ takeMVar actionVar
    turnOver <- handleAction action
    unless turnOver $ takeTurn (nameVar, actionVar)

makeMVars :: IO (Map.Map Color (MVar Name, MVar PlayerAction))
makeMVars = do
    red <- pair
    blue <- pair
    orange <- pair
    white <- pair
    return $ Map.fromList
            [(Red, red), (Orange, orange), (White, white), (Blue, blue)]

    where pair = do
            nameVar <- newEmptyMVar
            actionVar <- newEmptyMVar
            return (nameVar, actionVar)

playGame :: IO Name
playGame = do
    mvars <- makeMVars
    let mvarLookup c = Map.findWithDefault (error "impossible failure") c mvars
    forkIO $ ioThread $ mvarLookup Blue
    forkIO $ ioThread $ mvarLookup Red
    forkIO $ ioThread $ mvarLookup Orange
    forkIO $ ioThread $ mvarLookup White
    names <- mapM (takeMVar . fst) mvars
    game <- liftIO $ initialize names
    let go = do
            advancePlayer
            Game{..} <- S.get
            takeTurn $ mvarLookup currentPlayer
            final <- gameOver
            if final then
                return $ name (getPlayer currentPlayer players)
            else go
    S.evalStateT go game

rollDice :: MyState Int
rollDice = do
    let die = Random.getRandomR (1,6)
    roll <- liftM2 (+) die die
    liftIO $ putStr "roll: " >> print roll
    return roll
