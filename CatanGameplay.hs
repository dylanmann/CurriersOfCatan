{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module CatanGamePlay where

import Control.Monad (liftM2, unless)
import qualified Control.Monad.Random as Random
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.Trans.State.Strict as S
-- import qualified Control.Monad.State as S
import CatanBoard
import CatanTypes
import CatanActions
import CatanActionParsing

defaultPlayers :: Players
defaultPlayers = makePlayers
    [(Blue,"blue"),(Red,"red"),(White,"white"),(Orange,"orange")]

setupPlayers :: IO Players
setupPlayers = return $ defaultPlayers

initialize :: IO Game
initialize = do
    b <- setupBoard
    d <- shuffleM devCards
    p <- setupPlayers
    return $
     Game b p defaultRoads defaultBuildings (desert b) Nothing Nothing d White

advancePlayer :: MyState ()
advancePlayer = do
    roll <- rollDice
    case roll of
        2 ->  allocateRewards Two
        3 ->  allocateRewards Three
        4 ->  allocateRewards Four
        5 ->  allocateRewards Five
        6 ->  allocateRewards Six
        8 ->  allocateRewards Eight
        9 ->  allocateRewards Nine
        10 -> allocateRewards Ten
        11 -> allocateRewards Eleven
        12 -> allocateRewards Twelve
        _ -> error "impossible"
    game@Game{..} <- S.get
    S.put(game{currentPlayer = nextPlayer currentPlayer})
    liftIO $ putStr "nextPlayer: "
    liftIO $ print $ nextPlayer currentPlayer

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM

takeTurn :: MyState ()
takeTurn = do
    action <- liftIO getNextAction
    turnOver <- handleAction action
    g <- S.get
    liftIO $ print g
    unless turnOver takeTurn

playGame :: IO Name
playGame = do
    game <- liftIO initialize
    S.evalStateT go game where
    go = do
        advancePlayer
        takeTurn
        final <- gameOver
        if final then do
            Game{..} <- S.get
            return $ name (getPlayer currentPlayer players)
        else go

rollDice :: MyState Int
rollDice = do
    let die = Random.fromList (fmap (flip(,)1) [1..6])
    roll <- liftM2 (+) die die
    liftIO $ putStr "roll: ">> print roll
    if roll == 7 then rollDice else return roll
