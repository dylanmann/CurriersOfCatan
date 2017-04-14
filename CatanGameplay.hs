{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module CatanGamePlay where

import Control.Monad (liftM2)
import qualified Control.Monad.Random as Random
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.State(MonadState)
import Control.Monad.IO.Class(MonadIO)
import qualified Control.Monad.State as S
import CatanTypes
import CatanActions
import CatanBoard

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
    return $ Game b p [] [] (desert b) Nothing Nothing d Red

-- gameOver is current player has 10 VP (only on their turn)
gameOver :: MyState Bool
gameOver = do
    Game{..} <- S.get
    let c = currentPlayer
        p = getPlayer c players
        bVP = sum $ map buildingVP $ filter ((== c) . buildingColor) buildings
        cVP = sum $ map cardVP $ cards p
        armyVP = if largestArmy == Just c then 2 else 0
        roadVP = 0 -- TODO: unimplemented
    return $ bVP + cVP + armyVP + roadVP >= 10

advancePlayer :: (MonadState Game m, MonadRandom m) => m Color
advancePlayer = do
    game@Game{..} <- S.get
    let next = nextPlayer currentPlayer
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
    S.put(game{currentPlayer = nextPlayer currentPlayer})
    return next

rollDice :: (MonadRandom m) => m Int
rollDice = do
    let die = Random.fromList (fmap (flip(,)1) [1..6])
    roll <- liftM2 (+) die die
    if roll == 7 then rollDice else return roll
    where

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM
