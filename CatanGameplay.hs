{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module CatanGamePlay where

import Control.Monad (liftM2)
import qualified Control.Monad.Random as Random
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.State(MonadState)
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
gameOver :: (MonadState Game m) => m Bool
gameOver = do
    Game{..} <- S.get
    let c = currentPlayer
        p = getPlayer c players
        bVP = sum $ map buildingVP $ filter (ownedBy c) buildings
        cVP = sum $ map cardVP $ cards p
        armyVP = if largestArmy == Just c then 2 else 0
        roadVP = 0 -- TODO: unimplemented
    return $ bVP + cVP + armyVP + roadVP >= 10


rollDice :: (MonadRandom m) => m Int
rollDice = liftM2 (+) die die where
    die :: (MonadRandom m) => m Int
    die = Random.fromList (fmap (flip(,)1) [1..6])

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM
