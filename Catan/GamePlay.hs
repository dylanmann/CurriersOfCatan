{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, NamedFieldPuns #-}

module GamePlay where

import Control.Monad (liftM2, unless)
import Control.Monad.Random.Class(getRandomR)
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.State as S
import Control.Concurrent.MVar
import Control.Concurrent(forkIO)
import Types
import Actions
import ActionParsing

main :: IO Name
main = playGame

defaultPlayers :: IO Players
defaultPlayers = makePlayers
  [(Blue,"blue"),(Red,"red"),(White,"white"),(Orange,"orange")]

getName :: Color -> [Name] -> IO (Color,Name)
getName c used = do
  putStr $ show c
  putStrLn " player, What is your name?"
  name <- getLine
  if name `notElem` used then return (c, name) else getName c used

setupPlayers :: IO Players
setupPlayers = do
  b <- getName Blue []
  r <- getName Red [snd b]
  w <- getName White (map snd [b, r])
  o <- getName Orange (map snd [b, r, w])
  makePlayers [b, r, o, w]

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

isPlayedCard :: PlayerAction -> Bool
isPlayedCard (PlayCard _) = True
isPlayedCard _ = False


takeTurn :: Bool -> MyState ()
takeTurn playedCard = do
  g@Game{..} <- S.get
  CatanMVars{..} <- getCatanMVars
  action <- liftIO $ do
        print g
        putMVar requestVar NextMove
        putMVar nameVar (name (getPlayer currentPlayer players))
        takeMVar actionVar
  turnOver <- handleAction action
  unless turnOver $ takeTurn $ playedCard || isPlayedCard action


playGame :: IO Name
playGame = do
  game <- liftIO initialize
  let ioThread c = forkIO . commandLineInput c . mvars . getPlayer c $ players game
  _ <- ioThread Red
  _ <- ioThread White
  _ <- ioThread Blue
  _ <- ioThread Orange
  let go = do
      advancePlayer
      Game{..} <- S.get
      takeTurn False
      final <- gameOver
      if final then
        return $ name (getPlayer currentPlayer players)
      else go
  S.evalStateT go game

rollDice :: MyState Int
rollDice = do
  let die = getRandomR (1,6)
  roll <- liftM2 (+) die die
  liftIO $ putStr "roll: " >> print roll
  return roll
