{-|
Module      : Catan.GamePlay
Description : Server/ Main module of the game
Copyright   : (c) Dylan Mann, David Cao 2017
License     : GPL-3
Maintainer  : mannd@seas.upenn.edu
Stability   : experimental
Portability : POSIX

Contains the code for setting up the game and running the main event loop.
playGame runs the game.  Project was made for Advanced Programming course
(CIS552) at Penn in the Spring 2017 semester.

-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, NamedFieldPuns #-}

module GamePlay where

import CatanGUI
import Control.Monad (liftM2, unless)
import Control.Monad.Random.Class(getRandomR)
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.State as S
import Control.Concurrent.MVar.Lifted
import Control.Concurrent(forkIO)
import Types
import Actions
import ActionParsing

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
  if name `notElem` used && ' ' `notElem` name then return (c, name)
    else getName c used

setupPlayers :: IO Players
setupPlayers = do
  b <- getName Blue []
  r <- getName Red [snd b]
  w <- getName White (map snd [b, r])
  o <- getName Orange (map snd [b, r, w])
  return $ makePlayers [b, r, o, w]

makeMVars :: IO CatanMVars
makeMVars = do v1  <- newEmptyMVar
               v2  <- newEmptyMVar
               v3  <- newEmptyMVar
               v4  <- newEmptyMVar
               v5  <- newEmptyMVar
               v6  <- newEmptyMVar
               v7  <- newEmptyMVar
               return $ CatanMVars v1 v2 v3 v4 v5 v6 v7

initialize :: IO Game
initialize = do
  b <- setupBoard
  d <- shuffleM devCards
  let p = defaultPlayers
  m <- makeMVars
  let des = (desert b)
  return $
   Game b p defaultRoads defaultBuildings des Nothing Nothing d White Nothing [] m

-- | rolls the dice, reacts, and changes the turn to the next player's turn
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
  CatanMVars{..} <- getCatanMVars
  putMVar rollVar roll

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM

isPlayedCard :: PlayerAction -> Bool
isPlayedCard (PlayMonopoly _)       = True
isPlayedCard PlayKnight             = True
isPlayedCard (PlayYearOfPlenty _ _) = True
isPlayedCard (PlayRoadBuilding _ _) = True
isPlayedCard _                      = False

-- | A player's turn.  Communicates with the UI thread and loops until turn is over
-- argument is whether a card has been played so far on the turn
takeTurn :: Bool -> MyState ()
takeTurn playedCard = do
  g@Game{..} <- S.get
  CatanMVars{..} <- getCatanMVars
  putMVar gameVar g
  putMVar requestVar NextMove
  putMVar nameVar (name (getPlayer currentPlayer players))
  action <- takeMVar actionVar
  liftIO $ print action
  turnOver <- handleAction action
  resetErr
  unless turnOver $ takeTurn $ playedCard || isPlayedCard action

resetErr :: MyState ()
resetErr = do
  g <- S.get
  S.put $ g{errorMessage = Nothing}

-- | cleans up after a player's turn is over
endTurn :: MyState (Maybe Name)
endTurn = do
  Game{..} <- S.get
  movePendingCards
  final <- gameOver
  if final then
    return $ Just $ name (getPlayer currentPlayer players)
  else return Nothing

-- Main server thread.  Sets up UI thread and plays turns until the game is over
playGame :: IO Name
playGame = do
  game <- liftIO initialize
  let guiThread = forkIO $ beginGUI game
  -- let ioThread c = forkIO $ commandLineInput c mvars
  _ <- guiThread
  -- _ <- ioThread Red
  -- _ <- ioThread White
  -- _ <- ioThread Blue
  -- _ <- ioThread Orange
  let go = do
      advancePlayer
      takeTurn False
      winner <- endTurn
      case winner of
        Just w -> return w
        Nothing -> go
  S.evalStateT go game

rollDice :: MyState Int
rollDice = liftM2 (+) die die where
  die = getRandomR (1,6)
