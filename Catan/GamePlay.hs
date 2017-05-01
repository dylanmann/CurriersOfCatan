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

import Prelude hiding(log)
import CatanGUI
import Control.Monad (liftM2, unless, when)
import Control.Monad.Random.Class(getRandomR)
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.State as S
import Control.Concurrent.MVar.Lifted
import Control.Concurrent(forkIO)
import Types
import Actions

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

log :: Show a => a -> MyState ()
log str = liftIO $ do putStr $ "[GAME]  "
                      print str


-- | rolls the dice, reacts, and changes the turn to the next player's turn
advancePlayer :: Bool -> MyState ()
advancePlayer firstTurn = do
  CatanMVars{..} <- getCatanMVars
  roll <- rollDice
  if firstTurn && roll == 7 then advancePlayer True else do
    putMVar rollVar roll
    movePendingCards
    game <- S.get
    let newGame = game{currentPlayer = nextPlayer $ currentPlayer game}
    S.put newGame
    case roll of
      2  -> allocateRewards Two
      3  -> allocateRewards Three
      4  -> allocateRewards Four
      5  -> allocateRewards Five
      6  -> allocateRewards Six
      7  -> do log "putting roll seven"
               putMVar gameVar newGame
               log " put roll seven"
               rollSeven
      8  -> allocateRewards Eight
      9  -> allocateRewards Nine
      10 -> allocateRewards Ten
      11 -> allocateRewards Eleven
      12 -> allocateRewards Twelve
      _  -> error "impossible"


shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM

isPlayedCard :: PlayerAction -> Bool
isPlayedCard PlayMonopoly{}     = True
isPlayedCard PlayKnight         = True
isPlayedCard PlayYearOfPlenty{} = True
isPlayedCard PlayRoadBuilding{} = True
isPlayedCard _                  = False

-- | A player's turn.  Communicates with the UI thread and loops until turn is over
-- argument is whether a card has been played so far on the turn
takeTurn :: Bool -> MyState ()
takeTurn playedCard = do
  CatanMVars{..} <- getCatanMVars
  action <- takeMVar actionVar
  when (action == EndGame) $ error "Handle end of game better than this"
  log action
  turnOver <- handleAction action
  when turnOver $ advancePlayer False
  g <- S.get
  log "putting take turn"
  _ <- tryTakeMVar gameVar
  putMVar gameVar g
  log "put take turn"
  resetErr
  unless turnOver $ takeTurn $ playedCard || isPlayedCard action

-- | resets the error variable in the game state
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
  game@Game{..} <- liftIO initialize
  let guiThread = forkIO $ beginGUI $ mvars
  _ <- guiThread
  newg <- S.execStateT (advancePlayer True) game
  putMVar (gameVar mvars) newg
  let go = do
      CatanMVars{..} <- getCatanMVars
      takeTurn False
      winner <- endTurn
      case winner of
        Just w -> return w
        Nothing -> go
  S.evalStateT go newg

rollDice :: MyState Int
rollDice = liftM2 (+) die die where
  die = getRandomR (1,6)
