{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards, NamedFieldPuns #-}

module CatanGamePlay where

import Data.Maybe(fromJust, isNothing, mapMaybe)
import Control.Monad (liftM2, unless)
import qualified Control.Monad.Random as Random
import Control.Monad.Random(MonadRandom)
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
import qualified Control.Monad.State as S
import CatanBoard
import CatanTypes
import CatanActions
import CatanActionParsing

main :: IO Name
main = playGame

defaultPlayers :: Players
defaultPlayers = makePlayers
    [(Blue,"blue"),(Red,"red"),(White,"white"),(Orange,"orange")]

setupPlayers :: IO Players
setupPlayers = do
    b <- getName Blue []
    r <- getName Red [snd b]
    w <- getName White (map snd [b, r])
    o <- getName Orange (map snd [b, r, w])
    return $ makePlayers [b, r, o, w]

getName :: Color -> [Name] -> IO (Color,Name)
getName c used = do
    putStr $ show c
    putStrLn $ " player, What is your name?"
    name <- getLine
    if name `notElem` used then return (c, name) else getName c used

initialize :: IO Game
initialize = do
    b <- setupBoard
    d <- shuffleM devCards
    p <- setupPlayers
    return $
     Game b p defaultRoads defaultBuildings (desert b) Nothing Nothing d White

rollSeven :: MyState ()
rollSeven = do liftIO $ putStr "penalty victims: "
               victims <- rollSevenPenalty
               liftIO $ print victims
               newRobber <- liftIO promptForRobber
               moveRobber newRobber

stealFromOneOf :: [(Color, Name)] -> MyState()
stealFromOneOf l = do
    game@Game{..} <- S.get
    liftIO $ putStr "options to steal from: " >> print (map snd l)
    c <- liftIO $ getPlayerChoiceFrom l
    let resChoices = allResources $ getPlayer c players
    case resChoices of
        [] -> liftIO $ putStrLn "no resources"
        hd:_ -> S.put (game {players = recieve [hd] currentPlayer (spend [hd] c players)}) >>
                liftIO (putStr "stole 1 " >> print hd)


-- TODO: pick a player to steal from, pick resource to take
moveRobber :: TileLocation -> MyState ()
moveRobber t = do
    game@Game{..} <- S.get
    let options = mapMaybe (playerAtCorner board) buildings
    S.put(game{robberTile = t})
    case options of
        []  -> liftIO $ putStrLn "no adjacent settlements"
        l   -> stealFromOneOf (zip l $ map (name . flip getPlayer players) l)
    where playerAtCorner board b =
           let corner = getCorner board (buildingLoc b) in
           if t `elem` rewardLocs corner
            then Just $ buildingColor b
            else Nothing


advancePlayer :: MyState ()
advancePlayer = do
    roll <- rollDice
    case roll of
        7 ->  rollSeven
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
    let next = nextPlayer currentPlayer
    S.put(game{currentPlayer = next})

shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleM

takeTurn :: MyState ()
takeTurn = do
    Game{currentPlayer, players} <- S.get
    action <- liftIO $ getNextAction (name (getPlayer currentPlayer players))
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
    liftIO $ putStr "roll: " >> print roll
    return roll
