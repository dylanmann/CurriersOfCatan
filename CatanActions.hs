{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module CatanActions where

import CatanTypes
import CatanBoard

import qualified Data.List as List
import qualified Control.Monad.State as S
import Debug.Trace(trace)

import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Monad(when, unless)
import Data.Maybe(fromJust, isNothing, mapMaybe)

type MyState = S.StateT Game IO

data PlayerAction = BuildRoad CornerLocation CornerLocation
                  | BuildCity CornerLocation
                  | BuildSettlement CornerLocation
                  | PlayCard ProgressCard
                  | BuyCard
                  | TradeWithBank Resource Resource Int
                  | TradeWithPlayer [Resource] Color [Resource]
                  | EndTurn
                  | EndGame


buildingVP :: Building -> Int
buildingVP Settlement{} = 1
buildingVP City{}       = 2

cardVP :: DevCard -> Int
cardVP VictoryPoint = 1
cardVP _            = 0

spend :: [Resource] -> Color ->  Players -> Players
spend rs c ps = foldr step ps rs where
  step r = updPlayer (\p -> p {resources = updResource (flip (-) 1) r $ resources p}) c

recieve :: [Resource] -> Color ->  Players -> Players
recieve rs c ps = foldr step ps rs where
  step r = updPlayer (\p -> p {resources = updResource (+1) r $ resources p}) c

rollRewards :: Board -> Token -> TileLocation -> Building -> (Color, [Resource])
rollRewards board roll robber b =
    let notRobber = filter (/= robber) $ buildingTileLocs board b
        rs = mapMaybe (produces roll . getTile board) notRobber in
    case b of
        City c _       -> (c, concatMap (replicate 2) rs)
        Settlement c _ -> (c, rs)

allocateRewards :: Token -> MyState ()
allocateRewards roll = do
    game@Game{..} <- S.get
    let rewards = map (rollRewards board roll robberTile) buildings
        step (c, rs) = recieve rs c
    S.put (game {players = foldr step players rewards})


-- updateRoads :: (Color, CornerLocation, CornerLocation) -> MyState Bool
-- updateRoads (c, new1, new2) = do
--     game@Game{..} <- S.get
--     let interrupted = filter (\x -> elem new1 x || elem new2 x) paths



buildRoad :: CornerLocation -> CornerLocation -> MyState Bool
buildRoad loc1 loc2 = do
    game@Game{..} <- S.get
    let c = currentPlayer
        existing (Settlement c1 l) = l `elem` [loc1,loc2] && c == c1
        existing (City       c1 l) = l `elem` [loc1,loc2] && c == c1
        connects (l1,l2,c1) | c == c1 =
          not . null $ [loc1, loc2] `List.intersect` [l1, l2]
        connects _ = False
        containsRoad new1 new2 = any sameRoad where
            sameRoad (old1, old2, _) = (old1 == new1 && old2 == new2) ||
                             (old2 == new1 && old1 == new2)
        newPs = spend [Lumber, Brick] c players
        validP = validPlayer $ getPlayer c newPs
        newRoad = not $ containsRoad loc1 loc2 roads
        contiguous = any existing buildings || any connects roads
        newRs =  (loc1, loc2, c) : roads
        update = S.put(game { players = newPs, roads = newRs})
    if validP && newRoad && contiguous then update >> return True else
        return False

buildCity :: CornerLocation -> MyState Bool
buildCity loc = do
    game@Game{..} <- S.get
    let c = currentPlayer
        newPs = spend [Ore, Ore, Ore, Grain, Grain] c players
        validP = validPlayer $ getPlayer c newPs
        validB = Settlement c loc `elem` buildings
        newBs  = City c loc : List.delete (Settlement c loc) buildings
        update = S.put(game { players = newPs, buildings = newBs})
    if validP && validB then update >> return True else
        return False

buildSett :: CornerLocation -> MyState Bool
buildSett cor = do
    game@Game{..} <- S.get
    let c = currentPlayer
        newPs = spend [Brick, Lumber, Wool, Grain] c players
        validP = validPlayer $ getPlayer c newPs
        validB = freeAdjacent cor buildings && connects c cor roads
        newBs =  Settlement c cor : buildings
        update = S.put(game { players = newPs, buildings = newBs})
    if validP && validB then update >> return True else
        return False
    where freeAdjacent = all . noTouch
          noTouch new b = new `notElem` adjacentCorners (buildingLoc b)
          connects c loc = any (overlap loc c)
          overlap loc c (l1, l2, c1) = c == c1 && (loc `elem` [l1, l2])

playerTrade :: [Resource] -> Color -> [Resource] -> MyState Bool
playerTrade rs1 c2 rs2
  | not $ null $ rs1 `List.intersect` rs2 = return False
  | null rs1 || null rs2 = return False
  | otherwise = do
    game@Game{..} <- S.get
    let c1 = currentPlayer
        spendRs = spend rs1 c1 . spend rs2 c2
        getRs = recieve rs1 c2 . recieve rs2 c1
        newPs  = spendRs $ getRs players
        validP1 = validPlayer $ getPlayer c1 newPs
        validP2 = validPlayer $ getPlayer c2 newPs
        update = S.put(game { players = newPs })
    if validP1 && validP2 then update >> return True else return False

-- Trade resources to the bank if you have them
tradeWithRatio :: Int -> Resource -> Resource -> Int -> MyState Int
tradeWithRatio ratio r1 r2 amountToTrade
  | r1 == r2 || amountToTrade < 2 = return 0
  | otherwise = do
    game@Game{..} <- S.get
    let c = currentPlayer
        yield   = amountToTrade `quot` ratio
        cost    = yield * ratio
        spendRs = spend (replicate cost r1) c
        getRs   = recieve (replicate yield r2) c
        newPs   = spendRs $ getRs players
        validP  = validPlayer $ getPlayer c newPs
        update  = S.put(game { players = newPs })
    when validP update
    return yield

genericTrade :: Resource -> Resource -> Int -> MyState Int
genericTrade r1 r2 amount = do
    Game{..} <- S.get
    let bs = filter (\b -> currentPlayer == buildingColor b) buildings
        harborAtBuilding b = snd $ getCorner board (buildingLoc b)
        hs = map harborAtBuilding bs
        trade i = tradeWithRatio i r1 r2 amount
    if Just (SpecialHarbor r1) `elem` hs then trade 2
    else if Just GenericHarbor `elem` hs then trade 3
    else trade 4

-- TODO: actually make the cards do something
playCard :: ProgressCard -> MyState Bool
playCard prog = do
    game@Game{..} <- S.get
    let c = currentPlayer
        card = Progress prog
        Player{..} = getPlayer c players
    if card `notElem` cards then return False else do
        let newCards p = p{cards = List.delete card cards}
        S.put(game {players = updPlayer newCards c players})
        return True

updateArmy :: MyState ()
updateArmy = do
    game@Game{..} <- S.get
    let c = currentPlayer
        currentP = getPlayer c players
        army = length . filter (== Knight) . cards
        update = S.put (game { largestArmy = Just c })
    case largestArmy of
        Just leader | army currentP > army (getPlayer leader players) -> update
        Nothing     | army currentP >= 5                              -> update
        _ -> return ()

drawCard :: MyState (Maybe DevCard)
drawCard = do
    game@Game{..} <- S.get
    case deck of
      [] -> return Nothing
      card : rest -> do S.put( game { deck = rest } )
                        return $ Just card

unDrawCard :: DevCard -> MyState ()
unDrawCard card = do
    game <- S.get
    S.put (game { deck = card : deck game })


buyCard :: MyState Bool
buyCard = do
    game@Game{..} <- S.get
    maybeCard <- drawCard
    if isNothing maybeCard then return False else do
        let c = currentPlayer
            card = fromJust maybeCard
            addCard = updPlayer (\p -> p { cards = card : cards p }) c
            newPs = addCard $ spend [Ore, Wool, Grain] c players
            validP = validPlayer $ getPlayer c newPs
            update = S.put(game { players = newPs })
        if validP then update else unDrawCard card
        updateArmy
        return validP

rollSevenPenalty :: MyState [Name]
rollSevenPenalty = do
    game@Game{..} <- S.get
    let isVictim (c, p) = length (allResources p) > 7 && c /= currentPlayer
        victims = filter isVictim (allPlayers players)
    newPlayers <- foldr discard (return players) victims
    S.put(game { players = newPlayers })
    return (map (name . snd) victims)
    where discard (c,p) mps = do
            l <- shuffleM (allResources p)
            ps <- mps
            return $ spend (take (length l `quot` 2) l) c ps



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

handleAction :: PlayerAction -> MyState Bool
handleAction a = case a of
        BuildRoad l1 l2           -> ignoreB $ buildRoad l1 l2
        BuildCity l               -> ignoreB $ buildCity l
        BuildSettlement l         -> ignoreB $ buildSett l
        PlayCard c                -> ignoreB $ playCard c
        BuyCard                   -> ignoreB buyCard
        TradeWithBank r1 r2 i     -> ignoreI $ genericTrade r1 r2 i
        TradeWithPlayer rs1 c rs2 -> ignoreB $ playerTrade rs1 c rs2
        EndTurn                   -> return True
        EndGame                   -> error "over"

ignoreI :: MonadIO m => m Int -> m Bool
ignoreI m = do
  res <- m
  when (res == 0) $ liftIO $ putStrLn "not successful"
  return False

ignoreB :: MonadIO m => m Bool -> m Bool
ignoreB m = do
  res <- m
  unless res $ liftIO $ putStrLn "not successful"
  return False
