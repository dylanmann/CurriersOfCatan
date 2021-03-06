{-|
Module      : Catan.Actions
Description : Module for processing PlayerActions recieved by the server
Copyright   : (c) Dylan Mann, David Cao 2017
License     : GPL-3
Maintainer  : mannd@seas.upenn.edu
Stability   : experimental
Portability : POSIX


Handles PlayerActions received by the server.  Most important function is
handleAction, which matches on an action and delegates to the next function.
All functions in this module return a bool for their success, and write errors
to the errorMessage field of the Game State record.
-}
{-# OPTIONS_HADDOCK prune, show-extensions #-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Actions(handleAction,
               rollSeven,
               gameOver,

               MyState,

               allocateRewards,
               spend,
               recieve,

               getCatanMVars,

               movePendingCards)
               where
import Prelude hiding(log)

import qualified Data.List as List
import qualified Control.Monad.State as S

import Control.Concurrent.MVar.Lifted
import System.Random.Shuffle(shuffleM)
import Control.Monad.IO.Class(liftIO)
-- import Control.Monad(unless)
import Data.Maybe(mapMaybe)

import Types

-- | Monadic type used by the Server thread for maintaining state
type MyState = S.StateT Game IO

err :: String -> MyState Bool
err str = do g <- S.get
             S.put (g { errorMessage = Just str })
             return False

buildingVP :: Building -> Int
buildingVP Settlement{} = 1
buildingVP City{}       = 2

cardVP :: DevCard -> Int
cardVP VictoryPoint = 1
cardVP _            = 0

-- | deducts one of each resource in the list to a player's hand
spend :: [Resource] -> Color ->  Players -> Players
spend rs c ps = foldr step ps rs where
  step r = updPlayer (\p -> p {resources = updResource (flip (-) 1) r $ resources p}) c

-- | adds one of each resource in the list to a player's hand
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

-- | given a token, gives the proper rewards based on each player's buildings
allocateRewards :: Token -> MyState ()
allocateRewards roll = do
    game@Game{..} <- S.get
    let rewards = map (rollRewards board roll robberTile) buildings
        step (c, rs) = recieve rs c
    log rewards
    S.put (game {players = foldr step players rewards})

-- | constructs a road at the given corner locations if valid
buildRoad :: CornerLocation -> CornerLocation -> MyState Bool
buildRoad loc1 loc2 = do
    game@Game{..} <- S.get
    let c = currentPlayer
        connects r = let (l1,l2,c1) = getRoad r in c1 == c &&
          (not . null $ [loc1, loc2] `List.intersect` [l1, l2])
        containsRoad new1 new2 = any sameRoad where
            sameRoad r = let (old1, old2, _) = getRoad r in
                             (old1 == new1 && old2 == new2) ||
                             (old2 == new1 && old1 == new2)
        newPs = spend [Lumber, Brick] c players
        validP = validPlayer $ getPlayer c newPs
        newRoad = not $ containsRoad loc1 loc2 roads
        contiguous = any connects roads
    case mkRoad (loc1, loc2, c) of
            Nothing -> err "locations must be adjacent"
            Just r -> do let newRs = r:roads
                             update = S.put(game { players = newPs,
                                                roads = newRs,
                                                longestRoad = newLongestRoad longestRoad newRs})
                         if      not validP     then err "not enough resources"
                         else if not newRoad    then err "not new road"
                         else if not contiguous then err "not connected"
                         else update >> return True

-- | constructs a city at the given corner locations if valid
buildCity :: CornerLocation -> MyState Bool
buildCity loc = do
    game@Game{..} <- S.get
    let c = currentPlayer
        newPs = spend [Ore, Ore, Ore, Grain, Grain] c players
        validP = validPlayer $ getPlayer c newPs
        validB = Settlement c loc `elem` buildings
        newBs  = City c loc : List.delete (Settlement c loc) buildings
        update = S.put(game { players = newPs, buildings = newBs})
    if      not validP then err "City requires 3 Ore and 2 Grain"
    else if not validB then err "Settlement you own must exist on same tile"
    else update >> return True

-- | constructs a settlement at the given corner locations if valid
buildSett :: CornerLocation -> MyState Bool
buildSett cor = do
    game@Game{..} <- S.get
    let c = currentPlayer
        newPs = spend [Brick, Lumber, Wool, Grain] c players
        validP = validPlayer $ getPlayer c newPs
        validB = freeAdjacent cor buildings
        validL = connects c cor roads
        isNew  = all (notSame cor) buildings
        newBs =  Settlement c cor : buildings
        update = S.put(game { players = newPs, buildings = newBs})
    if      not validP then err "Settlement requires Brick, Lumber, Wool, and Grain"
    else if not validB then err "All adjacent corners must be unbuilt"
    else if not validL then err "Settlement must connect to existing roads"
    else if not isNew  then err "Settlement must not be on top of another"
    else update >> return True
    where freeAdjacent = all . noTouch
          noTouch new b = new `notElem` adjacentCorners (buildingLoc b)
          connects c loc = any (overlap loc c)
          overlap loc c r = let (l1, l2, c1) = getRoad r in
                            c == c1 && (loc `elem` [l1, l2])
          notSame loc (Settlement _ l) = loc /= l
          notSame loc (City       _ l) = loc /= l

-- | if both players approved already, trades one of each of rs1 from the
--   CurrentPlayer to c2 in exchange for one of each of rs2
playerTrade :: [Resource] -> Color -> [Resource] -> MyState Bool
playerTrade rs1 c2 rs2
  | not $ null $ rs1 `List.intersect` rs2 = err "none of the traded resources can overlap"
  | null rs1 || null rs2 = err "Resources cannot be traded for nothing"
  | otherwise = do
    game@Game{..} <- S.get
    let c1 = currentPlayer
        spendRs = spend rs1 c1 . spend rs2 c2
        getRs = recieve rs1 c2 . recieve rs2 c1
        newPs  = spendRs $ getRs players
        validP1 = validPlayer $ getPlayer c1 newPs
        validP2 = validPlayer $ getPlayer c2 newPs
        update = S.put(game { players = newPs })
    if      not validP1 then err "You do not have enough resources"
    else if not validP2 then err "Other player must have enough resources"
    else update >> return True

-- | Exchange amountToTrade resources at the given ratio
tradeWithRatio :: Int -> Resource -> Resource -> Int -> MyState Bool
tradeWithRatio ratio r1 r2 amountToTrade
  | r1 == r2 = err "you cannot trade for the same resource"
  -- | amountToTrade < 2 = err "you did not offer enough resources"
  | otherwise = do
    game@Game{..} <- S.get
    let c = currentPlayer
        -- yield   = amountToTrade `quot` ratio
        cost    = amountToTrade * ratio
        spendRs = spend (replicate cost r1) c
        getRs   = recieve (replicate amountToTrade r2) c
        newPs   = spendRs $ getRs players
        validP  = validPlayer $ getPlayer c newPs
        update  = S.put(game { players = newPs })
    if not validP then err ("you do not have enough " ++ show r1)
        else update >> return True

-- | Trade resources to the bank or the best harbor you have for that resource
genericTrade :: Resource -> Resource -> Int -> MyState Bool
genericTrade r1 r2 amount = do
    Game{..} <- S.get
    let bs = filter (\b -> currentPlayer == buildingColor b) buildings
        harborAtBuilding b = snd $ getCorner board (buildingLoc b)
        hs = map harborAtBuilding bs
        trade i = tradeWithRatio i r1 r2 amount
    if Just (SpecialHarbor r1) `elem` hs then trade 2
    else if Just GenericHarbor `elem` hs then trade 3
    else trade 4

-- | Play a card that the current player has in their hand
playCard :: DevCard -> MyState Bool
playCard VictoryPoint = err "you cannot play a victory point card"
playCard card = do
    game@Game{..} <- S.get
    let c = currentPlayer
        Player{..} = getPlayer c players
    if card `notElem` cards then err "you don't have that card" else do
        let newCards p = p{cards = List.delete card cards}
        S.put(game {players = updPlayer newCards c players})
        case card of
            Knight -> updateArmy >> moveRobber
            _ -> return ()
        return True

-- | play a given progress card, given that it is valid
playMonopoly :: Resource -> MyState Bool
playMonopoly r = do
    success <- playCard $ Progress Monopoly
    if not success then return False else do
        game@Game{..} <- S.get
        let newPs = foldr (step currentPlayer) players (allPlayers players)
        S.put $ game {players = newPs}
        return True
        where
          allR p = filter (== r) (allResources p)
          step c (c2,p) = recieve (allR p) c . spend (allR p) c2

playRoadBuilding :: CornerLocation -> CornerLocation -> CornerLocation -> CornerLocation -> MyState Bool
playRoadBuilding c1 c2 c3 c4 = do
    success <- playCard $ Progress RoadBuilding
    if not success then return False else do
        game@Game{..} <- S.get
        case (mkRoad (c1, c2, currentPlayer), mkRoad (c3, c4, currentPlayer)) of
            (Just r1, Just r2) -> do
                v1 <- validRoad r1 roads
                v2 <- validRoad r2 roads
                if v1 then do
                    let newRoads = r1:roads
                    valid2 <- validRoad r2 newRoads
                    if valid2 then do
                        S.put(game { roads = r2:newRoads,
                                     longestRoad = newLongestRoad longestRoad newRoads})
                        return True
                        else err2 "second road is invalid"
                else if v2 then do
                    let newRoads = r2:roads
                    valid1 <- validRoad r1 newRoads
                    if valid1 then do
                        S.put(game { roads = r1:newRoads,
                                  longestRoad = newLongestRoad longestRoad newRoads})
                        return True
                        else err2 "first road is invalid"
                    else err2 "invalid road choices"
            (_, _) -> err2 "you cannot build roads there"
    where err2 str = do g@Game{..} <- S.get
                        S.put(g { players =
                            updPlayer (\p -> p{cards = Progress RoadBuilding:cards p}) currentPlayer players })
                        err str

playYearOfPlenty :: Resource -> Resource -> MyState Bool
playYearOfPlenty r1 r2 = do
    success <- playCard $ Progress YearOfPlenty
    if not success then return False else do
        game@Game{..} <- S.get
        S.put(game{players = recieve [r1,r2] currentPlayer players})
        return True

-- | check if a road is unique and connects to existing roads
validRoad :: Road -> Roads -> MyState Bool
validRoad r rs = do
    Game{..} <- S.get
    let (_,_,c) = getRoad r
        connects r1 r2 = let (l1,l2,c1)   = getRoad r1
                             (l3, l4, c2) = getRoad r2 in
            c1 == c2 && (not . null $ [l1, l2] `List.intersect` [l3, l4])
        containsRoad = any sameRoad rs where
            sameRoad r2 = let (old1, old2, c1) = getRoad r2 in
                (r == r2) || (getRoad r == (old2, old1, c1))
        unique = not containsRoad
        contiguous = any (connects r) rs
    return $ unique && contiguous && c == currentPlayer

-- | update a player's army size count and check for the largest army condition
updateArmy :: MyState ()
updateArmy = do
    game@Game{..} <- S.get
    let c = currentPlayer
        currentP = getPlayer c players
        addToArmy p = p{knights = knights p + 1}
        updateNoLA = S.put (game { players = updPlayer addToArmy c players })
        updateLA   = S.put (game { largestArmy = Just c,
                                     players = updPlayer addToArmy c players})
    case largestArmy of
        Just curr | knights currentP >= knights (getPlayer curr players) -> updateLA
        Nothing   | knights currentP >= 2 -> updateLA
        _ -> updateNoLA

-- | draw a card from the deck
drawCard :: MyState (Maybe DevCard)
drawCard = do
    game@Game{..} <- S.get
    case deck of
      [] -> return Nothing
      card : rest -> do S.put( game { deck = rest } )
                        return $ Just card

-- | for undoing drawing a card from the deck
unDrawCard :: DevCard -> MyState ()
unDrawCard card = do
    game <- S.get
    S.put (game { deck = card : deck game })

-- | for moving cards bought this turn to your deck at the conclusion of a player's turn
movePendingCards :: MyState ()
movePendingCards = do
    g@Game{..} <- S.get
    let newPs = updPlayer
                 (\p -> p {cards = cards p ++ pendingCards}) currentPlayer players
    S.put(g {players = newPs, pendingCards = []})

-- | buys a card and places it in the player's pending cards
buyCard :: MyState Bool
buyCard = do
    maybeCard <- drawCard
    game@Game{..} <- S.get
    case maybeCard of
        Nothing -> err "no cards left to buy"
        Just card -> do
            log ("drew: " ++ show card)
            let c = currentPlayer
                newPs = spend [Ore, Wool, Grain] c players
                validP = validPlayer $ getPlayer c newPs
                update = case card of
                    VictoryPoint -> S.put(game { players =
                            updPlayer (\p -> p{cards = card:cards p}) c newPs })
                    _            -> S.put(game { players = newPs,
                                             pendingCards = card:pendingCards })
            if validP then update >> return True
                else unDrawCard card >> err "cards cost Ore, Wool and Grain"

-- | penalizes all players with more than 7 resources for when a 7 is rolled
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

-- | checks win conditions, the game is over when the current player has 10 VP
gameOver :: MyState Bool
gameOver = do
    Game{..} <- S.get
    let c = currentPlayer
        p = getPlayer c players
        bVP = sum $ map buildingVP $ filter ((== c) . buildingColor) buildings
        cVP = sum $ map cardVP $ cards p
        armyVP = if largestArmy == Just c then 2 else 0
        roadVP = if longestRoad == Just c then 2 else 0
    return $ bVP + cVP + armyVP + roadVP >= 10

getCatanMVars :: MyState CatanMVars
getCatanMVars = do
    Game{..} <- S.get
    return mvars

-- | called after the roll in the main thread is a 7.
--   Moves robber and penalizes players with too many resources.
rollSeven :: MyState ()
rollSeven = do CatanMVars{..} <- getCatanMVars
               victims <- rollSevenPenalty
               log $ "penalty victims: " ++ show victims
               moveRobber

-- | method that asks the user which of the possibilities they would like to steal
--   from in the case of a robber movement
stealFromOneOf :: [(Name, Color)] -> MyState()
stealFromOneOf [] = do
    CatanMVars{..} <- getCatanMVars
    putMVar stealVar []
stealFromOneOf l = do
    game@Game{..} <- S.get
    CatanMVars{..} <- getCatanMVars
    putMVar stealVar l
    c <- takeMVar colorVar
    let resChoices = allResources $ getPlayer c players
    case resChoices of
        [] -> liftIO $ putStrLn "no resources"
        hd:_ -> S.put (game {players = recieve [hd] currentPlayer (spend [hd] c players)}) >>
                liftIO (putStr "stole 1 " >> print hd)

-- | testing method that allows the user to get free resources
cheat :: [Resource] -> MyState Bool
cheat rs = do
    game@Game{..} <- S.get
    S.put(game{players = recieve rs currentPlayer players})
    return True

log :: Show a => a -> MyState ()
log str = liftIO $ do putStr "[GAME]  "
                      print str


-- | prompts user thread for input and moves the robber to that location, with all the effects
moveRobber :: MyState ()
moveRobber = do
    game@Game{..} <- S.get
    CatanMVars{..} <- getCatanMVars
    t <- takeMVar robberVar
    let options = mapMaybe (playerAtCorner board t) buildings
    S.put $ game{robberTile = t}
    log "putting game move robber in actions"
    putMVar gameVar $ game{robberTile = t}
    log "put game move robber in actions"
    case options of
        []  -> return ()
        l   -> stealFromOneOf (zip (map (name . flip getPlayer players) l) l)
    where playerAtCorner board t b =
           let corner = getCorner board (buildingLoc b) in
           if t `elem` rewardLocs corner
            then Just $ buildingColor b
            else Nothing

-- | delegates the game logic for a given player action.  Returns whether the player's turn is over
handleAction :: PlayerAction -> MyState Bool
handleAction a = case a of
        BuildRoad l1 l2              -> handle $ buildRoad l1 l2
        BuildCity l                  -> handle $ buildCity l
        BuildSettlement l            -> handle $ buildSett l
        PlayMonopoly r               -> handle $ playMonopoly r
        PlayKnight                   -> handle $ playCard Knight
        PlayYearOfPlenty r1 r2       -> handle $ playYearOfPlenty r1 r2
        PlayRoadBuilding l1 l2 l3 l4 -> handle $ playRoadBuilding l1 l2 l3 l4
        BuyCard                      -> handle buyCard
        TradeWithBank r1 r2 i        -> handle $ genericTrade r1 r2 i
        TradeWithPlayer rs1 c rs2    -> handle $ playerTrade rs1 c rs2
        EndTurn                      -> return True
        Cheat rs                     -> handle $ cheat rs
        EndGame                      -> error "over"


handle :: MyState Bool -> MyState Bool
handle status = do
      _ <- status
      return False
