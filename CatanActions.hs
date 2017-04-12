{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
module CatanActions where

import CatanTypes
import CatanBoard

import qualified Data.List as List
import qualified Control.Monad.State as S

import Control.Monad(when, unless)
import Data.Maybe(fromJust, isNothing, mapMaybe)

type MyState = S.State Game

buildingVP :: Building -> Int
buildingVP Settlement{} = 1
buildingVP City{}       = 2

ownedBy :: Color -> Building -> Bool
ownedBy c1 (Settlement c2 _) = c1 == c2
ownedBy c1 (City c2 _)       = c1 == c2

cardVP :: DevCard -> Int
cardVP VictoryPoint = 1
cardVP _            = 0


rollRewards :: Token -> TileLocation -> Building -> (Color, [Resource])
rollRewards roll robber b = case b of
    City c _       -> (c, concatMap (replicate 2) value)
    Settlement c _ -> (c, value)
    where value = mapMaybe payingTile (buildingTiles b)
          payingTile t@(Paying _ token l) | token == roll && l /= robber =
              produces t
          payingTile _ = Nothing

validPlayer :: Player -> Bool
validPlayer = all (>= 0) . resources

-- TODO: refactor spend to return a state so that it just spends from game
spend :: [Resource] -> Color ->  Players -> Players
spend rs c ps = foldr step ps rs where
  step r = updPlayer (\p -> p {resources = updResource (flip (-) 1) r $ resources p}) c

recieve :: [Resource] -> Color ->  Players -> Players
recieve rs c ps = foldr step ps rs where
  step r = updPlayer (\p -> p {resources = updResource (+1) r $ resources p}) c

allocateRewards :: Token -> MyState ()
allocateRewards roll = do
    game <- S.get
    let bs = buildings game
        ps = players game
        robber  = robberTile game
        rewards = map (rollRewards roll robber) bs
        step (c, rs) g = let newPs = recieve rs c ps in
                        g { players = newPs }
    S.put (foldr step game rewards)


buildRoad :: Color -> CornerLocation -> CornerLocation -> MyState ()
buildRoad c loc1 loc2 = do
    game <- S.get
    let ps = players game
        rs = roads game
        bs = buildings game
        newPs = spend [Lumber, Brick] c ps
        validP = validPlayer $ getPlayer c newPs
        newRoad = not $ containsRoad loc1 loc2 rs
        contiguous = (any existing bs) || (any connects rs)
        newRs =  (loc1, loc2, c) : rs
        update = S.put(game { players = newPs, roads = newRs})
    when (validP && newRoad && contiguous) update
    where existing (Settlement c1 (_, l)) = l `elem` [loc1,loc2] && c == c1
          existing (City       c1 (_, l)) = l `elem` [loc1,loc2] && c == c1
          connects (l1,l2,c1) | c == c1 =
            not . null $ [loc1, loc2] `List.intersect` [l1, l2]
          connects _ = False
          containsRoad new1 new2 = any sameRoad where
              sameRoad (old1, old2, _) = (old1 == new1 && old2 == new2) ||
                               (old2 == new1 && old1 == new2)

buildCity :: Color -> Corner -> MyState ()
buildCity c cor = do
    game <- S.get
    let ps = players game
        bs = buildings game
        newPs = spend [Ore, Ore, Ore, Grain, Grain] c ps
        validP = validPlayer $ getPlayer c newPs
        validB = Settlement c cor `elem` bs
        newBs =  City c cor : List.delete (Settlement c cor) bs
        update = S.put(game { players = newPs, buildings = newBs})
    when (validP && validB) update
    return()

-- TODO need to check adjacent clearings
buildSett :: Color -> Corner -> MyState ()
buildSett c cor = do
    game <- S.get
    let ps = players game
        bs = buildings game
        newPs = spend [Brick, Lumber, Wool, Grain] c ps
        validP = validPlayer $ getPlayer c newPs
        validB = containsBuild cor bs
        newBs =  Settlement c cor : bs
        update = S.put(game { players = newPs, buildings = newBs})
    when (validP && validB) update
    return()
    where containsBuild :: Corner -> [Building] -> Bool
          containsBuild = any . samePlace
          samePlace new (Settlement _ old) = old == new
          samePlace new (City _ old)       = old == new

updateArmy :: Color -> MyState ()
updateArmy c = do
    game <- S.get
    let ps = players game
        currentP = getPlayer c ps
        army = length . filter (== Knight) . cards
        update = S.put (game { largestArmy = Just c })
    case largestArmy game of
        Just leader | army currentP > army (getPlayer leader ps) -> update
        Nothing     | army currentP >= 5                         -> update
        _ -> return ()

drawCard :: MyState (Maybe DevCard)
drawCard = do
    game <- S.get
    case deck game of
      [] -> return Nothing
      card : rest -> do S.put( game { deck = rest } )
                        return $ Just card

unDrawCard :: DevCard -> MyState ()
unDrawCard card = do
    game <- S.get
    S.put (game { deck = card : (deck game) })

playerTrade :: Color -> [Resource] -> Color -> [Resource] -> MyState ()
playerTrade c1 rs1 c2 rs2
  | not $ null (rs1 `List.intersect` rs2) = return ()
  | otherwise = do
    game <- S.get
    let ps = players game
        spendRs = spend rs1 c1 . spend rs2 c2
        getRs = recieve rs1 c2 . recieve rs2 c1
        newPs  = spendRs $ getRs ps
        validP1 = validPlayer $ getPlayer c1 newPs
        validP2 = validPlayer $ getPlayer c2 newPs
        update = S.put(game { players = newPs })
    when (validP1 && validP2) update

-- Trade resources to the bank if you have them
tradeWithRatio :: Int -> Color -> Resource -> Resource -> Int -> MyState Int
tradeWithRatio ratio c r1 r2 amountToTrade
  | r1 == r2 || amountToTrade < 2 = return 0
  | otherwise = do
    game <- S.get
    let yield   = amountToTrade `quot` ratio
        cost    = yield * ratio
        spendRs = spend (replicate cost r1) c
        getRs   = recieve (replicate yield r2) c
        newPs   = spendRs $ getRs (players game)
        validP  = validPlayer $ getPlayer c newPs
        update  = S.put(game { players = newPs })
    when validP update
    return yield

genericTrade :: Color -> Resource -> Resource -> Int -> MyState Int
genericTrade c r1 r2 amount = do
    game <- S.get
    let bs = filter (ownedBy c) (buildings game)
        hs = map harborAtBuilding bs
        trade i = tradeWithRatio i c r1 r2 amount

    if Just (SpecialHarbor r1) `elem` hs
        then trade 2
    else if Just GenericHarbor `elem` hs
        then trade 3
    else trade 4
    where
        harborAtBuilding (City       _ ((_,h), _)) = h
        harborAtBuilding (Settlement _ ((_,h), _)) = h

buyCard :: Color -> MyState ()
buyCard c = do
    game <- S.get
    maybeCard <- drawCard
    unless (isNothing maybeCard) $ do
        let card = fromJust maybeCard
            ps = players game
            addCard = updPlayer (\p -> p { cards = card : cards p }) c
            newPs = addCard $ spend [Ore, Wool, Grain] c ps
            validP = validPlayer $ getPlayer c newPs
            update = S.put(game { players = newPs })
        if validP then update else unDrawCard card

-- discard needs to be randomized
rollSevenPenalty :: Color -> MyState ()
rollSevenPenalty current = do
    game <- S.get
    let ps = players game
        victims = filter isVictim (allPlayers ps)
        newPlayers = foldr discard ps victims
    S.put(game { players = newPlayers })
    where isVictim (c, player) =
            length (allResources player) > 7 && c /= current
          discard (c,p) = let l = allResources p in
                          spend (take (length l `quot` 2) l) c


-- -- TODO pick  aplayer to steal from
-- moveRobber :: TileLocation -> MyState ()
-- moveRobber t = do
--     game <- S.get
--     let bs = buildings game
--         options = mapMaybe playerAtCorner (buildingTiles bs)
--     game
--     where
--         playerAtCorner ls (City c (_, l)) | l `elem` ls = Just c
--         playerAtCorner ls (Settlement c (_, l)) | l `elem` ls = Just c
--         playerAtCorner _ _ = Nothing
