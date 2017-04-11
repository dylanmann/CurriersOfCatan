module CatanActions where

import Data.Maybe(mapMaybe)
import qualified Data.List as List
import qualified Control.Monad.State as S
import qualified Data.Tuple.Select as Tuple
import qualified Data.Tuple.Update as Tuple
import Control.Monad.State(State)
import Control.Monad (guard, when, unless)
import Data.Maybe(fromJust, isNothing)
import CatanTypes

getResource :: Resource -> Resources -> Int
getResource Brick = Tuple.sel1
getResource Lumber = Tuple.sel2
getResource Ore = Tuple.sel3
getResource Grain = Tuple.sel4
getResource Wool = Tuple.sel5

setResource :: Resource -> Int -> Resources -> Resources
setResource Brick = Tuple.upd1
setResource Lumber = Tuple.upd2
setResource Ore = Tuple.upd3
setResource Grain = Tuple.upd4
setResource Wool = Tuple.upd5

updResource :: (Int -> Int) -> Resource -> Resources -> Resources
updResource f r rs = setResource r (f (getResource r rs)) rs

produces :: Tile -> Maybe Resource
produces (Desert, _, _) = Nothing
produces (terr, _, _)   = (Just . toEnum . fromEnum) terr

buildingVP :: Building -> Int
buildingVP (Settlement _ _) = 1
buildingVP (City _ _)   = 2

ownedBy :: Color -> Building -> Bool
ownedBy c1 (Settlement c2 _) = c1 == c2
ownedBy c1 (City c2 _)       = c1 == c2

cardVP :: DevCard -> Int
cardVP VictoryPoint = 1
cardVP _            = 0

-- TODO is there a nicer way
reduceResources :: [Resource] -> Resources
reduceResources = foldr (updResource (+1)) (0,0,0,0,0)

cornerRewards :: Token -> Corner -> Resources
cornerRewards tok (r,_) = reduceResources $ mapMaybe produces (ts r) where
    ts (TwoTiles t1 t2)        = filter payingTile [t1, t2]
    ts (ThreeTiles t1 t2 t3)   = filter payingTile [t1, t2, t3]
    ts (GenericHarbor t1 t2)   = filter payingTile [t1, t2]
    ts (SpecialHarbor t1 t2 _) = filter payingTile [t1, t2]
    payingTile (_, t, _)       = tok == t

buildingRewards :: Token -> Building -> (Color, Resources)
buildingRewards tok (Settlement c loc) = (c, cornerRewards tok loc)
buildingRewards tok (City c loc)       = (c, mapR (* 2) $ cornerRewards tok loc)

addResources :: Resources -> Player -> Player
addResources r p = p {resources = combineResources (resources p) r} where
    combineResources (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) =
        (a1+a2, b1+b2, c1+c2, d1+d2, e1+e2)

updPlayer :: (Player -> Player) -> Color -> Players -> Players
updPlayer f Blue ps   = ps { bluePlayer = f $ bluePlayer ps }
updPlayer f Red ps    = ps { redPlayer = f $ redPlayer ps }
updPlayer f Orange ps = ps { orangePlayer = f $ orangePlayer ps }
updPlayer f White ps  = ps { whitePlayer = f $ whitePlayer ps }

getPlayer :: Color -> Players -> Player
getPlayer Blue   = bluePlayer
getPlayer Red    = redPlayer
getPlayer Orange = orangePlayer
getPlayer White  = whitePlayer

validPlayer :: Player -> Bool
validPlayer p = let (a,b,c,d,e) = resources p in
                all (>= 0) [a,b,c,d,e]

-- TODO: refactor spend to return a state so that it just spends from game
spend :: [Resource] -> Color ->  Players -> Players
spend rs c ps = foldr spend1 ps rs where
    spend1 r = updPlayer (\p -> p {resources = updResource (\x -> x - 1) r $ resources p}) c

allocateRewards :: Token -> State Game ()
allocateRewards tok = do
    game <- S.get
    let bs = buildings game
        ps = players game
        rewards = map (buildingRewards tok) bs
        step (c, r) g = let newPs = updPlayer (addResources r) c ps in
                        g { players = newPs }
    S.put (foldr step game rewards)

containsRoad :: CornerLocation -> CornerLocation -> Roads -> Bool
containsRoad new1 new2 = any sameRoad where
    sameRoad (old1, old2, _) = (old1 == new1 && old2 == new2) ||
                               (old2 == new1 && old1 == new2)


-- TODO: check if roads connects to existing roads or settlements
buildRoad :: Color -> CornerLocation -> CornerLocation -> State Game ()
buildRoad c loc1 loc2 = do
    game <- S.get
    let ps = players game
        rs = roads game
        newPs = spend [Lumber, Brick] c ps
        validP = validPlayer $ getPlayer c newPs
        validR = not $ containsRoad loc1 loc2 rs
        newRs =  (loc1, loc2, c) : rs
        update = S.put(game { players = newPs, roads = newRs})
    when (validP && validR) update


buildCity :: Color -> Corner -> State Game ()
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

containsBuild :: Corner -> [Building] -> Bool
containsBuild new = any samePlace where
    samePlace (Settlement _ old) = old == new
    samePlace (City _ old)       = old == new

-- TODO need to check adjacent clearings
buildSett :: Color -> Corner -> State Game ()
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


updateArmy :: Color -> State Game ()
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

drawCard :: State Game (Maybe DevCard)
drawCard = do
    game <- S.get
    case deck game of
        [] -> return Nothing
        card : rest -> do S.put( game { deck = rest } )
                          return $ Just card

unDrawCard :: DevCard -> State Game ()
unDrawCard card = do
    game <- S.get
    S.put (game {deck = card : (deck game) } )


-- TODO: need to randomize card type
buyCard :: Color -> State Game ()
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


-- gameOver is player C has 10 VP (only on their turn)
gameOver :: Color -> State Game Bool
gameOver c = do
    game <- S.get
    let p = getPlayer c $ players game
        bVP = sum $ map buildingVP $ filter (ownedBy c) (buildings game)
        cVP = sum $ map cardVP $ cards p
        armyVP = if largestArmy game == Just c then 2 else 0
        roadVP = 0 -- TODO: unimplemented
    return $ bVP + cVP + armyVP >= 10
