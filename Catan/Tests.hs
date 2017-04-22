module Tests where

import Test.HUnit
import qualified Control.Monad.State as S

import Data.Maybe(mapMaybe, fromJust)

import Board
import Types
import Actions
import GamePlay


defaultGame :: IO Game
defaultGame = do
  b <- setupBoard
  d <- shuffle devCards
  let p = defaultPlayers
  m <- makeMVars
  return $
   Game b p defaultRoads defaultBuildings (desert b) Nothing Nothing d White Nothing [] m

gameWithPlayer :: Color -> Player -> IO Game
gameWithPlayer c p = do
  g <- defaultGame
  return $ g {players = updPlayer (const p) c (players g),
              currentPlayer = c}

playerWithResources :: [Resource] -> Player
playerWithResources rs = getPlayer Red (recieve rs Red defaultPlayers)

playerWithCards :: [DevCard] -> Player
playerWithCards cs = let p = getPlayer Red defaultPlayers in
                     p { cards = cs }

cl :: (Int, Int) -> CornerLocation
cl = fromJust . uncurry makeCornerLocation

roads1 :: Roads
roads1 = fmap convert [((0,0), (0,1), White), ((0,1), (0,2), White),
  ((0,2), (1,2), White), ((2,2), (1,2), White)] where
  convert (t1, t2, c) = (cl t1, cl t2, c)

roads2 = fmap convert [((1,0), (1,1), Red), ((1,1), (2,1), Red),
  ((2,1), (3,1), Red), ((3,1), (4,1), Red)] where
  convert (t1, t2, c) = (cl t1, cl t2, c)


longestRoadT :: Test
longestRoadT = TestList[ newLongestRoad (Just White) roads1 ~?= Just White,
  newLongestRoad (Just Red) roads1 ~?= (Just White),
  newLongestRoad Nothing roads1 ~?= (Just White),
  newLongestRoad Nothing ((cl (29,2), cl (0,2), Red):roads1) ~?= Nothing,
  newLongestRoad (Just Red) (roads1 ++ roads2) ~?= Just Red,
  newLongestRoad (Just White) (roads1 ++ roads2) ~?= Just White]


goTest0 :: IO Test
goTest0 = do
  let p = playerWithCards (replicate 10 VictoryPoint)
  g <- gameWithPlayer Red p
  res <- S.evalStateT gameOver (g{currentPlayer = Red})
  return $ test $ assertBool "GameOver" res

goTest1 :: IO Test
goTest1 = do
  let p = playerWithCards (replicate 9 VictoryPoint)
  g <- gameWithPlayer Red p
  res <- S.evalStateT gameOver (g{currentPlayer = Red,
                                  buildings = []})
  return $ test $ assertBool "GameOver" (not res)

goTest2 :: IO Test
goTest2 = do
  d <- defaultGame
  let p = playerWithResources (concat $ replicate 10 [Brick, Lumber, Wool, Grain])
  g <- gameWithPlayer Red p

  let bs = mapMaybe (fmap (City Red)  . uncurry makeCornerLocation)
       [(0,0), (0,2), (2,2), (4,2), (6,2)]
  res <- S.evalStateT gameOver (g{buildings = bs})
  return $ test $ assertBool "GameOver" res

goTest3 :: IO Test
goTest3 = do
  d <- defaultGame
  let p = playerWithResources (concat $ replicate 10 [Brick, Lumber, Wool, Grain])
  g <- gameWithPlayer Red p

  let bs = mapMaybe (fmap (City Red)  . uncurry makeCornerLocation)
       [(0,0), (0,2), (2,2), (4,2)]
  res <- S.evalStateT gameOver (g{buildings = bs})
  return $ test $ assertBool "GameOver" (not res)

gameOverTest :: IO ()
gameOverTest = do
  ts <- fmap TestList (sequence [goTest0,goTest1,goTest2, goTest3])
  runTestTT ts
  return ()
