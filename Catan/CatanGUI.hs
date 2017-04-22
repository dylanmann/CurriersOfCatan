{-# LANGUAGE RecordWildCards #-}

module CatanGUI (beginGUI) where

import           Control.Monad                      (void)

import           Types
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG

import Control.Concurrent.MVar
import Control.Concurrent(threadDelay)

{-----------------------------------------------------------------------------
    SVG
------------------------------------------------------------------------------}
data WindowSize = WindowSize {x :: Int, y :: Int}
-- hexSize :: Int 
hexSize = 60

beginGUI :: Game -> IO ()
beginGUI cmvars = startGUI defaultConfig { jsLog = \_ -> putStr "" } (setup cmvars)

mkButton :: String -> UI (Element, Element)
mkButton title = do
  button <- UI.button #. "button" #+ [string title]
  view   <- UI.p #+ [element button]
  return (button, view)

setup :: Game -> Window -> UI ()
setup game@Game{..} w = void $ do
  let CatanMVars{..} = mvars
  return w # set title "Settlers of Catan"

  heading <- UI.h1 # set text "Settlers of Catan"

  (endturnbutton, endturnview) <- mkButton "End Turn"
  on UI.click endturnbutton $ \_ -> do
    liftIO $ endTurn game

  getBody w #+ [element heading
               , UI.div #+ [element endturnview, svgElems, UI.h3 # set text "lol"]
               , UI.div # set html strCircle #+ [UI.h3 # set text "fuck dylan"]
               , UI.div #+ [background game]
               ]

svgElems :: UI Element
svgElems = do
  context <- SVG.svg
    # set SVG.width "400"
    # set SVG.height "400"
  elemCircle <- SVG.polygon
    # set SVG.class_ "hex"
    # set SVG.points "300,150 225,280 75,280 0,150 75,20 225,20"
    # set SVG.stroke "green"
    # set SVG.stroke_width "4"
    # set SVG.fill "yellow"
  on UI.hover elemCircle $ const $ do 
    element elemCircle # set SVG.fill "blue"
  on UI.leave elemCircle $ const $ do 
    element elemCircle # set SVG.fill "yellow"
  return context #+ [element elemCircle]



strCircle :: String
strCircle = "<svg width=\"150\" height=\"100\">"
         ++ "  <circle cx=\"100\" cy=\"50\" r=\"40\" stroke=\"gray\" stroke-width=\"4\" fill=\"orange\" />"
         ++ "</svg>"

background :: Game -> UI Element
background game = do
  let height = 10 * hexSize  
  bg <- SVG.svg
    # set SVG.width "2000"
    # set SVG.height "2000"
  let hexes = map drawHex (take 1 tileIndices)
  return bg #+ hexes

  where 
    drawHex index = do 
      let (x,y) = hexToPixel $ tileToAxial index 
      hex <- SVG.circle
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.r (show hexSize)
        # set SVG.stroke "black"
        # set SVG.stroke_width "1"
        # set SVG.fill "blue"
      element hex 

-- hexToPixel :: (Integral t1, Integral t) => (Double, Double) -> (t, t1)
hexToPixel (q1, r1) = 
    let q = fromIntegral q1
        r = fromIntegral r1 
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r in
    ((round x) + 1000, (round y) + 1000)

endTurn game@Game{..} = do
  let CatanMVars{..} = mvars 
  r <- takeMVar requestVar
  case r of
    NextMove -> do 
      game <- takeMVar gameVar
      _ <- takeMVar nameVar
      putMVar actionVar EndTurn
      -- check if robber needs to be moved here
      -- render new game here
    _ -> undefined

     -- MoveRobber ->          -- shouldn't happen??
     --                       do tile <- promptForRobber
     --                          putMVar robberVar tile
     --                          takeMVar gameVar >>= print

     -- StealFrom ps ->       do color <- getChoiceFrom ps
     --                          putMVar colorVar color

     -- RoadBuildingChoice -> do roads <- promptForRoadBuilder c
     --                          putMVar roadVar roads

     -- YearOfPlentyChoice -> do rs <- promptForYOP
     --                          putMVar yopVar rs

     -- MonopolyChoice ->     do res <- promptForMonopoly
     --                          putMVar monopolyVar res

renderGame game = undefined