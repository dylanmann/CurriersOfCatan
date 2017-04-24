{-# LANGUAGE RecordWildCards #-}

module CatanGUI (beginGUI) where

import           Control.Monad                      (void)

import           Types
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG.Elements  as SVG
import qualified Graphics.UI.Threepenny.SVG.Attributes  as SVG hiding (filter)
import qualified Graphics.UI.Threepenny.SVG.Attributes as SVGA (filter)
import Control.Concurrent.MVar
import Control.Concurrent(threadDelay)

{-----------------------------------------------------------------------------
    SVG
------------------------------------------------------------------------------}
data WindowSize = WindowSize {x :: Int, y :: Int}
hexSize :: Num a => a
hexSize = 80

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
  return w # set title "Curriers of Catan"

  heading <- UI.h1 # set text "Curriers of Catan"

  (endturnbutton, endturnview) <- mkButton "End Turn"
  on UI.click endturnbutton $ \_ -> liftIO $ endTurn game

  getBody w #+ [element heading
               , UI.div #+ [background game]
               , element endturnview]

hexPoints x1 y1 r1 =
  unwords (map hexCorner [0..5])
  where hexCorner i =
         let x = fromIntegral x1
             y = fromIntegral y1
             r = fromIntegral r1
             angle_deg = 60 * i + 30
             angle_rad = pi / 180 * angle_deg in
         show (x + r * (cos angle_rad)) ++ "," ++ show (y + r * (sin angle_rad))


flatHexPoints x1 y1 r1 =
  unwords (map hexCorner [0..5])
  where hexCorner i =
         let x = fromIntegral x1
             y = fromIntegral y1
             r = fromIntegral r1
             angle_deg = 60 * i
             angle_rad = pi / 180 * angle_deg in
         show (x + r * (cos angle_rad)) ++ "," ++ show (y + r * (sin angle_rad))


-- shadow x y = do
--   filt <- SVG.filter
--     # set SVG.id "f4"
--     # set SVG.x (show x)
--     # set SVG.y (show y)
--     # set SVG.width "200%"
--     # set SVG.height "200%"
--   offset <- SVG.feOffset
--     # set SVG.result "offOut"
--     # set SVG.in_ "SourceGraphic"
--     # set SVG.dx "20"
--     # set SVG.dy "20"
--   mat <- SVG.feColorMatrix
--     # set SVG.result "matrixOut"
--     # set SVG.in_ "offOut"
--     # set SVG.type_ "matrix"
--     # set SVG.values "0.2 0 0 0 0 0 0.2 0 0 0 0 0 0.2 0 0 0 0 0 1 0"
--   blur <- SVG.feGaussianBlur
--     # set SVG.result "blurOut"
--     # set SVG.in_ "matrixOut"
--     # set SVG.stdDeviation "10"
--   blend <- SVG.feBlend
--     # set SVG.in_ "SourceGraphic"
--     # set SVG.in2 "blurOut"
--     # set SVG.mode "normal"
--   SVG.defs #+ [element filt #+ [element offset, element mat, element blur, element blend]]

background :: Game -> UI Element
background game = do
  let height = 12 * hexSize :: Integer
  context <- SVG.svg
    # set SVG.width (show height)
    # set SVG.height (show height)
  bg <- SVG.polygon
    # set SVG.class_ "hex"
    # set SVG.points (flatHexPoints (height `div` 2) (height `div` 2) (height `div` 2))
    # set SVG.stroke "black"
    # set SVG.stroke_width "1"
    # set SVG.fill "rgb(129,207,224)"
  let hexes = map drawHex tileIndices
  return context #+ ((element bg) : hexes)

  where
    drawHex index = do
      let (x,y) = hexToPixel $ tileToAxial index
      let color = getTileColor index
      let token = getToken index
      context <- SVG.g
      hex <- SVG.polygon
        # set SVG.class_ "hex"
        # set SVG.points (hexPoints (x + 6 * hexSize) (y + 6 * hexSize) hexSize)
        # set SVG.stroke "black"
        # set SVG.stroke_width "1"
        # set SVG.fill color
      if token == "" then return context #+ [element hex] else do
        circ <- SVG.circle
          # set SVG.r "30"
          # set SVG.cx (show (x + 6 * hexSize))
          # set SVG.cy (show (y + 6 * hexSize))
          # set SVG.stroke "black"
          # set SVG.stroke_width "1"
          # set SVG.fill "rgb(228, 241, 254)"
        t <- SVG.text
          # set SVG.text_anchor "middle"
          # set SVG.alignment_baseline "central"
          # set SVG.x (show (x + 6 * hexSize))
          # set SVG.y (show (y + 6 * hexSize))
          # set SVG.font_size "35"
          # set SVG.font_family "Palatino"
          # set SVG.font_weight "bold"
          # set text token
          # set SVG.fill "black"
        return context #+ [element hex, element circ, element t]
    getTileColor index =
      let tile = getTile (board game) index in
      case tile of
        Paying t _ -> case t of
          Hills -> "rgb(179, 94, 30)"
          Forest -> "rgb(30, 130, 76)"
          Mountains -> "rgb(190, 144, 212)"
          Fields -> "rgb(135, 211, 124)"
          Pasture -> "rgb(245, 215, 110)"
        Desert -> "rgb(253, 227, 167)"
    getToken index = case getTile (board game) index of
        Paying _ tok -> show $ case tok of
          Two    -> 2
          Three  -> 3
          Four   -> 4
          Five   -> 5
          Six    -> 6
          Eight  -> 8
          Nine   -> 9
          Ten    -> 10
          Eleven -> 11
          Twelve -> 12
        Desert -> ""

hexToPixel :: (Int, Int) -> (Int, Int)
hexToPixel (q1, r1) =
    let q = fromIntegral q1
        r = fromIntegral r1
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r in
    (round x, round y)

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
