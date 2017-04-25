{-# LANGUAGE RecordWildCards #-}

module CatanGUI (beginGUI) where

import           Control.Monad(when, void)


import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG.Elements  as SVG
import qualified Graphics.UI.Threepenny.SVG.Attributes as SVG hiding (filter)
import qualified Graphics.UI.Threepenny.SVG.Attributes as SVGA (filter)
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent(threadDelay)
import           Data.Maybe(isJust, fromJust)
import           Types
import           Actions(getCatanMVars)
import           ActionParsing

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
  r <- liftIO $ takeMVar rollVar
  liftIO (putStr "roll: " >> print r)
  heading <- UI.h1 # set text "Curriers of Catan"

  (endturnbutton, endturnview) <- mkButton "End Turn"
  on UI.click endturnbutton $ \_ -> liftIO $ endTurn mvars

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

shadow :: UI Element
shadow = do
  d <- SVG.defs
  filt <- SVG.filter
    # set SVG.id "f1"
    # set SVG.width "200%"
    # set SVG.height "200%"
  offset <- SVG.feOffset
    # set SVG.result "offOut"
    # set SVG.in_ "SourceGraphic"
    # set SVG.dx "2"
    # set SVG.dy "2"
  mat <- SVG.feColorMatrix
    # set SVG.result "matrixOut"
    # set SVG.in_ "offOut"
    # set SVG.type_ "matrix"
    # set SVG.values "0.2 0 0 0 0 0 0.2 0 0 0 0 0 0.2 0 0 0 0 0 1 0"
  blur <- SVG.feGaussianBlur
    # set SVG.result "blurOut"
    # set SVG.in_ "matrixOut"
    # set SVG.stdDeviation "50"
  blend <- SVG.feBlend
    # set SVG.in_ "SourceGraphic"
    # set SVG.in2 "blurOut"
    # set SVG.mode "normal"
  return d #+ [element filt #+ [element offset, element mat, element blur, element blend]]

background :: Game -> UI Element
background game@Game{..} = do
  let height = 12 * hexSize :: Integer
  context <- SVG.svg
    # set SVG.width (show height)
    # set SVG.height (show height)
  bg <- SVG.polygon
    # set SVG.class_ "hex"
    # set SVG.points (flatHexPoints (height `div` 2) (height `div` 2) (height `div` 2))
    # set SVG.stroke "rgb(34, 49, 63)"
    # set SVG.stroke_width "1"
    # set SVG.fill "rgb(129,207,224)"
    # set SVGA.filter "url(#f1)"
  let hexes = map drawHex tileIndices
  let bs = map drawBuilding buildings
  return context #+ ((shadow: element bg : hexes) ++ bs)

  where
    drawHex index = do
      let (x',y') = hexToPixel $ tileToAxial index
      let x = (x' + 6 * hexSize)
      let y = (y' + 6 * hexSize)
      let color = getTileColor index
      let token = getToken index
      context <- SVG.g
      hex <- SVG.polygon
        # set SVG.class_ "hex"
        # set SVG.points (hexPoints x y hexSize)
        # set SVG.stroke "rgb(34, 49, 63)"
        # set SVG.stroke_width "2"
        # set SVG.fill color
      if token == "" then return context #+ [element hex] else do
        circ <- SVG.circle
          # set SVG.r "30"
          # set SVG.cx (show x)
          # set SVG.cy (show y)
          # set SVG.stroke "rgb(34, 49, 63)"
          # set SVG.stroke_width "1"
          # set SVG.fill "rgb(228, 241, 254)"
          # set SVGA.filter "url(#f1)"
        t <- SVG.text
          # set SVG.text_anchor "middle"
          # set SVG.alignment_baseline "central"
          # set SVG.x (show x)
          # set SVG.y (show y)
          # set SVG.font_size "35"
          # set SVG.font_family "Palatino"
          # set SVG.font_weight "bold"
          # set text token
          # set SVG.fill "black"
        return context #+ [element hex, element circ, element t]
    getTileColor index =
      let tile = getTile board index in
      case tile of
        Paying t _ -> case t of
          Hills -> "rgb(179, 94, 30)"
          Forest -> "rgb(30, 130, 76)"
          Mountains -> "rgb(190, 144, 212)"
          Fields -> "rgb(135, 211, 124)"
          Pasture -> "rgb(245, 215, 110)"
        Desert -> "rgb(253, 227, 167)"
    getToken index = show (fst $ tileToAxial index) ++ "  " ++
                     show (snd $ tileToAxial index)
      -- case getTile (board game) index of
        -- Paying _ tok -> show $ case tok of
        --   Two    -> 2
        --   Three  -> 3
        --   Four   -> 4
        --   Five   -> 5
        --   Six    -> 6
        --   Eight  -> 8
        --   Nine   -> 9
        --   Ten    -> 10
        --   Eleven -> 11
        --   Twelve -> 12
        -- Desert -> ""
    drawBuilding (Settlement c l) = do
      let (x',y', top) = cornerToPixel $ cornerToAxial l
      let x = x' + 6 * hexSize
      let y = y' + 6 * hexSize + ((if top then negate else id) $ hexSize)
      circ <- SVG.circle
        # set SVG.r "20"
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.stroke "rgb(34, 49, 63)"
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
        # set SVGA.filter "url(#f1)"
      return circ
    drawBuilding (City c l) = undefined

colorToRGB :: Color -> String
colorToRGB c = case c of
  Blue -> "blue"
  Red -> "red"
  Orange -> "orange"
  White -> "white"

cornerToPixel :: (Int, Int, Bool) -> (Int, Int, Bool)
cornerToPixel (q1, r1, t) =
    let q = fromIntegral q1
        r = fromIntegral r1
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r in
    (round x, round y, t)

hexToPixel :: (Int, Int) -> (Int, Int)
hexToPixel (q1, r1) =
    let q = fromIntegral q1
        r = fromIntegral r1
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r in
    (round x, round y)

tile1 = fromJust (makeTileLocation 0 1)
tile2 = fromJust (makeTileLocation 0 0)

endTurn CatanMVars{..} = do
  putMVar actionVar EndTurn
  game@Game{..} <- takeMVar gameVar
  roll <- takeMVar rollVar
  putStr "roll: " >> print roll
  r <- takeMVar requestVar
  case r of
    NextMove -> do print currentPlayer
                   when (roll == 7) $ do
                     print "move robber"
                     if robberTile == tile1 then
                        putMVar robberVar tile2
                        else
                          putMVar robberVar tile1
                     takeMVar gameVar
                     r <- takeMVar requestVar
                     case r of
                       StealFrom ps -> do
                         color <- getChoiceFrom ps
                         putMVar colorVar color
                       _ -> return ()
                     endTurn mvars

    _ -> print "shouldnt hit here"



renderGame game = undefined
