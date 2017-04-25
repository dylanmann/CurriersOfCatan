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
  game <- liftIO $ takeMVar gameVar

  liftIO $ print currentPlayer
  liftIO (putStr "roll: " >> print r)
  heading <- UI.h1 # set text "Curriers of Catan"

  (endturnbutton, endturnview) <- mkButton "End Turn"
  (buildRoadButton, buildRoadView) <- mkButton "build road"

  div <- UI.div # set SVG.id "back"

  getBody w #+ [element heading
               , return div #+ [background game]
               , element endturnview
               , element buildRoadView]
  on UI.click endturnbutton $ \_ -> liftIO $ endTurn mvars

  on UI.click buildRoadButton $ \_ -> do _ <- liftIO $ sendAction (Cheat [Brick, Lumber]) mvars
                                         g <- liftIO $ sendAction (BuildRoad (fromJust $ makeCornerLocation 2 (-1) False) (fromJust $ makeCornerLocation 1 0 True)) mvars
                                         w <- askWindow
                                         es <- getElementsByClassName w "render"
                                         e <- getElementById w "mainHex"
                                         (return $ fromJust $ e) #+ [foreground g]
                                         foldr (\x acc -> delete x >> acc) (return ()) es
                                         return ()
hexPoints ::  (Integral t1, Integral t2, Integral t3) => t1 -> t2 -> t3 -> String
hexPoints x1 y1 r1 =
  unwords (map hexCorner [0..5])
  where hexCorner i =
         let x = fromIntegral x1
             y = fromIntegral y1
             r = fromIntegral r1
             angle_deg = 60 * i + 30
             angle_rad = pi / 180 * angle_deg in
         show (x + r * (cos angle_rad)) ++ "," ++ show (y + r * (sin angle_rad))

flatHexPoints ::  (Integral t1, Integral t2, Integral t3) => t1 -> t2 -> t3 -> String
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

foreground :: Game -> UI Element
foreground Game{..} = do
  let bs = map drawBuilding buildings
      rs = map drawRoad roads
      (x, y) = hexToPixel robberTile
  robber <- SVG.circle
    # set SVG.r "40"
    # set SVG.cx (show x)
    # set SVG.cy (show y)
    # set SVG.stroke "rgb(103, 128, 159)"
    # set SVG.stroke_width "1"
    # set SVG.fill "rgb(103, 128, 159)"
    # set SVG.opacity ".7"
  SVG.g #+ (element robber : (bs ++ rs))
  where
    drawBuilding (Settlement c l) =
      let (x,y) = cornerToPixel l in
      SVG.circle
        # set SVG.r "20"
        # set SVG.class_ "render"
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
    drawBuilding (City c l) = undefined
    drawRoad r = do
      let (l1, l2, c) = getRoad r
      let (x1,y1) = cornerToPixel l1
      let (x2,y2) = cornerToPixel l2
      g <- SVG.g # set SVG.class_ "render"
      return g #+ [SVG.line
        # set SVG.r "20"
        # set SVG.x1 (show x1)
        # set SVG.y1 (show y1)
        # set SVG.x2 (show x2)
        # set SVG.y2 (show y2)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "10"
       , SVG.circle
        # set SVG.r "5"
        # set SVG.cx (show x1)
        # set SVG.cy (show y1)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
       , SVG.circle
        # set SVG.r "5"
        # set SVG.cx (show x2)
        # set SVG.cy (show y2)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
       ]

background :: Game -> UI Element
background game@Game{..} = do
  let height = 12 * hexSize :: Integer
  context <- SVG.svg
    # set SVG.id "mainHex"
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
  return context #+ ((shadow: element bg : hexes) ++ [foreground game])

  where
    drawHex index = do
      let (x,y) = hexToPixel index
      let color = getTileColor index
      let token = getToken index
      hex <- SVG.polygon
        # set SVG.class_ "hex"
        # set SVG.points (hexPoints x y hexSize)
        # set SVG.stroke "rgb(34, 49, 63)"
        # set SVG.stroke_width "2"
        # set SVG.fill color
      if token == "" then SVG.g #+ [element hex] else do
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
        SVG.g #+ [element hex, element circ, element t]
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
    getToken index = --show (fst $ tileToAxial index) ++ "  " ++
                     --show (snd $ tileToAxial index)
      case getTile board index of
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

colorToRGB :: Color -> String
colorToRGB c = case c of
  Blue -> "rgb(65, 131, 215)"
  Red -> "rgb(217, 30, 24)"
  Orange -> "rgb(248, 148, 6)"
  White -> "rgb(236,236,236)"

cornerToPixel :: CornerLocation -> (Int, Int)
cornerToPixel cl =
    let (q1, r1, t) = cornerToAxial cl
        q = fromIntegral q1
        r = fromIntegral r1
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r
        x' = x + 6 * hexSize
        y' = y + 6 * hexSize + ((if t then negate else id) $ hexSize)
    in (round x', round y')


hexToPixel :: TileLocation -> (Int, Int)
hexToPixel cl =
    let (q1, r1) = tileToAxial cl
        q = fromIntegral q1
        r = fromIntegral r1
        x = hexSize * (q + r/2.0) * sqrt 3
        y = hexSize * 3.0/2.0 * r
        x' = (x + 6 * hexSize)
        y' = (y + 6 * hexSize) in
    (round x', round y')


tile1 = fromJust (makeTileLocation 0 1)
tile2 = fromJust (makeTileLocation 0 0)

sendAction :: PlayerAction -> CatanMVars -> IO Game
sendAction action CatanMVars{..} = do
  putMVar actionVar action
  game@Game{..} <- takeMVar gameVar
  mapM_ putStrLn errorMessage
  when (action == PlayKnight) (robberSequence game)
  return game

robberSequence :: Game -> IO ()
robberSequence Game{..} = do
  let CatanMVars{..} = mvars
  if robberTile == tile1 then
    putMVar robberVar tile2
    else
      putMVar robberVar tile1
  takeMVar gameVar
  r <- tryTakeMVar stealVar
  case r of
    Nothing -> return ()
    Just ps -> let color = snd $ head ps in
                  putMVar colorVar color

endTurn :: CatanMVars -> IO ()
endTurn m@CatanMVars{..} = do
  _ <- sendAction EndTurn m
  roll <- takeMVar rollVar
  putStr "roll: " >> print roll
  game@Game{..} <- takeMVar gameVar
  print $ currentPlayer
  when (roll == 7) $ robberSequence game


renderGame game = undefined
