{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module CatanGUI (beginGUI) where

import           Control.Monad(when, void)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG.Elements  as SVG
import qualified Graphics.UI.Threepenny.SVG.Attributes  as SVG hiding (filter, mask)
import           Control.Concurrent.MVar.Lifted
import           Data.Maybe(fromJust)
import           Types
import           Control.Monad.Base
import Control.Concurrent(threadDelay)

{-----------------------------------------------------------------------------
    SVG
------------------------------------------------------------------------------}
-- data WindowSize = WindowSize {x :: Int, y :: Int}
hexSize :: Num a => a
hexSize = 60

beginGUI :: CatanMVars -> IO ()
beginGUI cmvars = startGUI defaultConfig { jsLog = \_ -> putStr "" } (setup cmvars)

mkButton :: String -> UI (Element, Element)
mkButton buttonTitle = do
  button <- UI.button #. "button actionButton" #+ [string buttonTitle]
  view   <- UI.p #+ [element button]
  return (button, view)

setup :: CatanMVars -> Window -> UI ()
setup CatanMVars{..} w = void $ do
  _ <- return w # set title "Curriers of Catan"

  game@Game{..} <- takeMVar gameVar

  heading <- UI.h1 # set text "Curriers of Catan"
  subHeading <- UI.h2 # set text ("Current Player: " ++ show currentPlayer)
                      # set SVG.id "player"
  (endturnbutton, endturnview)     <- mkButton "End Turn"
  (buildRoadButton, buildRoadView) <- mkButton "build road"
  (buildSettButton, buildSettView) <- mkButton "build settlement"
  (buildCityButton, buildCityView) <- mkButton "build city"

  buttons <- row [element endturnview
                  , element buildRoadView
                  , element buildSettView
                  , element buildCityView]

  menu <- column [element heading
                 , element subHeading
                 , drawResources game
                 , drawCards game]
            # set UI.id_ "menu"
  -- let (board, tiles) = background game
  -- let board = background game

  -- let tiles = background game
  -- let board = drawBoard tiles
  -- let _ = setHover tiles setTileHover setTileLeave

  _ <- getBody w #+ [ element menu
                     , element buttons
                     , background game ]

  on UI.click endturnbutton $ \_ -> endTurn $ mvars

  on UI.click buildRoadButton $
      \_ -> makeCorners $ \i1 -> makeCorners $ (\i2 -> do _ <- sendAction (Cheat [Lumber, Brick]) mvars
                                                          _ <- sendAction (BuildRoad i1 i2) mvars
                                                          return ())


  on UI.click buildSettButton $
      \_ -> makeCorners $ (\index -> do _ <- sendAction (Cheat [Lumber, Grain, Wool, Brick]) mvars
                                        _ <- sendAction (BuildSettlement index) mvars
                                        return ())

  on UI.click buildCityButton $
      \_ -> makeCorners $ (\index -> do _ <- sendAction (Cheat [Ore, Ore, Ore, Grain, Grain]) mvars
                                        _ <- sendAction (BuildCity index) mvars
                                        return ())


drawResources :: Game -> UI Element 
drawResources Game{..} = do
  resourcesp <- UI.p 
    # set UI.id_ "resources"
    # set UI.text ("Resources: " ++ (show (resources (getPlayer currentPlayer players))))
  return resourcesp

drawCards :: Game -> UI Element
drawCards Game{..} = do
  let devcards = cards (getPlayer currentPlayer players)
  let listitems = map (\c -> do 
      button <- UI.button
        # set UI.class_ "button list-group-item"
        # set UI.type_ "button"
        # set UI.text_ (show c)
      return button) devcards
  cardsTitle <- UI.h3 # set text "Development cards:"
  list <- UI.div
    # set UI.id_ "devcardsdiv"
    # set UI.class_ "list-group cards"
    #+ ((element cardsTitle):listitems)
  return list


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

-- shadow :: UI Element
-- shadow = do
--   d <- SVG.defs
--   filt <- SVG.filter
--     # set SVG.id "f1"
--     # set SVG.width "200%"
--     # set SVG.height "200%"
--   offset <- SVG.feOffset
--     # set SVG.result "offOut"
--     # set SVG.in_ "SourceGraphic"
--     # set SVG.dx "2"
--     # set SVG.dy "2"
--   mat <- SVG.feColorMatrix
--     # set SVG.result "matrixOut"
--     # set SVG.in_ "offOut"
--     # set SVG.type_ "matrix"
--     # set SVG.values "0.2 0 0 0 0 0 0.2 0 0 0 0 0 0.2 0 0 0 0 0 1 0"
--   blur <- SVG.feGaussianBlur
--     # set SVG.result "blurOut"
--     # set SVG.in_ "matrixOut"
--     # set SVG.stdDeviation "50"
--   blend <- SVG.feBlend
--     # set SVG.in_ "SourceGraphic"
--     # set SVG.in2 "blurOut"
--     # set SVG.mode "normal"
--   return d #+ [element filt #+ [element offset, element mat, element blur, element blend]]

foreground :: Game -> UI Element
foreground Game{..} = do
  let bs = map drawBuilding buildings
      rs = map drawRoad roads
      (x, y) = hexToPixel robberTile
  robber <- SVG.circle
    # set SVG.class_ "render"
    # set SVG.r (show (hexSize / 2))
    # set SVG.cx (show x)
    # set SVG.cy (show y)
    # set SVG.stroke "rgb(103, 128, 159)"
    # set SVG.stroke_width "1"
    # set SVG.fill "rgb(103, 128, 159)"
    # set SVG.opacity ".7"
    # set SVG.pointer_events "none"
  SVG.g #+ (element robber : (bs ++ rs))
  where
    drawBuilding (Settlement c l) =
      let (x,y) = cornerToPixel l in
      SVG.circle
        # set SVG.r (show (hexSize/3))
        # set SVG.class_ "render"
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
    drawBuilding (City c l) =
      let (x, y) = cornerToPixel l in
      SVG.circle
        # set SVG.r (show (hexSize/2))
        # set SVG.class_ "render"
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.stroke (colorToRGB c)
        # set SVG.stroke_width "1"
        # set SVG.fill (colorToRGB c)
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
        # set SVG.stroke_width "0"
        # set SVG.fill (colorToRGB c)
       , SVG.circle
        # set SVG.r "5"
        # set SVG.cx (show x2)
        # set SVG.cy (show y2)
        # set SVG.stroke_width "0"
        # set SVG.fill (colorToRGB c)
       ]


getTileColor :: Board -> TileLocation -> String
getTileColor board index =
  let tile = getTile board index in
  case tile of
    Paying t _ -> case t of
      Hills -> "rgb(179, 94, 30)"
      Forest -> "rgb(30, 130, 76)"
      Mountains -> "rgb(190, 144, 212)"
      -- Mountains -> "rgb(171, 183, 183)"
      Fields -> "rgb(135, 211, 124)"
      Pasture -> "rgb(245, 215, 110)"
    Desert -> "rgb(253, 227, 167)"

makeHexGroup :: Board -> [UI Element]
makeHexGroup board = map drawHex tileIndices
  where
    drawHex index = do
      let (x, y) = hexToPixel index
      let color = getTileColor board index
      let token = getToken index
      hex <- SVG.polygon
        # set SVG.class_ "hex tile"
        # set SVG.points (hexPoints x y hexSize)
        # set SVG.stroke "rgb(34, 49, 63)"
        # set SVG.stroke_width "2"
        # set SVG.fill color
        # set UI.value (show index)
      if token == "" then SVG.g #+ [element hex] else do
        circ <- SVG.circle
          # set SVG.r "25"
          # set SVG.cx (show x)
          # set SVG.cy (show y)
          # set SVG.stroke "rgb(34, 49, 63)"
          # set SVG.stroke_width "1"
          # set SVG.fill "rgb(228, 241, 254)"
          # set SVG.pointer_events "none"
        t <- SVG.text
          # set SVG.text_anchor "middle"
          # set SVG.alignment_baseline "central"
          # set SVG.x (show x)
          # set SVG.y (show y)
          # set SVG.font_size "30"
          # set SVG.font_family "Palatino"
          # set SVG.font_weight "bold"
          # set text token
          # set SVG.fill "black"
          # set SVG.pointer_events "none"
        SVG.g #+ [element hex, element circ, element t]
    getToken index = --show (fst $ tileToAxial index) ++ "  " ++
                   --show (snd $ tileToAxial index)
      case getTile board index of
        Paying _ tok -> show $ case tok of
          Two    -> 2 :: Int
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


background :: Game -> UI Element
background game@Game{..} = do
  let height = 12 * hexSize
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
  let g = SVG.g # set SVG.id "hexGroup" #+ (makeHexGroup board)
  return context #+ (element bg : g : [foreground game])

  -- makeHabors board = 
  --   let harborCorners = 
  --     [((0,-2, True), (0,-3))
  --     ,((1,-2, True), (2,-3))
  --     ,((2,-1, True), (3,-2))
  --     ,((2,1, True), (3,0))
  --     ,((1,2, True), (1,2))
  --     ,((-1,3, True), (-1,3))
  --     ,((-3,3, True), (-3,3))
  --     ,((-3,2, True), (-3,1))
  --     ,((-2,0, True), (-2,-1))]
  --   map drawHarbors harborCorners

  -- drawHarbors cornerInx hexInx = 
  --   let (x, y) = h2p hexInx 

  -- h2p (q1, r1) =
  --   let q = fromIntegral q1
  --       r = fromIntegral r1
  --       x = hexSize * (q + r/2.0) * sqrt 3
  --       y = hexSize * 3.0/2.0 * r
  --       x' = (x + 6 * hexSize)
  --       y' = (y + 6 * hexSize) in
  --   (round x', round y')

colorToRGB :: Color -> String
colorToRGB c = case c of
  Blue   -> "rgb(65, 131, 215)"
  Red    -> "rgb(217, 30, 24)"
  Orange -> "rgb(248, 148, 6)"
  White  -> "rgb(236,236,236)"

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

sendAction :: PlayerAction -> CatanMVars -> UI Game
sendAction action CatanMVars{..} = do
  putMVar actionVar action
  game@Game{..} <- takeMVar gameVar
  renderGame game
  liftIO $ mapM_ putStrLn errorMessage
  when (action == PlayKnight) (robberSequence game)
  return game

disableClicking :: Board -> UI ()
disableClicking board = do
  turnButtons True
  resetBoard board

disableTiles = do
  runFunction $ ffi "$('.tile').each(function(i){$(this).unbind('click')})"
  runFunction $ ffi "$('.tile').each(function(i){$(this).unbind('mouseenter mouseleave')})"
  turnButtons True

turnButtons :: Bool -> UI ()
turnButtons b = do
  w <- askWindow
  es <- getElementsByClassName w "button"
  foldr (\button acc -> element button # set UI.enabled b >> acc) (return ()) es

robberSequence :: Game -> UI ()
robberSequence Game{..} = do
  w <- askWindow
  let CatanMVars{..} = mvars
  tiles <- getElementsByClassName w "tile"
  turnButtons False
  foldr (\tile acc -> do
    on UI.hover tile $ \_ ->
      element tile # set SVG.fill "white"
    on UI.leave tile $ \_ -> do
      index <- get UI.value tile
      let color = getTileColor board (read index)
      element tile # set SVG.fill color
    on UI.click tile $ \_ -> do
      index <- get UI.value tile
      liftIO $ print "UI putting robber"
      putMVar robberVar (read index)
      liftIO $ print "UI taking game robber"
      _ <- tryTakeMVar gameVar
      g <- takeMVar gameVar
      liftIO $ print "UI took game robber"
      renderGame g
      disableClicking board
      liftIO $ threadDelay 10000
      r <- tryTakeMVar stealVar
      case r of
        Nothing -> return ()
        Just ps -> let color = snd $ head ps in
                  putMVar colorVar color
      liftIO $ print "UI taking extra game"
      _ <- takeMVar gameVar
      liftIO $ print "UI took extra game"
      return ()
    acc) (return ()) tiles

endTurn :: CatanMVars -> UI ()
endTurn m@CatanMVars{..} = do
  w <- askWindow
  liftIO $ print "UI taking game Action"
  g@Game{..} <- sendAction EndTurn m
  liftIO $ print "UI took game Action"
  roll <- takeMVar rollVar
  liftIO $ do putStr "roll: "
              print roll
  e <- getElementById w "player"
  _ <- element (fromJust e) # set text ("Current Player: " ++ (show currentPlayer))
  re <- getElementById w "resources"
  _ <- element (fromJust re) # set UI.text ("Resources: " ++ (show (resources (getPlayer currentPlayer players))))
  liftIO $ threadDelay (400 * 1000)
  when (roll == 7) $ robberSequence g

makeCorners :: (CornerLocation -> UI ()) -> UI ()
makeCorners onClick = do
  w <- askWindow
  e <- getElementById w "mainHex"
  _ <- (return $ fromJust $ e) #+ (map draw cornerIndices)
  turnButtons False
  return ()
  where draw l = do
          let (x,y) = cornerToPixel l
          corner <- SVG.circle
            # set SVG.r (show (hexSize/2))
            # set SVG.class_ "corner"
            # set SVG.cx (show x)
            # set SVG.cy (show y)
            # set SVG.stroke_width "0"
            # set SVG.fill "rgb(108, 122, 137)"
            # set SVG.fill_opacity "0.0"
            # set UI.value (show l)
          on UI.hover corner $ \_ ->
              element corner # set SVG.fill_opacity "0.5"
          on UI.leave corner $ \_ ->
              element corner # set SVG.fill_opacity "0.0"
          on UI.click corner $ \_ -> do
              index <- get UI.value corner
              deleteCorners
              onClick (read index)
              turnButtons True
          element corner


deleteCorners :: UI ()
deleteCorners = do
  w <- askWindow
  es <- getElementsByClassName w "corner"
  foldr (\x acc -> delete x >> acc) (return ()) es

renderGame :: Game -> UI ()
renderGame game = do
  w <- askWindow
  es <- getElementsByClassName w "render"
  e <- getElementById w "mainHex" 
  _ <- (return $ fromJust $ e) #+ [foreground game]
  cdiv <- getElementById w "devcardsdiv"
  menu <- getElementById w "menu"
  _ <- (return $ fromJust $ menu) #+ [drawCards game]
  foldr (\x acc -> delete x >> acc) (return ()) ((fromJust cdiv):es)

resetBoard :: Board -> UI ()
resetBoard board = do
  w <- askWindow
  es <- getElementsByClassName w "tile"
  e <- getElementById w "hexGroup"
  _ <- (return $ fromJust $ e) #+ (makeHexGroup board)
  foldr (\x acc -> delete x >> acc) (return ()) es

instance MonadBase IO UI where
  liftBase = liftIO
