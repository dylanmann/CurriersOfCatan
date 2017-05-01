{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module CatanGUI (beginGUI) where


import Prelude hiding(log)
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
beginGUI cmvars =
  startGUI defaultConfig { jsCustomHTML = Just "catan.html",
                           jsStatic = Just "static",
                           jsLog = \_ -> putStr "" } (setup cmvars)

bootstrapRow :: [UI Element] -> UI Element
bootstrapRow elems = UI.div # set UI.class_ "row" #+ elems

mkButton :: String -> String -> UI Element
mkButton buttonTitle classes =
  UI.button #. ("btn myspacing " ++ classes) # set UI.type_ "button" #+ [string buttonTitle]

setup :: CatanMVars -> Window -> UI ()
setup CatanMVars{..} w = void $ do
  _ <- return w # set title "Curriers of Catan"

  roll <- takeMVar rollVar
  game@Game{..} <- takeMVar gameVar

  heading <- UI.h1 # set text "Curriers of Catan"
  subHeading <- UI.h2 # set text ("Current Player: " ++ show currentPlayer)
                      # set UI.id_ "player"
  rollResult <- UI.h4 # set text ("Roll: " ++ show roll) # set UI.id_ "roll"

  endturnbutton   <- mkButton "End Turn" "btn-outline-danger btn-sm"
  buildRoadButton <- mkButton "Build Road" "btn-outline-primary btn-sm"
  buildSettButton <- mkButton "Build Settlement" "btn-outline-primary btn-sm"
  buyCardButton   <- mkButton "Buy Card" "btn-outline-primary btn-sm"
  buildCityButton <- mkButton "Build City" "btn-outline-primary btn-sm"
  cheatButton     <- mkButton "cheater" "btn-outline-danger btn-sm"

  buttons <- bootstrapRow [element buildRoadButton
                  , element buildSettButton
                  , element buildCityButton
                  , element buyCardButton
                  , element cheatButton
                  , element endturnbutton]

  menu <- UI.div # set UI.id_ "menu"
                 #+ [element subHeading
                 , element rollResult
                 , drawResources game
                 , drawKnights game
                 , drawTrading game
                 , drawCards game]

  sidebar <- UI.div # set UI.class_ "col-lg-5"
                    #+ [ element menu
                    , UI.div # set UI.class_ "row" #+ [UI.h4 # set text "Actions:"]
                    , element buttons
                    ]

  boardDiv <- UI.div # set UI.class_ "col-lg-7"
                     #+ [background game]

  gameRow <- bootstrapRow [element boardDiv, element sidebar]
  container <- UI.div # set UI.class_ "container-fluid main-container" #+ [element heading, element gameRow]

  _ <- getBody w #+ [ element container ]

  on UI.click endturnbutton $ \_ -> endTurn mvars

  on UI.click buildRoadButton $
      \_ -> makeCorners $ \i1 -> makeCorners (\i2 -> do _ <- sendAction (BuildRoad i1 i2) mvars
                                                        return ())

  on UI.click buildSettButton $
      \_ -> makeCorners (\index -> do _ <- sendAction (BuildSettlement index) mvars
                                      return ())

  on UI.click buildCityButton $
      \_ -> makeCorners (\index -> do _ <- sendAction (BuildCity index) mvars
                                      return ())

  on UI.click buyCardButton $ \_ -> do
      _ <- sendAction BuyCard mvars
      return ()

  on UI.click cheatButton $ \_ -> do
      _ <- sendAction (Cheat resourceRadioValues) mvars
      return ()

drawResources :: Game -> UI Element
drawResources Game{..} =
  UI.p
    # set UI.id_ "resources"
    # set UI.text ("Resources: " ++ show (resources (getPlayer currentPlayer players)))

drawKnights :: Game -> UI Element
drawKnights Game{..} =
  UI.p
    # set UI.id_ "knights"
    # set UI.text ("Knights: " ++ (show (knights (getPlayer currentPlayer players))))

drawCards :: Game -> UI Element
drawCards g@Game{..} = do
  let devcards = cards (getPlayer currentPlayer players)
  let playableCardsList = map (\c -> do
      button <- UI.button
        # set UI.class_ "card list-group-item list-group-item-action"
        # set UI.type_ "button"
        #+ [string (show c)]

      on UI.click button $ \_ ->
        case c of
          VictoryPoint -> return ()
          Knight -> handlePlayKnight g mvars
          Progress Monopoly -> do
            maybeRes <- getRadioSelection "from"
            case maybeRes of
              Just r -> do
                _ <- sendAction (PlayMonopoly r) mvars
                return ()
              _ -> return ()
          Progress YearOfPlenty -> do
            maybeRes1 <- getRadioSelection "from"
            maybeRes2 <- getRadioSelection "to"
            case (maybeRes1, maybeRes2) of
              (Just r1, Just r2) -> do
                _ <- sendAction (PlayYearOfPlenty r1 r2) mvars
                return ()
              (_,_) -> return ()
          Progress _ -> return ()

      return button) devcards
  let pendingCardsList = map (\c -> UI.button
              # set UI.class_ "card list-group-item"
              # set UI.enabled False
              # set UI.type_ "button"
              #+ [string (show c ++ " (PENDING)")]) pendingCards
  cardsTitle <- UI.h4 # set text "Development cards:"
  list <- UI.div
    # set UI.class_ "list-group cards"
    #+ (playableCardsList ++ pendingCardsList)
  UI.div # set UI.id_ "devcardsdiv" #+ [element cardsTitle, element list]

-- is there a nicer way to do this?
resourceRadioValues :: [Resource]
resourceRadioValues = [Brick, Lumber, Ore, Grain, Wool]

getRadioSelection :: String -> UI (Maybe Resource)
getRadioSelection t = foldr (checkRadio t) (return Nothing) resourceRadioValues
  where
    checkRadio tag res acc = do
      let r = show res
      w <- askWindow
      let radioid = tag ++ r
      maybeElem <- getElementById w radioid
      let e = fromJust maybeElem
      checked <- get UI.checked e
      if checked then return (Just res) else acc

drawTrading :: Game -> UI Element
drawTrading Game{..} = do
  let fromResourceRadios = map (makeRadio "from") resourceRadioValues
  let toResourceRadios = map (makeRadio "to") resourceRadioValues
  let fromDiv = UI.div # set UI.class_ "from-group" # set UI.id_ "fromDiv" #+ ((UI.legend # set text "Resource to trade"):fromResourceRadios)
  let toDiv = UI.div # set UI.class_ "form-group" # set UI.id_ "fromDiv" #+ ((UI.legend # set text "Resource to receive"):toResourceRadios)
  submitButton <- UI.button #. "btn btn-outline-success btn-sm" # set text "Trade With Bank"
  form <- UI.div #+ [UI.h4 # set text "Trade With Bank:", fromDiv, toDiv, element submitButton]

  on UI.click submitButton $ \_ -> do
      maybeFrom <- getRadioSelection "from"
      maybeTo <- getRadioSelection "to"
      case (maybeFrom, maybeTo) of
        (Just r1, Just r2) -> do
          _ <- sendAction (TradeWithBank r1 r2 1) mvars
          return ()
        (_, _) -> return ()

  return form
  where
    makeRadio tag res =
      let r = show res in
        UI.label
          # set UI.class_ "btn btn-secondary btn-sm active myspacing-sm"
          #+ [UI.input # set UI.type_ "radio" # set UI.id_ (tag ++ r) # set UI.name tag # set UI.value r
          , UI.span # set UI.class_ "custom-control-indicator"
          , UI.span # set UI.class_ "custom-control-description" # set text ("   " ++ r)]


hexPoints ::  (Integral t1, Integral t2, Integral t3) => t1 -> t2 -> t3 -> String
hexPoints x1 y1 r1 =
  unwords (map hexCorner [0..5])
  where hexCorner i =
         let x = fromIntegral x1
             y = fromIntegral y1
             r = fromIntegral r1
             angle_deg = 60 * i + 30
             angle_rad = pi / 180 * angle_deg in
         show (x + r * cos angle_rad) ++ "," ++ show (y + r * sin angle_rad)

flatHexPoints ::  (Integral t1, Integral t2, Integral t3) => t1 -> t2 -> t3 -> String
flatHexPoints x1 y1 r1 =
  unwords (map hexCorner [0..5])
  where hexCorner i =
         let x = fromIntegral x1
             y = fromIntegral y1
             r = fromIntegral r1
             angle_deg = 60 * i
             angle_rad = pi / 180 * angle_deg in
         show (x + r * cos angle_rad) ++ "," ++ show (y + r * sin angle_rad)

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
        # set SVG.fill (colorToRGB c)
    drawBuilding (City c l) = do
      let (x, y) = cornerToPixel l
      g <- SVG.g # set SVG.class_ "render"
      return g #+ [SVG.circle
        # set SVG.r (show (hexSize/3 + 5))
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.fill (colorToRGB c),
        SVG.circle
        # set SVG.r "10"
        # set SVG.cx (show x)
        # set SVG.cy (show y)
        # set SVG.fill "rgb(34, 49, 63)"
        ]
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
  let g = SVG.g # set SVG.id "hexGroup" #+ makeHexGroup board
  let harbors = foldr makeHabors [] cornerIndices
  let elemList = element bg : (harbors ++ (g : [foreground game]))
  return context #+ elemList
  where
    makeHabors cornerLoc acc =
      case getCorner board cornerLoc of
        (_, Just GenericHarbor) ->
          let (x,y) = cornerToPixel cornerLoc
              h = SVG.circle
                # set SVG.r (show (hexSize/2))
                # set SVG.cx (show x)
                # set SVG.cy (show y)
                # set SVG.fill "rgb(108, 122, 137)"
                # set SVG.stroke "rgb(34, 49, 63)"
                # set SVG.stroke_width "1"
          in
          h:acc
        (_, Just (SpecialHarbor r)) ->
          let (x,y) = cornerToPixel cornerLoc
              h = SVG.circle
                # set SVG.r (show (hexSize/2))
                # set SVG.cx (show x)
                # set SVG.cy (show y)
                # set SVG.fill (getHarborColor r)
                # set SVG.stroke "rgb(34, 49, 63)"
                # set SVG.stroke_width "1"
          in
          h:acc
        (_, Nothing) -> acc

    getHarborColor r = case r of
                        Brick -> "rgb(179, 94, 30)"
                        Lumber -> "rgb(30, 130, 76)"
                        Ore -> "rgb(190, 144, 212)"
                        Grain -> "rgb(135, 211, 124)"
                        Wool -> "rgb(245, 215, 110)"

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
        y' = y + 6 * hexSize + (if t then negate else id) hexSize
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
  log "taking sendaction"
  game@Game{..} <- takeMVar gameVar
  log "took sendaction"
  renderGame game
  mapM_ log errorMessage
  when (action == PlayKnight) (robberSequence game)
  return game


disableClicking :: Board -> UI ()
disableClicking board = do
  turnButtons True
  resetBoard board


turnButtons :: Bool -> UI ()
turnButtons b = do
  w <- askWindow
  es <- getElementsByClassName w "button"
  foldr (\button acc -> element button # set UI.enabled b >> acc) (return ()) es

robberSequence :: Game -> UI ()
robberSequence Game{..} = do
  log "robber sequence"
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
      log "UI putting robber"
      putMVar robberVar (read index)
      log "UI taking game robber"
      _ <- tryTakeMVar gameVar
      g <- takeMVar gameVar
      log "UI took game robber"
      renderGame g
      disableClicking board
      liftIO $ threadDelay 10000
      r <- takeMVar stealVar
      case r of
        [] -> return ()
        ps -> let color = snd $ head ps in
                  putMVar colorVar color
      log "UI taking extra game"
      _ <- takeMVar gameVar
      log "UI took extra game"
      e <- getElementById w "roll"
      _ <- element (fromJust e) # set text "Roll: 7"
      return ()
    acc) (return ()) tiles

handlePlayKnight :: Game -> CatanMVars -> UI ()
handlePlayKnight g CatanMVars{..} = do
  log "putting playknight"
  putMVar actionVar PlayKnight
  log "put playknight"
  robberSequence g

endTurn :: CatanMVars -> UI ()
endTurn m@CatanMVars{..} = do
  w <- askWindow
  log "UI taking game Action"
  g@Game{..} <- sendAction EndTurn m
  log "UI took game Action"
  roll <- takeMVar rollVar
  let roll7prompt = if roll == 7 then "\tPlace the robber on a new tile." else ""
  let rollStr = "Roll: " ++ show roll ++ roll7prompt
  rolle <- getElementById w "roll"
  _ <- element (fromJust rolle) # set text rollStr
  e <- getElementById w "player"
  _ <- element (fromJust e) # set text ("Current Player: " ++ show currentPlayer)
  liftIO $ threadDelay (400 * 1000)
  when (roll == 7) $ robberSequence g

makeCorners :: (CornerLocation -> UI ()) -> UI ()
makeCorners onClick = do
  w <- askWindow
  e <- getElementById w "mainHex"
  _ <- return (fromJust e) #+ map draw cornerIndices
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
            # set SVG.fill "white"
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
renderGame game@Game{..} = do
  w <- askWindow
  es <- getElementsByClassName w "render"
  e <- getElementById w "mainHex"
  re <- getElementById w "resources"
  _ <- element (fromJust re) # set UI.text ("Resources: " ++ show (resources (getPlayer currentPlayer players)))
  ke <- getElementById w "knights"
  _ <- element (fromJust ke) # set UI.text ("Knights: " ++ show (knights (getPlayer currentPlayer players)))
  _ <-  return (fromJust e) #+ [foreground game]
  cdiv <- getElementById w "devcardsdiv"
  menu <- getElementById w "menu"
  _ <- return (fromJust menu) #+ [drawCards game]
  foldr (\x acc -> delete x >> acc) (return ()) (fromJust cdiv:es)

resetBoard :: Board -> UI ()
resetBoard board = do
  w <- askWindow
  es <- getElementsByClassName w "tile"
  e <- getElementById w "hexGroup"
  _ <- return (fromJust e) #+ makeHexGroup board
  foldr (\x acc -> delete x >> acc) (return ()) es

instance MonadBase IO UI where
  liftBase = liftIO

log :: Show a => a -> UI ()
log str = liftIO $ do putStr "[UI]  "
                      print str
