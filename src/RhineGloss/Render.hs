{-# LANGUAGE Arrows            #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


module RhineGloss.Render where

import Data.Maybe

--rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except

--gloss
import Graphics.Gloss

--lambdalumps
import RhineGloss.Arrowized.Model.Gamestate
import qualified RhineGloss.Arrowized.Model.Tetronimo as T
import RhineGloss.Arrowized.Model.Lib

--does this need to be an excvept, to handle isItSettled?
renderGamestate :: Monad m => BehaviourF m Float Gamestate Picture
renderGamestate = proc game -> do
    playStateScreen <- renderPlaystate'  -< game
    returnA                              -< pictures
      [
      playStateScreen
       -- to test state
      , ( Color black
          $ text (playstatetostring $ playState game))
      , renderPlayState' game

        ]

renderActive :: Monad m => BehaviourF m Float Gamestate Picture
renderActive = proc game -> do
    scoreOnScreen   <- renderScore -< score $ game
    returnA                        -< pictures
        [ playfieldBorder
        , (renderSettledBlocks $ settledTetronimos game)
        , (renderTetronimo $ currentTetronimo game)
        , (renderNextTetronimo $ nextTetronimo game)
        , (renderHeldTetronimo $ hold game)
        , (renderDifficulty $ (difficulty game))
        , renderPlayText

        , scoreOnScreen]

--purely for debugging - remove once different playstates are fucntioning
playstatetostring :: PlayState -> String
playstatetostring x
    | x == Over   = "Over"
    | x == Paused = "Paused"
    | x == Active = "Active"
    | otherwise   = "nostate"

-- we need a way to handle change of playstate that actually checks the gamestate.

-- safely :: MSFExcept m a b Empty -> MSF m a b
-- throwOn :: Monad m => e -> MSF (ExceptT e m) Bool ()
-- throw the exception when the input is true.

--the different playstates, depending on a verification of the gamestate.
renderPlaystate' :: Monad m => BehaviourF m Float Gamestate Picture
renderPlaystate' = safely $ do
  renderPlaystate
  -- safe $ arr $ const $ Blank


--draft new attempt at BFexcept
renderPlaystate :: Monad m => BehaviourFExcept m Float Gamestate Picture Empty
renderPlaystate = do
    try $ proc game -> do
        _ <- throwOn () -< playState game == Active
        renderGameOver -< score game
    try $ proc game -> do
        _ <- throwOn () -< playState game /= Active
        renderActive    -< game
    try $ proc game -> do
        _ <- throwOn () -< playState game == Active
        renderPaused   -< score game
    renderPlaystate

renderGameOver :: Monad m => BehaviourF m Float Int Picture
renderGameOver = proc score -> do
    returnA   -< pictures
        [ Color red $ rectangleSolid 600 1300
        , Color white
            $ translate (-195) 0
            $ scale 0.5 0.5 $text "Game Over!"
        , Color white
            $ translate (-150) (-200)
            $ scale 0.3 0.3
            $ text "Hit r to reset"
        , Color white
            $ translate (-150) (-150)
            $ scale 0.3 0.3
            $ text ("You scored: " ++ (show $ score)) ]

renderPaused :: Monad m => BehaviourF m Float Int Picture
renderPaused = proc score -> do
  returnA    -< pictures
      [ Color azure $ rectangleSolid 600 1300
      , Color black
          $ translate (-195) 0
          $ scale 0.5 0.5 $ text "Paused"
      , Color black
          $ translate (-150) (-200)
          $ scale 0.3 0.3
          $ text "Hit p to go back"
      , Color black
          $ translate (-150) (-150)
          $ scale 0.3 0.3
          $ text ("Current score: " ++ (show $ score)) ]

--pure alternative for testing purposes
renderPlayState' :: Gamestate -> Picture
renderPlayState' game
    | playState game == Paused =
    renderPaused'$ score game
    | playState game == Over =
        renderGameOver' $ score game
    | otherwise = Blank

renderGameOver' :: Int -> Picture
renderGameOver' score = pictures
        [ Color red $ rectangleSolid 600 1300
        , Color white
            $ translate (-195) 0
            $ scale 0.5 0.5 $text "Game Over!"
        , Color white
            $ translate (-150) (-200)
            $ scale 0.3 0.3
            $ text "Hit r to reset"
        , Color white
            $ translate (-150) (-150)
            $ scale 0.3 0.3
            $ text ("You scored: " ++ (show $ score)) ]


renderPaused' :: Int ->  Picture
renderPaused' score = pictures
    [ Color azure $ rectangleSolid 600 1300
    , Color black
        $ translate (-195) 0
        $ scale 0.5 0.5 $ text "Paused"
    , Color black
        $ translate (-150) (-200)
        $ scale 0.3 0.3
        $ text "Hit p to go back"
    , Color black
        $ translate (-150) (-150)
        $ scale 0.3 0.3
        $ text ("Current score: " ++ (show $ score)) ]

renderScore :: Monad m => BehaviourF m Float Int Picture
renderScore = proc score -> do
  returnA               -< pictures [
    Color black
    $ translate (-250) (-600)
    $ scale 0.3 0.3
    $ text ("score: ")
    ,
    Color black
    $ translate (-140) (-600)
    $ scale 0.3 0.3
    $ text (show $ score)
    ]

--straight imports - division for refactoring purposes

playfieldBorder :: Picture
playfieldBorder = Color orange $ rectangleWire 500 1100

renderSettledBlocks :: T.SettledBlocks -> Picture
renderSettledBlocks blocks = Pictures $ map renderFromPos blocks

renderTetronimo :: T.Tetronimo  -> Picture
renderTetronimo tet = Pictures [
      (Color tetColor $ (renderFromPos $ T.first tet)),
      (Color tetColor $ (renderFromPos $ T.second tet)),
      (Color tetColor $ (renderFromPos $ T.third tet)),
      (Color tetColor $ (renderFromPos $ T.fourth tet))]
        where tetColor = colorScheme $ (T.shape tet)

renderNextTetronimo :: T.Tetronimo  -> Picture
renderNextTetronimo tet =
          translate (-100) (360)
          $ scale (0.5) (0.5)
          $ Pictures [
            (Color tetColor $ (renderFromPos $ T.first tet)),
            (Color tetColor $ (renderFromPos $ T.second tet)),
            (Color tetColor $ (renderFromPos $ T.third tet)),
            (Color tetColor $ (renderFromPos $ T.fourth tet))]
                where tetColor = colorScheme $ (T.shape tet)

renderHeldTetronimo :: Maybe T.Tetronimo  -> Picture
renderHeldTetronimo maybeTet
  | maybeTet == Nothing =
          Color black
          $ translate (200) (590)
          $ scale 0.3 0.3
          $ text (":(")
  | otherwise           =
            translate (200) (360)
          $ scale (0.5) (0.5)
          $ Pictures [
            (Color tetColor $ (renderFromPos $ T.first tet)),
            (Color tetColor $ (renderFromPos $ T.second tet)),
            (Color tetColor $ (renderFromPos $ T.third tet)),
            (Color tetColor $ (renderFromPos $ T.fourth tet))]
                where tetColor = colorScheme $ (T.shape tet)
                      tet = fromJust $ maybeTet

colorScheme :: T.Shape -> Color
colorScheme shape
  | shape == T.IShape = azure
  | shape == T.OShape = rose
  | shape == T.JShape = blue
  | shape == T.LShape = orange
  | shape == T.TShape = magenta
  | shape == T.SShape = chartreuse
  | shape == T.ZShape = red
  | otherwise       = black

renderFromPos :: T.Pos -> Picture
renderFromPos pos = translate x y $ rectangleSolid cellHeight cellHeight
  where x = fst $ fromCentreToPlayfield pos
        y = snd $ fromCentreToPlayfield pos

renderPlayText :: Picture
renderPlayText = Pictures [
      Color black
      $ translate (50) (590)
      $ scale 0.3 0.3
      $ text ("hold:")
      ,
      Color black
      $ translate (-250) (590)
      $ scale 0.3 0.3
      $ text ("next:")
      ]

-- renderScore :: Int -> Picture
-- renderScore score = Pictures [
--   Color black
--   $ translate (-250) (-600)
--   $ scale 0.3 0.3
--   $ text ("score: ")
--   ,
--   Color black
--   $ translate (-140) (-600)
--   $ scale 0.3 0.3
--   $ text (show score)
--   ]

renderDifficulty :: Int -> Picture
renderDifficulty difficulty = Pictures [
  Color black
  $ translate (50) (-600)
  $ scale 0.3 0.3
  $ text ("difficulty: ")
  ,
  Color black
  $ translate (205) (-600)
  $ scale 0.3 0.3
  $ text (show difficulty)
  ]

-- | helper for renderFromPos - calculates where the block should be in relation to
--   its initial position in the centre of the display -- centre is 250, 550
fromCentreToPlayfield :: T.Pos -> (Float, Float)
fromCentreToPlayfield pos =
  ((x - 225), (y - 525))
  where x = ((fromIntegral $ T.xcoord pos) * 50.00)
        y = ((fromIntegral $ T.ycoord pos) * 50.00)

cellHeight :: Float
cellHeight = 45.00
