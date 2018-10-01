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

--render the gamestate
renderGamestate :: Monad m => BehaviourF m Float Gamestate Picture
renderGamestate = proc game -> do
    playStateScreen <- safely renderPlaystate  -< game
    returnA                                    -< pictures
      [ playStateScreen
      -- , renderPlayState game
      ]

--render the active game screen
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
        , scoreOnScreen
        ]

--the different playstates, depending on a verification of the gamestate.
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

-- renderPlayState' :: Gamestate -> Picture
-- renderPlayState' game
--     | playState game == Paused =
--     renderPaused'$ score game
--     | playState game == Over =
--         renderGameOver' $ score game
--     | otherwise = Blank

renderGameOver :: Monad m => BehaviourF m Float Int Picture
renderGameOver = proc score -> do
    returnA   -< pictures
        [ Color red $ rectangleSolid 600 1000
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
      [ Color azure $ rectangleSolid 600 1000
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
playfieldBorder = Color orange $ rectangleWire 367 800

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
          translate (-100) (210)
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
          $ translate (200) (440)
          $ scale 0.3 0.3
          $ text (":(")
  | otherwise           =
            translate (200) (210)
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
      $ translate (50) (440)
      $ scale 0.3 0.3
      $ text ("hold:")
      ,
      Color black
      $ translate (-250) (440)
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
  ((x - 158.5), (y - 375))
  where x = ((fromIntegral $ T.xcoord pos) * 36.36)
        y = ((fromIntegral $ T.ycoord pos) * 36.36)

cellHeight :: Float
cellHeight = 32.72
