{-# LANGUAGE PatternGuards #-}

module Render where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game

--lambdalumps
import Lib
import IO.Interface
import IO.LeftRight
import IO.RandomTetronimo
import IO.Rotate

-- --TODO: refactor this away from do syntax
renderGame :: Int -> IO ()
renderGame difficultyInput = do
  --TODO: Handle input from user before game begins in order to determine the difficulty and therefore the seed.
  let state = Gamestate
                (getTetronimo 13)
                (getTetronimo 12)
                []
                (13)
                (0)
                (difficultyInput)
  play getDisplay
      --this doesn't change atm TODO: alter this state through input.
       white (difficulty state) state
       renderGamestate handleEvent stepThrough

--helper function
getDisplay = InWindow "LambdaLumps" (600, 1300) (10, 10)

renderGamestate :: Gamestate -> Picture
renderGamestate game
  | (isItSettled tetronimo blocks) && gameOver tetronimo =
                  Pictures
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
                          $ text ("You scored: " ++ (show $ gameScore)) ]
  | otherwise          =
                  Pictures
                        [ (renderSettledBlocks blocks)
                        , (renderTetronimo tetronimo)
                        , (renderNextTetronimo next)
                        , playfieldBorder
                        , (renderScore gameScore)]
  where blocks    = settledTetronimos game
        tetronimo = currentTetronimo game
        gameScore = score game
        next      = nextTetronimo game



-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

stepThrough :: Float -> Gamestate -> Gamestate
stepThrough _ game =
 settle nxnxtet (Gamestate
                  (nextTetronimo game)
                  (currentTetronimo game)
                  (settledTetronimos game)
                  (seed game)
                  (score game)
                  (difficulty game))
                      where nxnxtet = getTetronimo (seed game)

playfieldBorder :: Picture
playfieldBorder = Color orange $ rectangleWire 500 1100

renderSettledBlocks :: SettledBlocks -> Picture
renderSettledBlocks blocks = Pictures $ map renderFromPos blocks

-- coloured red for identification
renderTetronimo :: Tetronimo -> Picture
renderTetronimo tet = Pictures [
      (Color tetColor $ (renderFromPos $ first tet)),
      (Color tetColor $ (renderFromPos $ second tet)),
      (Color tetColor $ (renderFromPos $ third tet)),
      (Color tetColor $ (renderFromPos $ fourth tet))]
        where tetColor = colorScheme $ (shape tet)

renderNextTetronimo :: Tetronimo -> Picture
renderNextTetronimo tet =
          translate (0) (380)
          $ scale (0.5) (0.5)
          $ Pictures [
            (Color tetColor $ (renderFromPos $ first tet)),
            (Color tetColor $ (renderFromPos $ second tet)),
            (Color tetColor $ (renderFromPos $ third tet)),
            (Color tetColor $ (renderFromPos $ fourth tet))]
                where tetColor = colorScheme $ (shape tet)

colorScheme :: Shape -> Color
colorScheme shape
  | shape == IShape = chartreuse
  | shape == OShape = red
  | shape == JShape = azure
  | shape == LShape = blue
  | shape == TShape = rose
  | shape == SShape = magenta
  | shape == ZShape = orange
  | otherwise       = black

renderFromPos :: Pos -> Picture
renderFromPos pos = translate x y $ rectangleSolid cellHeight cellHeight
  where x = fst $ fromCentreToPlayfield pos
        y = snd $ fromCentreToPlayfield pos

renderScore :: Int -> Picture
renderScore score =
  Color black
  $ translate (-20) (-600)
  $ scale 0.3 0.3
  $ text (show score)


-- | helper for renderFromPos - calculates where the block should be in relation to
--   its initial position in the centre of the display -- centre is 250, 550
fromCentreToPlayfield :: Pos -> (Float, Float)
fromCentreToPlayfield pos =
  ((x - 225), (y - 525))
  where x = ((fromIntegral $ xcoord pos) * 50.00)
        y = ((fromIntegral $ ycoord pos) * 50.00)

cellHeight :: Float
cellHeight = 45.00
