{-# LANGUAGE PatternGuards #-}

module Render where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game

--lambdalumps
import Lib
import IO.Interface
import IO.Rotate
import IO.LeftRight

-- --TODO: refactor this away from do syntax
main :: IO ()
main = do
  let state = Gamestate (spawn 4) (spawn 1) []
  play getDisplay
       white 3 state
       renderGamestate handleEvent stepThrough

--helper function
getDisplay = InWindow "LambdaLumps" (600, 1200) (10, 10)

renderGamestate :: Gamestate -> Picture
renderGamestate game
  | (isItSettled tetronimo blocks) && gameOver tetronimo =
                  Pictures
                      [ Color red $ rectangleSolid 600 1200
                      , Color white $ translate (-195) 0 $ scale 0.5 0.5 $text "Game Over!"
                      , Color white $ translate (-160) (-150) $ scale 0.3 0.3 $text "Hit r to reset"]
  | otherwise          =
                  Pictures
                        [ (renderSettledBlocks blocks)
                        , (renderTetronimo tetronimo)
                        , playfieldBorder]
  where blocks    = settledTetronimos game
        tetronimo = currentTetronimo game



-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

stepThrough :: Float -> Gamestate -> Gamestate
stepThrough _ game =
 settle nxnxtet (Gamestate (nextTetronimo game) (currentTetronimo game) (settledTetronimos game))
                      where nxnxtet = spawn 6

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

--helper for renderFromPos - calculates where the block should be in relation to its initial position in the centre of the display -- -- centre is 250, 550 TODO: Better name

fromCentreToPlayfield :: Pos -> (Float, Float)
fromCentreToPlayfield pos =
  ((x - 225), (y - 525))
  where x = ((fromIntegral $ xcoord pos) * 50.00)
        y = ((fromIntegral $ ycoord pos) * 50.00)

cellHeight :: Float
cellHeight = 45.00
