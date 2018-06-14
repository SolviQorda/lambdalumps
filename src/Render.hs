{-# LANGUAGE PatternGuards #-}

module Render where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


--lambdalumps
import Lib

-- --TODO: refactor this away from do syntax
main :: IO ()
main = do
  let state = Gamestate (spawn 4) (spawn 1) []
  play getDisplay
       white 5 state
       renderGamestate handleEvent stepThrough

--helper function
getDisplay = InWindow "LambdaLumps" (600, 1200) (10, 10)

renderGamestate :: Gamestate -> Picture
renderGamestate game
  | (isItSettled tetronimo blocks) && gameOver tetronimo =
                  Pictures
                      [ Color white $ text "Game Over!"
                      , Color red $ rectangleSolid 600 1200]
  | otherwise          =
                  Pictures
                        [ (renderSettledBlocks blocks)
                        , (renderTetronimo tetronimo)
                        , playfieldBorder]
  where blocks    = settledTetronimos game
        tetronimo = currentTetronimo game

handleEvent :: Event -> Gamestate -> Gamestate
handleEvent event game
    --left KeyLeft
    | EventKey (SpecialKey KeyLeft) down _ _ <- event
    , Gamestate _ _ _                        <- game
    -- settle :: Tetronimo -> Gamestate -> Gamestate TODO: refactor
    = Gamestate (tetronimoLeft $ currentTetronimo game) (nextTetronimo game) (settledTetronimos game)
    --right eventKey
    | EventKey (SpecialKey KeyRight) down _ _ <- event
    , Gamestate _ _ _                <- game
    = Gamestate (tetronimoRight $ currentTetronimo game) (nextTetronimo game) (settledTetronimos game)
    | otherwise = game

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

--settle :: (nextnext) Tetronimo -> Gamestate -> Gamestate
--gameOver :: Tetronimo -> Bool

stepThrough :: Float -> Gamestate -> Gamestate
stepThrough _ game =
 settle nxnxtet (Gamestate (currentTetronimo game) (nextTetronimo game) (settledTetronimos game))
                      where nxnxtet = spawn 2

playfieldBorder :: Picture
playfieldBorder = Color orange $ rectangleWire 500 1100


renderSettledBlocks :: SettledBlocks -> Picture
renderSettledBlocks blocks = Pictures $ map renderFromPos blocks

-- coloured red for identification
renderTetronimo :: Tetronimo -> Picture
renderTetronimo tet = Pictures [
      (Color red $ (renderFromPos $ first tet)),
      (Color red $ (renderFromPos $ second tet)),
      (Color red $ (renderFromPos $ third tet)),
      (Color red $ (renderFromPos $ fourth tet))]

renderFromPos :: Pos -> Picture
renderFromPos pos = translate x y $ rectangleSolid cellHeight cellHeight
  where x = fst $ fromCentreToPlayfield pos
        y = snd $ fromCentreToPlayfield pos

--helper for renderFromPos - calculates where the block should be in relation to its initial position in the centre of the display -- -- centre is 250, 550 TODO: Better name

fromCentreToPlayfield :: Pos -> (Float, Float)
fromCentreToPlayfield pos =
  ((x - 225), (y - 550))
  where x = ((fromIntegral $ xcoord pos) * 50.00)
        y = ((fromIntegral $ ycoord pos) * 50.00)

cellHeight :: Float
cellHeight = 45.00

-- testBlocks :: Picture
-- testBlocks = renderSettledBlocks [Pos 0 0, Pos 0 1, Pos 0 2, Pos 0 3,
--                Pos 1 0, Pos 1 1, Pos 1 2, Pos 1 3,
--                Pos 2 0, Pos 2 1, Pos 2 2, Pos 2 3,
--                Pos 3 0, Pos 3 1, Pos 3 2, Pos 3 3,
--                Pos 4 0, Pos 4 1, Pos 4 2, Pos 4 3,
--                Pos 5 0, Pos 5 1, Pos 5 2, Pos 5 3,
--                Pos 6 0, Pos 6 1, Pos 6 2, Pos 6 3,
--                Pos 7 0, Pos 7 1, Pos 7 2, Pos 7 3,
--                Pos 8 0, Pos 8 1, Pos 8 2, Pos 8 3,
--                Pos 9 0, Pos 9 1, Pos 9 2, Pos 9 3]
--
-- testTetronimo :: Picture
-- testTetronimo = renderTetronimo $
--     Tetronimo (Pos 9 0) (Pos 9 1) (Pos tw9 2) (Pos 9 3)
--
-- 0, 0 should be 25, 25, assuming that a cell is 50x50 with internal padding of 5 on each side.
