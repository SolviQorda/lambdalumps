module Gloss.Render where

--base
import Data.Maybe

--gloss
import Graphics.Gloss

--lambdalumps
import Model.DifficultyManager
import Model.Gamestate
import Model.Lib
import Model.RandomTetronimo
import Model.Tetronimo as T

--lamdbdalumps io
import IO.Interface
import IO.LeftRight
import IO.Rotate


renderGameGloss :: Int -> IO ()
renderGameGloss difficultyInput = do
  let state = Gamestate
                (getTetronimo $ s)
                (getTetronimo $ (s + 1))
                []
                (Nothing)
                (s)
                (0)
                (difficultyInput)
                (False)
  play getDisplay
      white (difficulty state) state
      renderGamestate parseEvent stepThrough
        where s = 8 - difficultyInput

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
  | paused game        =
                  Pictures
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
                          $ text ("Current score: " ++ (show $ gameScore)) ]
  | otherwise          =
                  Pictures
                        [ (renderSettledBlocks blocks)
                        , (renderTetronimo tetronimo)
                        , (renderNextTetronimo next)
                        , playfieldBorder
                        , (renderScore gameScore)
                        , (renderHeldTetronimo $ hold game)
                        , (renderDifficulty $ (100 - difficulty game) `div` 10)
                        , renderPlayText]
  where blocks    = settledTetronimos game
        tetronimo = currentTetronimo game
        gameScore = score game
        next      = nextTetronimo game

stepThrough :: Float -> Gamestate -> Gamestate
stepThrough steps game
  | paused game = game
  | otherwise   =
       settle nxnxtet (Gamestate
                        (nextTetronimo game)
                        (currentTetronimo game)
                        (settledTetronimos game)
                        (hold game)
                        (seed game)
                        (score game)
                        (difficultyValue steps)
                        (paused game))
     where nxnxtet = getTetronimo (seed game)

playfieldBorder :: Picture
playfieldBorder = Color orange $ rectangleWire 500 1100

renderSettledBlocks :: SettledBlocks -> Picture
renderSettledBlocks blocks = Pictures $ map renderFromPos blocks

renderTetronimo :: Tetronimo -> Picture
renderTetronimo tet = Pictures [
      (Color tetColor $ (renderFromPos $ T.first tet)),
      (Color tetColor $ (renderFromPos $ T.second tet)),
      (Color tetColor $ (renderFromPos $ T.third tet)),
      (Color tetColor $ (renderFromPos $ T.fourth tet))]
        where tetColor = colorScheme $ (T.shape tet)

renderNextTetronimo :: Tetronimo -> Picture
renderNextTetronimo tet =
          translate (-100) (360)
          $ scale (0.5) (0.5)
          $ Pictures [
            (Color tetColor $ (renderFromPos $ T.first tet)),
            (Color tetColor $ (renderFromPos $ T.second tet)),
            (Color tetColor $ (renderFromPos $ T.third tet)),
            (Color tetColor $ (renderFromPos $ T.fourth tet))]
                where tetColor = colorScheme $ (T.shape tet)

renderHeldTetronimo :: Maybe Tetronimo -> Picture
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
      $ text ("next:")]

renderScore :: Int -> Picture
renderScore score = Pictures [
  Color black
  $ translate (-250) (-600)
  $ scale 0.3 0.3
  $ text ("score: "),
  Color black
  $ translate (-140) (-600)
  $ scale 0.3 0.3
  $ text (show score) ]

renderDifficulty :: Int -> Picture
renderDifficulty difficulty = Pictures [
  Color black
  $ translate (50) (-600)
  $ scale 0.3 0.3
  $ text ("difficulty: "),
  Color black
  $ translate (205) (-600)
  $ scale 0.3 0.3
  $ text (show difficulty)
  ]

-- | helper for renderFromPos - calculates where the block should be in relation to
--   its initial position in the centre of the display -- centre is 250, 550
fromCentreToPlayfield :: Pos -> (Float, Float)
fromCentreToPlayfield pos =
  ((x - 225), (y - 525))
  where x = ((fromIntegral $ xcoord pos) * 50.00)
        y = ((fromIntegral $ ycoord pos) * 50.00)


cellHeight :: Float
cellHeight = 45.00
