module IO.Hold where

import Data.Maybe

import Lib
import IO.RandomTetronimo

handleHold :: Gamestate -> Gamestate
handleHold game
  | hold game == Nothing = holdTet game
  | otherwise            = swapHeldTet game

holdTet :: Gamestate -> Gamestate
holdTet game =
  Gamestate (getTetronimo $ seed game)
            (nextTetronimo game)
            (settledTetronimos game)
            (Just $ spawn . spawnCode . shape $ currentTetronimo game)
            (seed game)
            (score game)
            (difficulty game)
            (paused game)

swapHeldTet :: Gamestate -> Gamestate
swapHeldTet game =
  Gamestate (nextTetronimo game)
            (spawn . spawnCode . shape $ fromJust $ hold game)
            (settledTetronimos game)
            (Just $ spawn . spawnCode . shape $ currentTetronimo game)
            (seed game)
            (score game)
            (difficulty game)
            (paused game)


--need to update the intro in main
