module RhineGloss.Arrowized.IO.Hold where

import Data.Maybe

--lambdalumps
import RhineGloss.Arrowized.Model.Gamestate
import RhineGloss.Arrowized.Model.Tetronimo

--lambdalumps io

import RhineGloss.Arrowized.Model.RandomTetronimo
handleHold :: Gamestate -> Gamestate
handleHold game
  | hold game == Nothing = holdTet game
  | otherwise            = swapHeldTet game

holdTet :: Gamestate -> Gamestate
holdTet game =
           game {
           nextTetronimo = (getTetronimo $ seed game),
           hold = (Just $ spawn . spawnCode . shape $ currentTetronimo game)
         }

swapHeldTet :: Gamestate -> Gamestate
swapHeldTet game =
           game {
           currentTetronimo = (spawn . spawnCode . shape $ fromJust $ hold game),
           hold = (Just $ spawn . spawnCode . shape $ currentTetronimo game)
         }
