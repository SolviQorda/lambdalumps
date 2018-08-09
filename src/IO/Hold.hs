module IO.Hold where

import Data.Maybe

--lambdalumps
import Model.Gamestate
import Model.Tetronimo

--lambdalumps io
import Model.RandomTetronimo

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
