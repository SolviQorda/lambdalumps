{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}


module RhineGloss.GameBuilder where

import Data.Functor.Identity

--gloss-rhine
import FRP.Rhine.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- lambdalumps gloss
import IO.Interface
import Gloss.Render

--LambdaLumps
import Model.Gamestate
import Model.Tetronimo

--lambdalumps io
import Model.RandomTetronimo

import Model.DifficultyManager
-- flowGloss Source#

--
-- :: Display
-- Display mode (e.g. InWindow or FullScreen).
-- -> Color
-- Background color.
-- -> Int
-- Number of simulation steps per second of real time.
-- -> GlossRhine a
-- The gloss-compatible Rhine.
--
renderGameRhineGloss :: Int -> IO ()
renderGameRhineGloss difficultyInput = flowGloss
                getDisplay
                white
                100
                $ glossRhine

---- | The main 'SyncSF' governing events, game logic and graphics.
--   An event is produced whenever the user presses a key

game''' :: GlossSyncSF Event
game''' = feedback getGamestate $ proc (events, gamestate) -> do
  timeStep <- timeInfoOf sinceStart -< ()
  let newState = foldr (.) id (parseEvent <$> events) $ stepThru gamestate timeStep
  returnA                          -< (renderGamestate newState, newState)

glossRhine :: GlossRhine Event
glossRhine = buildGlossRhine Just game'''

stepThru :: Gamestate -> Float -> Gamestate
stepThru game steps
  | paused game = game
  | (floor $ steps * 100) `mod` (difficultyValue steps) == 0  =
       settle nxnxtet (Gamestate
                        (nextTetronimo game)
                        (currentTetronimo game)
                        (settledTetronimos game)
                        (hold game)
                        (seed game)
                        (score game)
                        (difficultyValue steps)
                        (paused game))
  | otherwise = game
     where nxnxtet = getTetronimo (seed game)


--combine all the graphics into one picture
-- graphics :: Monad m => BehaviourF m Float Gamestate Picture
-- graphics = proc game@Gamestate -> do
--    returnA  -< translate 0 yOffset $ renderGamestate game
