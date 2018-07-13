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
import Gloss.Interface
import Gloss.Render

--LambdaLumps
import Model.Gamestate
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
renderGame :: Int -> IO ()
renderGame difficultyInput =  flowGloss
                getDisplay
                white
                difficultyInput
                $ glossRhine

-- gloss event
-- handleEvent :: Event -> Maybe Gamestate
-- handleEvent $ stepThrough $ parseEvent
--
-- buildGlossRhine :: (Event -> Maybe a)
--                   -- a (SyncSF representing the game loop)
--                     -> GlossSyncSF
--                     -> GlossRhine a

---- | The main 'SyncSF' governing events, game logic and graphics.
--   An event is produced whenever the user presses a key
--how essential is sinceStart
--timeInfoOf sinceInit >>> arr (* 50) >>> arr gears
--maybe timeInfoOf isnt necessary at fall
-- (not . null) :: t a -> bool
--game = arr (not . null) >>> gamelogic >>> graphics

--Need: SyncSF m cl a b -> SyncSF m2 (HoistClock m1 m2 cl) a b

-- | GlossSyncSf ::  SyncSF Identity GlossSimulationClock [a] Picture
--SyncSF m cl ab = MSF (ReaderT (TimeInfo cl) m) a b
game :: GlossSyncSF Gamestate
game =  arrMSync_ $ Identity $ renderGamestate $ nextGamestate getGamestate

--need some equivalent of stepThrough
--how to distinguish between the initial gamestate and the progression through gamestates

glossRhine :: GlossRhine Gamestate
glossRhine = buildGlossRhine handleEvent game

gameLogic
  :: (Monad m, TimeDomain td, Diff td ~ Float)
  => BehaviorF m td Float Gamestate
gameLogic = feedback 0 $ proc (eventRequest, seedOld) -> do


--feedback :: Monad m => c -> MStreamF m (a, c) (b, c) -> MStreamF m a b

--potentially graphics should be the only thing left in render, and the gloss stuff can go in its own module for clarity.

--combine all the graphics into one picture
-- graphics :: Monad m => BehaviourF m Float Gamestate Picture
-- graphics = proc game@Gamestate -> do
--    [
--     (renderSettledBlocks blocks)
--   , (renderTetronimo tetronimo)
--   , (renderNextTetronimo next)
--   , playfieldBorder
--   , (renderScore gameScore)
--   , (renderHeldTetronimo $ hold game)
--   , renderPlayText
--   ]
