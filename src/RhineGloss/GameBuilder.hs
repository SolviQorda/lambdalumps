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
import Model.Tetronimo
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
                difficultyInput
                $ glossRhine

game'' :: GlossSyncSF Gamestate
game'' = timeInfoOf sinceStart >>> arr (stepThru getGamestate) >>> arr renderGamestate

glossRhine :: GlossRhine Gamestate
glossRhine = buildGlossRhine handleEvent game''

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
-- game :: GlossSyncSF Gamestate
-- game =  arrMSync_ (nextGamestate getGamestate) >>> gameLogic >>> graphics
--
-- game' :: GlossSyncSF Gamestate
-- game' = arr (not . null) >>> gameLogic >>> graphics


--need some equivalent of stepThrough
--how to distinguish between the initial gamestate and the progression through gamestates


-- gameLogic
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => BehaviorF m td Float Gamestate
-- gameLogic = feedback 0 $ proc game -> do
--   nextTetronimo     <- nextTetSF     -< game
--   currentTetronimo  <- currentTetSF  -< game
--   settledTetronimos <- settledTetsSF -< game
--   hold              <- holdSF        -< game
--   seed              <- seedSF        -< game
--   score             <- scoreSF       -< game
--   difficulty        <- difficultySF  -< game
--   paused            <- pausedSF      -< game
--     -- newGamestate <- gamestateAsMSF -< ()
--     --but this is useless if the original Gamestate doesn't have the SYNCsf's as types
--   returnA                  -< (Gamestate {..})
-- --
--
-- nextTetSF :: Gamestate -> Tetronimo
-- nextTetSF game = nextTetronimo game
--
-- currentTetSF :: Gamestate -> Behaviour m td Tetronimo
-- currentTetSF   game = arrMSync_ $ currentTetronimo game
--
-- settledTetsSF :: Gamestate -> Behaviour m td SettledBlocks
-- settledTetsSF game = arrMSync_ $ settledTetronimos game
--
-- holdSF :: Gamestate -> Behaviour m td (Maybe Tetronimo)
-- holdSF game = arrMSync_ $ hold game
--
-- seedSF :: Gamestate -> Behaviour m td Int
-- seedSF game = arrMSync_ $ seed game
--
-- scoreSF :: Gamestate -> Behaviour m td Int
-- scoreSF game = arrMSync_ $ score game
--
-- difficultySF :: Gamestate -> Behaviour m td Int
-- difficultySF game = arrMSync_ $ difficulty game
--
-- pausedSF :: Gamestate -> Behaviour m td Bool
-- pausedSF game = arrMSync_ $ paused game


-- gamestateAsMSF
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => Behaviour m td Gamestate
-- gamestateAsMSF = do
--   game <- safely nextGamestate -< game
--   returnA                          -< Gamestate {..}
--
--potentially graphics should be the only thing left in render, and the gloss stuff can go in its own module for clarity.

--combine all the graphics into one picture
-- graphics :: Monad m => BehaviourF m Float Gamestate Picture
-- graphics = proc game@Gamestate -> do
--    returnA  -< translate 0 yOffset $ renderGamestate game
