{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
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

renderGameRhineGloss :: IO ()
renderGameRhineGloss = flowGloss
                getDisplay
                white
                100
                $ glossRhine

-- hoist the Gamestate onto a RhineGloss-friendly SF
game :: GlossSyncSF Event
game = feedback getGamestate $ proc (events, gamestate) -> do
  timeStep <- timeInfoOf sinceStart -< ()
  let newState = foldr (.) id (parseEvent <$> events) $ stepThru gamestate timeStep
  returnA                           -< (renderGamestate newState, newState)

glossRhine :: GlossRhine Event
glossRhine = buildGlossRhine Just game

-- | handle a step of the clock, and only step through the gamestate
--   if the period determined by the difficulty has passed

stepThru :: Gamestate -> Float -> Gamestate
stepThru game steps
  | paused game = game
  | (floor $ steps * 100) `mod` (difficultyValue steps) == 0 =
       settle nxnxtet $ nextGamestate game steps
  | otherwise = game
     where nxnxtet = getTetronimo (seed game)

--get the next gamestate, with the right difficulty for the time passed

nextGamestate :: Gamestate -> Float -> Gamestate
nextGamestate game steps = game {difficulty = (100 - difficultyValue steps) `div` 10}
