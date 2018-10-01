{-# LANGUAGE Arrows            #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module RhineGloss.GameBuilder where

import Data.Functor.Identity

--Rhine
import FRP.Rhine.SyncSF.Except

--gloss-rhine
import FRP.Rhine.Gloss
-- import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- lambdalumps gloss
import RhineGloss.Arrowized.IO.Interface

--LambdaLumps
import RhineGloss.Arrowized.Model.DifficultyManager
import RhineGloss.Arrowized.Model.Gamestate
import RhineGloss.Arrowized.Model.Lib
import RhineGloss.Arrowized.Model.RandomTetronimo
import RhineGloss.Arrowized.Model.Tetronimo
import RhineGloss.Render

renderGameRhineGloss :: IO ()
renderGameRhineGloss = flowGloss
                        getDisplay
                        white
                        100
                        $ glossRhine

getDisplay = InWindow "LambdaLumps" (600, 1000) (10, 10)

game :: GlossSyncSF Event
game = feedback getGamestate $ proc (events, gamestate) -> do
    -- inputGiven <- safely parseEvents -< (events, gamestate)
    newState   <- safely stepThru    -< gamestate
    pics       <- renderGamestate    -< newState
    let finalNewState = foldr (.) id (parseEvent' <$> events) $ newState
    returnA                          -< (pics, finalNewState)

glossRhine :: GlossRhine Event
glossRhine = buildGlossRhine Just game

-- | handle a step of the clock, and only step through the gamestate
--   if the period determined by the difficulty has passed

stepThru :: Monad m => BehaviourFExcept m Float Gamestate Gamestate Empty
stepThru = do
    try $ proc game -> do
        steps <- timeInfoOf sinceStart -< ()
        _     <- throwOn ()            -< ((playState game) == Paused)
                                            || ((playState game) == Over)
                                            || ((floor $ steps * 100) `mod` (difficultyValue steps)) /= 0
        settle                         -< nextGamestate game steps
    try $ proc game -> do
        steps <- timeInfoOf sinceStart -< ()
        _ <- throwOn ()     -< ((playState game) == Active)
                                && ((floor $ steps * 100) `mod` (difficultyValue steps) == 0)
        returnA -< game
    stepThru

--get the next gamestate, with the right difficulty for the time passed

nextGamestate :: Gamestate -> Float -> Gamestate
nextGamestate game steps = game {difficulty = (100 - difficultyValue steps) `div` 10}
