{-# LANGUAGE PatternGuards #-}

module Gloss.Interface where

--lambdalumps
import Model.Gamestate

--lambdalumps io
import IO.Drop
import IO.Hold
import IO.LeftRight
import IO.RandomTetronimo
import IO.Rotate

--gloss package
import Graphics.Gloss.Interface.Pure.Game as G

handleEvent :: Event -> Maybe Gamestate
handleEvent e = Just $ parseEvent e getGamestate


parseEvent :: G.Event -> Gamestate -> Gamestate
parseEvent (G.EventKey key keyState _ _ ) game
    --move to the left
    | G.SpecialKey G.KeyLeft  <- key
    , G.Down                  <- keyState
    = Gamestate (nextTetronimo game)
                (tetronimoLeft (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (difficulty game)
                (paused game)
    --move to the right
    | G.SpecialKey G.KeyRight  <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (tetronimoRight (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (difficulty game)
                (paused game)
    -- handle rotation
    | G.SpecialKey G.KeyTab    <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (rotateCW (currentTetronimo game))
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (difficulty game)
                (paused game)
    -- handle drops
    | G.SpecialKey G.KeyEnter    <- key
    , G.Down                     <- keyState
    = Gamestate (nextTetronimo game)
                (dropTet (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (difficulty game)
                (paused game)
    -- handle hold if no existing hold
    | G.SpecialKey G.KeyShiftL   <- key
    , G.Down                     <- keyState
    = handleHold game
parseEvent (G.EventKey key keyState _ _) game
    -- reset game TODO: Introduce the random tetronimo function
    | G.Char 'r'                <- key
    , G.Down                    <- keyState
    = Gamestate (getTetronimo (seed game))
                (getTetronimo (pred $ seed game))
                []
                (Nothing)
                (13)
                (0)
                (2)
                (False)
    | G.Char 'd'                <- key
    , G.Down                    <- keyState
    = Gamestate (nextTetronimo game)
                (currentTetronimo game)
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (succ $ difficulty game)
                (paused game)
    | G.Char 'p'                <- key
    , G.Down                    <- keyState
    = Gamestate (nextTetronimo game)
                (currentTetronimo game)
                (settledTetronimos game)
                (hold game)
                (seed game)
                (score game)
                (succ $ difficulty game)
                (handlePause $ paused game)
parseEvent _ game
    = game

handlePause :: Bool -> Bool
handlePause status
  | status    = False
  | otherwise = True
