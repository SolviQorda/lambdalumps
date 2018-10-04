{-# LANGUAGE PatternGuards #-}

module IO.Interface where

--lambdalumps
import Model.Gamestate
import Model.RandomTetronimo

--lambdalumps io
import IO.Drop
import IO.Hold
import IO.LeftRight
import IO.Rotate

--gloss package
import Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss

parseEvent :: G.Event -> Gamestate -> Gamestate
parseEvent (G.EventKey key keyState _ _ ) game
    --move to the left
    | G.SpecialKey G.KeyLeft  <- key
    , G.Down                  <- keyState
    = game {currentTetronimo = (tetronimoLeft (currentTetronimo game) (settledTetronimos game))}
    --move to the right
    | G.SpecialKey G.KeyRight  <- key
    , G.Down                   <- keyState
    = game {currentTetronimo = (tetronimoRight (currentTetronimo game) (settledTetronimos game))}
    -- handle rotation
    | G.SpecialKey G.KeyTab    <- key
    , G.Down                   <- keyState
    = game {currentTetronimo = (rotateTet (currentTetronimo game) (settledTetronimos game))}
    -- handle drops
    | G.SpecialKey G.KeyEnter  <- key
    , G.Down                   <- keyState
    = game {currentTetronimo = (dropTet (currentTetronimo game) (settledTetronimos game))}
    -- handle hold if no existing hold
    | G.SpecialKey G.KeyShiftL   <- key
    , G.Down                     <- keyState
    = handleHold game
parseEvent (G.EventKey key keyState _ _) game
    | G.Char 'r'                <- key
    , G.Down                    <- keyState
    = getGamestate
    | G.Char 'p'                <- key
    , G.Down                    <- keyState
    = game {paused = handlePause $ paused game }
parseEvent _ game
    = game

handlePause :: Bool -> Bool
handlePause status
  | status    = False
  | otherwise = True
