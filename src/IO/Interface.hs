{-# LANGUAGE PatternGuards #-}

module IO.Interface where

import Lib
import IO.Drop
import IO.LeftRight
import IO.Rotate

import Graphics.Gloss.Interface.Pure.Game as G

handleEvent :: G.Event -> Gamestate -> Gamestate
handleEvent (G.EventKey key keyState _ _ ) game
    --move to the left
    | G.SpecialKey G.KeyLeft  <- key
    , G.Down                  <- keyState
    = Gamestate (nextTetronimo game)
                (tetronimoLeft (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
    --move to the right
    | G.SpecialKey G.KeyRight  <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (tetronimoRight (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
    -- handle rotation
    | G.SpecialKey G.KeyTab    <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (rotateCW (currentTetronimo game))
                (settledTetronimos game)
    -- handle drops
    | G.SpecialKey G.KeyEnter    <- key
    , G.Down                     <- keyState
    = Gamestate (nextTetronimo game)
                (dropTet (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
handleEvent (G.EventKey key keyState _ _) game
    -- reset game TODO: Introduce the random tetronimo function
    | G.Char 'r'                <- key
    , G.Down                    <- keyState
    = Gamestate (spawn 5) (spawn 7) []
handleEvent _ game
    = game
