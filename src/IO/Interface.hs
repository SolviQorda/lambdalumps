{-# LANGUAGE PatternGuards #-}

module IO.Interface where

import Lib
import IO.Drop
import IO.LeftRight
import IO.RandomTetronimo
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
                (seed game)
                (score game)
                (difficulty game)
    --move to the right
    | G.SpecialKey G.KeyRight  <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (tetronimoRight (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
                (seed game)
                (score game)
                (difficulty game)
    -- handle rotation
    | G.SpecialKey G.KeyTab    <- key
    , G.Down                   <- keyState
    = Gamestate (nextTetronimo game)
                (rotateCW (currentTetronimo game))
                (settledTetronimos game)
                (seed game)
                (score game)
                (difficulty game)
    -- handle drops
    | G.SpecialKey G.KeyEnter    <- key
    , G.Down                     <- keyState
    = Gamestate (nextTetronimo game)
                (dropTet (currentTetronimo game) (settledTetronimos game))
                (settledTetronimos game)
                (seed game)
                (score game)
                (difficulty game)
handleEvent (G.EventKey key keyState _ _) game
    -- reset game TODO: Introduce the random tetronimo function
    | G.Char 'r'                <- key
    , G.Down                    <- keyState
    = Gamestate (getTetronimo (seed game))
                (getTetronimo (pred $ seed game))
                []
                (13)
                (0)
                (2)
    | G.Char 'd'                <- key
    , G.Down                    <- keyState
    = Gamestate (nextTetronimo game)
                (currentTetronimo game)
                (settledTetronimos game)
                (seed game)
                (score game)
                (succ $ difficulty game)
handleEvent _ game
    = game
