{-# LANGUAGE Arrows            #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies      #-}



module RhineGloss.Arrowized.IO.Interface where

--lambdalumps
import RhineGloss.Arrowized.Model.Gamestate
import RhineGloss.Arrowized.Model.RandomTetronimo

--lambdalumps io
import RhineGloss.Arrowized.IO.Drop
import RhineGloss.Arrowized.IO.Hold
import RhineGloss.Arrowized.IO.LeftRight
import RhineGloss.Arrowized.IO.Rotate

--gloss package
import Graphics.Gloss.Interface.Pure.Game as G
import Graphics.Gloss

--Rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except

-- parseEvents as a BehaviourFExcept
-- parseEvents :: Monad m => BehaviourFExcept m Float (Event, Gamestate) Gamestate Empty
-- parseEvents = do
--     try $ proc ((G.EventKey key keyState _ _), game) -> do
--         -- let event = (G.EventKey key keyState _ _
--         _       <- throwOn () -< G.Char 'p' /= key && G.Down /= keyState
--         returnA               -< game {playState = handlePause $ playState game}
--     parseEvents

parseEvent' :: G.Event -> Gamestate -> Gamestate
parseEvent' (G.EventKey key keyState _ _ ) game
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
    = game {currentTetronimo = (rotateCW (currentTetronimo game))}
    -- handle drops
    | G.SpecialKey G.KeyEnter  <- key
    , G.Down                   <- keyState
    = game {currentTetronimo = (dropTet (currentTetronimo game) (settledTetronimos game))}
    -- handle hold if no existing hold
    | G.SpecialKey G.KeyShiftL   <- key
    , G.Down                     <- keyState
    = handleHold game
parseEvent' (G.EventKey key keyState _ _) game
    | G.Char 'r'                <- key
    , G.Down                    <- keyState
    = getGamestate
    | G.Char 'p'                <- key
    , G.Down                    <- keyState
    = game {playState = handlePause $ playState game}
parseEvent' _ game
    = game

handlePause :: PlayState -> PlayState
handlePause status = case status of
   Paused -> Active
   Active -> Paused
   Over   -> Over
