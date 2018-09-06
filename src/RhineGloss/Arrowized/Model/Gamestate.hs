{-# LANGUAGE Arrows            #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}


module RhineGloss.Arrowized.Model.Gamestate where

import RhineGloss.Arrowized.Model.Lib
import RhineGloss.Arrowized.Model.RandomTetronimo
import RhineGloss.Arrowized.Model.Score
import RhineGloss.Arrowized.Model.Tetronimo

import FRP.Rhine
import FRP.Rhine.SyncSF.Except

data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks,
    hold              :: Maybe Tetronimo,
    seed              :: Int,
    score             :: Int,
    difficulty        :: Int,
    playState         :: PlayState
} deriving (Eq)

data PlayState = Active | Paused | Over
  deriving (Eq, Show)

--initial gamestate - want to use RandT
getGamestate :: Gamestate
getGamestate = Gamestate
                    (getTetronimo $ 13)
                    (getTetronimo $ (14))
                    []
                    (Nothing)
                    (13)
                    (0)
                    (5)
                    (Active)

-- | TODO: Arrowize
-- | Given that this is the next next tetronimo, output the gamestate that arises from that

settle :: Monad m => BehaviourF m Float Gamestate Gamestate
settle = proc game -> do
    readyForNewTet <- safely settleStates -< game                -- get a Bool on whether it's time to settle
    newGame        <- safely handleSettle -< (readyForNewTet, game) -- settle the current bool and output the gamestate that results
    returnA                               -< newGame

settleStates :: Monad m => BehaviourFExcept m Float Gamestate Bool Empty
settleStates = do
    try $ proc game -> do
        _ <- throwOn () -< isItSettled (currentTetronimo game) (settledTetronimos game)
        returnA         -< False
    try $ proc game -> do
        _ <- throwOn () -< not $ isItSettled (currentTetronimo game) (settledTetronimos game)
        returnA         -< True
    settleStates

handleSettle :: Monad m => BehaviourFExcept m Float (Bool, Gamestate) Gamestate Empty
handleSettle = do
    try $ proc (readyToSettle, game) -> do
        _       <- throwOn () -< readyToSettle == False
        returnA               -< isTheGameOver game {
                                      nextTetronimo     = getTetronimo $ seed game,
                                      currentTetronimo  = nextTetronimo game,
                                      settledTetronimos = getCollapsed $ tetToBlocks
                                                            (currentTetronimo game)
                                                            (settledTetronimos game),
                                      seed              = (succ $ (seed game)),
                                      score             = scoreForClear
                                                            (snd $ tetToBlocks
                                                                (currentTetronimo game)
                                                                (settledTetronimos game))
                                                            (difficulty game) (score game)
                                            }
    try $ proc (readyToSettle, game) -> do
        _ <- throwOn () -< readyToSettle == True
        returnA               -< game {
                                      currentTetronimo  = move $ currentTetronimo game,
                                      settledTetronimos = settledTetronimos game,
                                      score             = (scoreForSoftDrop $ score game)
                                            }
    handleSettle

getCollapsed :: ([Pos], Int) -> [Pos]
getCollapsed (x, y) = collapseBlocks x

tetToBlocks :: Tetronimo -> [Pos] -> ([Pos], Int)
tetToBlocks tet blocks = clear $ ((settleTetronimo tet) ++ blocks)

--is the game over? Then Switch the playstate
isTheGameOver :: Gamestate -> Gamestate
isTheGameOver game
  | (isItSettled tetronimo blocks) && topYCoord tetronimo > screenRoof = game {playState = Over}
  | otherwise                                                          = game
    where blocks    = settledTetronimos game
          tetronimo = currentTetronimo  game

-- settle' :: Tetronimo -> Gamestate -> Gamestate
-- settle' nxnxtet game
    -- | isItSettled tet blocks
        -- = game {nextTetronimo     = nxnxtet,
            -- currentTetronimo  = nextTetronimo game,
            -- settledTetronimos = (collapseBlocks . fst $ clearedBlocks),
            -- seed              = (succ $ (seed game)),
            -- score             = (scoreForClear (snd $ clearedBlocks) (difficulty game) (score game) )}
    -- | otherwise
    --  = game {currentTetronimo  = move tet,
        -- settledTetronimos = blocks,
        -- score             = (scoreForSoftDrop $ score game)}
            -- where
            --   tet           = currentTetronimo game
            --   blocks        = settledTetronimos game
            --   clearedBlocks = clear $ ((settleTetronimo tet) ++ blocks)
