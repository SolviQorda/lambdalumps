module Model.Gamestate where

--lambdalumps
import Model.Lib
import Model.RandomTetronimo
import Model.Score
import Model.Tetronimo

--lambdalumps io
import IO.Rotate

import Data.Fixed

data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks,
    hold              :: Maybe Tetronimo,
    seed              :: Int,
    score             :: Int,
    difficulty        :: Int,
    paused            :: Bool
} deriving (Eq)

--initial gamestate
getGamestate :: Gamestate
getGamestate = Gamestate
                    (getTetronimo $ 13)
                    (getTetronimo $ (14))
                    []
                    (Nothing)
                    (13)
                    (0)
                    (1)
                    (False)

-- | Given that this is the next next tetronimo, output the gamestate that arises from that
settle :: Tetronimo -> Gamestate -> Gamestate
settle nxnxtet game
  | isItSettled tet blocks
        = game {nextTetronimo     = nxnxtet,
                currentTetronimo  = nextTetronimo game,
                settledTetronimos = (collapseBlocks . fst $ clearedBlocks),
                seed              = (succ $ (seed game)),
                score             = (scoreForClear (snd $ clearedBlocks) (difficulty game) (score game) )}
  | otherwise
        = game {currentTetronimo  = move tet,
                settledTetronimos = blocks,
                score             = (scoreForSoftDrop $ score game)}
    where
      tet           = currentTetronimo game
      blocks        = settledTetronimos game
      clearedBlocks = clear $ ((settleTetronimo tet) ++ blocks)
