{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Model.Gamestate where

--lambdalumps
import Model.Lib
import Model.Score
import Model.Tetronimo
--lambdalumps io
import IO.RandomTetronimo

--rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except


data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks,
    hold              :: Maybe Tetronimo,
    --For the RNG TODO: implement
    seed              :: Int,
    score             :: Int,
    difficulty        :: Int,
    paused            :: Bool
} deriving (Eq)

-- logic of the whole app
-- gameLogic
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => BehaviorF m td Float Gamestate
-- gameLogic = feedback 0 $ proc (eventRequest, seedOld) -> do
--     -- settle               <- currrentTetronimo     -< getGamestate
--   -- -- nextTetronimo     :: Tetronimo,
--     nextTetronimo     <- getRandomTetronimo          -< (seed + 1)
--   -- -- currentTetronimo  :: Tetronimo,
--   -- currentTetronimo  <- getTetronimo          -< (seed)
--   -- -- settledTetronimos :: SettledBlocks,
--   -- settledTetronimos <- settledTetronimos     -< getGamestate?
--   -- -- hold              :: Maybe Tetronimo,
--   -- hold              <- hold                   -< getGamestate?
--   -- -- seed              :: Int,
--     seed                                        -< seedOld
--     -- score             <- score                  -< getGamestate?
--   -- -- score             :: Int,
--   -- -- difficulty        :: Int,
--   -- difficulty        <-
--   -- paused            <-   paused              -< getGamestate
--     returnA                                    -< (Gamestate {..}, 3)

  -- stepThrough :: Float -> Gamestate -> Gamestate
  -- stepThrough _ game
  --   | paused game = game
  --   | otherwise =
  --        settle nxnxtet (Gamestate
  --                         (nextTetronimo game)
  --                         (currentTetronimo game)
  --                         (settledTetronimos game)
  --                         (hold game)
  --                         (seed game)
  --                         (score game)
  --                         (difficulty game)
  --                         (paused game))
  --                             where nxnxtet = getTetronimo (seed game)


getGamestate :: Gamestate
getGamestate = initialGamestate

nextGamestate :: Gamestate -> Gamestate
nextGamestate game = settle nxnxtet (Gamestate
                       (nextTetronimo game)
                       (currentTetronimo game)
                       (settledTetronimos game)
                       (hold game)
                       (seed game)
                       (score game)
                       (difficulty game)
                       (paused game))
                           where nxnxtet = getTetronimo (seed game)


initialGamestate :: Gamestate
initialGamestate = Gamestate
                    (getTetronimo $ 13)
                    (getTetronimo $ (14))
                    []
                    (Nothing)
                    (13)
                    (0)
                    (3)
                    (False)

-- | Given that this is the next next tetronimo, output the gamestate that arises from that
settle :: Tetronimo -> Gamestate -> Gamestate
settle nxnxtet g
  | isItSettled tet blocks
        = Gamestate nxnxtet
                    nxtet
                    (collapseBlocks . fst $ clearedBlocks)
                    (hold g)
                    (succ $ (seed g))
                    (scoreForClear (snd $ clearedBlocks) level (score g))
                    (difficulty g)
                    (paused g)
  | otherwise
        = Gamestate nxtet
                    (move tet)
                    blocks
                    (hold g)
                    (seed g)
                    (scoreForSoftDrop $ score g)
                    (difficulty g)
                    (paused g)
    where
      tet           = currentTetronimo g
      nxtet         = nextTetronimo g
      blocks        = settledTetronimos g
      clearedBlocks = clear $ ((settleTetronimo tet) ++ blocks)
