{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}

module RhineGloss.Arrowized.Model.Tetronimo where

data Tetronimo =
     Tetronimo   {  first    :: Pos,
                    second   :: Pos,
                    third    :: Pos,
                    fourth   :: Pos,
                    shape    :: Shape,
                    rotation :: Rotation
  } deriving (Eq, Show)

data TetState
  = Falling
  | Settled
  | Violation
  deriving (Eq, Show)

data Shape
  = IShape
  | LShape
  | JShape
  | SShape
  | ZShape
  | TShape
  | OShape
  deriving (Show, Eq)

data Rotation
  = Zero
  | Ninety
  | OneEighty
  | TwoSeventy
  deriving (Show, Eq)

data Pos = Pos {
    xcoord :: Int,
    ycoord :: Int
  } deriving (Show, Eq, Ord)

type SettledBlocks = [Pos]

--what would this look like? Is it seeking to replace the main managers in gamestate? I'm going to draft functionality here.

-- tetronimoStates
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => BehaviourFExcept m td (_, _) TetState Empty
  --if a tet is settled, draw a new one?
  --if a tet is violation, game over?
  --otherwise continue to fall?
