{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-- {-# LANGUAGE ScopedTypeVariables    #-}

module Model.Tetronimo where

--rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except

--ordered from bottom to top, left to right.
data Tetronimo =
     Tetronimo { first    :: Pos,
                 second   :: Pos,
                 third    :: Pos,
                 fourth   :: Pos,
                 shape    :: Shape,
                 rotation :: Rotation
                 --current :: Bool
    } deriving (Eq, Show)

data Shape = IShape | LShape | JShape | SShape | ZShape | TShape | OShape deriving (Show, Eq)

data Rotation = Zero | Ninety | OneEighty | TwoSeventy deriving (Show, Eq)

data Pos = Pos {
  xcoord :: Int,
  ycoord :: Int
  } deriving (Show, Eq, Ord)

  --list of settled tetronimos - no longer matters what shape they are, so we just have a list.
  --we therefore need to handle whether a settled tetronimo is a breach of gameover before we output the new gamestate.
type SettledBlocks = [Pos]
--
-- theTetronimo
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => Behavior m td TetronimoState
-- theTetronimo = proc _ -> do
--   theshape <- safely tetronimoShapeStates -< ()
--   returnA                            -< TetronimoState {..}
--
-- tetronimoShapeStates
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => BehaviourFExcept m td a TetronimoShape Empty
-- --BehaviourFExcept m Float a Weather Empty
-- tetronimoShapeStates = do
--   sequence_ [ try $ timer 12 >>> arr (const theState)
--             | theState <- [IShape, LShape, JShape, SShape, ZShape, TShape, OShape]
--             ]
--   tetronimoShapeStates
--
-- tetronimoRotationStates
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => Behaviour m td TetronimoRotation
-- tetronimoRotationStates = arr_ Zero
--
--
-- data TetronimoState = TetronimoState
--   { theshape    :: TetronimoShape
--   , therotation :: TetronimoRotation } deriving Show
--
-- data TetronimoShape
--   = IShape | LShape | JShape | SShape | ZShape | TShape | OShape
--   deriving (Show)
--
-- data TetronimoRotation
--   = Zero | Ninety | OneEighty | TwoSeventy deriving (Show, Eq)
