module Tetronimo where


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

-- --what kind of states can a tetronimo have
-- tetronimoStates
--   :: (Monad m, TimeDomain td, Diff td ~ Float)
--   => BehaviourF m td (_, _) Tetronimo current
-- tetronimoStates = do
--   --if there is space below, move down, if the clock has ticked
--   try $ move 
