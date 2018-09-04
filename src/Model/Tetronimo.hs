
module Model.Tetronimo where

--ordered from bottom to top, left to right.
data Tetronimo =
     Tetronimo { first    :: Pos,
                 second   :: Pos,
                 third    :: Pos,
                 fourth   :: Pos,
                 shape    :: Shape,
                 rotation :: Rotation
    } deriving (Eq, Show)

data Shape = IShape | LShape | JShape | SShape | ZShape | TShape | OShape deriving (Show, Eq)

data Rotation = Zero | Ninety | OneEighty | TwoSeventy deriving (Show, Eq)

data Pos = Pos {
  xcoord :: Int,
  ycoord :: Int
  } deriving (Show, Eq, Ord)

type SettledBlocks = [Pos]
