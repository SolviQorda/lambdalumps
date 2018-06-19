module Lib where

import Data.List
import System.Random

--is the game over? Tetronimo needs to be settled
gameOver :: Tetronimo -> Bool
gameOver setTet
  | topYCoord setTet > screenRoof = True
  | otherwise                     = False

--what rows need to be cleared - built from the SettledBlocks dataype.
--if length of filtered list of yPos's with ycoord = x is 10, then get rid of that sublist, look at the next sublist. 18 is the top row.
clear :: SettledBlocks -> SettledBlocks
clear bs = go 0 bs
  where
    go z [] = []
    go z (b:bs)
      | z == (succ screenRoof)         = (b:bs)
      | length ys == 10 = go (succ z) ((b:bs) \\ ys)
      | otherwise       = go (succ z) (b:bs)
        where ys = filter (\x -> ycoord x == z) (b:bs)


--TODO: a version of clear leveraging the playfield datatype, which inherits lists of settledBlocks, and then does a lookup on each row to see if it `elem` Empty -> if it doesn't then it clears.

-- | Given that this is the next next tetronimo, output the gamestate that arises from that
settle :: Tetronimo -> Gamestate -> Gamestate
settle nxnxtet g
  | isItSettled tet blocks = Gamestate nxnxtet nxtet (collapseBlocks . clear $ (settleTetronimo tet)++ blocks)
  | otherwise              = Gamestate nxtet (move tet) blocks
    where
      tet    = currentTetronimo g
      nxtet  = nextTetronimo g
      blocks = settledTetronimos g
-- some gameover check.

--Using a case syntax for now until i can improve the algorithm. TODO: Decide whether to use a sorting function
-- is the pos == a block with the same x and a y that is one less
isItSettled :: Tetronimo -> SettledBlocks -> Bool
isItSettled tet set
  | (ycoord $ first tet)  == 0                                         = True
  | (ycoord $ third tet)  == 0                                         = True
  | (ycoord $ second tet) == 0                                         = True
  | (ycoord $ fourth tet) == 0                                         = True
  | Pos (xcoord $ first tet)(pred $ ycoord $ first tet) `elem` set     = True
  | Pos (xcoord $ second tet)(pred $ ycoord $ second tet) `elem` set   = True
  | Pos (xcoord $ third tet)(pred $ ycoord $ third tet) `elem` set     = True
  | Pos (xcoord $ fourth tet)(pred $ ycoord $ fourth tet) `elem` set   = True
  | otherwise                                                          = False

--Int until I think of a better way - assuming an RNG
spawn :: Int -> Tetronimo
spawn x
  -- s tetronimo
  | x == 1    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 5 20) (Pos 6 20) SShape Zero
  -- z tetronimo
  | x == 2    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 3 20) (Pos 4 20) ZShape Zero
  -- t tetronimo
  | x == 3    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 4 20) (Pos 5 19) TShape Zero
  -- i tetronimo
  | x == 4    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 5 19) (Pos 6 19) IShape Zero
  -- o tetronimo
  | x == 5    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 4 20) (Pos 5 20) OShape Zero
  -- j tetronimo
  | x == 6    = Tetronimo (Pos 4 19) (Pos 5 19) (Pos 6 19) (Pos 4 20) JShape Zero
  -- l tetronimo
  | x == 7    = Tetronimo (Pos 3 19) (Pos 4 19) (Pos 5 19) (Pos 5 20) LShape Zero
  --error
  | otherwise = Tetronimo (Pos 4 19) (Pos 4 19) (Pos 4 19) (Pos 4 19) IShape Zero

-- getRandom :: IO Int
-- getRandom = do
--   r1 <- getStdGen
--   let (x, r2) = randomR (1, 7) r1
--   setStdGen r2
--   return x

--hard set for now.

screenRoof :: Int
screenRoof = 18

topYCoord :: Tetronimo -> Int
topYCoord tet = maximum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

lowYCoord :: Tetronimo -> Int
lowYCoord tet = minimum [(ycoord $ first tet), (ycoord $ second tet), (ycoord $ third tet), (ycoord $ fourth tet)]

--just a one cell fall for now TODO: handle input, direction, rotation.
move :: Tetronimo -> Tetronimo
move tet = Tetronimo
            (Pos (xcoord $ first tet) (pred (ycoord $ first tet)))
            (Pos (xcoord $ second tet) (pred (ycoord $ second tet)))
            (Pos (xcoord $ third tet) (pred (ycoord $ third tet)))
            (Pos (xcoord $ fourth tet) (pred (ycoord $ fourth tet)))
            (shape tet)
            (rotation tet)

moveBlock :: Pos -> Pos
moveBlock bloc = Pos (xcoord bloc) (pred $ ycoord bloc)
--helper function for clear - moves all settledBlocks down by one.

--this isn't going to work because it will return an empty list for all the empty rows.
collapseBlocks :: SettledBlocks -> SettledBlocks
collapseBlocks xs = go 1 xs
  where
    go z xs
    --for x in xs // or fo rz
    --are there blocks on row z? is there an empty row below x?
      | z == (succ screenRoof) = xs
      | (length $ qs) == 0                = go (succ z) xs
      | ((length $ qs) > 0) && (length ys == 0)        = go (succ z) (map moveBlock qs)
      | otherwise              = go (succ z) xs
      where qs = filter (\x -> (ycoord x) == z) xs
            ys = filter (\x -> (succ $ ycoord x) == z) xs

settleTetronimo :: Tetronimo -> SettledBlocks
settleTetronimo tet = [first tet, second tet, third tet, fourth tet]

--
-- | Types

data Gamestate =
  Gamestate {
    nextTetronimo     :: Tetronimo,
    currentTetronimo  :: Tetronimo,
    settledTetronimos :: SettledBlocks
}

--list of settled tetronimos - no longer matters what shape they are, so we just have a list.
--we therefore need to handle whether a settled tetronimo is a breach of gameover before we output the new gamestate.
type SettledBlocks = [Pos]

--ordered from bottom to top, left to right.
data Tetronimo =
     Tetronimo { first    :: Pos,
                 second   :: Pos,
                 third    :: Pos,
                 fourth   :: Pos,
                 shape    :: Shape,
                 rotation :: Rotation
    } deriving (Show)

data Shape = IShape | LShape | JShape | SShape | ZShape | TShape | OShape deriving (Show, Eq)

data Rotation = Zero | Ninety | OneEighty | TwoSeventy deriving (Show, Eq)

data Pos = Pos {
  xcoord :: Int,
  ycoord :: Int
  } deriving (Show, Eq, Ord)
