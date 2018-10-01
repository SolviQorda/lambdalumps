module IO.Drop where

import Data.List

--LambdaLumps
import Model.Lib
import Model.Tetronimo

dropTet :: Tetronimo -> SettledBlocks -> Tetronimo
dropTet tet blocks =
    let nextTet = tet { first  = dropOne (first tet)
                      , second = dropOne (second tet)
                      , third  = dropOne (third tet)
                      , fourth = dropOne (fourth tet)
                      }
    in if nextTet `isInside` blocks || belowFloor nextTet
       then tet
       else dropTet nextTet blocks



isInside :: Tetronimo -> SettledBlocks -> Bool
isInside tet blocks = any (\pos -> pos `elem` blocks) [first tet, second tet, third tet, fourth tet]


dropOne :: Pos -> Pos
dropOne (Pos x y) = Pos x (y - 1)


belowFloor :: Tetronimo -> Bool
belowFloor tet = any (\pos -> ycoord pos < 0) [first tet, second tet, third tet, fourth tet]


