module Score where

--This SCORE was used in Nintendo's versions of Tetris for NES, for Game Boy, and for Super NES.

--Level	  Points for  Points for   Points for   Points for
--          1 line	   2 lines	     3 lines	   4 lines

--0	        40	         100	          300	        1200
--1	        80	         200	          600	        2400
--2	        120	         300	          900	        3600

-- For each piece, the game also awards the number of points equal to the number of grid spaces that the player has continuously soft dropped the piece. Unlike the points for lines, this does not increase per level.

scoreForSoftDrop :: Int -> Int
scoreForSoftDrop x = 4 + x

scoreForClear :: Int -> Int -> Int -> Int
scoreForClear numberOfLines level score
    | numberOfLines == 1 = score + (40 * (level + 1))
    | numberOfLines == 2 = score + (100 * (level + 1))
    | numberOfLines == 3 = score + (300 * (level + 1))
    | numberOfLines == 4 = score + (1200 * (level + 1))
    | otherwise          = score
