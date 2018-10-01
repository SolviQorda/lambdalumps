module RhineGloss.Arrowized.Model.Score where
--This SCORE was used in Nintendo's versions of Tetris for NES, for Game Boy, and for Super NES.

--Level	  Points for  Points for   Points for   Points for
--          1 line	   2 lines	     3 lines	   4 lines

--1	        40	         100	          300	        1200
--2	        80	         200	          600	        2400
--3	        120	         300	          900	        3600

-- For each piece, the game also awards the number of points equal to the number of grid spaces that the player has continuously soft dropped the piece. Unlike the points for lines, this does not increase per level.

scoreForSoftDrop :: Int -> Int
scoreForSoftDrop x = 4 + x

scoreForClear :: Int -> Int -> Int -> Int
scoreForClear numberOfLines difficulty score
    | numberOfLines == 1 = score + (40 * (difficulty))
    | numberOfLines == 2 = score + (100 * (difficulty))
    | numberOfLines == 3 = score + (300 * (difficulty))
    | numberOfLines == 4 = score + (1200 * (difficulty))
    | otherwise          = score
