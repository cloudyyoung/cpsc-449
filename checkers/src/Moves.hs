module Moves where

import Checkers.Types

{-
  	Get all possible simple and jump moves
-}
moves :: GameState -> ([Move], [Move])
moves g = (simple_moves g, jump_moves g)

{-
  	Get all possible simple move for every pawn and king.

  	simple_piece: 	For red, every piece can move to (x + 1, y - 1) or (x - 1, y - 1).
                	For black, every piece can move to (x + 1, y + 1) or (x - 1, y + 1).
                	If the slot is not occupied (not placed with anything)
                	and it is on the board (a valid slot),
                	then it is a possible for this piece move from current slot
                	to the slot.
  	simply_king:  	For both colors, can move to (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1).
                	If the slot is not occupied (not placed with anything)
                	and it is on the board (a valid slot),
                	then it is a possible for this king move from current slot
                	to the slot.
  	coord':       	Decide if current piece should turn into king piece,
                	assign KorP to the corrdinate.
-}
simple_moves :: GameState -> [Move]
simple_moves g = case (status g) of
	(Turn Black) -> (simple_piece (blackPieces g)) ++ (simple_king (blackKings g))
	(Turn Red)   -> (simple_piece (redPieces g)) ++ (simple_king (redKings g))
	GameOver     -> []
  	where
		simple_piece xs = [[P (x,y), coord' (x',y')] |
							(x,y) <- xs, (x',y') <- [(x + 1,y + (dir g)), (x - 1,y + (dir g))],
							notoccupied (x',y') g,
							onboard (x',y')]
		simple_king  xs = [[K (x,y), K (x',y')] |
							(x,y) <- xs, (x',y') <- [(x + 1,y + 1), (x + 1,y - 1), (x - 1,y + 1), (x - 1,y - 1)],
							notoccupied (x',y') g,
							onboard (x',y')]
		coord' (x,y)    = if is_king (x,y) (status g)
							then K (x,y)
							else P (x,y)


{-
  	Get all possible jump move for every piece and king.

  	jump_over:    	If the jump is over, then return empty list.
  	jump_piece:   	Get rest of the jump steps for each piece, 
                	concat with the starting coordinate.
  	jump_piece':  	Get following coordinate for a given start coordiante.
                	For red, every piece can move to (x + 2, y + 2) and (x - 2, y + 2),
                	if respectively, (x + 1, y + 1) and (x - 1, y + 1) is occupied by oponent,
                	and the destination is not occupied by anything.
                	For black, every piece can move to (x + 2, y - 2) and (x - 2, y - 2),
                	if respectively, (x + 1, y - 1) and (x - 1, y - 1) is occupied by oponent,
                	and the destination is not occupied by anything.
                	If a piece is passing the oponent's boundary, it turns into and continue as King,
  	jump_king:    	Get rest of the king steps for each king, 
                	concate with the starting coordinate.
  	jump_king':   	Similar with jump_piece'.
-}  
jump_moves :: GameState -> [Move]
jump_moves g = case (status g) of
    (Turn Black) -> (jump_piece (blackPieces g)) ++ (jump_king (blackKings g))
    (Turn Red)   -> (jump_piece (redPieces g)) ++ (jump_king (redKings g))
    GameOver     -> []
  	where
		jump_over []                = [[]]
		jump_over x                 = x
		jump_piece xs               = [(P (x,y)):ys | (x,y) <- xs, ys <- jump_piece' (x,y) [] (x,y)]
		jump_piece' start rem (x,y) = [(coord'' (x'',y'')):ys |
										((x',y'),(x'',y'')) <- [((x + 1,y + (dir g)),(x + 2,y + 2 * (dir g))),((x - 1,y + (dir g)),(x - 2,y + 2 * (dir g)))],
										not(elem (x',y') rem),
										opponent_occupied (x',y') g,
										notoccupied (x'',y'') g || start == (x'', y''),
										onboard (x'', y''),
										ys <- if (is_king (x'',y'') (status g))
											then jump_over (jump_king' start ((x',y'):rem) (x'',y''))
											else jump_over (jump_piece' start ((x',y'):rem) (x'',y''))]
		jump_king xs                = [(K (x,y)):ys | (x,y) <- xs, ys <- jump_king' (x,y) [] (x,y)]
		jump_king' start rem (x,y)  = [(K (x'',y'')):ys |
										((x',y'),(x'',y'')) <- [((x + 1,y + 1),(x + 2,y + 2)), ((x - 1,y + 1),(x - 2,y + 2)), ((x + 1,y - 1),(x + 2,y - 2)), ((x - 1,y - 1),(x - 2,y - 2))],
										not(elem (x',y') rem),
										opponent_occupied (x',y') g,
										notoccupied (x'',y'') g || start == (x'', y''),
										onboard (x'', y''),
										ys <- jump_over (jump_king' start ((x',y'):rem) (x'',y''))]
		coord'' (x,y)               = if is_king (x,y) (status g)
										then K (x,y)
										else P (x,y)

{-
  	Return direction sign.
-}
dir :: GameState -> Int
dir g = case (status g) of
        (Turn Red) -> -1
        (Turn Black) -> 1

{-
  	Return true if the piece on coordinate is turning King.
-}
is_king :: Coord -> Status -> Bool
is_king (x,y) status = case status of
						(Turn Red)    -> y == 0
						(Turn Black)  -> y == 7
						GameOver      -> False

{-
  	Return true if the coordinate is not occupied.
-}
notoccupied :: Coord -> GameState -> Bool
notoccupied (x,y) g
    | elem (x,y) (blackPieces g)  = False
    | elem (x,y) (blackKings g)   = False
    | elem (x,y) (redPieces g)    = False
    | elem (x,y) (redKings g)     = False
    | otherwise                   = True

{-
  	Return true if the coordinate is valid.
-}
onboard :: Coord -> Bool
onboard (x,y) = (0 <= x) && (x <= 7) && (0 <= y) && (y <= 7)

{-
  	Return true if the coordinate is occupied by opponent's piece.
-}
opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied (x,y) g
    | (status g == (Turn Red)) && (elem (x,y) (blackPieces g))  = True
    | (status g == (Turn Red)) && (elem (x,y) (blackKings g))   = True
    | (status g == (Turn Black)) && (elem (x,y) (redPieces g))  = True
    | (status g == (Turn Black)) && (elem (x,y) (redKings g))   = True
    | otherwise                                                 = False


