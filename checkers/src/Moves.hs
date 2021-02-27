module Moves where

import Checkers.Types


moves :: GameState -> ([Move], [Move])
moves g = (simple_moves g, jump_moves g)


simple_moves :: GameState -> [Move]
simple_moves g = case (status g) of 
    (Turn Black) -> (simple_piece (blackPieces g)) ++ (simple_king (blackKings g))
    (Turn Red)   -> (simple_piece (redPieces g)) ++ (simple_king (redKings g))
    GameOver     -> []
  where
    simple_piece xs = [[P (x,y), coord'] | 
                        (x,y) <- xs, (x',y') <- [(x + 1,y + (dir g)), (x - 1,y + (dir g))], 
                        coord' <- if is_king (x',y') g
                                  then [K (x',y')]
                                  else [P (x',y')],
                        notoccupied (x',y') g, 
                        onboard (x',y')]
    simple_king  xs = [[K (x,y), K (x',y')] | 
                        (x,y) <- xs, (x',y') <- [(x + 1,y + 1), (x + 1,y - 1), (x - 1,y + 1), (x - 1,y - 1)], 
                        notoccupied (x',y') g, 
                        onboard (x',y')]


jump_moves :: GameState -> [Move]
jump_moves g = case (status g) of 
    (Turn Black) -> (jump_piece (blackPieces g)) ++ (jump_king (blackKings g))
    (Turn Red)   -> (jump_piece (redPieces g)) ++ (jump_king (redKings g))
    GameOver     -> []
  where
    jump_over []                = [[]]
    jump_over x                 = x
    jump_piece xs               = [(P (x,y)):ys | (x,y) <- xs, ys <- jump_piece' (x,y) [] (x,y)]
    jump_piece' start rem (x,y) = [coord:ys | 
                                    ((x',y'),(x'',y'')) <- [((x + 1,y + (dir g)),(x + 2,y + 2 * (dir g))),((x - 1,y + (dir g)),(x - 2,y + 2 * (dir g)))], 
                                    not(elem (x',y') rem), 
                                    opponent_occupied (x',y') g, 
                                    notoccupied (x'',y'') g || start == (x'', y''), 
                                    onboard (x'', y''),
                                    coord <-  if (is_king (x'',y'')) g
                                              then [K (x'',y'')]
                                              else [P (x'',y'')],
                                    ys <- if (is_king (x'',y''))  g
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


dir :: GameState -> Int
dir g = case (status g) of 
        (Turn Red) -> -1
        (Turn Black) -> 1

is_king :: Coord -> GameState -> Bool
is_king (x,y) g = case (status g) of
                  (Turn Red) -> y == 0
                  (Turn Black) -> y == 7

notoccupied :: Coord -> GameState -> Bool
notoccupied (x,y) g
    | elem (x,y) (blackPieces g)  = False
    | elem (x,y) (blackKings g)   = False
    | elem (x,y) (redPieces g)    = False
    | elem (x,y) (redKings g)     = False
    | otherwise                   = True

onboard :: Coord -> Bool
onboard (x,y) = (0 <= x) && (x<=7) && (0 <= y) && (y <= 7)

opponent_occupied :: Coord -> GameState -> Bool
opponent_occupied (x,y) g
    | (status g == (Turn Red)) && (elem (x,y) (blackPieces g))  = True
    | (status g == (Turn Red)) && (elem (x,y) (blackKings g))   = True
    | (status g == (Turn Black)) && (elem (x,y) (redPieces g))  = True
    | (status g == (Turn Black)) && (elem (x,y) (redKings g))   = True
    | otherwise                                                 = False


