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
    simple_piece xs = [[(x,y), (x',y')] | (x,y) <- xs, (x',y') <- [(x + 1,y + dir), (x - 1,y + dir)], notoccupied (x',y') g, onboard (x',y')]
    simple_king  xs = [[(x,y), (x',y')] | (x,y) <- xs, (x',y') <- [(x + 1,y + 1), (x + 1,y - 1), (x - 1,y + 1), (x - 1,y - 1)], notoccupied (x',y') g, onboard (x',y')]
    dir             = case (status g) of 
                        (Turn Red) -> -1
                        (Turn Black) -> 1


jump_moves :: GameState -> [Move]
jump_moves g = case (status g) of 
    (Turn Black) -> (jump_piece (blackPieces g)) ++ (jump_king (blackKings g))
    (Turn Red)   -> (jump_piece (redPieces g)) ++ (jump_king (redKings g))
    GameOver     -> []
  where
    jump_over [] = [[]]
    jump_over z = z
    jump_piece xs = [(x,y):ys | (x,y) <- xs, ys <- jump_piece' (x,y) [] (x,y)]
    jump_piece' start rem (x,y) = undefined
    jump_king xs = [(x,y):ys | (x,y) <- xs, ys <- jump_king' (x,y) [] (x,y)]
    jump_king' start rem (x,y) = [(x'',y''):ys | ((x',y'),(x'',y'')) <- [((x + 1,y + 1),(x + 2,y + 2)),((x - 1,y + 1),(x - 2,y + 2)),((x + 1,y - 1),(x + 2,y - 2)),((x - 1,y - 1),(x - 2,y - 2))], not(elem (x',y') rem), opponent_occupied (x',y') g, start == (x'',y''), notoccupied (x'',y'') g, onboard (x'',y''), ys <- jump_over (jump_king' start ((x',y'):rem) (x'',y''))]


notoccupied :: Cord -> GameState -> Bool
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
    | (status g == Red) && (elem (x,y) (blackPieces g)) = True
    | (status g == Red) && (elem (x,y) (blackKings g))  = True
    | (status g == Black) && (elem (x,y) (redPieces g)) = True
    | (status g == Black) && (elem (x,y) (redKings g))  = True
    | otherwise                                         = False


