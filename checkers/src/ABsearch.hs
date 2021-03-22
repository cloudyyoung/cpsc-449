module ABsearch where

import Moves
import ApplyMove
import Heuristic
import Checkers.Types


black_ai :: GameState -> Move
black_ai g = best_move (move_score g (get_moves g) 4 (Turn Black)) [] bot

red_ai :: GameState -> Move
red_ai g = best_move (move_score g (get_moves g) 4 (Turn Red)) [] bot

top :: Int
top = 30000

bot :: Int
bot = -30000


abmaxprune :: GameState -> (Int, Int) -> Int -> Int
abmaxprune g (a, b) d
    | (status g) == GameOver    = top
    | a == b                    = a
    | d == 0                    = if (status g) == Turn Red
                                    then min (max (red_heuristic g) a) b
                                    else min (max (black_heuristic g) a) b
    | otherwise                 = abmaxprune' (children g') (a, b) d
    where
        g' = g {status = if (status g) == Turn Red then Turn Black else Turn Red}
        abmaxprune' [] (a, b) d = a
        abmaxprune' (g:gs) (a, b) d = abmaxprune' gs (a', b) d
            where
                a' = abminprune g (a, b) (d - 1)


abminprune :: GameState -> (Int, Int) -> Int -> Int
abminprune g (a, b) d
    | (status g) == GameOver    = top
    | a == b                    = b
    | d == 0                    = if (status g) == Turn Red
                                    then min (max (red_heuristic g) a) b
                                    else min (max (black_heuristic g) a) b
    | otherwise                 = abminprune' (children g') (a, b) d
    where
        g' = g {status = if (status g) == Turn Red then Turn Black else Turn Red}
        abminprune' [] (a, b) d = b
        abminprune' (g:gs) (a, b) d = abminprune' gs (a, b') d
            where
                b' = abmaxprune g (a, b) (d - 1)


minimax :: GameState -> Int -> Status -> Int
minimax g d s
    | d == 0    = if s == Turn Red
                    then red_heuristic g
                    else black_heuristic g
    | otherwise = if (status g) == s
                    then maximum(map (\g -> minimax g (d - 1) s) (children g))
                    else minimum(map (\g -> minimax g (d - 1) s) (children g))


move_score :: GameState -> [Move] -> Int -> Status -> [(Move, Int)]
move_score g [] d s = []
move_score g (m:ms) d s = (m, minimax (apply_move m g) (d - 1) s) : move_score g ms d s


best_move :: [(Move, Int)] -> Move -> Int -> Move
best_move [] move max = move
best_move ((m,s):xs) move max
    | s > max   = best_move xs m s
    | otherwise = best_move xs move max


children :: GameState -> [GameState]
children g = foldr (\move xs -> (apply_move move g):xs) [] (get_moves g)


get_moves :: GameState -> [Move]
get_moves g = if (snd (moves g)) == []
                then fst (moves g)
                else snd (moves g)
