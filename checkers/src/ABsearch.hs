module ABsearch where

import Moves
import ApplyMove
import Heuristic
import Checkers.Types



black_ai :: GameState -> Move
black_ai g = undefined

red_ai :: GameState -> Move
red_ai g = undefined

top :: Int
top = 30000

bot :: Int
bot = -30000

abmaxprune :: GameState -> (Int, Int) -> Int -> Int
abmaxprune g (a, b) d
    | a == b                        = a
    | (moves g) == ([],[]) || d == 0     = if (status g) == Turn Red
                                        then min (max (red_heuristic g) a) b
                                        else min (max (black_heuristic g) a) b
    | otherwise                     = abmaxprune' (children g) (a, b) d
    where
        children g = foldr (\move xs -> (apply_move move g):xs) [] (moves g)
        abmaxprune' [] (a, b) d = a
        abmaxprune' (g:gs) (a, b) d = abmaxprune' gs (a', b) d
            where
                a' = abminprune g (a, b) (d - 1)


abminprune :: GameState -> (Int, Int) -> Int -> Int
abminprune g (a, b) d
    | a == b                    = b
    | (moves g) == ([],[]) || d == 0   = if (status g) == Turn Red
                                    then min (max (red_heuristic g) a) b
                                    else min (max (black_heuristic g) a) b
    | otherwise                 = abminprune' (children g) (a, b) d
    where
        children g = foldr (\move xs -> (apply_move move g):xs) [] (moves g)
        abminprune' [] (a, b) d = b
        abminprune' (g:gs) (a, b) d = abminprune' gs (a, b') d
            where
                b' = abmaxprune g (a, b) (d - 1)


minimax :: GameState -> Int -> Status -> Int
minimax g depth status
    | depth == 0 || (moves g) == ([],[]) =   if status == Turn Red
                                        then red_heuristic g
                                        else black_heuristic g
    | otherwise =                       if (status g) == status
                                        then maximum(map (\g -> minimax g (depth - 1) status) (children g))
                                        else minimum(map (\g -> minimax g (depth - 1) status) (children g))
    where
        children g = foldr (\move xs -> (apply_move move g):xs) [] (moves g)

