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

children :: GameState -> [GameState]
children g = foldr (\move xs -> (apply_move move g):xs) [] moves'
    where
        moves' = if (snd (moves g)) == []
                    then fst (moves g)
                    else snd (moves g)


abmaxprune :: GameState -> (Int, Int) -> Int -> Int
abmaxprune g (a, b) d
    | a == b                            = a
    | (moves g) == ([],[]) || d == 0    = if (status g) == Turn Red
                                            then min (max (red_heuristic g) a) b
                                            else min (max (black_heuristic g) a) b
    | otherwise                         = abmaxprune' (children g) (a, b) d
    where
        abmaxprune' [] (a, b) d = a
        abmaxprune' (g:gs) (a, b) d = abmaxprune' gs (a', b) d
            where
                a' = abminprune g (a, b) (d - 1)


abminprune :: GameState -> (Int, Int) -> Int -> Int
abminprune g (a, b) d
    | a == b                    = b
    | (moves g) == ([],[]) || d == 0    = if (status g) == Turn Red
                                            then min (max (red_heuristic g) a) b
                                            else min (max (black_heuristic g) a) b
    | otherwise                         = abminprune' (children g) (a, b) d
    where
        abminprune' [] (a, b) d = b
        abminprune' (g:gs) (a, b) d = abminprune' gs (a, b') d
            where
                b' = abmaxprune g (a, b) (d - 1)


minimax :: GameState -> Int -> Status -> Int
minimax g d s
    | d == 0 || (moves g) == ([],[])    = if s == Turn Red
                                            then red_heuristic g
                                            else black_heuristic g
    | otherwise                         = if (status g) == s
                                            then maximum(map (\g -> minimax g (d - 1) s) (children g))
                                            else minimum(map (\g -> minimax g (d - 1) s) (children g))

