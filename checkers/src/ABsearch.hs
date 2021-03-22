module ABsearch where

import Moves
import ApplyMove
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
abmaxprune g (a, b) d = undefined

abminprune :: GameState -> (Int, Int) -> Int -> Int
abminprune g (a, b) d = undefined

minimax :: GameState -> Int -> Status -> Int
minimax g depth status
        | depth == 0 || (moves g) == [] =   if status == Turn Red
                                            then red_hueristic g
                                            else black_hueristic g
        | otherwise =                       if (status g) == status
                                            then maximum(map (\g -> minimax g (depth - 1) status) (children g))
                                            else minimum(map (\g -> minimax g (depth - 1) status) (children g))
    where
        children g = foldr (\move xs -> (apply_move move g):xs) [] (moves g)

