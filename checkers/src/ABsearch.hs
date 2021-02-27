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
