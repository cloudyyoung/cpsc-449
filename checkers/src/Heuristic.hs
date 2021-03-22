module Heuristic where

import Moves
import ApplyMove
import Checkers.Types



black_heuristic:: GameState -> Int
black_heuristic g = length(blackPieces g) - length(redPieces g) + 2 * (length(blackKings g) - length(redKings g))

red_heuristic::GameState -> Int
red_heuristic g = length(redPieces g) - length(blackPieces g) + 2 * (length(redKings g) - length(blackKings g))
