module ApplyMove where

import Moves
import Checkers.Types



apply_move :: Move -> GameState -> GameState
apply_move m g
    | inmoves m (moves g) = undefined
    | otherwise = g{message = "Illegal move!!"}


make_simple_move :: PieceState -> GameState -> GameState
make_simple_move [start,end] g
    | status g == (Turn Black) && elem start (blackKings g) 
        = undefined
    | status g == (Turn Black) && elem start (blackPieces g)
        = if is_king end (Turn Black)
            then undefined
            else undefined
    | status g == (Turn Red) && elem start (redKings g) 
        = g{redKings = replace start end (redKings g)
            , status = change_player g
            , message = ""} 
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king end (Turn Red) 
            then undefined
            else undefined
    | otherwise = g{message = "invalid make_simple_move"}


make_jump_move :: PieceState -> GameState -> GameState
make_jump_move (start:(next:rest)) g 
    | status g == (Turn Black) && elem start (blackKings g) 
        = undefined
    | status g == (Turn Black) && elem start (blackPieces g) 
        = if is_king next (Turn Black)
            then undefined
            else undefined
    | status g == (Turn Red) && elem start (redKings g)
        = make_jump_move (next:rest)
                (st{blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , redKings = next:(remove start (redKings g))
                    , message = ""})
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king next (Turn Red)
            then undefined
            else undefined
    | otherwise = g{message = "invalid make_jump_move"}


is_king = undefined

replace = undefined

remove = undefined

change_player = undefined
