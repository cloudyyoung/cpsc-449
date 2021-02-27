module ApplyMove where

import Moves
import Checkers.Types


apply_move :: Move -> GameState -> GameState
apply_move move g
    | elem move (jump_moves g)                          = make_jump_move move g
    | elem move (simple_moves g) && null (jump_moves g) = make_simple_move move g
    | otherwise                                         = g {message = "Illegal move! A jump is available:  " ++ show (jump_moves g)}


porkcoord_to_coord :: PorK Coord -> Coord
porkcoord_to_coord (P x) = x
porkcoord_to_coord (K x) = x


make_simple_move :: Move -> GameState -> GameState
make_simple_move [pc_start,pc_end] g
    | (status g) == (Turn Black) && elem start (blackKings g)                               = g' {blackKings  = replace start end (blackKings g)}
    | (status g) == (Turn Black) && elem start (blackPieces g) && is_king end (Turn Black)  = g' {blackPieces = remove start (blackPieces g), blackKings = end:(blackKings g)}
    | (status g) == (Turn Black) && elem start (blackPieces g)                              = g' {blackPieces = replace start end (blackPieces g)}
    | (status g) == (Turn Red)   && elem start (redKings g)                                 = g' {redKings    = replace start end (redKings g)}
    | (status g) == (Turn Red)   && elem start (redPieces g)   && is_king end (Turn Red)    = g' {redPieces   = remove start (redPieces g), redKings = end:(redKings g)}
    | (status g) == (Turn Red)   && elem start (redPieces g)                                = g' {redPieces   = replace start end (redPieces g)}
    | otherwise                                                                             = gx
    where
        start = porkcoord_to_coord pc_start
        end   = porkcoord_to_coord pc_end
        g'    = g {status = change_player (status g), message = "", history = [pc_start,pc_end]:(history g)}
        gx    = g {message = "Invalid simple move"}


make_jump_move :: Move -> GameState -> GameState
make_jump_move (pc_start:pc_next:rest) g
    | (status g) == (Turn Black) && elem start (blackKings g)                               = undefined
    | (status g) == (Turn Black) && elem start (blackPieces g) && is_king next (Turn Black) = undefined
    | (status g) == (Turn Black) && elem start (blackPieces g)                              = undefined
    | (status g) == (Turn Red)   && elem start (redKings g)                                 = make_jump_move (pc_next:rest) (g {blackKings = remove (jumped start next) (blackKings g), blackPieces = remove (jumped start next) (blackPieces g), redKings = replace start next (redKings g), message = ""})
    | (status g) == (Turn Red)   && elem start (redPieces g)   && is_king next (Turn Red)   = undefined
    | (status g) == (Turn Red)   && elem start (redPieces g)                                = undefined
    | otherwise                                                                             = g {message = "invalid make_jump_move"}
    where
        start = porkcoord_to_coord pc_start
        next  = porkcoord_to_coord pc_next

-- is_king is defined in Moves.hs

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y zs = [if (z == x) then y else z | z <- zs]

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x ys = [y | y <- ys, y /= x]

change_player :: Status -> Status
change_player status = case status of
                        (Turn Red)   -> (Turn Black)
                        (Turn Black) -> (Turn Red)
                        GameOver     -> GameOver

jumped :: Coord -> Coord -> Coord
jumped (x,y) (x',y') = ((x + x') // 2, (y + y') // 2)

(//) = quot