module ApplyMove where

import Moves
import Checkers.Types

{-
    Apply move to the game state.

    If the given move is a simple move but there are possible jump moves,
    or the given move is neither a simple move nor jump move,
    the move is illegal.
-}
apply_move :: Move -> GameState -> GameState
apply_move move g
    | elem move (jump_moves g)                          = make_jump_move move g
    | elem move (simple_moves g) && null (jump_moves g) = make_simple_move move g
    | otherwise                                         = g {message = "Illegal move! A jump is available:  " ++ show (jump_moves g)}

{-
    Extract PorK Coord to Coord.
-}
porkcoord_to_coord :: PorK Coord -> Coord
porkcoord_to_coord (P x) = x
porkcoord_to_coord (K x) = x

{-
    Apply a given simple move.

    For both colors,
    If the starting piece is a king, then update the coordinate.
    If the starting piece is a pawn but end up been a king, 
    then remove the pawn from pawn list and add it to king list.
    If the starting piece is a pawn and no change, then update the coordinate.
    Otherwise, this simple move is invalid.

    pc_start, pc_end:   P/K start and P/K end. PorK Coord type.
    start, end:         Coord start and Coord end. Coord type.
    g':                 Common game state change for any valid simple move. 
                        Includes: change player, update message and add to history.
    gx:                 Error game state. Update message and change nothing.
-}
make_simple_move :: Move -> GameState -> GameState
make_simple_move [pc_start,pc_end] g
    | (status g) == (Turn Black) && elem start (blackKings g)                               = g' {blackKings  = replace start end (blackKings g)}
    | (status g) == (Turn Black) && elem start (blackPieces g) && is_king end (Turn Black)  = g' {blackPieces = remove start (blackPieces g), blackKings = end:(blackKings g)}
    | (status g) == (Turn Black) && elem start (blackPieces g)                              = g' {blackPieces = replace start end (blackPieces g)}
    | (status g) == (Turn Red)   && elem start (redKings g)                                 = g' {redKings    = replace start end (redKings g)}
    | (status g) == (Turn Red)   && elem start (redPieces g)   && is_king end (Turn Red)    = g' {redPieces   = remove start (redPieces g),     redKings = end:(redKings g)}
    | (status g) == (Turn Red)   && elem start (redPieces g)                                = g' {redPieces   = replace start end (redPieces g)}
    | otherwise                                                                             = gx
    where
        start = porkcoord_to_coord pc_start
        end   = porkcoord_to_coord pc_end
        g'    = g {status = change_player (status g), message = "", history = [pc_start,pc_end]:(history g)}
        gx    = g {message = "Invalid simple move"}

{-
    Apply a given jump move.

    Similar to make_simple_move. 
    Since a jump move envolves multiple steps, update game state after each step,
    and recursively invoke make_jump_move for following steps.
    For each step, if there are oponent's piece to be middle, 
    remove the piece as well.

    pc_start, pc_end:   P/K start and P/K end. PorK Coord type.
    start, end:         Coord start and Coord end. Coord type.
    jump:               The coordinate of middle piece, if there is any.
    gj:                 Recursively invoke make_jump_move for following steps.
    gh:                 Append current step to history stack recuesively.
                        This function is wrapping on the outside of gj, 
                        the base case of gj is appending the last step to the game history as a list,
                        every time recursived gj returns, gh appends the current step to the 
                        head of game history. This ends up with every step been appended to
                        the game history, inverted recursively.
    gb, gr:             Common game state change for removing middle oponent's piece.
    gx:                 Error game state. Update message and change nothing.
-}
make_jump_move :: Move -> GameState -> GameState
make_jump_move [pc_end] g = g {status = change_player (status g), message = "", history = [pc_end]:(history g)}
make_jump_move (pc_start:pc_next:rest) g
    | (status g) == (Turn Black) && elem start (blackKings g)                               = gj (gb {blackKings  = replace start next (blackKings g)})
    | (status g) == (Turn Black) && elem start (blackPieces g) && is_king next (Turn Black) = gj (gb {blackKings  = next:(blackKings g), blackPieces = remove start (blackPieces g)})
    | (status g) == (Turn Black) && elem start (blackPieces g)                              = gj (gb {blackPieces = replace start next (blackPieces g)})
    | (status g) == (Turn Red)   && elem start (redKings g)                                 = gj (gr {redKings    = replace start next (redKings g)})
    | (status g) == (Turn Red)   && elem start (redPieces g)   && is_king next (Turn Red)   = gj (gr {redKings    = next:(redKings g),     redPieces = remove start (redPieces g)})
    | (status g) == (Turn Red)   && elem start (redPieces g)                                = gj (gr {redPieces   = replace start next (redPieces g)})
    | otherwise                                                                             = gx
    where
        start = porkcoord_to_coord pc_start
        next  = porkcoord_to_coord pc_next
        jump  = middle start next
        gj g' = gh (make_jump_move (pc_next:rest) g')
        gh g' = g' {history = (pc_start:(head (history g'))):(tail (history g'))}
        gb    = g {redKings   = remove jump (redKings g),   redPieces   = remove jump (redPieces g)}
        gr    = g {blackKings = remove jump (blackKings g), blackPieces = remove jump (blackPieces g)}
        gx    = g {message = "Invalid jump move"}

-- is_king is defined in Moves.hs

{-
    Replace an element with another element in a list.
-}
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y zs = [if (z == x) then y else z | z <- zs]

{-
    Remove an element in a list.
-}
remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x ys = [y | y <- ys, y /= x]

change_player :: Status -> Status
change_player status = case status of
                        (Turn Red)   -> (Turn Black)
                        (Turn Black) -> (Turn Red)
                        GameOver     -> GameOver

{-
    Returns the middle coordinate between two given coordinates.
-}
middle :: Coord -> Coord -> Coord
middle (x,y) (x',y') = ((x + x') // 2, (y + y') // 2)

(//) = quot