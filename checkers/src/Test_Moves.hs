module Test_Moves where

import Moves
import Checkers.Types



g0 = initialGameState

-- simple_moves testcases
g1 = GameState { blackPieces = [(1,0)]               
               , blackKings = []
               , redPieces = [(3,4)]
               , redKings = []
               , status = Turn Red
               , message = ""
               , history = [] }

g2 = GameState { blackPieces = [(1,0)]               
               , blackKings = []
               , redPieces = []
               , redKings = [(3,3)]
               , status = Turn Red
               , message = ""
               , history = [] }

-- jump_moves testcases
g3 = GameState { blackPieces = [(2,2)]               
               , blackKings = []
               , redPieces = [(3,3)]
               , redKings = []
               , status = Turn Red
               , message = ""
               , history = [] }
               
g4 = GameState { blackPieces = [(6,1),(4,1),(2,1)]               
               , blackKings = []
               , redPieces = []
               , redKings = [(7,2)]
               , status = Turn Red
               , message = ""
               , history = [] }               


test [] = putStrLn ""
test ((name, game):xs) = do 
    putStrLn ""
    putStrLn (name ++ " testcase:")
    putStrLn (show $ moves game)
    putStrLn ""
    test xs


main = test [ ("initial game", g0),
              ("simple_moves Pawn", g1),
              ("simple_moves King", g2),
              ("jump_moves Pawn", g3),
              ("jump_moves King", g4) ]

