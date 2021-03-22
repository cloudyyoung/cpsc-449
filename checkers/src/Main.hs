module Main where

import Moves
import ApplyMove
import Heuristic
import ABsearch
import Checkers.Types
import Checkers.FrontEnd.Types
--import qualified Checkers.FrontEnd.Basic as B
import qualified Checkers.FrontEnd.Terminal as T



main :: IO ()
main = T.frontend $ GameConfig { engine = apply_move
                               , blackMove = Human
                               , redMove = Human
                               , state = initialGameState }


