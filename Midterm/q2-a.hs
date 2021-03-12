

import Prelude

myfoldr::b->(a->b->b)->[a]->b
myfoldr b _ _ = b

f :: [a] -> [a] -> [a]
f = myfoldr id g where
    g a h x = h (a:x)

main :: IO ()
-- main = print(f [10] [1])
main = print(f "abc" "def")