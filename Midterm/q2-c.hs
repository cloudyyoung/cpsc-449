

import Prelude

mymap::(a->b)->[a]->[b]
mymap f a = a

-- f::[Int]->[Int]
-- f :: [a]->[a]
f::(a->b)->[a]->[b]
f = mymap mymap

g a = a

main :: IO ()
main = print(f g [1,2])