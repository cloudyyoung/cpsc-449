

import Prelude

mymap::(a->b)->[a]->[b]
mymap f a = a

f::[Int]->[Int]
f = mymap (mymap (mymap h))

main :: IO ()
main = print(f [10])