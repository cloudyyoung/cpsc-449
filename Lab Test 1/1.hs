
import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, drop, const, reverse, map, product)


main :: IO ()
main = print(myOR [False,True,False])
-- main = print(foldr (+) [1,2,3])

foldr:: (a -> b -> b) -> b -> [a] -> b
foldr b f [] = b
foldr b f (x:xs) = f x (foldr b f xs)

myOR:: [Bool] -> Bool
myOR = foldr myor False


myor:: Bool -> Bool -> Bool
myor False False = False
myor _ _ = True