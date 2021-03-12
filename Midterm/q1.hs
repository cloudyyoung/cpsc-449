
import Prelude hiding (foldr)

foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

ord_inlist x = foldr (\a b -> a == x || (x >= a && b)) False

main :: IO ()
main = print(ord_inlist 2 [1,4,8,11,22])