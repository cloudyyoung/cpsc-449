
import Prelude hiding (foldr)

foldr f g [] = g
foldr f g (x:xs) = f x (foldr f g xs)

group :: (Num a, Ord a) => (a->a->Bool)->[a]->[[a]]
group nbr xs = foldr f [] xs
  where f x []          = [[x]]
        f x ((y:z):ys)
          | nbr x y     = (x:(y:z)):ys
          | otherwise   = [x]:(y:z):ys
          

f1 :: (Num a, Ord a) => a -> a -> Bool
f1 a b = abs(a - b) <= 1

main :: IO()
main = print(group f1 [2,1,3,4,5,5,4,7,4,3,3])