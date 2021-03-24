

import Prelude hiding (replicate, zip, maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, drop, const, reverse, map, product)


main :: IO ()
main = do
    print(merge [1,9,10] [2,6,8])

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y    = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)
