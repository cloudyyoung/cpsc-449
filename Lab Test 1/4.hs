
import Prelude hiding (replicate, zip, maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, drop, const, reverse, map, product)


main :: IO ()
main = do
    -- print(zip [1,2,3] ['a', 'b', 'c'])
    -- print(replicate 4 99)
    print(grow "now!")


grow :: String -> String
grow string = [y | (char, times) <- (zip string [1..]), y <- (replicate times char)]


zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (a:ax) (b:bx) = (a, b):(zip ax bx)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate t a = a:(replicate (t-1) a)