
import Prelude

main :: IO ()
main = do
    print(mystery [1,2,3])
    -- print(myhead [1,9,10,2,6,8])
    -- print(mytail [1,9,10,2,6,8])
    -- print(myzip [1,9,10] [2,6,8])
    print(mystery "abccdeffghii")
    print(mystery "abcdefg")
    -- print(myzip [1,2,3,4,5,6] (mytail [1,2,3,4,5,6]))

data SF a = SS a | FF deriving (Show, Eq) 

myhead :: [a] -> SF a
myhead [] = FF
myhead (a:as) = SS a

mytail :: [a] -> [a]
mytail [] = []
mytail (_:xs) = xs 

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs) = (a,b):(myzip as bs) 

mystery :: (Eq a) => [a] -> SF a
-- mystery xs = myhead [x | (x, y) <- myzip xs (mytail xs), x == y] 
mystery xs = foldr (\x y -> if (fst x == snd x) then SS (fst x) else y) FF (myzip xs (mytail xs))
