
pullback :: (Eq c) => (a->c)->(b->c)->[a]->[b]->[(a,b)]
pullback f g as bs = [ (x,y) | x <- as, y <- bs, f x == g y]

-- q4-2
pullback2 f g as bs = concat(map(\x -> concat(map(\y -> if f x == g y then [(x,y)] else [])bs))as)

main :: IO()
-- main = print(pullback (\a -> a) (\b -> b) [1] [1,2])
-- main = print(pullback (\a -> a) (\b -> b) [1,3] [1,2])
main = print(pullback2 (\a -> a) (\b -> b) [1,3] [1,2])