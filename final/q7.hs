import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, drop, const, reverse, map, product)


foldr f g [] = g
foldr f g (x:xs) = f x (foldr f g xs)

map f [] = []
map f (x:xs) = (f x):(map f xs)

length [] = 0
length (x:xs) = 1 + (length xs)


prefixes []     = []
prefixes (x:xs) = [x] : map (x:) (prefixes xs)


misery xs = []:[s|t<-suffer xs,s<-prefixes t]
        
suffer ys 
  = (\(x,xs) -> x:xs) 
    (foldr (\x (l,ls)->(x:l,l:ls))
           ([],[]) 
           ys)
