
group :: (Num a, Ord a) => [a] -> [[a]]
group = foldr f []
  where f x []        = [[x]]
        f x (ys@(y:_):yss)
          | abs(x - y) <= 1     = (x:ys):yss
          | otherwise = [x]:ys:yss

main :: IO()
-- main = print(group [2,1,3,4,5,5,4,7,4,3,3])
main = print(map (3:) ([[3,2]] ++ []))