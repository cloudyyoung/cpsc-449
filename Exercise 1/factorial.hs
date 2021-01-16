
factorial :: Int -> Int
factorial x
    | x <= 0    = 1
    | otherwise = product [1..x]