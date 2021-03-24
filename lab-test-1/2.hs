
main :: IO ()
-- main = print(f (0, (1, 2)))
main = do
    print "abcd"
    print [('a', 'b'), ('c', 'd')]
    -- print ((('a':['b']):('c':['d'])):[])
    print ('a':('b':'c':'d':[]))
    print ["ab", "cd"]


f:: (Int, (Int, Int)) -> Int
f (x, (y, z)) = 0