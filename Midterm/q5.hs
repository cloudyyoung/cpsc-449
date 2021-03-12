
data STree a = Sn (STree a) a (STree a)
                | Tp
            deriving (Show, Eq)
            
foldST :: (b -> a -> b -> b) -> b -> (STree a) -> b
foldST g t (Tp) = t
foldSt g t (Sn Tp 0 Tp) = g t 0 t
foldSt g t (Sn s1 a s2) = g (foldST g t s1) a (foldST g t s2)

main :: IO()
-- main = print(1)
-- main = print(paths Tp)
main = print(paths (Sn Tp 0 Tp))
-- main = print(paths (Sn (Sn Tp 1 Tp) 0 (Sn Tp 2 Tp)))

paths :: STree Int -> [[Int]]
paths (Tp) = []
-- paths (Sn Tp a Tp) = [[a]]
paths (Sn s1 a s2) = foldST g [[]] (Sn s1 a s2)
    where 
        g [[]] a [[]] = [[a]]
        g b a c = map (a:) (b ++ c)