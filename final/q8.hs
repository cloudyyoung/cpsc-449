data STree a = Sip
             | Sode (STree a) a (STree a)
            
data SF a = SS a
          | FF
          deriving Show

foldSTree :: a -> (a -> b -> a -> a) -> STree b -> a
foldSTree sip sode Sip = sip
foldSTree sip sode (Sode t1 a t2) = sode (foldSTree sip sode t1) a (foldSTree sip sode t2)


inSTree :: Ord a => a -> (STree a) -> Bool
inSTree a (Sip) = False
inSTree a (Sode c b d)
    | a == b    = True
    | otherwise = inSTree a c || inSTree a d


strange :: STree Int -> (Bool, SF(Int, Int))
strange (Sip) = (True, FF)
strange (Sode a b c)
    | sorted(treeList)    = (True, SS(min, max))
    | otherwise           = (False, SS(min, max))
    where
        treeList = listSTree (Sode a b c)
        min = minimum(treeList)
        max = maximum(treeList)

listSTree (Sip) = []
listSTree (Sode a b c) = (listSTree a) ++ [b] ++ (listSTree c)

sorted xs = foldr (&&) True (map (\(a,b) -> a <= b) (zip xs (tail xs)))


