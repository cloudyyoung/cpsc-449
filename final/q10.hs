

data Tree a = Tip a
            | Node (Tree a) (Tree a)

mapTree1 f (Tip a) = Tip (f a)
mapTree1 f (Node t1 t2) 
       = Node (mapTree1 f t1) 
              (mapTree1 f t2)

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree tip node (Tip a) = tip a
foldTree tip node (Node t1 t2) = node (foldTree tip node t1) (foldTree tip node t2)

mapTree2 f = foldTree (\a->Tip(f a)) Node 


