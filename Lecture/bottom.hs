{-  
    Scott's bottom: this is an "element" of each type which never terminates.  It is, 
    thus, an "undefined" or "divergent" element.  Any program which causes bottom to be 
    evaluated will also not terminate ... SO one can test whether a program "touches" an
    argument with this function!!

    It is named after the great computer scientist, philosopher, and mathematician Dana Scott.
-}

bottom::a
bottom = bottom

--  data Bool = True | False
--     deriving (Eq,Show)

-- different versions of (&&) !
-- First this is how it is programmed in the prelude
--     ... we use the "section" of the infix operator
--     so we can program it just like any other function
--
-- (&&):: Bool -> Bool -> Bool
-- (&&) True True = True
-- (&&) _     _   = False

-- different versions of and which have different behaviors

myand1:: Bool -> Bool -> Bool
myand1 True True = True
myand1 True False = False
myand1 False True = False
myand1 False False = False

myand2:: Bool -> Bool -> Bool
myand2 _ False = False
myand2 False _ = False
myand2 True True = True

-- Try False && bottom ... why do the other versions of  and
-- behave slightly differently???

{- In the prelude there is the datatype 
      data Maybe a = Nothing | Just a
                   deriving (Eq,Ord,Read,Show)
   here we create our own "Maybe" datatype called 
   "success or fail" written "SF" and play with it.  -}

data SF a = SS a |  FF
     deriving (Eq,Show)

-- This allows us a cleaner way to catch the error when one enters a negative
-- in Fibonacci:

fibstep (n,m) = (m,n+m)

myiterate:: Integer -> (a -> a) -> a -> a
myiterate 0 f = id
myiterate n f = myiterate (n-1) f . f

fibsf:: Integer -> (SF Integer)
fibsf n | n < 0 = FF
        | n < 2 = SS 1
        | otherwise = SS ((fst.myiterate n fibstep) (0,1))

-- mapping the success or fail data to booleans
sf_2_bool:: SF a -> Bool
sf_2_bool (SS x) = True
sf_2_bool FF = False

-- try sf_2_bool (SS bottom) !!!

--  division with exceptions (note the explicit coercion
--  of integers into floating point)

sfdivide:: Int -> Int -> (SF Float)
sfdivide n m | m == 0 = FF
             | otherwise = SS ((fromIntegral n)/(fromIntegral m))

--  Composing Maybe/Success or Fail functions ---

(>/>):: (a -> (SF b)) -> (b -> (SF c)) -> (a -> (SF c))
(>/>) f g = \x -> case f x of
                        FF -> FF
                        SS y -> g y

-- map for Maybe/Success or Fail
sfmap:: (a -> b) -> ((SF a) -> (SF b))
sfmap f FF = FF
sfmap f (SS x) = SS (f x)
             

--
-- Initial foray into lists
--

myhead::[a] -> (SF a)
myhead [] = FF
myhead (a:_) = SS a

mytail::[a] -> (SF [a])
mytail [] = FF
mytail (x:xs) = SS xs

myappend:: [a] -> [a] ->[a]
myappend [] ys = ys
myappend (x:xs) ys =x:(myappend xs ys)

flatten:: [[a]] -> [a]
flatten [] = []
flatten (xs:xss) = myappend xs (flatten xss)

mymap::(a  -> b) -> ([a] -> [b])
mymap f [] = []
mymap f (x:xs) = (f x):(mymap f xs)

mylength [] = 0
mylength (x:xs) = 1 + (mylength xs)

-- Try [bottom,bottom,bottom]
-- Try length [bottom,bottom,bottom] ...
-- Try myappend [bottom,bottom] [bottom,bottom,bottom]
-- Try length (myappend [bottom,bottom] [bottom,bottom,bottom])