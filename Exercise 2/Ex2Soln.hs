{-- :                                                                                  EXERCISE 2: SOLUTION TEMPLATE

Instructions:
-----------------
1. Write your solutions to the exercise in this file. For each problem, a template for the solution is provided. Modify the templates
    to provide your solution. You can add extra  functions to implement your solution. 

2. LEAVE the templates of the problems which you have not solved, unmodified. DO NOT ERASE the templates of the problems you 
    have not solved. 

3. GradeScope will test all the questions from the template each time. Top scoring 6 answers will be considered for your grade. No marks will be 
    awarded for unsolved problems. 

We will be updating the autograder with new test cases and the assignments will be graded by the updated autograded after the submission 
due date. But, do make sure that your solutions passes the test cases of the autograder available at the time of submission. 

Good luck!
:-}

module Submission where

import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, drop, const, reverse, map, product)



main :: IO ()
-- main = print (bsort (<) [-2,6,-4,8,0,3])
main = print (multMat [[1.0,2.0,3.0], [4.0,5.0,6.0]] [[7,8], [9,10], [1,12]])
-- main = print(bisection f1 (2.215, 2.217))
-- main = print(bisection f2 (-2, 2))
-- main = print(bisection cos (3*pi/2,pi/2))
-- main = print(bisection cos (0,pi))
-- main = print(bisection cos (pi,pi))
-- main = print(bisection cos (0,0))
-- main = print(fact)

f1 x = (x - 3.3537435) ^ 3 + 1.4734366
f2 x = x ^ 2

-- reverse
reverse :: [a] -> [a]
reverse [] = []
reverse xs = last xs : reverse(init xs)

-- take
take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take size (x:xs) = x:(take (size - 1) xs)

-- drop
drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 list = list
drop size (x:xs) = drop (size - 1) xs

--product
product :: (Num a) => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)

-- (++)
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x:(xs ++ ys)

-- (!=)
(!=) :: (Eq a) => a -> a -> Bool
(!=) = (/=)

-- (//)
(//) = quot

-- (%)
(%) = rem

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x):(map f xs)

--filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x       = x:(filter f xs)
    | otherwise = filter f xs

-- length
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length(xs)

-- 1 
-- Check if for all T/F, the result is T
twoTautology :: ((Bool,Bool) -> Bool) -> Bool
twoTautology f
    | f(True,True)   != True    = False
    | f(True,False)  != True    = False
    | f(False,True)  != True    = False
    | f(False,False) != True    = False
    | otherwise                 = True

-- Check if the T/F values for f1 and f2 are the same
twoEquiv :: ((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
twoEquiv f1 f2
    | f1(True,True)   != f2(True,True)      = False
    | f1(True,False)  != f2(True,False)     = False
    | f1(False,True)  != f2(False,True)     = False
    | f1(False,False) != f2(False,False)    = False
    | otherwise                             = True

-- 2
-- isPrime:     list all the divisors of a given number, excluding 1 and itself, 
--              if the list is empty, means it has no divisor other than 1 and itself,
--
--              which means it's a prime number
-- fermat:      apply the given number with the formula
--
-- badFermat':  if current fermat number is a prime number, then keep fermat,
--              otherwise, it's a bad fermat number, return
badFermat :: Integer
badFermat = let 
                isPrime n    = [x | x <- [2..n-1], mod n x == 0] == []
                fermat  n    = 2 ^ (2 ^ n) + 1
                badFermat' n = if isPrime (fermat n) then badFermat' (n + 1) else n
            in badFermat' 1


-- 3 
data SF a = FF | SS a  
            deriving (Eq, Show)

-- collatz:         apply perspective formula for even and odd numbers
--
-- collatzIndex':   if x is less than 1, then it's invalid input, FF
--                  if x is equal to 1, then return itself
--                  if the next collatz number is equal to 1, then append 1 and reverse the result lise
--                  otherwise, keep getting collatz numbers
collatzIndex ::  Int -> SF [Int]
collatzIndex n = let
                    collatz x
                        | x % 2 == 0        = x // 2
                        | otherwise         = 3 * x + 1
                    collatzIndex' x list
                        | x < 1             = FF
                        | x == 1            = SS [1]
                        | collatz x == 1    = SS (reverse (1:list))
                        | otherwise         = collatzIndex' (collatz x) ((collatz x):list)
                in collatzIndex' n [n]

-- 4
e :: Double
e = exp (-34)

-- If the signs of fa and fb are not different, then input is invalid, Nothing
-- If the absolute value of fm is within the error range, return m
-- Or if the difference of a and b are within the error range, return  
-- If fa and fm have different sign, narrow the range down to a and m
-- If fm and fb have different sign, narrow the range down to m and b
-- Otherwise, fails and return Nothing
bisection::(Double->Double)->(Double,Double)->Maybe Double
bisection f (a, b)
    | not (diffSign fa fb)          = Nothing
    | (abs fm) < e                  = Just m
    | d < e                         = Just m
    | diffSign fa fm                = bisection f (a, m)
    | diffSign fb fm                = bisection f (m, b)
    | otherwise                     = Nothing
    where
        m = (a + b) / 2
        d = (b - a) / 2
        fa = f a
        fb = f b
        fm = f m
        diffSign a b
            | a < 0 && b > 0 = True
            | a > 0 && b < 0 = True
            | otherwise      = False

-- 5
-- bsort Bubble Sort
-- swap: for a list, if the first element is greater than the second, then swap their positions
-- sorted: for a list, return true if it's sorted, otherwise return false
-- bsort': if the list is sorted, then return list, otherwise keep sorting
bsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
bsort _ []  = []
bsort _ [x] = [x]
bsort f list = let
                    swap [] = []
                    swap [x] = [x]
                    swap (x:y:xs) 
                        | f x y     = x:(swap (y:xs))
                        | otherwise = y:(swap (x:xs))
                    sorted [x,y] = (f x y)
                    sorted (x:y:xs) = (f x y) && (sorted (y:xs))
                    bsort' list
                        | sorted list = list
                        | otherwise   = bsort' (swap list)
                in bsort' list

-- qsort Quick Sort
-- Let the first element of the list be the pivot
-- left: the left side of list, filter all the element less than the pivot
--       use qsort recursively until the sublist only contains a pivot
--       then the list is sorted
-- Similar with right side
-- Concatenate the left, pivot itself and the right side of list
qsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
qsort _ []  = []
qsort _ [x] = [x]
qsort f (x:xs) = let
                    left  = qsort f [y | y <- xs, not(f x y)]
                    right = qsort f [z | z <- xs, f x z]
                in left ++ [x] ++ right

-- msort Merge Sort
-- half: the length of half of a given list
-- left: split the given list by half, recursively msort left half
--       until the sublist only contains a single element
-- Similar with right
-- merge: merge two lists together, smaller one on the left
msort:: Integral a =>  (a -> a -> Bool) -> [a] -> [a]
msort _ []  = []
msort _ [x] = [x]
msort f list = let
                    half  = length list // 2
                    left  = msort f (take half list)
                    right = msort f (drop half list)
                    merge [] y = y
                    merge x [] = x
                    merge (x:xs) (y:ys)
                        | f x y     = x:(merge xs (y:ys))
                        | otherwise = y:(merge (x:xs) ys)
                in merge left right

-- 6
type Matrix a = [[a]] 
type DoubleMatrix = Matrix Double

-- Validity of a matrix
-- If the length of one line is different from previous line, this matrix is invalid
validate :: Matrix a -> Bool
validate x =    let
                    validate' size [x] = size == length x
                    validate' size (x:xs) = (size == length x) && validate' (length x) xs
                in validate' (length (head x)) x

-- Size of a matrix
-- Returns (row, column)
size :: Matrix a -> (Int,Int)
size (x:xs) = (length (x:xs), length x)

-- Transpose of Matrix
-- transpose': get head of each column and put them into a list as row, or vice versa
-- transpose'': check if the matrix is valid, then transpose'
transpose:: Matrix a -> (Maybe (Matrix a))
transpose [] = Just []
transpose x =   let
                    transpose' ([]:_) = []
                    transpose' matrix = (map head matrix):(transpose' (map tail matrix))
                    transpose'' matrix
                        | validate matrix = Just(transpose' matrix)
                        | otherwise       = Nothing
                in transpose'' x

-- Addition of Matrix
-- addMat': check if the two matrix are valid, then check if they have equal size
--          then add matrix
-- addRow: iterare each row of the matrix
-- addColumn: ie=terate each column of the current row, add together
addMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
addMat [] [] = Just []
addMat x [] = Nothing
addMat [] y = Nothing
addMat x y =    let
                    addMat' x y
                        | validate x != True    = Nothing
                        | validate y != True    = Nothing
                        | (size x) != (size y)  = Nothing
                        | otherwise             = Just(addRow x y [])
                    addRow [] [] res = res
                    addRow (x:xs) (y:ys) res = (addColumn x y []):(addRow xs ys res)
                    addColumn [] [] res = res
                    addColumn (x:xs) (y:ys) res = (x + y):(addColumn xs ys res)
                in addMat' x y

-- Multiplication of Matrix
-- multMat': Check if two matrix are valid, 
--           and if column amount of first is equal to row amount of second matrix
--           then multiply
-- multRow: iterate each row of the first matrix
-- multColumn: iterate each column of the second matrix
-- multList: multiply the two together
multMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
multMat [] [] = Just []
multMat x y =   let
                    multMat' x y
                        | validate x != True         = Nothing
                        | validate y != True         = Nothing
                        | snd(size x) != fst(size y) = Nothing
                        | otherwise                  = Just(multRow x y)
                    multRow [] _ = []
                    multRow (x:xs) y = (multColumn x y):(multRow xs y)
                    multColumn _ [] = []
                    multColumn x y = (multList x (map head y)):(multColumn x (map tail y))
                    multList [] [] = 0
                    multList (x:xs) (y:ys) = (x * y) + (multList xs ys)
                in multMat' x y

-- 7
nreverse :: (Ord a, Integral a) => [a] -> [a]
nreverse _ = []

freverse :: (Ord a, Integral a) => [a] -> [a]
freverse _ = []


hreverse :: (Ord a, Integral a) => [a] -> [a]
hreverse  _ =  []

-- 8
data STree a = Node (STree a) a (STree a) | Leaf  deriving (Show)

isAVL:: (Ord a, Integral a) => STree a -> Bool
isAVL _ = True

-- 9
-- Calculate the product from 1 to 1891, is the result of (1891!)
fact :: Integer
fact = product [1..1891]

-- 10
data Rose a = RS a [Rose a]   deriving (Show)

rTree = RS 1 [RS 0 [], RS 2 [RS 3 [], RS 4 [RS 5 []], RS 9 []], RS 6 [RS 7 [], RS 8 []]]

widthRose :: Integral a =>  Rose a -> Int
widthRose _ = 0