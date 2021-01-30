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

import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, const)



-- main :: IO ()
-- main = print (twoTautology fun)

fun :: (Bool, Bool) -> Bool
fun (False, True) = False
fun (x,y)         = True


data SF a = FF | SS a  
            deriving (Eq, Show)
-- 1 
twoTautology :: ((Bool,Bool) -> Bool) -> Bool
-- Provide your answer below
twoTautology f
    | f(True,True)   /= True    = False
    | f(True,False)  /= True    = False
    | f(False,True)  /= True    = False
    | f(False,False) /= True    = False
    | otherwise                 = True


twoEquiv :: ((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
-- Provide your answer below
twoEquiv f1 f2
    | f1(True,True)   /= f2(True,True)      = False
    | f1(True,False)  /= f2(True,False)     = False
    | f1(False,True)  /= f2(False,True)     = False
    | f1(False,False) /= f2(False,False)    = False
    | otherwise                             = True

--2
badFermat :: Integer
badFermat = 0


-- 3 
collatzIndex ::  Int -> SF [Int]
-- Provide your answer below
collatzIndex _ = FF


-- 4
e :: Double
e = exp (-150)

bisection::(Double->Double)->(Double,Double)->Maybe Double
-- Provide your answer below
bisection _ _ =  Just 0.0

-- 5
bsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
bsort _ _ = []        


qsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
qsort _ _  = []

msort:: Integral a =>  (a -> a -> Bool) -> [a] -> [a]
msort _ _  = []

-- 6
type Matrix a = [[a]] 
type DoubleMatrix = Matrix Double

transpose:: Matrix a -> (Maybe (Matrix a))
transpose _ = Nothing

addMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
addMat _ _  = Nothing

multMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
multMat _ _ = Nothing

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
fact :: Integer
fact  = 0

-- 10
data Rose a = RS a [Rose a]   deriving (Show)

rTree = RS 1 [RS 0 [], RS 2 [RS 3 [], RS 4 [RS 5 []], RS 9 []], RS 6 [RS 7 [], RS 8 []]]

widthRose :: Integral a =>  Rose a -> Int
widthRose _ = 0