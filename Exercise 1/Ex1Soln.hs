{-- :                                                                                  EXERCISE 1: SOLUTION TEMPLATE

Instructions:
-----------------
1. Write your solutions to the exercise in this file. For each problem, a template for the solution is provided. Modify the templates to provide your solution. You can add extra functions to implement your solution. 

Please do not forget to add appropriate comments to you code!

2. LEAVE the templates of the problems which you have not solved, unmodified. DO NOT ERASE the templates of the problems you 
    have not solved. 

3. UPDATE THE "answered" LIST BELOW with the list of problems you have solved. i.e, if you implemented twoTautology function for q1, add 
    "twoTautology" to the "answered" list. The GradeScope autograder will test all the solutions whch is a member of this list and update your 
    grade. If you answer more than required number of questions, top scoring 6 answers will be considered for your grade. No marks will be 
    awarded for unsolved problems. 

4.  Do not change the name of this file.

We will be updating the autograder with new test cases and the assignments will be graded by the updated autograded after the submission 
due date. But, do make sure that your solutions passes the test cases of the autograder available at the time of submission. 

Good luck!
:-}

module Ex1Soln where   

data SF a = FF | SS a deriving Show

-- IMPORTANT: List all the functions that you have implemented in the assignment. Your submission will be graded only based on the items in this list.
-- Uncomment the functions you have implemented
answered = [
    "avgThree",
    "maxThree",
    "invFac",
    "myGcd",
    "binom",
    "grow",
    "instrictorder",
    "cheapItems",
    -- "sortByCost",
    "divisors"
    -- "substring",
    -- "sublists"
    ]

-- main :: IO ()
-- main = print (divisors 20)

-- 1 
-- Convert the sum of the three and divide by 3.0
avgThree:: Int -> Int -> Int -> Float
avgThree x y z = realToFrac(x + y + z) / 3.0

--2
-- Return: (the maximum number, the length of a list which all the elements are equal to the max number)
-- The length of the list is the occurent time of the max number
maxThree:: Int -> Int -> Int -> (Int,Int)
maxThree x y z = (maximum [x, y, z], length [a | a <- [x, y, z], a == maximum [x, y, z]])

-- 3 
-- If the given number is negative or zero, then FF
-- Start from 1 for factorial number, keep multiplying the next number to get the factorial value, once the value is reached, return the factorial number
invFac:: Integer -> SF Integer
invFac x
    | x <= 0    = FF
    | otherwise = let invFac' targetFac facNum currentFac
                        | (currentFac * facNum) < targetFac = invFac' targetFac (facNum + 1) (currentFac * facNum) -- Getting next fac
                        | (currentFac * facNum) >= targetFac = SS facNum -- Return
        in invFac' x 1 1

-- 4
-- If either x or y is 0, then return the other one
-- According to the algorithm, we keep dividing multiplier and remaider until it reaches 0
-- x represents dividend and y represents divisor
-- Use `abs` because GCD's are always positive
myGcd :: Int -> Int -> Int
myGcd x 0 = abs x
myGcd 0 y = abs y
myGcd x y 
    | abs x < abs y = myGcd y x         -- dividend has to always be the greater one (greater for positive and smaller for negative)
    | x == y        = abs y             -- if dividend and divisor are the same, then GCD is itself
    | mod x y /= 0  = myGcd y (mod x y) -- if remainder is not 0, then keep dividing
    | otherwise     = abs y             -- if remainder is 0, then return the remainder as GCD

-- 5
-- Use Texas ranges to rewrite the formula, basically
binom:: Integer -> Integer -> Integer
binom n k
    | n < 1             = error "n out of defined range"
    | k < 0 || k > n    = error "k out of defined range"
    | otherwise         = quot (product [(n - 0), (n - 1)..(n - k + 1)]) (product [1..k])

-- 6
-- For each character in the string, zip them with a number starts from 1 
-- representing how many times the character will be repeated,
-- e.g, (first, 1), (second, 2), (third, 3), etc,
-- and each zipped combination is represented as (char, times).
-- Then for each combination, repeat `times` of `char` and repesented as y,
-- e.g, y will be: first, secondsecond, thirdthirdthird, etc
-- The returned list will be populated eventually as: [first, secondsecond, thirdthirdthird...]
grow :: String -> String
grow string = [y | (char, times) <- zip string [1..], y <- take times (repeat char)]

-- 7
-- If list is empty or only contain one element, then it is always True
-- Comparing the first two elements, if the left one is smaller than the right one,
-- and also do the same for the rest of the list.
-- Eventually it forms a chain, expanded as: (a < b) && (b < c) && (c < d) && [d]
-- If the fact is that a < b < c < d, then: True && True && True && True, returns True
-- If the fact is that b < c < a < d, then: False && True && True && True, returns False
instrictorder:: [Int]-> Bool
instrictorder [] = True
instrictorder [x] = True
instrictorder (x:y:ys) = x < y && instrictorder (y:ys)

-- 8
-- For each element in the given list `items`, extract `name` and `price`,
-- if `price` is less than the given threshold price, then populate this item with its `name` to the result list
cheapItems:: [(String,Int)] -> Int -> [String]
cheapItems items threshold = [name | (name, price) <- items, price < threshold]

-- 9
sortByCost :: [(String,Int)] -> [(String,Int)]
sortByCost _ = []

-- 10
-- From a list contains 1 to the given number, take each number from the list,
-- calculate the remainder of given number / list number, if the remainder is 0,
-- then the list number is at least a divisor of the given number.
-- Meanwhile, this divisor also has to be a prime number, so to check that,
-- from a list contains 1 to the divisor, take each number from the list, 
-- calculate the remainder of divisor / list number, if the remainder is 0, 
-- then the list number is a divisor of the divisor.
-- After we have the list of divisor of divisor, if this list only contains 1 and the divisor itself,
-- then this divisor is a prime number.
divisors:: Integer -> [Integer]
divisors x
    | x <= 1    = []
    | otherwise = [y | y <- [1..x], rem x y == 0 && [1,y] == [z | z <- [1..y], rem y z == 0]]

-- 11
substring :: String -> String -> Bool
substring _ _ = False

-- 12
sublists:: [a] -> [[a]]
sublists _ = []