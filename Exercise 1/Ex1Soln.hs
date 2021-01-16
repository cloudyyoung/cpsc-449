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

data SF a = FF | SS a  
            deriving Show

-- IMPORTANT: List all the functions that you have implemented in the assignment. Your submission will be graded only based on the items in this list.
-- Uncomment the functions you have implemented
answered = [
  "avgThree"
--    "maxThree",
--    "invFac",
--   "myGcd",
--   "binom",
--    "grow",
--    "instrictorder",
--  "cheapItems"
--    "sortByCost",
--   "divisors",
--    "substring",
--   "sublists"
    ]

main :: IO ()
main = print (maxThree 9 9 7)

-- 1 
avgThree:: Int -> Int -> Int -> Float
avgThree x y z = realToFrac(x + y + z) / 3.0

--2

maxThree:: Int -> Int -> Int -> (Int,Int)
maxThree x y z = (maximum [x, y, z], 1)

-- 3 
invFac:: Integer -> SF Integer
invFac _ = FF

-- 4
myGcd :: Int -> Int -> Int
myGcd _ _ = 0

-- 5
binom:: Integer -> Integer -> Integer
binom _ _ = 0  

-- 6
grow :: String -> String
grow _ = ""

-- 7
instrictorder:: [Int]-> Bool
instrictorder _ = False

-- 8
cheapItems:: [(String,Int)] -> Int -> [String]
cheapItems _ _ = []

-- 9
sortByCost :: [(String,Int)] -> [(String,Int)]
sortByCost _ = []

-- 10
divisors:: Integer -> [Integer]
divisors _ = []

-- 11
substring :: String -> String -> Bool
substring _ _ = False

-- 12
sublists:: [a] -> [[a]]
sublists _ = []