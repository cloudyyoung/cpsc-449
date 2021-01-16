{-   The Finonacci numbers  
     Note: this sequence was known by the Indian mathematician Pingala in 350bc! 
           Fibonacci simply rediscovered them ... 
     [from wikipedia:]
     Fibonacci considers the growth of a hypothetical, idealized (biologically unrealistic) rabbit population, 
     assuming that: a newly born pair of rabbits, one male, one female, are put in a field; rabbits are able 
     to mate at the age of one month so that at the end of its second month a female can produce another pair 
     of rabbits; rabbits never die and a mating pair always produces one new pair (one male, one female) 
     every month from the second month on. Fibonacci posed the puzzle: how many pairs will there be in 
     one year?

     Answer: 144 -}

--  (0) Basic Fibonaccci:

fib:: Integer -> Integer  -- (don't forget to specify the type!!!)
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

-- What happens with (a) fib 50 (b) fib (-1) (c) fib 2.5????

--  (1) Basic Fibonacci with guards and simple error handling

fib1:: Integer -> Integer
fib1 n
   | n < 0     = error "Positive numbers only please ..."
   | n == 0    = 0
   | n == 1    = 1
   | otherwise = (fib1 (n-1)) + (fib1 (n-2))

{-  Here is a trace of fib1 3
    [fib1 3]
    3 < 0
       ...false
    3 == 0
       ...false
    3 == 1 
       ...false
    otherwise
    =[fib1(3-1) + fib1(3-2)]
    =[fib1(3-1)] + fib1(3-2)
     [fib1 3-1]
      [3-1] < 0   -- test guard
      [3-1]   -- evaluate for guard
      2 
      2 < 0   -- evaluate guard
       ...false
     2 == 0
       ...false
     2 == 1
       ...false
     otherwise
     = [fib1(2-1) + fib1(2-2)]
       [fib1(2-1)] + fib1(2-2)
       fib1 [2-1]
        [2-1] < 0  -- test guard
        [2-1] -- evaluate for guard
        1
        1 < 0 -- evaluate guard 
           ...false
        1 == 0
           ...false
        1 == 1 
           ...true
        = 1 -- evaluated fib1 (2-1)
       1 + [fib1(2-2)]
       [fib1 (2-2)]
       [2-2] < 0
       [2-2]
       0
       0 < 0
         ...false
       0 == 0
         ...true
       = 0
       [1 + 0]
       1
       [1 + 1]
       2
     2   -- return for fib 3
 -}

-- (3) Fast Fibonacci:
-- This is a more efficient version which does not repeat calculations
-- This uses a where clause

fibstep (n,m) = (m,n+m)
dec n = n-1

ffib:: Integer -> Integer
ffib n
  | n < 0 = error "Fibonacci only takes positive integers"
  | otherwise = pfib n (0,1)
 where
  pfib 0 (n,m) = n
  pfib r (n,m) = pfib (r-1) (fibstep (n,m))

-- (4) Or you can write this with a let clause
ffib':: Integer -> Integer
ffib' n
  | n < 0 = error "Fibonacci only takes positive integers"
  | otherwise =
     let
        pfib 0 (n,m) = n
        pfib r (n,m) = pfib (r-1) (fibstep (n,m))
     in pfib n (0,1)


--(5) Or you can use an anonymous function
fibP 0 = (0,1)
fibP n = (\(n,m) -> (m,n+m)) (fibP (n-1))

-- (6) )r you could use the composition operator `.'
fibP' 0 = (0,1)
fibP' n = (fibstep . fibP'. dec) n


-- (7) The more efficient version which does not repeat calculations
-- Can be written with composition and a where clause in this form
ffibc:: Integer -> Integer
ffibc n
  | n < 0 = error "Fibonacci only takes positive integers"
  | otherwise = pfib n (0,1)
 where
  pfib 0 = fst
  pfib r = pfib (r-1) . fibstep


-- (8) The average of three numbers (for an example of currying)
average::Float -> Float -> Float -> Float
average a b c = (a+b+c)/3

-- (9) doubling an integer
double:: Integer -> Integer
double n = n + n

-- (10) iterating a function 
myiterate:: Integer -> (a -> a) ->  a -> a
myiterate 0 f = id
myiterate n f = myiterate (n-1) f . f

-- (11) Exponential
exponential:: Integer -> Integer
exponential n = myiterate n double 1

-- (11) Fibonacci again using iteration!
fibonacci:: Integer -> Integer
fibonacci n = (fst. myiterate n fibstep) (0,1)