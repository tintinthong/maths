module Sequences where

import BaseFunctions as Base(
	choose, 
	permute
	)

import Data.List
{-Integer Sequences (OEIS)-}

-- Fibonacci A000045
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Triangle 
triangle  :: Int -> Int 
triangle  n = div (n*(n+1)) 2

-- Pascal A007318	
pascal :: Int -> Int -> Int
pascal row column = choose row column

-- Catalan A000108	
catalan :: Int -> Int
catalan 0 = 1
catalan n=sum [catalan i * catalan (n-1-i) | i<-[0..n-1]]

-- Divisors
divs :: Int -> [Int]
divs 1= [1]
divs n =  1 : filter ((==0) . mod n) [2 .. n `div` 2]++[n]
--divisors n = [i | i <- [1..(n-1)], n `mod` i == 0]

-- Sum of Positive Divisors A000203
sumDivs :: Int -> Int
sumDivs n = sum $ divs n

-- Number of Divisors 
numDivs :: Int -> Int 
numDivs n = length $ divs n  -- think about whether less computing can be done


-- Busy Beaver A060843

--Pascal Rows (recursively builds list


-- Prime numbers A000040

-- Mersenne Prime A000043, A000668

-- Stirling numbers









