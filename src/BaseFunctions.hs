module BaseFunctions where

-- Factorial 
factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1..n]

-- k-permutations of n
permute :: Int -> Int -> Int 
permute n k = div (factorial n) $factorial $n-k

-- k-combinations of n
choose :: Int -> Int -> Int 
choose n k = div (permute n k) $factorial k

-- Binomial sums
binomialSum :: Int -> Int 
binomialSum n = 2^n

-- Prime Factorise n  
primeFactors :: Int -> [Int]
primeFactors n =
  case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

{-
primeFactors 1 = []
primeFactors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
-}




