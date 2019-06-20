-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A1

-- Problem 2

-- conditional expression
fastExp1 :: Double -> Integer -> Double
fastExp1 n k = if k == 0
                     then  1
                 else if k `mod` 2 == 0
                     then (fastExp1 n (k`div`2)) * (fastExp1 n (k`div`2)) 
                 else n * (fastExp1 n (k-1)) 

-- guarded equation
fastExp2 :: Double -> Integer -> Double
fastExp2 n k
    | k == 0 = 1
    | k `mod` 2 == 0 = (fastExp1 n (k`div`2)) * (fastExp1 n (k`div`2))
    | otherwise = n * (fastExp1 n (k-1)) 

-- pattern matchings
fastExp3 :: Double -> Integer -> Double
fastExp3 n 0 = 1
fastExp3 n k = check (k `mod` 2)
    where check 0 = (fastExp1 n (k`div`2)) * (fastExp1 n (k`div`2))
          check 1 = n * (fastExp1 n (k-1))