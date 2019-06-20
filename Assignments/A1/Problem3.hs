-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A1

-- Problem 3

-- a)
luhnDouble :: Integer -> Integer
luhnDouble n = if (n * 2) > 9
                     then ((n*2)-9)
                 else (n * 2) 

-- b)
luhn :: Integer -> Integer -> Integer -> Integer -> Bool
luhn a b c d = if (((luhnDouble a) + b + (luhnDouble c) + d) `mod` 10) == 0
                     then True
                 else False