-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A3a

--Problem 2

-- a):
altMap :: [t -> t] -> [t] -> [t]
altMap [] [] = []
altMap [] xs = xs
altMap ys [] = []
altMap (y:ys) (x:xs) = (y x) : (altMap (ys++[y]) xs)

-- b):
luhnDouble :: (Ord a, Num a) => a -> a
luhnDouble x | (2 * x) <= 9 = (2 * x)
             | otherwise = (2 * x - 9)

luhn :: Integral a => [a] -> Bool
luhn [] = False
luhn num | (length num) `mod` 2 == 0 = if (sum (altMap [luhnDouble, id] num)) `mod` 10 == 0
                                                     then True
                                              else False
            | otherwise = if (sum (altMap [id, luhnDouble] num)) `mod` 10 == 0
                                                     then True
                                              else False