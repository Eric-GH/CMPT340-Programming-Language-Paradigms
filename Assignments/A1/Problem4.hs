-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A1

-- Problem 4

-- a)
averageThree :: Integer -> Integer -> Integer -> Double
averageThree a b c = (fromIntegral(a + b + c)) / 3

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = if a == b && b == c
                                then 0
                            else if (x > average && y > average) 
                                 || (x> average && z > average) 
                                 || (y > average && z > average)
                                then 2
                            else 1
                            where average = averageThree a b c
                                  x = fromIntegral a
                                  y = fromIntegral b
                                  z = fromIntegral c


-- bå)
averageThreeInOne :: (Integer, Integer, Integer) -> Double
averageThreeInOne (a, b, c) = (fromIntegral(a + b + c)) / 3


-- c)
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (a, b, c) = if check == 0
                                then (a, b, c)
                        else if a <= b
                                then if b <= c
                                          then (a, b, c)
                                     else if a <= c
                                          then (a, c, b)
                                     else (c, a, b)
                        else if c <= b
                                then (c, b, a)
                        else if a <= c
                                then (b, a, c)
                        else (b, c, a)
                        where check = howManyAboveAverage a b c












