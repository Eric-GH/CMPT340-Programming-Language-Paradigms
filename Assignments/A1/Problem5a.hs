-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A1

-- Problem 5

-- a)
compose3 :: (Double -> Double) -> (Double -> Double) -> (Double -> Double) -> Double -> Double
compose3 f g h x = f(g(h x))

-- Test Cases
test1 x = x + 1
test2 x = x + 10
test3 x = x - 6
