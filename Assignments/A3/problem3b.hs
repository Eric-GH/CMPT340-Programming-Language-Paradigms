-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A3b

--Problem3:

perfectInt :: [Integer]
perfectInt = [n | n <- [1 ..], n == (sum [m | m <- [1 .. (n-1)], n `mod` m == 0])]