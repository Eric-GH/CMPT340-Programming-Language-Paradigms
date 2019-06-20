-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A3b

--Problem2:

pyth :: Integer -> [(Integer, Integer, Integer)]
pyth n = [(x,y,z) | x <- [1 .. n], y <- [1 .. n], z <-[1 .. n], ((x^2) + (y^2) == (z^2))]