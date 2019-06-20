-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A3b

--Problem1:

twoComprehensions :: [(Integer, Integer)]
twoComprehensions = concat [[(x,y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
