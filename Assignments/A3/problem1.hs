-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A3a

--Problem 1
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)


-- a):
newmap :: (b -> a) -> [b] -> [a]
newmap f = unfold (null) (f . head) (tail)

-- b):
newiterate :: (a -> a) -> a -> [a]
newiterate f = unfold (const False) id f


-- c):
repHalve :: [a] -> [[a]]
repHalve i = unfold null first second i
     where first i = (take (check i) i)
           second i = (drop (check i) i)
           check i | (length i) `mod` 2 == 0 = (length i) `div` 2
                   | otherwise = (length i) `div` 2 + 1