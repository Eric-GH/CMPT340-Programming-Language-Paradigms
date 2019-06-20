-- Name: Hao Li
-- NSID: hal356
-- Student#: 11153054
-- CMPT 340 A2

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--									Part 1 | Problem 2
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
newCurry :: ((a,b,c) -> d) -> (a -> b -> c -> d)
newCurry = \f x y z -> f (x,y,z)


unCurry :: (a -> b -> c -> d) -> (a,b,c) -> d
unCurry = \f (x,y,z) -> f x y z

---for test the curry functions---
curryTestLargest (n, m, k) | (n > m && n > k) = n
                             | (m > n && m > k) = m
                             | otherwise = k


uncurryTestLargest n m k | (n > m && n > k) = n
                       | (m > n && m > k) = m
                       | otherwise = k


-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--									Part 2 | Problem 3
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------
--                               Data Declarations
--------------------------------------------------------------------------------------
type Numerator = Integer
type Denominator = Integer


data MyFraction = MyFraction (Numerator,Denominator)

instance Show MyFraction where
     show = fractionShow 
fractionShow :: MyFraction -> String
fractionShow (MyFraction (a,b)) | (b == 0) = "Denominator can not be zero"
                                | ((a `mod` b) == 0) = (show (a `div` b))
                                | otherwise = (show a) ++ "/"++ (show b)


--------------------------------------------------------------------------------------
--                     numerator, denominator, whole and fractional
--------------------------------------------------------------------------------------
numerator :: MyFraction -> Numerator
numerator (MyFraction (a,b)) = a

denominator :: MyFraction -> Denominator
denominator (MyFraction (a,b)) = b


whole :: MyFraction -> Integer
whole (MyFraction (a,b)) | (abs a) < b = 0
                         | otherwise = if a >= 0
                                          then (a `div` b)
                                       else ((abs(a) `div` b) * (-1))
fractional :: MyFraction -> MyFraction
fractional (MyFraction (a,b)) | a > 0 = MyFraction (a - (whole(MyFraction(a,b))*b),b)
                              | otherwise = MyFraction((((abs a) - (abs(whole(MyFraction (a,b))) * b))*(-1)), b)



--------------------------------------------------------------------------------------
--                     (==), (<=), (>=), (<), (>)
--------------------------------------------------------------------------------------
instance Eq MyFraction where
    (==) = myFractionEq
myFractionEq :: MyFraction -> MyFraction -> Bool
myFractionEq (MyFraction (m,n)) (MyFraction (m1,n1)) = if (m * n1) == (m1 * n)
                                                         then True
                                                       else False

  

instance Ord MyFraction where
    (>) = biggerFraction
    (<) = smallerFraction
    (<=) = smalleqFraction
    (>=) = bigeqFraction
smallerFraction :: MyFraction -> MyFraction -> Bool
smallerFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = if ((m * n1) < (m1 * n))
                                                              then True
                                                          else False

biggerFraction :: MyFraction -> MyFraction -> Bool
biggerFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = if ((m * n1) > (m1 * n))
                                                              then True
                                                          else False

bigeqFraction :: MyFraction -> MyFraction -> Bool
bigeqFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = if ((m * n1) >= (m1 * n))
                                                              then True
                                                          else False

smalleqFraction :: MyFraction -> MyFraction -> Bool
smalleqFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = if ((m * n1) <= (m1 * n))
                                                              then True
                                                          else False

--------------------------------------------------------------------------------------
--                    (+), (-), (*), (/), (negate) (abs)
--------------------------------------------------------------------------------------

instance Num MyFraction where
    (+) = addFraction
    (-) = subFraction
    (*) = mulFraction
    negate = negateFraction  
    abs = absFraction
    signum = sigFraction -- just for ignor the warning
    fromInteger = intFraction -- just for ignor the warning

instance Fractional MyFraction where
    (/) = divFraction
    fromRational = ratFraction -- just for ignor the warning

addFraction :: MyFraction -> MyFraction -> MyFraction
addFraction (MyFraction (m,n)) (MyFraction (m1,n1))
          | (n == n1) = MyFraction ((m + m1),n)
          | otherwise = MyFraction (((m*n1) + (m1*n)),(n*n1))


subFraction :: MyFraction -> MyFraction -> MyFraction
subFraction (MyFraction (m,n)) (MyFraction (m1,n1))
          | (n == n1) = MyFraction ((m - m1),n)
          | otherwise = MyFraction (((m*n1) - (m1*n)),(n*n1))

mulFraction :: MyFraction -> MyFraction -> MyFraction
mulFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = (MyFraction ((m*m1),(n*n1)))


negateFraction :: MyFraction -> MyFraction
negateFraction (MyFraction (m,n)) = (MyFraction ((m * (-1)),n))

absFraction :: MyFraction -> MyFraction
absFraction (MyFraction (m,n)) | (m > 0) = (MyFraction (m,n))
                               | otherwise = (negateFraction (MyFraction (m,n)))

divFraction :: MyFraction -> MyFraction -> MyFraction
divFraction (MyFraction (m,n)) (MyFraction (m1,n1)) = if ((m*n1) < 0 &&(n*m1) < 0)
                                                         then (MyFraction ((abs (m*n1)),(abs (n*m1))))
                                                      else if (n*m1) < 0
                                                         then (MyFraction (( (-1) * (m*n1)),(abs (n*m1))))
                                                      else (MyFraction ((m*n1),(n*m1)))


-- just for ignor the warning
sigFraction :: MyFraction -> MyFraction
sigFraction (MyFraction (m,n)) = signum (MyFraction (m,n))

-- just for ignor the warning
intFraction :: Integer -> MyFraction
intFraction  n = (MyFraction (n,n))

-- just for ignor the warning
ratFraction :: Rational -> MyFraction
ratFraction n = (MyFraction (0,0))





-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
--									Part 3 | Problem 4
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

--(a)--
shuffle :: [a] -> [a] -> [a]
shuffle l1 [] = l1
shuffle [] l2 = l2
shuffle (x:xs) (y:ys) = x : y : (shuffle xs ys)


--(b)--
split :: [a] -> Int -> [[a]]
split list n = [(take n list), (drop n list)]


--(c)--
outshuffle :: [a] -> [a]
outshuffle list = shuffle firstlist secondlist
                  where firstlist = head (split list ((length list) `div` 2))
                        secondlist = last (split list ((length list) `div` 2))

inshuffle :: [a] -> [a]
inshuffle list = shuffle firstlist secondlist
                  where firstlist = last (split list ((length list) `div` 2))
                        secondlist = head (split list ((length list) `div` 2))

--(d)--
nshuffle :: (Eq a, Num a) => (b -> b) -> a -> b -> b
nshuffle f n list = if (n == 1)
                         then f(list)
                    else (nshuffle f (n-1) (f(list)))


--(e)--
howManyShuffles :: (Eq a, Num p) => (a -> a) -> a -> a -> p
howManyShuffles f list1 list2 = check
     where check = if (f list1) ==list2
                        then 1
                   else howManyShuffles f (f list1) list2 + 1









