import Test.QuickCheck
{- Lab 1
   Date: 2021-11-02
   Authors: Elin Forsberg, Joel Båtsman Hilmersson
   Lab group: 15
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 2 


-- B -------------------------
-- power1

power1 :: Integer -> Integer -> Integer
power1 n k  = product nList
   where listOfSizeN = [1..k]
         nList = [n | a <- listOfSizeN]

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer
power2 n k | k == 0    =  1
           | odd k     =  n * (n^(k - 1))
           | otherwise =  (n * n)^(div k 2)



-- D -------------------------
{- 
 2 3 : This will test when k is an even number
 2 4 : This will test when k is an odd number
 2 0 : This will test so we get the same result when k is 0, the results 
       should be 1 for every function
 0 5 : This will test so that the answer is 0
 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power1 n k == power2 n k)

--
powerTest :: Bool
powerTest = prop_powers 2 3 
            && prop_powers 2 4 
            && prop_powers 2 0 
            && prop_powers 0 5

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power n a == power1 n a) && (power1 n a == power2 n a)
              where a = abs(k) 
{-
 Used "abs" because negative k crasched tests. The lab assignment said that the
 powerfuction only should be able to take positive k numbers
-}