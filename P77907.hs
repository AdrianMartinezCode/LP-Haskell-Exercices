

absValue :: Int -> Int
absValue x
    | x < 0 = 0 - x
    | otherwise = x




-- quadrat :: Int -> Int
-- quadrat x = x * x


-- power :: Int -> Int -> Int
-- 
-- power 0 = 1
-- power n > 0 && even n = quadrat (power x (n `div` 2))
-- power n > 0 && odd n = quadrat (power x (n `div` 2)) * x


power :: Int -> Int -> Int


power x n
    | n == 0    = 1
    | even n    = quadrat (power x (div n 2))
    | otherwise = quadrat (power x (div n 2)) * x
    where
        quadrat x = x * x

{-
power :: Int -> Int -> Int
power x n
    | n == 0    = 1
    | even n    = y
    | otherwise = y * y * x
    where
        y = power x (div n 2)
-}

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + (slowFib (n-2))

quickFib :: Int -> Int
quickFib n = fst (quickFib' n)

-- quickFib' n ret (Fn, Fn - 1)
quickFib' :: Int -> (Int, Int)
quickFib' 0 = (0, 0)
quickFib' 1 = (1, 0)
quickFib' n = (fn, fn1)
    where
        (fn1, fn2) = quickFib' (n - 1)
        fn = fn1 + fn2


        
isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = isPrime' x 2

isPrime' :: Int -> Int -> Bool
isPrime' x d
    | mod x d == 0  = False
    | d*d > x       = True
    | otherwise     = isPrime' x (d + 1)
