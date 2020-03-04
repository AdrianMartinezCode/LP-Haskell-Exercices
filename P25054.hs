

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = (myLength xs) + 1

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = myMaximum' x (myMaximum xs)

myMaximum' :: Int -> Int -> Int
myMaximum' x y
	| x > y = x
	| x < y = y
	| otherwise = x


average :: [Int] -> Float
average x = snd (average' (x, 0))

average' :: ([Int], Int) -> (Int, Float)
average' ([],n) = (n, 0.0)
average' (x:xs, n) = (z, v + ((fromIntegral x) / (fromIntegral z)))
	where
		(z, v) = average' (xs, n+1)


--buildPalindrome :: [Int] -> [Int]

remove :: [Int] -> [Int] -> [Int]
remove x [] = x
remove xs (y:ys) = remove (removeElemFromList xs y) ys
--remove (x:xs) (y:ys)
--	| isElemInList xs y == True = remove xs ys
--	| otherwise 				= remove ([x] ++ xs) ys


{-}
isElemInList :: [Int] -> Int -> Bool
isElemInList [] y = False
isElemInList (x:xs) y
	| x == y 	= True
	| otherwise	= isElemInList xs y
-}

removeElemFromList :: [Int] -> Int -> [Int]
removeElemFromList [] _ = []
removeElemFromList (x:xs) y
	| x == y 		= removeElemFromList xs y
	| otherwise 	= [x] ++ (removeElemFromList xs y)


flatten :: [[Int]] -> [Int]
flatten [x] = x
flatten (xs:xss) = xs ++ flatten xss

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
	| even x == True	= (o, [x] ++ e)
	| otherwise			= ([x] ++ o, e)
		where
			(o, e) = oddsNevens(xs) 