insert :: [Int] -> Int -> [Int]
insert [] y = [y]
insert (x:xs) y
	| x < y 	= [x] ++ (insert xs y)
	| otherwise	= [y] ++ [x] ++ xs


isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x


remove :: [Int] -> Int -> [Int]
remove [] y = []
remove (x:xs) y
	| x == y 	= xs
	| otherwise = [x] ++ (remove xs y)

ssort :: [Int] -> [Int]
ssort [] = []
ssort (x:xs) = [x] ++ (ssort (remove xs x))

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge [] [] = []
merge (x:xs) (y:ys)
    | x < y = [x] ++ (merge xs ([y] ++ ys))
    | x > y = [y] ++ (merge ([x] ++ xs) ys)
    | otherwise = [x] ++ [y] ++ (merge xs ys)
    
msort :: [Int] -> [Int]
msort [] = []
msort (x:xs) = merge [x] (msort xs)

qsort :: [Int] -> [Int]
qsort
