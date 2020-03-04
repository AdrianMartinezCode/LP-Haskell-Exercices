eql :: [Int] -> [Int] -> Bool
eql x y
    | xl /= yl = False
        where
            xl = len x
            yl = len y
eql x y = all (==True) (zipWith (==) x y)


len :: [Int] -> Int
len [] = 0
len (_:xs) = len xs + 1

--eq :: Int -> Int -> Bool
--eq x y = x == y


prod :: [Int] -> Int
prod x = foldl (*) 1 x

prodOfEvens :: [Int] -> Int
prodOfEvens x = foldl (*) 1 (filter even x)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

--scalarProduct :: [Float] -> [Float] -> Float

