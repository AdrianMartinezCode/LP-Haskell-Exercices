import Data.List

----------------------
flatten :: [[Int]] -> [Int]
flatten xs = foldl1 (++) xs
----------------------

----------------------
myLength :: String -> Int
myLength xs = fst (mapAccumL descartaIsuma 0 xs)

descartaIsuma :: Int -> Char -> (Int, Char)
descartaIsuma x y = (x + 1, y)
----------------------


----------------------
myReverse :: [Int] -> [Int]
myReverse xs = foldl concatElemFirst [] xs 

concatElemFirst :: [Int] -> Int -> [Int]
concatElemFirst x y = [y] ++ x
----------------------

----------------------
countIn :: [[Int]] -> Int -> [Int]
countIn xss y = foldr (quantesOcurrenciesIAfegeix y) [] xss


quantesOcurrenciesIAfegeix :: Int -> [Int] -> [Int] -> [Int]
quantesOcurrenciesIAfegeix y xs xr = [length $ filter (isEqualList y) xs] ++ xr

isEqualList :: Int -> Int -> Bool
isEqualList y x = y == x
----------------------

--isEqualList :: Int -> [Int] -> Bool
--isEqualList y xs = fst $ foldl (\(t, x) y -> (x == y && t, y)) (True, y) xs


--t :: ([Int],Int) -> [Int] -> ([Int],Int)
--t rs y = filter



-- filter :: (a -> Bool) -> [a] -> [a]
-- map :: (a -> b) -> [a] -> [b]
-- length :: Foldable t => t a -> Int