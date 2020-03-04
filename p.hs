-- asddsad

{-

-}
factorial :: Integer -> Integer

{-
factorial 0 = 1
factorial n = n * factorial (n - 1)
-}

{-
factorial n
    | n == 0    = 1
    | True      = n * factorial (n - 1)
    
-}


factorial n =
    if n == 0
       then 1
       else n * factorial (n - 1)
       
       
mcd :: Integer -> Integer -> Integer

mcd a b
    | a == b    = a
    | a > b     = mcd (a - b) b
    | otherwise = mcd a (b - a)

    
llargada :: [a] -> Int

llargada [] = 0
-- indica que n'hi ha un element abans de la llista
-- x de tipus Int, xs de tipus [Int]
llargada (x:xs) = 1 + llargada xs




reves :: [a] -> [a]

reves [] = []
reves (x:xs) = reves xs ++ [x]
    

    
    

