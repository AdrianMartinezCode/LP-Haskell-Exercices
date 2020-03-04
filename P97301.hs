

fizzBuzz :: [Either Int String]
fizzBuzz = map fizzBuzz' [0..]

    
fizzBuzz' :: Int -> Either Int String
fizzBuzz' x
    | x `mod` 15 == 0   = Right "FizzBuzz"
    | x `mod` 5 == 0    = Right "Buzz"
    | x `mod` 3 == 0    = Right "Fizz"
    | otherwise         = Left x
