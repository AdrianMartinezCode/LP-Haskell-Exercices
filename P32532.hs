

divisors :: Int -> [Int]
--divisors n = [x | x <- [1..n], rem n x == 0]
divisors n = filter (\x -> rem n x == 0) [1..n]


nbDivisors :: Int -> Int
nbDivisors = length . divisors

moltCompost :: Int -> Bool
moltCompost n = all (\x -> nb > x) (take (n-1) $ [r | x <- [1..n], let r = nbDivisors x])
	where
		nb = nbDivisors n

