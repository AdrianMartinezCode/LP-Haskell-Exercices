roman2int :: String -> Int
roman2int s = fst $ roman2intR s
	where
		roman2intR [] = (0, 0)
		roman2intR (x:xs)
			| xp >= g = (xp + y, xp)
			| otherwise = (y - xp, g)
				where
					(y, g) = roman2intR xs
					xp = getValueSymbol x


getValueSymbol :: Char -> Int
getValueSymbol 'I' = 1
getValueSymbol 'V' = 5
getValueSymbol 'X' = 10
getValueSymbol 'L' = 50
getValueSymbol 'C' = 100
getValueSymbol 'D' = 500
getValueSymbol 'M' = 1000




roman2int' :: String -> Int
roman2int' s = fst $ foldr romanFoldF (0,0) s
	where 
		romanFoldF c (y, g)
			| x >= g = (x + y, x)
			| otherwise = (y - x, g)
				where
					x = getValueSymbol c



arrels :: Float -> [Float]
arrels x = [x] ++ arrelsP x x

arrelsP :: Float -> Float -> [Float]
arrelsP x fn1 = [fn] ++ arrelsP x fn
	where
		fn = (1.0 / 2.0) * (fn1 + (x / fn1))


arrel :: Float -> Float -> Float
arrel x e = pred e (arrels x) 0.0
	where
		pred e (fn:xs) fn1
			| (abs (fn - fn1)) < e = fn
			| otherwise = pred e xs fn


data LTree a = Leaf a | Node (LTree a) (LTree a)

instance Show a => Show (LTree a) where
	show (Leaf a) = "{" ++ (show a) ++ "}"
	show (Node a b) = "<" ++ (show a) ++ "," ++ (show b) ++ ">"


--build :: [a] -> LTree



--build' :: [a] -> Int -> Ltree
--build' 


log2Floor :: Int -> Int
log2Floor a = floor $ (log c) / (log 2)
	where
		c = fromIntegral a :: Float


zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf a) (Leaf b) = Just (Leaf (a, b))
zipLTrees (Node aa ab) (Node ba bb) = 
	do
		n1 <- zipLTrees aa ba
		let n1B = n1
		n2 <- zipLTrees ab bb
		Just (Node n1B n2)
zipLTrees _ _ = Nothing

