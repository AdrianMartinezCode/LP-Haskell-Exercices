
data STree a = Nil | Node Int a (STree a) (STree a)
	deriving (Show)

isOk :: STree a -> Bool
isOk t = 
	case isOk' t of
		Nothing -> False
		Just _ -> True


isOk' :: STree a -> Maybe Int
isOk' Nil = Just 0
isOk' (Node n _ a b) = 
	do
		na <- isOk' a
		let nsa = na
		nb <- isOk' b
		func na nb n
	where
		func a b n
			| a + b + 1 == n = Just n
			| otherwise = Nothing


nthElement :: STree a -> Int -> Maybe a
nthElement (Node _ elem Nil Nil) th
	| th == 1 = Just elem
	| otherwise = Nothing
nthElement (Node n elem a Nil) th
	| th > n = Nothing
	| th == n = Just elem
	| otherwise = nthElement a (th - 1)
nthElement (Node n elem Nil b) th
	| th > n = Nothing
	| th == n = Just elem
	| otherwise = nthElement b (th - 1)
nthElement (Node n elem a b) th
	| th > n = Nothing
	| th == n - t2 = Just elem
	| th > t1 = nthElement b (th - t1 - 1)
	| otherwise = nthElement a (th - 1)
	where
		getTalles (Node t1 _ _ _) (Node t2 _ _ _) = (t1, t2)
		(t1, t2) = getTalles a b


div10 = flip div 10
t1 = Node 3 99 (Node 1 88 Nil Nil) (Node 1 22 Nil Nil)
t2 = Node 2 77 (Node 1 33 Nil Nil) Nil
t3 = Node 6 44 t1 t2
t4 = Node 7 55 t1 t2


--isOk t4
--nthElement t3 1
--nthElement t3 9
--nthElement t3 3
--nthElement t3 8