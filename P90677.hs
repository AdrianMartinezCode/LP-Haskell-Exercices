myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [x] = f x b
myFoldr f b (x:xs) = f x (myFoldr f b xs)


myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f a
	| p a == True 	= a
	| otherwise 	= myUntil p f (f a)


--myMap :: (a -> b) -> [a] -> [b]
--myMap _ [] = []
--myMap f (x:xs) = [f x] ++ (myMap f xs)

myMap :: (a -> b) -> [a] -> [b]




--myFilter :: (a -> Bool) -> [a] -> [a]
--myFilter _ [] = []
--myFilter p (x:xs)
--	| p x == True	= [x] ++ (myFilter p xs)
--	| otherwise		= myFilter p xs


--myAll :: (a -> Bool) -> [a] -> Bool
--myAll _ [] = True
--myAll p (x:xs)
--	| p x == True 	= myAll p xs
--	| otherwise		= False


--myAny :: (a -> Bool) -> [a] -> Bool
--myAny _ [] = False
--myAny p (x:xs)
--	| p x == True 	= True
--	| otherwise		= myAny p xs


myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = [(x,y)] ++ (myZip xs ys)


--myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--myZipWith _ _ [] = []
--myZipWith _ [] _ = []
--myZipWith f (x:xs) (y:ys) = [f x y] ++ (myZipWith f xs ys)