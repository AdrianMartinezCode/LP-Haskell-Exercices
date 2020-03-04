data Tree a = Node a (Tree a) (Tree a) | Empty 
    deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ fe fd) = 1 + size fe + size fd

height :: Tree a -> Int
height Empty = 0
height (Node _ fe fd) = 1 + max (height fe) (height fd)


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node x fe1 fd1) (Node y fe2 fd2)
    | x /= y        = False
    | otherwise     = equal fe1 fe2 && equal fd1 fd2

    
    
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x fe fd) = x : preOrder fe ++ preOrder fd


postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x fe fd) = postOrder fe ++ postOrder fd ++ [x]

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x fe fd) = inOrder fe ++ [x] ++ inOrder fd

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst x = breadthFirst' [x]
    
breadthFirst' :: [Tree a] -> [a]
breadthFirst' [] = []
breadthFirst' (Empty:xs) = []
breadthFirst' ((Node x fe fd):xs) = [x] ++ breadthFirst' (xs ++ [fe] ++ [fd])


build :: Eq a => [a] -> [a] -> Tree
build (p:ps) (i:is)
	| p == i 	= Node p Empty Empty
	| otherwise = build 


t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2


