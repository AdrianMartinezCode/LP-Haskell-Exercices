data Queue a = Queue [a] [a]
	deriving(Show)

create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push x (Queue head tail)	= Queue head (x:tail)


pop :: Queue a -> Queue a
pop (Queue [] cua)			= Queue (tail (reverse cua)) []
pop (Queue (x:head) tail)	= Queue head tail


top :: Queue a -> a
top (Queue [] tail) 	  = last tail
top (Queue (x:head) tail) = x


empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _)	= False

instance Eq a => Eq (Queue a)
	where
		(Queue [] []) == (Queue [] []) = True
		qx == qy = (not em_qx) && (not em_qy) && (top qx) == (top qy) && (pop qx) == (pop qy)
			where
				em_qx = empty qx
				em_qy = empty qy


--instance Eq a => Eq (Queue a)
--    where
--    	(Queue [] []) == (Queue [] []) = True
--    	(Queue _ [])  ==
--    	(Queue h1 []) == (Queue h2 []) 	= x == y && (Queue h1w []) == (Queue h2w [])
--    		where
--    			h1w = init h1
--    			x = last h1
--    			h2w = init h2
--    			y = last h2
--    	(Queue h1 []) == (Queue h2 (y:t2))	= x == y && (Queue h1w []) == (Queue h2 t2)
--    		where
--    			h1w = init h1
--    			x = last h1
--    	(Queue h1 (x:t1)) == (Queue h2 [])	= x == y && (Queue h1 t1) == (Queue h2w [])
--    		where
--    			h2w = init h2
--    			y = last h2
--    	(Queue h1 (x:t1)) == (Queue h2 (y:t2))	= x == y && (Queue h1 t1) == (Queue h2 t2)
--    	_ == _ = False

c3 = push 3 $ pop $ push 2 $ push 1 create
c4 = pop $ push 3 $ push 2 $ push 1 create
c5 = push 3 $ push 2 $ create
c6 = push 3 $ pop $ push 1 $ push 2 create
c7 = create
c8 = create
c9 = push 3 $ push 2 $ push 1 create
c10 = push 2 $ push 1 create