--import Control.Monad
--import Control.Applicative

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




--instance Functor (Queue)
--	where
--		fmap f q
--			| empty q = create
--			| otherwise = push (f (top q)) (fmap f (pop q))

instance Functor (Queue)
	where
		fmap f q = listToQueue (fmapQ' f q) create

fmapQ' :: (a -> b) -> Queue a -> [b]
fmapQ' f q
	| empty q = []
	| otherwise = [f $ top q] ++ fmapQ' f (pop q)

listToQueue :: [a] -> Queue a -> Queue a 
listToQueue [] q = q
listToQueue (x:xs) q = listToQueue xs (push x q)


translation :: Num b => b -> Queue b -> Queue b
translation x q = fmap (+x) q

c1 = push 4 $ push 3 $ pop $ push 2 $ push 1 create
check_1 = fmap (+0) c1



--fmap ((+5) <$> (+6)) c1
r1 = (+5) `fmap` (+6) `fmap` c1
r2 = ((+5).(+6)) `fmap` c1

--instance Applicative Queue
--	where
--		pure = create
--		qf <*> q
--			| empty qf 	= q
--			| otherwise = fmap f (qf' <*> q)
--				where
--					f = top qf
--					qf' = pop qf

instance Applicative Queue
	where
		pure a = push a $ create
		qf <*> q
			| empty qf = create
			| otherwise = concatQueues (fmap f q) (qf' <*> q)
				where
					f = top qf
					qf' = pop qf



concatQueues :: Queue a -> Queue a -> Queue a
concatQueues qa qb
	| empty qb = qa
	| otherwise = concatQueues (push (top qb) qa) (pop qb)


--instance Monad (Queue)
--	where
--		q >>= f
--			| empty q = create
--			| otherwise = push (f $ top q) ((pop q) >>= f)

instance Monad Queue
	where
		return e = push e $ create
		q >>= f = foldl (appendMaybeQueueToQueue) (create) (scanlQueue f q)
		q >> k = q >>= (\_ -> k) 


appendMaybeQueueToQueue :: Queue a -> Queue a -> Queue a
appendMaybeQueueToQueue qp sq
	| empty sq = qp
	| otherwise = push (top sq) qp

scanlQueue :: (a -> Queue b) -> Queue a -> [Queue b]
scanlQueue f q
	| empty q = []
	| otherwise = [f $ top q] ++ scanlQueue f (pop q)

 
-- Feu, utilitzant la notació do, una funció kfilter :: (p -> Bool) -> Queue p -> Queue p 
-- que selecciona tots els elements d’una cua que satisfan 
-- una propietat donada.
kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
	e <- q
	getQueueWithFilterElement f e

getQueueWithFilterElement :: (p -> Bool) -> p -> Queue p
getQueueWithFilterElement f e
	| f e = return e
	| otherwise = create
