--data Eval1Op = S | R | M | D
data Expr 	= S | R | M | D | Int

--eval1 :: String -> Int
--eval1 s = read (eval1' $ convertToExpr $ words s) :: Int

--convertToExpr :: [String] -> [Expr]
--convertToExpr [] = []
--convertToExpr (x:xs) = [getOperator x] ++ convertToExpr xs

--eval1' :: [Expr] -> Int -> Int -> (Int, [Expr])
--eval1' [e] x y = (getResult y x e, [])
--eval1' ((Eval1Op e):xs) x y = getResult y x e
--eval1' ((Int i):xs) x y = eval1' x (fst k) (snd k)
--	where
--		k = eval1' xs i y





--getResult :: Int -> Int -> Expr -> Int
--getResult a b S = a + b
--getResult a b R = a - b
--getResult a b M = a * b
--getResult a b D = div a b


--getOperator :: String -> Expr
--getOperator "+" = S
--getOperator "-" = R
--getOperator "*" = M
--getOperator "/" = D
--getOperator s = read s :: Int



--Problema 3: fsmap

--Definiu una funció fsmap :: a -> [a -> a] -> a que, donats un element x de tipus a 
--	i una llista fs de funcions de tipus a -> a, fa que fsmap x fs retorni l’aplicació 
--  (d’esquerra a dreta) de totes les funcions de fs a x. Es valorà com de succinta és 
--  la vostra solució.

fsmap :: a -> [a -> a] -> a
fsmap a [] = a
fsmap a (f:fs) = fsmap (f a) fs



--Problema 4: Dividir i vèncer

--Escriviu una funció d’ordre superior que definixi l’esquema de dividir i vèncer i 
--	utilitzeu-la per fer l’algorisme de quicksort per a llistes d’enters.

--La funció per dividir i vèncer ha de tenir aquesta interfície:
--  divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b

--on a és el tipus del problema, b és el tipus de la solució, i divideNconquer base 
--	divide conquer x utilitza:

-- base :: (a -> Maybe b) per obtenir la solució directa per a un problema si 
--	és trivial (quan és un Just b) o per indicar que no és trivial (quan és Nothing).
-- divide :: (a -> (a, a)) per dividir un problema no trivial en un parell de subproblemes 
--	més petits.
-- conquer :: (a -> (a, a) -> (b, b) -> b) per, donat un problema no trivial, els 
--	seus subproblemes i les seves respectives subsolucions, obtenir la 
--	solució al problema original.
-- x :: a denota el problema a solucionar.

--La funció pel quicksort ha de ser quickSort :: [Int] -> [Int] i ha d’utilitzar divideNconquer.


divideNconquer :: (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x =
	case (base x) of
		Just r -> r
		Nothing -> conquer x (x1, x2) (y1, y2)
	where
		(x1, x2) = divide x
		y1 = divideNconquer base divide conquer x1
		y2 = divideNconquer base divide conquer x2


quickSort :: [Int] -> [Int]
quickSort = divideNconquer base divide conquer
	where
		base [] = Just []
		base [x] = Just [x]
		base _ = Nothing

		divide (x:xs) = (lts, gts)
			where 
				lts = filter (<= x) xs
				gts = filter (> x) xs

		conquer (x:_) _ (ys1, ys2) = ys1 ++ [x] ++ ys2


--Problema 5: Racionals

--Definiu un tipus Racional per manipular nombres racionals positius amb operacions per:

--construir un racional a través d’un numerador i d’un denominador naturals, 
--	obtenir el numerador de la seva forma simplificada, obtenir el denominador 
--	de la seva forma simplificada.

--A més, feu que Racional pertanyi a la classe Eq i a la classe Show, fent que 
--	els racionals es mostrin de la forma "x/y".

--Seguiu aquesta interfície:
--     data Racional = ...
--     racional :: Integer -> Integer -> Racional
--     numerador :: Racional -> Integer
--     denominador :: Racional -> Integer

--Si voleu, podeu utilitzar la funció estàndard gcd que retorna el màxim comú 
--	divisor de dos naturals.

data Racional = Racional Integer Integer


instance Show Racional where
	show (Racional n d) = (show $ div n c) ++ "/" ++ (show $ div d c)
		where
			c = gcd n d

instance Eq Racional where
	(Racional n1 d1) == (Racional n2 d2) = (div n1 d1) == (div n2 d2)

racional :: Integer -> Integer -> Racional
racional a b = Racional a b
	

numerador :: Racional -> Integer
numerador (Racional n _) = n

denominador :: Racional -> Integer
denominador (Racional _ d) = d

--Problema 6: Arbre de Calkin-Wilf

--L’arbre de Calkin–Wilf és un arbre binari que representa tots els racionals 
--	positius. L’arbre té com arrel el racional 1/1 i, qualsevol node a/b té dos 
--	fills a/(a + b) i (a + b)/b.

--Aquests són els primers nivells de l’arbre de Calkin–Wilf: [figura extreta de Wikipedia]

--Escriviu una funció racionals :: [Racional] que retorni la llista infinita 
--	de tots els nombres racionals positius segons l’ordre de l’arbre de Calkin–Wilf.

--Per a fer-ho, utilitzeu el tipus Racional del problema anterior. També podeu 
--	aprofitar les definicions genèriques d’arbre infinit i del seu recorregut 
--	per nivells que es donen a continuació:
--     data Tree a = Node a (Tree a) (Tree a)
--     recXnivells :: Tree a -> [a]
--     recXnivells t = recXnivells' [t]
--         where recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
	where 
		recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

racionals :: [Racional]
racionals = recXnivells (rac' (Racional 1 1))
	where
		rac' (Racional n d) = Node (Racional n d) (rac' (Racional n (n + d))) (rac' (Racional (n + d) d))