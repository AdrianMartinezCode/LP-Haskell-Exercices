import Control.Monad

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

--eval1 :: Expr -> Int
--eval1 (Add (Val x) (Val y)) = x + y
--eval1 (Add x (Val y) 		= eval1 x + y
--eval1 (Add (Val x) y) 		= x + $ eval1 y


eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = eval1 x + (eval1 y)
eval1 (Sub x y) = eval1 x - (eval1 y)
eval1 (Mul x y) = eval1 x * (eval1 y)
eval1 (Div x y) = eval1 x `div` (eval1 y)




eval2 :: Expr -> Maybe Int
eval2 (Add x y)	= liftM2 (+) (eval2 x) (eval2 y)
eval2 (Sub x y) = liftM2 (-) (eval2 x) (eval2 y)
eval2 (Mul x y) = liftM2 (*) (eval2 x) (eval2 y)
eval2 (Div x y) = liftM2 (div) (eval2 x) ((eval2 y) >>= evalIf0)


evalIf0 :: Int -> Maybe Int
evalIf0 0 = Nothing
evalIf0 x = Just x


eval3 :: Expr -> Either String Int
eval3 (Val x)	= Right x
eval3 (Add x y) = liftM2 (+) (eval3 x) (eval3 y)
eval3 (Sub x y) = liftM2 (-) (eval3 x) (eval3 y)
eval3 (Mul x y) = liftM2 (*) (eval3 x) (eval3 y)
eval3 (Div x y) = liftM2 (div) (eval3 x) ((eval3 y) >>= evalIf0')


evalIf0' :: Int -> Either String Int
evalIf0' 0 = Left "div0"
evalIf0' x = Right x

