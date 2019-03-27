module Block2.Task1
  ( Expr(..)
  , ArithmeticError(..)
  , eval
  ) where

--Task1
data Expr
  = Cons Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

data ArithmeticError = DivisionByZero | NegateExponent
  deriving(Show, Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Cons a)  = return a
eval (Add a b) = (eval b) >>= (\y -> (eval a) >>= \x -> return ((+) x y))
eval (Sub a b) = (eval b) >>= (\y -> (eval a) >>= \x -> return ((-) x y))
eval (Mul a b) = (eval b) >>= (\y -> (eval a) >>= \x -> return ((*) x y))
eval (Div a b) = (eval b) >>= (\y -> (eval a) >>= \x -> if (y == 0)
  then Left DivisionByZero
  else return ((div) x y))
eval (Pow a b) = (eval b) >>= (\y -> (eval a) >>= \x -> if (y < 0)
  then Left NegateExponent
  else return ((^) x y))
