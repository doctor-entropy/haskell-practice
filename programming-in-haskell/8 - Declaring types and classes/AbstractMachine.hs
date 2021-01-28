-- An expression can be just an Integer or Additon of two expressions
-- For e.g: Val 2 | Add (Val 2) (Val 3) | Add (Val2) (Add (Val 3) (Val 5) )
data Expr = Val Int | Add Expr Expr

-- Define a control stack with operations
type Cont = [Op]
data Op = EVAL Expr | ADD Int

-- Evaluate the expression
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD m : c) n = exec c (m+n)

value :: Expr -> Int
value e = eval e []