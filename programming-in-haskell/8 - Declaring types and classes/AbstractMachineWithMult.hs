-- Is there a better way to do this??
-- Please let me know

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]
-- data Eval = EVALADD Expr | EVALMULT Expr
data Op = EVALADD Expr | EVALMULT Expr | ADD Int | MULT Int

-- Evaluate the expression
eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVALADD y : c)
eval (Mult x y) c = eval x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y : c) n = eval y (ADD n : c)
exec (ADD m : c) n = exec c (m+n)
exec (EVALMULT y : c) n = eval y (MULT n : c)
exec (MULT m : c) n = exec c (m*n)

value :: Expr -> Int
value e = eval e []
