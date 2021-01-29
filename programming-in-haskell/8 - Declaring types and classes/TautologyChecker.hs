-- Chapter Exercise section 8.6
-- Tautology checker
-- For expressions: 
-- A ∧ ¬A
-- (A ∧ B) ⇒ A
-- A ⇒ (A ∧ B)
-- (A ∧ (A ⇒ B)) ⇒ B

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- A ∧ ¬A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A ∧ B) ⇒ A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A ⇒ (A ∧ B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A ∧ (A ⇒ B)) ⇒ B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y
>>>>>>> 042954c0fa9e1af6020185821ac53c429f56e0a3

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

type Assoc k v = [(k, v)]
type Subst = Assoc Char Bool

substs :: Prop -> [Subst]
substs prop = map (zip vs) (bools (length vs))
                where vs = rmdups (vars prop)

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const x) = x
eval s (Var x) = find x s
eval s (Not x) = not (eval s x)
eval s (And x y) = (eval s x) && (eval s y)
eval s (Imply x y) = (eval s x) <= (eval s y)

isTaut :: Prop -> Bool
isTaut prop = and [eval s prop | s <- substs prop]
>>>>>>> 042954c0fa9e1af6020185821ac53c429f56e0a3
