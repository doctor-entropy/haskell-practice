-- Exercises

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) yss ++ yss
                where yss = subs xs

interleve :: a -> [a] -> [[a]]
interleve x [] = [[x]]
interleve x (y:ys) = (x:y:ys) : map (y:) (interleve x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleve x) (perms xs))

-- 1)
choices :: [a] -> [[a]]
choices xs = [xs'' | xs' <- subs xs, xs'' <- perms xs']

-- 2)
removeone :: Eq a => a -> [a] -> [a]
removeone p [] = []
removeone p (n:ns) | p == n = ns
                   | otherwise = n : removeone p ns

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _ = True
isChoice _      [] = False
isChoice (p:ps) ns = (elem p ns) && isChoice ps (removeone p ns)