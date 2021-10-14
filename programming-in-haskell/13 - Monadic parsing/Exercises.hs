import Parsers as P
-- Exercises

-- 1
notnewline :: P.Parser Char
notnewline = P.sat (/= '\n')

comment :: P.Parser ()
comment = do P.symbol "--"
             P.many (notnewline)
             P.char '\n'
             return ()

-- 5 (Currently causing Indentation issues inside do blocks)

-- data Expr = 
--       Add Expr Expr
--     | Mult Expr Expr
--     | Val Int
--     deriving Show

-- expr' :: Parser Expr
-- expr' = do t <- term'
--            do P.symbol "+"
--               e <- expr'
--               return (Add t e)
--            (P.<|>) return t

-- term' :: Parser Expr
-- term' = do f <- factor'
--            do P.symbol "*"
--               t <- term'
--               return (Mult f t)
--            (P.<|>) return f

-- factor' :: Parser Expr
-- factor' =
--     do P.symbol "("
--        e <- expr'
--        P.symbol ")"
--        return e
--     (P.<|>) fmap Val P.natural

-- 6 Done in ChapterExercisesTwo.hs
