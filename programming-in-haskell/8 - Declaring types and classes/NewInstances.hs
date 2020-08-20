{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Eq (..), undefined, Bool (..), Show, (&&))

data Maybe a = Nothing | Just a

-- 7
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Nothing == (Just a) = False
    (Just a) == Nothing = False
    (Just a) == (Just b) = a == b

data List a = Empty | Cons a (List a)
                    deriving Show

instance Eq a => Eq (List a) where
    Empty == Empty = True
    Empty == a = False
    a == Empty = False
    (Cons a as) == (Cons b bs) = (a == b && (as == bs))
