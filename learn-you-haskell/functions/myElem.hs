myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem n (x:xs)
	| n == x = True
	| otherwise = n `myElem` xs