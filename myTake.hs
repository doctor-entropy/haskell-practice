myTake :: (Num i, Ord i) => i -> [a] -> [a]
myTake n _
	| n <= 0 = []
myTake _ [] = []
myTake n (x:xs) = x:myTake (n-1) xs