replicateList :: (Num i, Ord i) => i -> a -> [a]
replicateList n x
	| n <= 0 = []
	| otherwise = x:replicateList (n-1) x