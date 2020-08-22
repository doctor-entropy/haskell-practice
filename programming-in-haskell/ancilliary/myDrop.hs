myDrop :: Integral b => b -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

myInit :: Int -> [a] -> [a]
myInit 1 (x:xs) = [x]
myInit _ [] = []
myInit n (x:xs) = [x] ++ myInit (n-1) xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs
