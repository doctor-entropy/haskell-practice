import Data.Char
import CaesarsCipher

freqTable = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
             0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
             6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count :: Char -> [Char] -> Int
count x xs = length [x | x' <- xs, x' == x]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

freqs :: [Char] -> [Float]
freqs xs = [percent (count x xs) m | x <- ['a'..'z']]
            where m = lowers xs

chiSqr :: [Float] -> [Float] -> Float
chiSqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- /////////////////////////////////////////////////////////

position :: Eq a => a -> [a] -> [Int]
position x xs = [i | (x', i) <- zip xs [0..], x' == x]

-- ////////////////////////////////////////////////////////

crack :: [Char] -> (Int, [Char]) 
crack xs = (factor, encode (-factor) xs)
            where 
                factor = head (position (minimum chiTab) chiTab)
                chiTab = [chiSqr (rotate n table') freqTable | n <- [0..25]]
                table' = freqs xs
