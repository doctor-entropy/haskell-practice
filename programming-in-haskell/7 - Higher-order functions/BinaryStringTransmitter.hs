import Data.Char

type Bit = Int

-- ENCODING

-- Example (Reversing binary digits for easy definiton of function)
-- 1101 = (1 * 1) + (2 * 1) + (4 * 0) + (8 * 1)
bit2int :: [Bit] -> Int
bit2int bits = sum [w * b | (w, b) <- zip weights bits]
                      where weights = iterate (*2) 1

-- Same function as above but using lambda expressions
-- abcd (1 * a) + (2 * b) + (4 * c) + (8*d)
-- a + 2 * (b + 2 * (c + 2 * (d + 2 * (0))))
bit2int' :: [Bit] -> Int
bit2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

-- Make to eight bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- TRANSMISSION
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- DECODING
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode bits = map (chr . bit2int') (chop8 bits)

-- Simulate transmission channel

channel :: [Bit] -> [Bit]
channel = id -- Perfect Transmission

transmit :: String -> String
transmit = decode . channel . encode
