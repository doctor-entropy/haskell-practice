-- Chpater problem
-- Tautology checker

type Bits = Int
int2bin :: Int -> [Bits]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

bools' :: Int -> [[Bool]]
bools' n = map (reverse . map conv . make n . int2bin) range
            where 
                range = [0..(2^n)-1]
                make n bs = take n (bs ++ repeat 0)
                conv 0 = False
                conv 1 = True

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n - 1)
