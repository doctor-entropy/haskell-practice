import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, gen') = random gen
        (secondCoin, gen'') = random gen'
        (thirdCoin, gen''') = random gen''
    in (firstCoin, secondCoin, thirdCoin)
