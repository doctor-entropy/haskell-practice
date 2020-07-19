calcBMI :: (RealFloat a) => [(a, a)] -> [a]
calcBMI xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height^2