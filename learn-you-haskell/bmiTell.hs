bmiTell :: (RealFloat a) => a -> [Char]
bmiTell bmi
    | bmi <= 18.5 = "You are underweight"
    | bmi <= 25.0 = "You are in shape"
    | bmi <= 30.0 = "You are overweight"
    | otherwise = "You are a whale"