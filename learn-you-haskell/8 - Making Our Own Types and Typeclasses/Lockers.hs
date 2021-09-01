import qualified Data.Map as Map

data LockerState = Taken | Free 
                    deriving (Eq, Show)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)


lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp id map =
    case Map.lookup id map of
        Nothing -> Left $ "The locker id " ++ show id ++ " is not present."
        Just (state, code) -> if state /= Taken
                                 then Right code
                                 else Left $ "The locker id " ++ show id ++ " is taken."


lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]  

clean :: (Either String Code) -> String
clean (Left a) = a
clean (Right a) = a
