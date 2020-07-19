describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                                [x] -> "singleton"
                                                xs -> "a longer list"