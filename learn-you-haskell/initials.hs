initials :: [Char] -> [Char] -> [Char]
initials firstName lastName = [f] ++ ". " ++ [l]
    where (f:_) = firstName
          (l:_) = lastName