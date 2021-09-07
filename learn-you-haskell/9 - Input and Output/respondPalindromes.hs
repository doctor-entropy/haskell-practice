main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes =
    unlines . map (\xs -> if isPalindrome xs then "is a palindrome" else "not a palindrome") . lines
        where isPalindrome xs = xs == reverse xs