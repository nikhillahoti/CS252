
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
 | x == (last xs) && isPalindrome (take (length xs - 1) xs) = True
 | otherwise = False