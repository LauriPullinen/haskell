isPalindrome :: String -> Bool
isPalindrome []     = True
isPalindrome (_:[]) = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)

numIsPalindrome :: (Show a) => a -> Bool
numIsPalindrome x = isPalindrome (show x)

palindromeProduct :: (Integral a, Show a) => a -> a
palindromeProduct n = maximum $ filter numIsPalindrome products
  where
    tripleDigits = [10^(n - 1) .. 10^n - 1]
    products = (*) <$> tripleDigits <*> tripleDigits
