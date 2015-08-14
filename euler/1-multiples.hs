-- Finds the sum of all multiples of 3 or 5 below n
multiples :: (Integral a) => a -> a
multiples n = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..n-1]
