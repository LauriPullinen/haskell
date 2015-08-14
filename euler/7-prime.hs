isPrime :: (Integral a, Eq a) => a -> Bool
isPrime n = foldl notDivisible True [2..quot n 2]
  where
    notDivisible = (\prev x -> prev && n `mod` x /= 0)

nthPrime :: Int -> Int
nthPrime n = filter isPrime [1..] !! (n)
