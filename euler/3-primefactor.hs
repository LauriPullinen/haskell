half :: (Integral a) => a -> a
half n = quot n 2

isPrime :: (Integral a, Eq a) => a -> Bool
isPrime n = foldl' notDivisible True [2..half n]
  where
    notDivisible = (\prev x -> prev && n `mod` x /= 0)

getPrimeFactors :: (Integral a, Eq a) => a -> [a]
getPrimeFactors n = filter (\x -> isPrime x && n `mod` x == 0) [2..half n]
