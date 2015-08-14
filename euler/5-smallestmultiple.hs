import Data.List

divisibleByEach :: (Integral a) => [a] -> a -> Bool
divisibleByEach [] _     = True
divisibleByEach (x:xs) n = n `mod` x == 0 && divisibleByEach xs n

smallestDivisibleByEach :: (Integral a) => [a] -> Maybe a
smallestDivisibleByEach list = find (\x -> divisibleByEach list x) [1..]
