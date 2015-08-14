sumOfSquares :: (Integral a) => [a] -> a
sumOfSquares l = sum $ map (^2) l

squareOfSum :: (Integral a) => [a] -> a
squareOfSum l = sum l ^2

difference :: (Integral a) => [a] -> a
difference l = abs $ squareOfSum l - sumOfSquares l
