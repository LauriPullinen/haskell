pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = first : pack rest
  where (first, rest) = span (==x) (x:xs)

-- run-length encoding
rlenc :: (Eq a) => [a] -> [(Int,a)]
rlenc list = map listToTuple packed
  where
    listToTuple = \list -> (length list, head list)
    packed = pack list
