pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = first : pack rest
  where (first, rest) = span (==x) (x:xs)
