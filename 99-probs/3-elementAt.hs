elementAt :: (Integral b) => [a] -> b -> a
elementAt [] _ = error "Empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i - 1)
