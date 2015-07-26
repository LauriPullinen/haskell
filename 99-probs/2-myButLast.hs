myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (_:[]) = error "Only one element in list"
myButLast (x:_:[]) = x
myButLast (_:_:xs) = myButLast xs
