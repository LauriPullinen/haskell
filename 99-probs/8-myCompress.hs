myCompress :: (Eq a) => [a] -> [a]
myCompress []         = []
myCompress (x:[])     = x:[]
myCompress (x:y:rest) = if x == y
  then myCompress (y:rest)
  else x:(myCompress (y:rest))
