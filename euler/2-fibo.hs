
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

-- Finds the sum of fibonacci numbers under n
evenFiboUnder :: (Integral a) => a -> a
evenFiboUnder n = sum $ takeWhile (<n) evenFibos
  where
    fibos = map fibonacci [0..]
    evenFibos = filter even fibos
