import Data.List

solveRPN :: (Num a, Read a, Fractional a, Floating a) => String -> a
solveRPN = head . foldl rpn [] . words
  where
    rpn (x:y:ys) "+" = (y + x):ys
    rpn (x:y:ys) "-" = (y - x):ys
    rpn (x:y:ys) "*" = (y * x):ys
    rpn (x:y:ys) "/" = (y / x):ys
    rpn (x:y:ys) "^" = (y ** x):ys
    rpn (x:xs) "ln"  = (log x):xs
    rpn (x:xs) "log" = (log x):xs
    rpn stack "sum"  = [sum stack]
    rpn stack number = (read number):stack
