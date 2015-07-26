maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum for empty list is undefined"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise =  maxTail
	where maxTail = maximum' xs
