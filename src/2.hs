myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "need at least 2 arguments"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs
