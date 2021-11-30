
encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode lst = map (\lst -> (head lst, length lst)) (pack lst)

pack :: (Eq a) => [a] -> [[a]] 
pack [] = []
pack lst = reverse (tail_recur lst [])
  where
    tail_recur:: (Eq a) => [a] ->[[a]] -> [[a]]
    tail_recur [] res = res
    tail_recur (x:xs) [] = tail_recur xs [[x]]
    tail_recur (x:xs) ((y:ys):res) = if x==y
                                     then tail_recur xs ((x:y:ys):res)
                                     else tail_recur xs ([x]:(y:ys):res)
