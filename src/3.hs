elementAt  ::(Num b, Ord b) => [a] -> b -> a
elementAt [] _  =  error "empty list"
elementAt xs count     | count <  0 = elementAt  (reverse xs) (- (count + 1))
elementAt (x:_) count | count == 0 =  x
elementAt (_:xs) count | count >  0 = elementAt xs  (count - 1)
