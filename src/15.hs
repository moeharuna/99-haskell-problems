repli :: [a] -> Integer -> [a]
repli lst count  = concatMap (rep count) lst
  where
    rep :: Integer -> a -> [a]
    rep 0 x = []
    rep n x = x:rep (n-1) x
