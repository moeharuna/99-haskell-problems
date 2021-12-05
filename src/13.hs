data Count a = Multiple Int a |
               Single a
  deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Count a]
encodeDirect xs = reverse (tail xs [] 0)
  where
    tail  :: (Eq a) => [a] -> [Count a] -> Int -> [Count a]
    tail [] result _  = result
    tail [x] result 0 = Single x:result
    tail [x] result n = Multiple  (n+1) x:result
    tail (x:y:xs) result 0 | x/=y = tail (y:xs) (Single x:result) 0
    tail (x:y:xs) result n | x/=y = tail (y:xs) (Multiple (n+1) x:result) 0
    tail (x:y:xs) result n | x==y = tail (y:xs) result $ n+1

encodeDirectFold :: (Eq a) => [a] -> [Count a]
encodeDirectFold = foldr
                   (\x y -> case y of
                       [] -> [Single x]
                       (Single y:ys)          | x==y -> Multiple 2 x:ys
                       lst@(Single y:_)       | x/=y -> Single x:lst
                       (Multiple num y:ys)    | x==y -> Multiple (num+1) y:ys
                       lst@(Multiple _ y:_)   | x/=y -> Single x:lst)
                   []
