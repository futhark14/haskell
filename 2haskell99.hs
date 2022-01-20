{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- [x] bättre substitut till (x:[])
prob1 :: [a] -> a
prob1 [] = error "empty list"
prob1 [x] = x
prob1 (x:xs) = prob1 xs


prob2 :: [a] -> a
prob2[] = error "E"
prob2 xs = reverse xs !! 1


-- notera [x,_]
myButLast'' :: [p] -> p
myButLast'' [] = error "sdf"
myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs

prob3 :: [a] -> Integer -> a
prob3 [] _ = error "tom lista"
prob3 (x:xs) 1 = x
prob3 (x:xs) number = prob3 xs (number-1)


prob4 :: [a] -> Integer
prob4 [] = 0
prob4 [x] = 1
prob4 (x:xs) = 1 + prob4 xs


prob5 :: [Char] -> [Char]
prob5 [] = []
prob5 (x:xs) = prob5 xs ++ [x]


reversex :: [a] -> [a]
reversex list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

prob6 :: Eq a => [a] -> Bool
prob6 [] = False
prob6 xs | xs == reverse xs = True
         | otherwise = False