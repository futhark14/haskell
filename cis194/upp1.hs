 
toDigits :: Integer ->  [Integer]
toDigits n
       | n <= 0 = []
       | otherwise = tdAux n []

tdAux :: Integer -> [Integer] -> [Integer]
tdAux n ns
    | n < 10 = (n:ns)
    | otherwise = tdAux (n `div` 10) [(mod n 10)] ++ ns


--Det här var hur jag hade tänkt det initiellt men notera, jag hade skrivit:
-- (n `mod` 10): ns
--och det är fel utan ytterligare paranteser
real_tdAux :: Integer -> [Integer] -> [Integer]
real_tdAux n ns
        | n < 10 = (n:ns)
        | otherwise = real_tdAux (n `div` 10) ((n `mod` 10): ns)


digitsToRev :: Integer -> [Integer]
digitsToRev n = reverse $ toDigits n

--Det här, utan argument, kallas point-free style.
--Det fungerar dock inte om det vore
--digitsToRev' = reverse $ toDigits
--inte heller
--digitsToRev' n = reverse . toDigits n

digitsToRev' :: Integer -> [Integer]
digitsToRev' = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ go $ reverse xs
    where
        go [] = []
        go [x] = []
        go (x:y:xs) = [x, y*2] ++ go xs


doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' xs = reverse $ zipWith (*) oneTwo $ reverse xs where
    oneTwo = 1 : 2 : oneTwo


doubleEveryOther'' :: [Integer] -> [Integer]
doubleEveryOther'' =
  doubleEven . addIndexes
  where
    addIndexes = flip zip [1..]
    doubleEven = map (\(d, i) -> if even i then 2*d else d)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
        | x >= 10 = sumDigits (toDigits x ++ xs)
        | otherwise = x + sumDigits xs

sumDigits' :: [Integer] -> Integer
sumDigits' = sum . concat . map toDigits

sumDigits3 :: [Integer] -> Integer
sumDigits3 = sum . concat . map toDigits

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

--bra alternativ
validate':: Integer -> Bool
validate' x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

--x flyttat ut
validate4 :: Integer -> Bool
validate4 x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

validate3 :: Integer -> Bool
validate3  = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n = undefined


validateTest :: Bool
validateTest = and
  [
    True == validate 4012888888881881,
    False == validate 4012888888881882
  ]