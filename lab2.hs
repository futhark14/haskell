import Data.List
circleArea :: Double -> Double
circleArea 0 = 0
circleArea r | r < 0 = error "negative length"
circleArea r = pi * r * r

squareArea :: Double -> Double
squareArea 0 = 0
squareArea r = r * r

squareCircleArea :: Double -> Double
squareCircleArea r = r * 2 * r - pi * r * r

circleSquareArea :: Double -> Double
circleSquareArea r = pi * r * r - sqrt (r * r + r * r)

rhymes :: String -> String -> Bool
rhymes str1 str2
  | (length str1 < 3 || length str2 < 3) && str1 == str2 = True
  | drop (length str1 - 3) str1 == drop (length str2 - 3) str2 = True
  | otherwise = False

{-
drJeep :: String -> String -> Bool
drJeep x y =
  not (length x < length y) && drop (length x - length y) x == y
-}

updatePersonName :: (String, String, String, Int, String) -> Int -> String -> (String, String, String, Int, String)
updatePersonName tupelX@(familyName, givenName, a, b, c) talX stringX
  | talX == 1 = (familyName, stringX, a, b, c)
  | talX == 2 = (stringX, givenName, a, b, c)
  | otherwise = tupelX

{- MyReplicate Times Element
 Makes a list of n many times of Element
 Pre: Times>=0
 Returns: A list of Element n many times
 Example: myReplicate 5 42 == [42,42,42,42,42]
          myReplicate 2 50 == [50,50]
 -}

myReplicate :: Integer -> a -> [a]
myReplicate n rep
  | n <= 0 = []
  | otherwise = rep : myReplicate (n -1) rep

{- fromTo low high
creates a list that with ever number of low to high
pre: The beginning of the sequence must be lower than the end
return: A list of every number between low and high including themselves
examples: fromTo 0 6 == [0,1,2,3,4,5,6]
          fromTo 9 7 == []
-}

fromTo :: Integer -> Integer -> [Integer]
fromTo low high
  | low > high = []
  | otherwise = low : fromTo (low + 1) high

{-isPrefix Mainstring String2
 Checks if string2 is a prefix of Mainstring
 pre: Mainstring /= []
 return: True if string2 is a prefix of Mainstring, false otherwise
 Example: isPrefix "Jultomte" "Jul" == True
          isPrefix "jultomte" "tomte" == False
-}


isPrefix :: String -> String -> Bool
isPrefix s1 [] = True
isPrefix [] s2 = False
isPrefix s1 s2
  | head s1 /= head s2 = False
  | otherwise = isPrefix (tail s1) (tail s2)

{- isSubstring Mainstring substring
isSubstring compares two different strings and checks if the second string is a substring of the first one
pre: substring  /= []
return: True if substring is a substring of Mainstring, false otherwise
Example: isSubstring "Jultomte" "tomte" ==True
         isSubstring "Jultomte" "x" == False
-}

isSubstring :: String -> String -> Bool
isSubstring [] subString = False
isSubstring mainString subString
  | isPrefix mainString subString = True
  | otherwise = isSubstring (tail mainString) subString


myInit :: [a] -> [a]
myInit [] = []
myInit [x] = []
myInit (x:xs) = x : myInit xs



stringOfInteger :: Integer -> String
stringOfInteger 0 = "0"
stringOfInteger 1 = "1"
stringOfInteger 2 = "2"
stringOfInteger 3 = "3"
stringOfInteger 4 = "4"
stringOfInteger 5 = "5"
stringOfInteger 6 = "6"
stringOfInteger 7 = "7"
stringOfInteger 8 = "8"
stringOfInteger 9 = "9"
stringOfInteger n
                  | n > 9 = stringOfInteger (n `div` 10) ++ stringOfInteger(n `mod` 10)
                  | n < 0 = "-" ++ stringOfInteger(abs n)


binaryConverter :: Num p => [Char] -> p
binaryConverter binary =
     if binary == ""
         then 0
         else binaryConverter (take (length binary - 1) binary) * 2 +
             if drop (length binary - 1) binary == "0"
                 then 0
                 else 1



sCounter :: String -> String -> Integer -> Integer
sCounter [] subString n = -1
sCounter mainString subString n | isPrefix mainString subString = n
                                | otherwise = sCounter (tail mainString) subString (n+1)

searchString :: String -> String -> Integer
searchString mainString subString = sCounter mainString subString 0



reversex :: [a] -> [a]
reversex list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

fromDecimals :: [Integer] -> Integer
fromDecimals [] = 0
fromDecimals (x:xs) = aux (x:xs) 0
    where
        aux [] n = n
        aux (x:xs) n = aux xs (n + (x * 10^length xs))

squareOfEven1 :: [Integer] -> [Integer]
squareOfEven1 [] = []
squareOfEven1 (x:xs) | x `mod` 2 == 0 = x^2 : squareOfEven1 xs
                     | otherwise = squareOfEven1 xs

squareOfEven2 :: [Integer] -> [Integer]
squareOfEven2 xs = [x^2 | x <- xs, x `mod` 2 == 0]


returnSameList :: Integer -> [Integer] -> [Integer]
returnSameList 0 (x:xs) = x:xs
returnSameList n (x:xs) = returnSameList (n-1) (x:xs)

returnChangedList :: Integer -> [Integer] -> [Integer]
returnChangedList 0 (x:xs) = x:xs
returnChangedList n (x:xs) = returnChangedList (n-1) (n:x:xs)


iKnowThis :: Integer -> [Integer] -> [Integer]
iKnowThis n xs = returnChangedList n xs

makeNumbersList :: Integer -> [Integer] -> [Integer]
makeNumbersList n xs = n : makeNumbersList (n-1) xs


callFun :: Integer -> [Integer]
callFun n = returnChangedList n []

forever :: Integer -> [Integer] -> [Integer]
forever n xs = forever (n-1) xs




myOtherLength :: [Integer] -> Integer
myOtherLength [] = 0
myOtherLength (x:xs) = 1 + myOtherLength xs

addFun :: [Integer] -> Integer
addFun [] = 0
addFun (x:xs) = x + addFun xs

addFirst :: [(Integer, Integer)] -> Integer
addFirst [] = 0
addFirst (x:xs) = fst x + addFirst xs

isElementOf :: Integer -> [Integer] -> Bool
isElementOf n [] = False
isElementOf n (x:xs) | n == x = True
                     | otherwise = isElementOf n xs


countElement :: Integer -> [Integer] -> Integer
countElement n [] = 0
countElement n (x:xs) | n == x = 1 + countElement n xs
                      | otherwise = countElement n xs


getElementList :: Integer -> [Integer] -> [Integer]
getElementList n [] = []
getElementList n (x:xs) | n == x = x : getElementList n xs
                        | otherwise = getElementList n xs


getElementList' :: Int -> [Int] -> [Int]
getElementList' n list = filter (== n) list


getElementList'' :: Int -> [Int] -> [Int]
getElementList'' n list = [x | x <- list, x == n]


removeElement :: Integer -> [Integer] -> [Integer]
removeElement n [] = []
removeElement n (x:xs) = [ x | x <- (x:xs), n /= x]


getLastN :: Int -> [Char] -> [Char]
getLastN _ [] = []
getLastN 0 (x:xs) = (x:xs)
getLastN n (x:xs) | n >= length (x:xs) = error "olmazz"
                  | otherwise = getLastN (n-1) xs


countRightTuples :: Int -> [(Int, Int)] -> Int
countRightTuples n [] = 0
countRightTuples n (x:xs) | n == fst x || n == snd x = 1 + countRightTuples n xs
                          | otherwise = countRightTuples n xs



-- viktigt exempel

countRightTuples' :: Int -> [(Int, Int)] -> Int
countRightTuples' _ [] = 0
countRightTuples' n ((a,b):xs) | (a == n || b == n) = 1 + countRightTuples' n xs
                               | otherwise = countRightTuples' n xs


cumulatedLength :: [(Int, ([Int], Int))] -> Int
cumulatedLength [] = 0
cumulatedLength ((x, (ys, y)):xs) = length ys + cumulatedLength xs


concatenateTwoStrings :: [Char] -> [Char] -> [Char]
concatenateTwoStrings [] [] = []
concatenateTwoStrings (x:xs) [] = x:xs
concatenateTwoStrings [] (y:ys) = y:ys
concatenateTwoStrings (x:xs) (y:ys) = x : concatenateTwoStrings xs (y:ys)

reverseIntList :: [Int] -> [Int]
reverseIntList [] = []
reverseIntList (x:xs) = reverseIntList xs ++ [x]


--jag glömmer altid otherwise =...
equalString :: String -> String -> Bool
equalString [] [] = True
equalString _ [] = False
equalString [] _ = False
equalString (x:xs) (y:ys) | x == y && equalString xs ys = True
                          | otherwise = False



myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ init_val []     = init_val
myfoldr f init_val (x:xs) = f x (myfoldr f init_val xs)

addNum' :: Num a => [a] -> a
addNum' = foldr (+) 0


addNum :: Num a => [a] -> a
addNum [] = 0
addNum (x:xs) = x + addNum xs


multiplyNum :: Int -> [Int] -> Int
multiplyNum n xs = n * addNum xs


-- the problem is that the ghciHelper has Integral that belongs to the set of Num
-- if ghciHelper had the type Num, then it would work. 
-- the callar can be narrower, but not wider
ghciHelper :: Integral a => [a] -> a
ghciHelper [] = 0
ghciHelper (x:xs) = 1 + ghciHelper xs


{-ghciAngry :: Num a => [a] -> a
ghciAngry numList = ghciHelper numList-}


ghciHappy :: [Int] -> Int
ghciHappy intList = ghciHelper intList



addSum :: Num a => (a -> a -> a) -> [a] -> a
addSum _ [] = 0
addSum (+) (x:xs) = (+) x (addSum (+) xs)


addSum' :: Num a => (a -> a -> a) -> [a] -> a
addSum' _ [] = 0
addSum' (+) (x:xs) = x + (addSum' (+) xs)


moreMath :: Num a => (a -> a -> a) -> [a] -> a
moreMath _ [] = 0
moreMath f (x:xs) = f x (moreMath f xs)


makeString :: Show a => [a] -> String
makeString [] = ""
makeString (x:xs) = show x ++ makeString xs

flipArguments :: (a -> b -> c) -> b -> a -> c
flipArguments f x y = f y x

sizeList :: [String] -> [Int]
sizeList [] = []
sizeList (x:xs) = length x : sizeList xs

multiples :: Int -> [Int] -> [Int]
multiples n [] = []
multiples n xs = filter ((==0) . (`mod` n)) xs

myConcat :: [String] -> String
myConcat xss = foldr (++) [] xss

bowman poole [floyd] = floyd
bowman poole (floyd:hal) =  poole floyd (bowman poole hal)
-- bowman (/) [2.5, 2.0, 0.2] == 0.25 (the example and its result)
-- bowman (/) [2.5, 2.0, 0.2]
-- / 2.5  bowman (/) [2.0, 0.2]
-- / 2.0 / 2.5 bowman (/) [0.2]
-- 0.2 / 2.0 / 2.5

func f [x] = x
func f (x:xs) = f x (func f xs)



-- (add1) . (add1) $ 0 fungerar
-- add1.add1 $ 0 fungerar
-- men add1 . add1 $ 0 fungerar inte
-- notera mellanslagen
add1 :: Num a => a -> a
add1 = (+ 1)


multiplyByTwo :: Num a => a -> a
multiplyByTwo = (* 2)


add2 :: Num a => a -> a
add2 n = add1 n

doFourTimes :: (a -> a) -> a -> a
doFourTimes f x = f.f.f.f $ x


myMap :: (a -> a) -> [a] -> [a]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFoldRight :: (a -> b -> b) -> b -> [a] -> b
myFoldRight _ acc [] = acc
myFoldRight f acc (x:xs) = f x (myFoldRight f acc xs)



sortTuplesList :: Ord a => [(a, a)] -> [(a, a)]
sortTuplesList list = sortBy (\ (_, y) (_, z) -> compare y z) list



myLength :: Int -> [Int] -> Int
myLength acc [] = acc
myLength acc (x:xs) = myLength (acc + 1) xs


lengthOfList :: [Int] -> Int
lengthOfList xs = myLength 0 xs


someMath :: [Int] -> [Int]
someMath [] = []
someMath (x:xs) | x `mod` 2 == 0 = x*5 : 0 : someMath xs
                | otherwise = x : someMath xs


someMathAux :: Int -> [Int]
someMathAux n | (mod n 2 == 0) = (n * 5) : [0]
              | otherwise = [n]
someMath' [] = []
someMath' (x:xs) = someMathAux x ++ someMath' xs


doStuffAux :: Int -> Int
doStuffAux n | n > 10 = 1
          | otherwise = 0

doStuff ::  [Int] -> [Int]
doStuff [] = []
doStuff (x:xs) = doStuffAux x : doStuff xs

myMergeAux :: [Int] -> [Int] -> [Int]
myMergeAux xs [] = xs
myMergeAux [] ys = ys
myMergeAux (x:xs) (y:ys) | x < y = x : myMergeAux xs (y:ys)
                         | otherwise = y : myMergeAux (x:xs) ys
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort list = myMergeAux (myMergeSort (take (div (length list) 2) list)) (myMergeSort (drop (div (length list) 2) list))


reverseIntList' :: [Int] -> [Int] -> [Int]
reverseIntList' [] acc = acc
reverseIntList' (x:xs) acc = reverseIntList' xs (x:acc)

nonsenseFun :: [Int] -> Int
nonsenseFun [] = 0
nonsenseFun (x:xs) =
   let list = reverseIntList' (x:xs) []
   in if head list >= 10
      then fib 10
      else fib 5
   where
      fib 0 = 0
      fib 1 = 1
      fib n = fib (n - 1) + fib (n - 2)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


nonsenseAux :: [Int] -> Int
nonsenseAux (x:xs) | x >= 10 = fib 10
                   | otherwise = fib 5


nonsenseFun' :: [Int] -> Int
nonsenseFun' [] = 0
nonsenseFun' (x:xs) = nonsenseAux (reverseIntList' (x:xs) [])
