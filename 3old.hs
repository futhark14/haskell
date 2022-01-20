 

isExpensive :: (String, Int) -> Bool
isExpensive (strnq, price) 
 | price > 100 = True
 | otherwise = False


aand :: (Bool, Bool, Bool) -> Bool
aand (b1, b2, b3)
 | b1 /= True = False
 | b2 /= True = False
 | b1 /= b3 = False
 | otherwise = True

matchNthChar :: Int -> String -> String -> Bool
matchNthChar n x y
    | (length x >= n && length x > 0) && (length y >= n && length y > 0) = x!!(n-1) == y!!(n-1)
    | otherwise = False


-- Uppgift 2



-- Labb 4:

prep4x1 :: Eq a => a -> [a] -> Int
prep4x1 _ [] = 0
prep4x1 elemX (x:xs)
 | elemX == x = 1 + (prep4x1 elemX xs)
 | otherwise = prep4x1 elemX xs

prep4x2 :: [Int] -> [Int]
prep4x2 [] = []
prep4x2 (x:xs)
 | odd x = 0 : (prep4x2 xs)
 | otherwise = 1 : prep4x2 xs

returnSameList' :: Int -> [Int] -> [Int]
returnSameList' n listX@(x:xs) 
 | n == 0 = (x:xs)
 | otherwise = returnSameList' (n-1) listX



-- Ett viktigt exempel. Den första funktionen är på formen n, n-1, n-2, n-3...a, b, c Den andra funktionen är på formen ... n-3, n-2, n-1, n, a, b, c
returnChangedList' :: Int -> [Int] -> [Int]
returnChangedList' 0 (x:xs) = 0 :(x:xs)
returnChangedList' n (x:xs) = n : (returnChangedList' (n-1) (x:xs))

returnChangedList :: Int -> [Int] -> [Int]
returnChangedList 0 list = 0 : list
returnChangedList n list = returnChangedList (n - 1) (n : list)


iKnowThis' :: Int -> [Int] -> [Int]
iKnowThis' 0 lizt@(x:xs) = 0 : lizt
iKnowThis' n lizt = iKnowThis' (n-1) (n: lizt)

iKnowThis :: Int -> [Int] -> [Int]
iKnowThis n list = returnChangedList n list

makeNumbersList' :: Int -> [Int] -> [Int]
makeNumbersList' n lizt = returnChangedList' n lizt

callFun' :: Int -> [Int] -> [Int]
callFun' n lizt = returnChangedList n lizt

callFun :: Int -> [Int]
callFun n = returnChangedList n []

foreverx' :: Int -> [Int] -> [Int]
foreverx' n (x:xs) = x*n : (foreverx' (n-1) (xs))

myLength' :: Int -> [Int] -> Int
myLength' n [] = n
myLength' n lizt@(x:xs) = myLength' (n+1) xs

myOtherLength' :: [Int] -> Int
myOtherLength' [] = 0
myOtherLength' (x:xs) = 1 + myOtherLength' xs

addFun' :: [Int] -> Int
addFun' [] = 0
addFun' (x:xs) = x + (addFun' xs)


-- Notera, I detta fall är x:xs en lista av tuplar. 
--Vore det t.ex mönster matchning med exFunc (a,b,c) = a + addFirst' xs skulle vara möjlig men eftersom x i detta fall är en två-tupel så måste man specifiera fst
addFirst' :: [(Int, Int)] -> Int
addFirst' [] = 0
addFirst' (x:xs) = fst x + (addFirst' xs)

--Labb 4:

--Notera, hade man skrivit myReplicate 0 toRep = toRep blir det fel Det bör vara myReplicate 0 toRep = [toRep]
{- MyReplicate Times Element
 Makes a list of n many times of Element
 Pre: Times>=0
 Returns: A list of Element n many times
 Example: myReplicate 5 42 == [42,42,42,42,42]
          myReplicate 2 50 == [50,50] -}
myReplicate :: Integer -> a -> [a]
--variant: n
myReplicate 0 toRep = []
myReplicate n toRep = toRep : myReplicate (n-1) toRep


{- fromTo low high
creates a list that with ever number of low to high
pre: The beginning of the sequence must be lower than the end
return: A list of every number between low and high including themselves
examples: fromTo 0 6 == [0,1,2,3,4,5,6]
          fromTo 9 7 == []
-}

fromTo :: Integer -> Integer -> [Integer]
fromTo i n
 | i > n = []
 | i <= n = i : fromTo (i+1) n



isPrefix :: String -> String -> Bool
isPrefix st1 [] = True
isPrefix [] st2 = False
isPrefix st1@(x:xs) st2@(y:ys)
 | x == y && isPrefix xs ys = True
 | otherwise = False

isSubstring :: String -> String -> Bool
isSubstring [] subS = False -- det här är basfallet, utan den blir det non-exhaustive
isSubstring mainS@(x:xs) subS@(y:ys)
 | isPrefix mainS subS == True = True
 | isSubstring xs subS == True = True -- här
 | otherwise = False
--där det står här hade jag använt isPrefix
--Men problemet med det är att vore det "abcde" "de" så hade det blivit "bcde" "de" och 
--programmet skulle kontrollera om "de" är prefix till "bcde", vilket det ju inte är 
-- och varje gång ge false.
-- Det här handlar ju om rekursion, så man måste varje gång ta bort head av mainS och jämföra med subS
--genom isPrefix. om isprefix ger True genom isSubstring calls, då är det ju en subS

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
stringOfInteger x
 | x <= 0 = "-" ++ deComp x
 | x > 9 = deComp x
     where
         deComp x = stringOfInteger (x `div` 10) ++ stringOfInteger (x `mod` 10)


isElementOf :: Integer -> [Integer] -> Bool
isElementOf n [] = False
isElementOf n (x:xs)
 | n == x = True
 | n /= x = isElementOf n xs
 | otherwise = False

 
countElement :: Integer ->  Integer -> [Integer] -> Integer
countElement n counter [] = counter
countElement n counter (x:xs)
 | n == x = countElement n (counter+1) xs
 | n /= x = countElement n counter xs

-- lärarens ex är bättre

countElement' :: Int -> [Int] -> Int
countElement' _ [] = 0
countElement' n (x:xs) | x == n = 1 + countElement' n xs
                      | otherwise = countElement' n xs

getElementList :: Int -> [Int] -> [Int]
getElementList n [] = []
getElementList n (x:xs)
 | n == x = [x] ++ getElementList n xs
 | otherwise = getElementList n xs


removeElement :: Integer -> [Integer] -> [Integer]
removeElement n [] = []
removeElement n (x:xs)
 | n /= x = x : removeElement n xs
 | otherwise = removeElement n xs


getLastN :: Int -> String -> String
getLastN n [] = []
getLastN 0 all@(x:xs) = (x:xs)
getLastN n all@(x:xs) | n >= (length all) = error "..."
getLastN n all@(x:xs) = getLastN (lastN-1) xs
 where
     lastN = length all - n


-- lärarens
getLastN' :: Int -> [Char] -> [Char]
getLastN' _ [] = []
getLastN' n (x:xs) | length (x:xs) <= n = (x:xs)
                  | otherwise = getLastN' n xs


--viktigt exempel om en lista av tupler

countRightTuples :: Int -> [(Int, Int)] -> Int
countRightTuples n [] = 0
countRightTuples n ((a,b):xs)
 | n == a || n == b = 1 + countRightTuples n xs
 | otherwise = countRightTuples n xs


only_even :: [Int] -> [Int]
only_even [] = []
only_even (x:xs) = [x | x <- xs, x `mod` 2 == 0]

select_second :: [a] -> a
select_second [] = error "xxxx"
select_second (x:y:xs) = y

select_second' :: [a] -> a
select_second' [] = error "xxx"
select_second' listX = head $ drop 1 listX


fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fib' = (fibs!!)
  where fibs = map fib [0..]


 

 

myInit :: [a] -> [a]
myInit [x] = []
myInit (x:xs) = x : myInit xs



multiply x y =
    case x * y of
        1 -> "one"
        2 -> "two"
        x -> show x

 
{- f xs ys 

 

   PRE: %PRECONDITION%
   RETURNS:
 -} 

f :: Ord a => [a] -> [a] -> [a]
f xs [] = xs
f [] ys = ys
f (x:xs) (y:ys)
  | x > y     = x : f xs ys
  | otherwise = y : f xs ys



fn 1 = 1
fn n = sum $ map fn [(n `div` 2)..n-1]

fn' 1 = 1

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs  
