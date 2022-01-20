{-# LANGUAGE OverloadedStrings #-}

module Functions(yahoo, unMaybe, makeFloat, supportAndResistanceValues,fibonacciVerticalValue, inflectionPoints, inRange, leastSquareSlope, stockBotEval) where

import Text.HTML.Scalpel(scrapeURL,text,(@:),(@=),URL)
import Test.HUnit

-----------------------------------------------------------------------------------------
------------------------------------- StockBot ------------------------------------------
------------- Albin Åberg Dahlberg, Lukas Lindén Thöming, Felix Agnerdahl ---------------
------------------------------------- Group 24 ------------------------------------------
----------------------------------------------------------------------------------------- 

{- Price represents the price of a stock as a Float
   INVARIANT: 
-}
type Price = Float

{- Evaluation is represented by a string
   INVARIANT: 
   The string is the evaluation of a stock
-}
type Evaluation = String

-- The website that is scraped.
yahoo :: URL
yahoo = "https://finance.yahoo.com/quote/"

{- unMaybe mby
   Extracts the string from mby.
   RETURNS: A string. If mby is Nothing, an empty string.
   EXAMPLES: unMaybe (Just "hej") == "hej"
             unMaybe Nothing      == ""
-}
unMaybe :: Maybe String -> String 
unMaybe (Just str) = str
unMaybe Nothing = ""

{- makeFloat mby
   Converts mby into a float.
   PRE: The string must not contain anything other than a float number.
   RETURNS: A float number.
   EXAMPLES: makeFloat (Just "hej")  == *** Exception: Prelude.read: no parse
             makeFloat (Just "1.23") == 1.23
             makeFloat (Just "0") == 0.0
   SIDE EFFECTS: if Nothing is the argument, an Exception is thrown for non-exhaustive patterns
-}
makeFloat :: Maybe String -> Float
makeFloat (Just str) = read str

{- supportAndResistanceValues Value
   Takes Value and creates a list with certain percentages of Value.
   RETURNS: A list of specific percentages of the inserted number.
   EXAMPLES: supportAndResistanceValues 10     == [7.64,6.18,5.0,3.82]
             supportAndResistanceValues 152.33 == [116.38012,94.13994,76.165,58.19006]
             supportAndResistanceValues 0      == [0.0,0.0,0.0,0.0]
-}
supportAndResistanceValues :: Price -> [Float]
supportAndResistanceValues v = v * 0.764 : v * 0.618 :  v * 0.5 : [v * 0.382]

{- fibonacciVerticalValue Values
   Yields the vertical distance between extreme points of a stock.
   RETURNS: The distance between the largest and smallest number in Values.
   EXAMPLES: fibonacciVerticalValue [3,6,10]               == 7.0
             fibonacciVerticalValue [58,32,76,13,15,16,71] == 63.0
-}
fibonacciVerticalValue :: [Price] -> Float
fibonacciVerticalValue xs = maximum xs - minimum xs

{- inflectionPoints Values
   Takes a list of prices and generates a list of support and resistance values 
   which in turn are substituted off of the largest number in Values.
   RETURNS: A list of inflection points.
   EXAMPLES: inflectionPoints [12,6,32,8] == [12.136,15.932001,19.0,22.068]
             inflectionPoints [0,0,0,0]   == [0.0,0.0,0.0,0.0]
-}
inflectionPoints :: [Price] -> [Float]
inflectionPoints [] = []
inflectionPoints xs = inflectionPoints' (maximum xs) (supportAndResistanceValues $ fibonacciVerticalValue xs)
                        where
                           {- inflectionPoints' Value Values
                              Subtracts all numbers in Values off of Value one by one and creates a list of the results.
                              RETURNS: A list of Float numbers.
                              EXAMPLES: inflectionPoints' 5.2 [1,2,3]     == [4.2,3.2,2.2]
                                        inflectionPoints' 13 [5,8,3,12,1] == [8.0,5.0,10.0,1.0,12.0]
                           -}
                           inflectionPoints' :: Price -> [Float] -> [Float]
                           inflectionPoints' highPrice xs = map (highPrice -) xs

{- inRange Value Values
   Checks whether Value is close to any number in Values.
   RETURNS: True if Value is within a 5% distance of any number in Values, otherwise False.
   EXAMPLES: inRange 5.0 [4,6,8,9] == False
             inRange 5.9 [4,6,8,9] == True
-}
inRange :: Price -> [Float] -> Bool
inRange v = any (\x -> v >= x*0.95 && v <= x*1.05)

{- leastSquareSlope Values
   Calculates the slope of a linear regression line by using the least squares method on the elements in Values.
   PRE: To get an accurate representation of the lines slope the list needs to contain 5 elements.
   RETURNS: The slope of a linear regression line.
   EXAMPLES: leastSquareSlope [15,12,20,34,25] == 4.2
             leastSquareSlope [10,20,30,40,50] == 10.0
             leastSquareSlope [5,4,3,2,1] == -1.0
-}
leastSquareSlope :: [Price] -> Float
leastSquareSlope xs = sum (zipWith (*) [-2,-1,0,1,2] (map (\ x -> x - (sum xs / 5)) xs)) / 10

{- stockBotEval Value 
   Checks if Value is within range of the inflection points and if the trendline is positive or negative.
   PRE: For an accurate response, list contains 5 elements.
   RETURNS: A message based on the functions results.
   EXAMPLES: stockBotEval 5 [4,5,6,7,4] == "Sell / Don't buy"
             stockBotEval 6 [8,5,6,5,4] == "Buy / Hold"
             stockBotEval 4 [8,5,6,6,5] == "The stock is likely to keep moving in a negative direction."
-}
stockBotEval :: Price -> [Price] -> Evaluation
stockBotEval v xs
   | inRange v (inflectionPoints xs) && leastSquareSlope xs >= 0 = "Sell / Don't buy"
   | inRange v (inflectionPoints xs) && leastSquareSlope xs < 0 = "Buy / Hold"
   | not (inRange v (inflectionPoints xs)) && leastSquareSlope xs >= 0 = "The stock is likely to keep moving in a positive direction."
   | not (inRange v (inflectionPoints xs)) && leastSquareSlope xs < 0 = "The stock is likely to keep moving in a negative direction."

-------------------------------------------------------------------------
--------------------------- Test Cases ----------------------------------
-------------------------------------------------------------------------

-- unMaybe
test1 = TestCase $ assertEqual "unMaybe (Just 1)" "1" $ unMaybe (Just "1")
test2 = TestCase $ assertEqual "unMaybe Nothing" "" $ unMaybe Nothing

-- makeFloat
test3 = TestCase $ assertEqual "makeFloat (Just 314)" 314.0 $ makeFloat (Just "314")
test4 = TestCase $ assertEqual "makeFloat (Just 1)" 1.0 $ makeFloat (Just "1")
test5 = TestCase $ assertEqual "makeFloat (Just 0)" 0.0 $ makeFloat (Just "0")

-- supportAndResistanceValues
test6 = TestCase $ assertEqual "supportAndResistanceValues 10" [7.64,6.18,5.0,3.82] $ supportAndResistanceValues 10
test7 = TestCase $ assertEqual "supportAndResistanceValues 1" [0.764,0.618,0.5,0.382] $ supportAndResistanceValues 1
test8 = TestCase $ assertEqual "supportAndResistanceValues 100" [76.4,61.799995,50.0,38.2] $ supportAndResistanceValues 100
 
-- inflectionPoints
test9 = TestCase $ assertEqual "inflectionPoints [10,20,30,40,50]" [19.44,25.28,30.0,34.72] $ inflectionPoints [10,20,30,40,50]
test10 = TestCase $ assertEqual "inflectionPoints []" [] $ inflectionPoints []
test11 = TestCase $ assertEqual "inflectionPoints [1,10,20,50,80]" [19.644001,31.178001,40.5,49.822] $ inflectionPoints [1,10,20,50,80]

-- inRange
test12 = TestCase $ assertEqual "inRange 50 [40,45,55,65,10]" False $ inRange 50 [40,45,55,65,10]
test13 = TestCase $ assertEqual "inRange 100 [10,30,97,118,200]" True $ inRange 100 [10,30,97,118,200]
test14 = TestCase $ assertEqual "inRange 137 [98,75,102,122,150]" False $ inRange 137 [98,75,102,122,150]
test15 = TestCase $ assertEqual "inRange 147 [98,75,102,122,150]" True $ inRange 147 [98,75,102,122,150]

-- leastSquareSlope
test16 = TestCase $ assertEqual "leastSquareSlope [1,4,7,10,13]" 3.0 $ leastSquareSlope [1,4,7,10,13]
test17 = TestCase $ assertEqual "leastSquareSlope [2,5,3,6,4]" 0.5 $ leastSquareSlope [2,5,3,6,4]
test18 = TestCase $ assertEqual "leastSquareSlope [4,6,3,5,2]" (-0.5) $ leastSquareSlope [4,6,3,5,2]

-- stockBotEval
test19 = TestCase $ assertEqual "stockBotEval 20 [10,20,30,40,50]" "Sell / Don't buy" $ stockBotEval 20 [10,20,30,40,50]
test20 = TestCase $ assertEqual "stockBotEval 50 [10,20,30,40,50]" "The stock is likely to keep moving in a positive direction." $ stockBotEval 50 [10,20,30,40,50]

-- evaluation
test21 = TestCase $ assertEqual 
            "StockBot's evaluation of  ++ ticker ++ :  ++ stockBotEval 48 [40,45,47,49,50]" "StockBot's evaluation of AAPL: Sell / Don't buy"
            $ "StockBot's evaluation of " ++ "AAPL" ++ ": " ++ stockBotEval 48 [40,45,47,49,50]
test22 = TestCase $ assertEqual 
            "StockBot's evaluation of  ++ SAS.ST ++ :  ++ stockBotEval 1.9740 [1.8700,1.9100,1.8140,1.8580,1.9400]" "StockBot's evaluation of SAS.ST: Sell / Don't buy"
            $ "StockBot's evaluation of " ++ "SAS.ST" ++ ": " ++ stockBotEval 1.9740 [1.8700,1.9100,1.8140,1.8580,1.9400]
test23 = TestCase $ assertEqual
            "StockBot's evaluation of  ++ SAS.ST ++ :  ++ stockBotEval 1.7 [1.8700,1.9100,1.8140,1.8580,1.9400]" "StockBot's evaluation of SAS.ST: The stock is likely to keep moving in a positive direction."
            $ "StockBot's evaluation of " ++ "SAS.ST" ++ ": " ++ stockBotEval 1.7 [1.8700,1.9100,1.8140,1.8580,1.9400]
test24 = TestCase $ assertEqual
            "StockBot's evaluation of  ++ SAS.ST ++ :  ++ stockBotEval 1.7 [1.9400,1.8580,1.8140,1.9100,1.8700]" "StockBot's evaluation of SAS.ST: The stock is likely to keep moving in a negative direction."
            $ "StockBot's evaluation of " ++ "SAS.ST" ++ ": " ++ stockBotEval 1.7 [1.9400,1.8580,1.8140,1.9100,1.8700]
test25 = TestCase $ assertEqual
           "StockBot's evaluation of  ++ SAS.ST ++ :  ++ stockBotEval 1.9740 [1.9400,1.8580,1.8140,1.9100,1.8700]" "StockBot's evaluation of SAS.ST: Buy / Hold"
            $ "StockBot's evaluation of " ++ "SAS.ST" ++ ": " ++ stockBotEval 1.9740 [1.9400,1.8580,1.8140,1.9100,1.8700]
-- for running all the tests
tests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25]
