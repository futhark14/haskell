{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Functions
import Text.HTML.Scalpel(scrapeURL,text,(@:),(@=),URL)

-----------------------------------------------------------------------------------------
------------- Albin Åberg Dahlberg, Lukas Lindén Thöming, Felix Agnerdahl ---------------
------------------------------------- Group 24 ------------------------------------------
----------------------------------------------------------------------------------------- 

{- Evaluation is represented by a string
   INVARIANT: 
   The string is the evaluation of a stock
-}
type Evaluation = String

{- main
   Evaluates the input stock and gives user advice on the stock.
   PRE: The input is a valid ticker of a stock.
   RETURNS: The stocks current price, the growth or loss of the latest open market day
            and either a prediction on the stocks movement or a suggestion on how to act.
   SIDE EFFECTS: Displays an instruction, scrapes yahoo finance on input stock, displays some of 
                 the scraped data, evaluates the data and returns a string containing an evaluation.
-}
main :: IO Evaluation
main = do
   putStrLn "Insert the ticker of the stock you want StockBot to evaluate."
   ticker   <- getLine
   price    <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["data-reactid" @= "32"])
   percent1 <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["class" @= "Trsdu(0.3s) Fw(500) Pstart(10px) Fz(24px) C($positiveColor)"])
   percent2 <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["class" @= "Trsdu(0.3s) Fw(500) Pstart(10px) Fz(24px) C($negativeColor)"])
   putStrLn "The current value of the stock and the day's growth is:"
   if
      percent1 == Nothing
      then 
         print (makeFloat price, unMaybe percent2)
      else do
         print (makeFloat price, unMaybe percent1)
   d1 <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "76"])
   d2 <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "91"])
   d3 <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "106"])
   d4 <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "121"])
   d5 <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "136"])
   return $ "StockBot's evaluation of " ++ ticker ++ ": " ++ stockBotEval (makeFloat price) (map makeFloat [d5,d4,d3,d2,d1])
