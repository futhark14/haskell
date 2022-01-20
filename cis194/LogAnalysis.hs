{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage [] = error "empty message"
parseMessage (x:y:z:xs) |x == 'E' = LogMessage (Error 11) (eSeverity z) (eMessage xs)
                    | otherwise = LogMessage (Error 1) 1 "no"

eNum :: String -> Int
eNum (y:xs) = read [y]

eSeverity :: String -> Int
eSeverity (z:xs) = read [z]

eMessage :: String -> String
eMessage xs = xs



