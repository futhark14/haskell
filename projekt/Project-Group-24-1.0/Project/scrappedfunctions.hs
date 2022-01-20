-- part of the URL for scraping weekly values.
weekly :: URL
weekly = "/history?period1=1582410119&period2=1614032519&interval=1wk&filter=history&frequency=1wk&includeAdjustedClose=true"

-- part of the URL for scraping monthly values.
monthly :: URL
monthly = "/history?period1=1582410119&period2=1614032519&interval=1mo&filter=history&frequency=1mo&includeAdjustedClose=true"

{- weeklyPriceClose
   Generates the closing prices of the past five weeks of the requested stock.
   RETURNS: A list of closing prices.
   SIDE EFFECTS: Returns the scraped items.
-}
weeklyPriceClose :: IO [Float]
weeklyPriceClose = do
    putStrLn "Of which stock do you want the closing price from the past five weeks?"
    ticker <- getLine
    w1     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "85"]) ---bytte dessa till id som gällde nu, funkar ej pga att i URL för weekly/monthly så är det en specifik period som specifieras
    w2     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "100"])
    w3     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "115"])
    w4     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "130"])
    w5     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "145"])
    return $ makeFloats [w1,w2,w3,w4,w5]

{- weeklyPriceHigh
   Generates the highest prices of the past five weeks of the requested stock.
   RETURNS: A list of the highest prices from the past five weeks.
   SIDE EFFECTS: Returns the scraped items.
-}
weeklyPriceHigh :: IO [Float]
weeklyPriceHigh = do
    putStrLn "Of which stock do you want the weekly highest price from the past five weeks?"
    ticker    <- getLine
    w1     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "72"])
    w2     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "87"])
    w3     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "102"])
    w4     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "117"])
    w5     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "132"])
    return $ makeFloats [w1,w2,w3,w4,w5]

{- weeklyPriceLow
   Generates the lowest prices of the past five weeks of the requested stock.
   RETURNS: A list of the lowest prices from the past five weeks.
   SIDE EFFECTS: Returns the scraped items.
-}
weeklyPriceLow :: IO [Float]
weeklyPriceLow = do
    putStrLn "Of which stock do you want the weekly lowest price from the past five weeks?"
    ticker    <- getLine
    w1     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "83"])
    w2     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "98"])
    w3     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "113"])
    w4     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "128"])
    w5     <- scrapeURL (yahoo ++ ticker ++ weekly) (text $ "span" @: ["data-reactid" @= "143"])
    return $ makeFloats [w1,w2,w3,w4,w5]

{- monthlyPriceClose
   Generates closing prices of the past five months of the requested stock.
   RETURNS: A list of closing prices.
   SIDE EFFECTS: Returns the scraped items.
-}
monthlyPriceClose :: IO [Float]
monthlyPriceClose = do
    putStrLn "Of which stock do you want the closing price from the past five months?"
    ticker <- getLine
    m1 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "76"])
    m2 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "91"])
    m3 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "106"])
    m4 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "121"])
    m5 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "136"])
    return $ makeFloats [m1,m2,m3,m4,m5]

{- monthlyPriceClose
   Generates the highest prices of the past five months of the requested stock.
   RETURNS: A list of closing prices.
   SIDE EFFECTS: Returns the scraped items.
-}
monthlyPriceHigh :: IO [Float]
monthlyPriceHigh = do
    putStrLn "Of which stock do you want the monthly highest price from the past five months?"
    ticker <- getLine
    m1 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "72"])
    m2 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "87"])
    m3 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "102"])
    m4 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "117"])
    m5 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "132"])
    return $ makeFloats [m1,m2,m3,m4,m5]

{- monthlyPriceLow
   Generates the lowest prices of the past five months of the requested stock.
   RETURNS: A list of closing prices.
   SIDE EFFECTS: Returns the scraped items.
-}
monthlyPriceLow :: IO [Float]
monthlyPriceLow = do
    putStrLn "Of which stock do you want the monthly lowest price from the past five months?"
    ticker <- getLine
    m1 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "74"])
    m2 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "89"])
    m3 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "104"])
    m4 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "119"])
    m5 <- scrapeURL (yahoo ++ ticker ++ monthly) (text $ "span" @: ["data-reactid" @= "134"])
    return $ makeFloats [m1,m2,m3,m4,m5]

{- pastFiveDays
   Generates the past five days and the closing prices of requested stock.
   RETURNS: A list of tuples which contains dates and closing prices.
   SIDE EFFECTS: Returns the scraped items.
-}
pastFiveDays :: IO [(Maybe String, Maybe String)]
pastFiveDays = do
    putStrLn "Of which stock do you want the closing price from the past five days?"
    ticker  <- getLine
    d1Date  <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "53"])
    d1Close <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "61"])
    d2Date  <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "68"])
    d3Date  <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "83"])
    d3Close <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "91"])
    d2Close <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "76"])
    d4Date  <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "98"])
    d4Close <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "106"])
    d5Date  <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "113"])
    d5Close <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "121"])
    return [(d1Date, d1Close),(d2Date, d2Close),(d3Date, d3Close),(d4Date, d4Close),(d5Date, d5Close)] 
    -- ändra så en funktion fixar istället för att skriva innan varje element

{- closePastFive
   Generates the closing price of the past five days of the requested stock.
   RETURNS: A list of closing prices.
   SIDE EFFECTS: Returns the scraped items
-}
closePastFive :: IO [Float]
closePastFive = do
    putStrLn "Of which stock do you want the closing price from the past five days?"
    ticker <- getLine
    d1     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "32"])
    d2     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "76"])
    d3     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "91"])
    d4     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "106"])
    d5     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "121"])
    return $ makeFloats [d1,d2,d3,d4,d5]

{- lowPastFive
   Generates the lowest price of the past five days of the requested stock.
   RETURNS: A list of lowest prices.
   SIDE EFFECTS: Returns the scraped items.
-}
lowPastFive :: IO [Float]
lowPastFive = do
    putStrLn "Of which stock do you want the closing price from the past five days?"
    ticker <- getLine
    d1     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "59"])
    d2     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "74"])
    d3     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "89"])
    d4     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "104"])
    d5     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "119"])
    return $ makeFloats [d1,d2,d3,d4,d5]

{- highPastFive
   Generates the highest prices of the past five days of the requested stock.
   RETURNS: A list of highest prices.
   SIDE EFFECTS: Returns the scraped items.
-}
highPastFive :: IO [Float]
highPastFive = do
    putStrLn "Of which stock do you want the closing price from the past five days?"
    ticker <- getLine
    d1     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "57"])
    d2     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "72"])
    d3     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "87"])
    d4     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "102"])
    d5     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "117"])
    return $ makeFloats [d1,d2,d3,d4,d5]

main :: IO [Float]
main = do
   putStrLn "Which stock do you want advice on?"
   ticker <- getLine
   price  <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["data-reactid" @= "61"])
   d1     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "32"])
   d2     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "76"])
   d3     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "91"])
   d4     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "106"])
   d5     <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "121"])
   return $ final (makeFloat price) (supportAndResistanceValues $ fibonacciVerticalValue (makeFloats [d1,d2,d3,d4,d5])

{- makeFloats justStrings
   Turns a list of Just Strings into a list of floats.
   PRE: The strings must not contain anything other than floats.
   RETURNS: A list of floats.
   EXAMPLES: makeFloats [(Just "hej")]                    == [*** Exception: Prelude.read: no parse
             makeFloats [(Just "123.45"), (Just "1.337")] == [123.45,1.337]
   SIDE EFFECTS: makeFloats [Nothing] == [*** Exeption: Prelude.read: no parse
-}
makeFloats :: [Maybe String] -> [Float]
makeFloats [] = [] -- behövs den här raden?
makeFloats xs = map (read . unMaybe) xs

main2 :: IO String
main2 = do
   ticker <- getLine
   price  <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["data-reactid" @= "32"])
   d1h    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "72"])
   d2h    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "87"])
   d3h    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "102"])
   d4h    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "117"])
   d5h    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "132"])
   d1l    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "74"])
   d2l    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "89"])
   d3l    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "104"])
   d4l    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "119"])
   d5l    <- scrapeURL (yahoo ++ ticker ++ "/history") (text $ "span" @: ["data-reactid" @= "134"])
   return $ stockBotEval (makeFloat price) (zipWith (\x y -> (x + y)/2) (map makeFloat [d5h,d4h,d3h,d2h,d1h]) (map makeFloat [d5l,d4l,d3l,d2l,d1l]))

{- currentPrice
   Generates the current price and the days gain/loss.
   RETURNS: A tuple of the current price and gain/loss.
   SIDE EFFECTS: Returns the scraped items.
-}
currentPrice :: IO (Float, String)
currentPrice = do
    putStrLn "Which stock do you want to look up?"
    ticker   <- getLine
    price    <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["data-reactid" @= "32"])
    percent1 <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["class" @= "Trsdu(0.3s) Fw(500) Pstart(10px) Fz(24px) C($positiveColor)"])
    percent2 <- scrapeURL (yahoo ++ ticker) (text $ "span" @: ["class" @= "Trsdu(0.3s) Fw(500) Pstart(10px) Fz(24px) C($negativeColor)"])
    if
        percent1 == Nothing
        then 
            return (makeFloat price, unMaybe percent2)
        else do
            return (makeFloat price, unMaybe percent1)

yv :: [Double]
yv = [48.51,46.23,91.71,184.68,142.9]

values = [142.9,184.68,91.71,46.23,48.51,86.0,101.0,44.7,40.0,42.4]

price = maximum values

final :: Float -> [Float] -> [Float] -- flyttade denna funktion så den finns inuti inflectionPoints istället
final highPrice xs = map (highPrice -) xs

testing = final price (supportAndResistanceValues $ fibonacciVerticalValue values)

test5 = TestCase $ assertEqual "makeFloats [(Just 1),(Just 2),(Just 3)]" [1.0, 2.0, 3.0] $ makeFloats [(Just "1"),(Just "2"),(Just "3")]
test6 = TestCase $ assertEqual "makeFloats [(Just 1)]" [1.0] $ makeFloats [(Just "1")]