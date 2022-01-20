import System.Random
import Data.Map
import Control.Monad

-- Is this the best way to represent a dice? The brackets looks like a cube.
type Dice = (Int, Int, Int, Int, Int)

data Player = P String (Map String Int)
  deriving (Show)

-- How do we include more than one player? String is the option and Int the result.
data Table = T [Player]
  deriving (Show)

score :: Player -> String -> Int -> Player
score player option result = undefined

genDice :: IO Dice
genDice = do
    a <- randomRIO (1, 6)
    b <- randomRIO (1, 6)
    c <- randomRIO (1, 6)
    d <- randomRIO (1, 6)
    e <- randomRIO (1, 6)
    return (a, b, c, d, e)

throwAllDice :: IO Dice
throwAllDice = genDice

throwSomeDice :: Dice -> [Int] -> IO Dice
throwSomeDice dice throws = do
 a <- (foldM throwOneDice dice throws)
 return a

throwOneDice :: Dice -> Int -> IO Dice
throwOneDice dice pos = do
  value <- randomRIO (1,6)
  return $ updateDice dice pos value

updateDice :: Dice -> Int -> Int -> Dice
updateDice (a, b, c, d, e) 1 value = (value, b, c, d, e)
updateDice (a, b, c, d, e) 2 value = (a, value, c, d, e)
updateDice (a, b, c, d, e) 3 value = (a, b, value, d, e)
updateDice (a, b, c, d, e) 4 value = (a, b, c, value, e)
updateDice (a, b, c, d, e) 5 value = (a, b, c, d, value)

sumDices :: Dice -> Int
sumDices dice = undefined

--OPTIONS
makeOption :: Dice -> Table
makeOption dice = undefined

validOption :: Dice -> Bool
validOption dice = undefined

bestOption :: Dice -> Dice
bestOption dice = undefined

{-writeheader = do
  printStrLn                        __
                      /  |
 __    __   ______   _$$ |_   ________  __    __
/  |  /  | /      \ / $$   | /        |/  |  /  |
$$ |  $$ | $$$$$$  |$$$$$$/  $$$$$$$$/ $$ |  $$ |
$$ |  $$ | /    $$ |  $$ | __  /  $$/  $$ |  $$ |
$$ \__$$ |/$$$$$$$ |  $$ |/  |/$$$$/__ $$ \__$$ |
$$    $$ |$$    $$ |  $$  $$//$$      |$$    $$ |
 $$$$$$$ | $$$$$$$/    $$$$/ $$$$$$$$/  $$$$$$$ |
/  \__$$ |                             /  \__$$ |
$$    $$/                              $$    $$/
 $$$$$$/    }                             $$$$$$/
 -}

--TABLE
isEmpty :: Table -> Bool
isEmpty table = undefined

insert :: Table -> Dice -> Table
insert table dice = undefined

showTable :: Table
showTable = undefined

sumPlayerScore :: Table -> Int
sumPlayerScore table = undefined

untilBouns :: Table -> Int
untilBouns table = undefined

genPlayer :: Int -> String -> Player
genPlayer = undefined

play :: IO ()
play = undefined

{-game :: IO ()
game = do
    putStrLn "Let's play some Yatzy!"
    putStrLn "Enter name of Player1"
    name1 <- getLine -- name of player 1
    putStrLn "Enter name of Player2 or type 'play' "
    name2 <- getLine
    if name2 == play then do
      play
    else do
      putStrLn "Enter name of Player3 or type 'play' "
      name3 <- getLine
      if name3 == play then do
        play
        else do play

        -} 
