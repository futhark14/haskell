import Game
import System.IO
import System.Exit
import Control.Monad
import Control.Exception (SomeException, catch, evaluate)
import Data.Char
import Data.Function (on)
import Data.List (sortBy)
import ComputerControl
import Test.HUnit

{-SOURCES FOR THE PROJECT:
- stack overflow answer that was used to build all functions that used random number generation: https://stackoverflow.com/a/30741139
- paper that inspired the computer player algorithm: http://people.uncw.edu/tagliarinig/Courses/380/F2018%20papers%20and%20presentations/High%20Rollers/Paper-CSC380.pdf
- rules for yatzy : https://www.gratisspela.se/kunskapsbas/hur-spelar-man-yatzy/
-}

{-displayPlayer p
    displays the relevant information for the player p
    SIDE: prints to the terminal
-}
displayPlayer :: Player -> IO ()
displayPlayer (Player name cpu l) = do
  putStrLn $ "Player " ++ show name ++ " current boardstate: "
  putStrLn " ___________________"
  printTable l
    where
        {-printTable table
            prints table in a way  so that it looks like a table
            SIDE: writes to the terminal
            PRE: no SlotType can't have a text representation longer than 12 nor can any number have a text representation longer than five
        -}
        printTable :: [(SlotType, Int)] -> IO()
        --Variant: length table
        printTable [] = putStrLn "|__________________|"
        printTable ((s,x):xs) = do 
            let p = if x == unset
                then "unset"
                else show x
            putStrLn ("|" ++ show s ++ replicate (12-(length (show s))) ' ' ++"|" ++ p ++ replicate (5- length p) ' ' ++ "|")
            printTable xs

{-takeInput d cpu
    rolls dice and asks the player which shall be kept when rerolling twice
    RETURNS: a selection
    SIDE: prints to the terminal, takes input and modifies the global rng
-}
takeInput :: Bool -> IO Selection
takeInput cpu = do
  dice <- generateSelection 5
  input dice 3 cpu

{- input d s cpu
    rerolls the dice based on user input
    RETURNS: d after being rerolled up to s times as user input specifies. If cpu is true then the computer will reroll instead
    SIDE: prints to the terminal, can take user input and modifies the global random number generator
-}
input :: Selection -> Int -> Bool -> IO Selection
input d s cpu = do
  if (s /= 0)
    then do
       putStrLn $ "Your dice are: " ++ show (map fst d)
       putStrLn $ "You may reroll " ++ (show s) ++ " times, enter y to do so"
       fin <- if cpu 
           then do 
            let r =  rerollDescision d
            putStrLn r
            return r
           else getLine
       if (fin == "y" || fin == "Y")
         then do
           dice <- reroll d cpu
           return dice
           input dice (s-1) cpu
         else do
           putStrLn $ "Your dice are: " ++ show (map fst d)
           return d
     else do
        putStrLn $ "Your dice are: " ++ show (map fst d)
        return d


{-getList
    takes input in the form of a list
    RETURNS: a list as specified by user input
    SIDE: Takes input from the user via the terminal
-}
getList :: IO [Int]
getList = fmap read getLine


{-keepDice ints s
    sets the dice that are shared between s and ints to be kept
    RETURNS: s with all that is in ints having the keep indicator be true
    PRE: length ints cannot be greater than the length of s and the numbers given in ints must appear in s
    EXAMPLE: keepDice [3,2,1] [(1,False),(6,False),(3,False),(2,False),(6,False)] == [(1,True),(6,False),(3,True),(2,True),(6,False)]
             keepDice [6,6] [(1,False),(6,False),(3,False),(2,False),(6,False)] == [(1,False),(6,True),(3,False),(2,False),(6,True)]
-}
keepDice :: [Int] -> Selection -> Selection
--Variant: length s
keepDice [] dice = dice
keepDice (y:ys) dice = keepDice ys (keepDiceInt y dice)
  where
    {-keepDiceInt n s
        Sets the first entry in s where the Int equals n and the boolean is False to True
        RETURNS: s with the first entry flagged with false where the int equals n set to True
        PRE: a value in s has an Int value that is equal to n
        EXAMPLES: keepDiceInt 6 [(6,True),(6,True),(6,False),(6,True),(1,False)] == [(6,True),(6,True),(6,True),(6,True),(1,False)]
                  keepDiceInt 5 [(5,False),(5,False),(5,False),(5,False),(5,False)] == [(5,True),(5,False),(5,False),(5,False),(5,False)]
    -}
    keepDiceInt :: Int -> Selection -> Selection
    keepDiceInt n ((x, bool) : xs)
        | (n /= x) = (x, bool) : (keepDiceInt n xs)
        | (n == x) && (bool /= True) = (x, True) : xs
        | (n == x) && (bool == True) = (x, True) : (keepDiceInt n xs)
        | otherwise = keepDiceInt n xs

{-reroll
    rerolls the dice based on user input. If cpu is true all actions are automated
    RETURNS: a selection based on a reroll of the dice the player chooses not to keep
    SIDE: prints to the terminal, takes user input and modifies the global random number generator
-}
reroll :: Selection -> Bool -> IO Selection
reroll tern cpu = do
  catch (do
            putStrLn "Enter the dice you wish to keep as a list"
            keep <- if cpu then return (getKeep tern) else getList
            rerolled <- roll (keepDice keep tern)
            return (map (\(i,_) -> (i,False)) rerolled))
            ((\_ -> do
                 putStrLn "Invalid, try again. Format: [1,2 ...]"
                 putStrLn $ "Your dice are: " ++show (map fst tern)
                 reroll tern cpu) :: SomeException -> IO Selection)

{-main
    Plays the game with a terminal gui
    SIDE: prints to the terminal, takes inputs and modifies the global rng
-}
main :: IO()
main = do
    players <- getPlayers 1
    playGame players

{-playGame players dice
    plays the game and displays the score when all players are finished
    SIDE: modifies the global random number generator, 
-}
playGame :: [Player] -> IO ()
--Variant: the amount of entries left to fill in each players table
playGame players = do
    pl <-  mapM doGameLoop players
    --checks if game is over
    if all isFinished pl
        then do
            let scorelist = sortBy (flip compare `on` snd) (map (\p@(Player name _ _) -> (name,sumScore p)) pl)
            mapM_ (\(name,score) -> putStrLn(name ++ ": " ++ (show score))) scorelist
            putStrLn "The game is now over. Here are the players in order of score:"
            putStrLn "press y to play again with the same players, press x to play again with new players, press anything else to end the game"
            a <- getLine
            if a == "y" || a == "Y"
                then playGame (map (\(Player name cpu _) -> generatePlayer name cpu) pl)
                else if a == "x" || a == "X"
                    then main
                    else exitWith ExitSuccess
        else playGame pl

{-doGameLoop dice p
    does one repetition of a round of yatzy for one player
    RETURNS: p after the round is done
    SIDE: prints to the terminal, takes input and modifies the global random number generator
-}
doGameLoop :: Player -> IO Player
doGameLoop p@(Player _ cpu _) = do
    displayPlayer p
    selection <- takeInput cpu
    getSlotType selection
    where
        {-getSlotType sel
            gets input for which slot to add the result to
            RETURNS: a player with sel applied to the slot
            SIDE: prints to the terminal and takes input
        -}
        getSlotType :: Selection -> IO Player
        getSlotType sel  = do
            print "write slot to assign it to"
            slot <- if cpu 
                then return $ show (selectSlot p sel) 
                else getLine
            catch (do
                target <- evaluate (read slot)
                print ("adding to slot " ++ (show target))
                evaluate (tick p sel target)
                ) ((\_ -> do 
                    putStrLn "Slot is either taken or does not exist"
                    getSlotType sel)::SomeException -> IO Player)

{-getPlayers i
    constructs a list of all players. i is used to show to the user which player is being added
    RETURNS: a list of players as specified by user input
    SIDE: takes input and prints to the terminal
-}
getPlayers :: Int -> IO [Player]
getPlayers i = do
    putStrLn ("Enter the name of player " ++ show i)
    name <- getLine
    cpu <- getComInput
    putStrLn "input y if you want to stop adding players"
    stop <- getLine
    rs <- do 
        if stop == "y" || stop == "Y"
            then return []
            else getPlayers (i+1)
    return (generatePlayer name cpu : rs)
    where
        {-getComInput
            gets user input on wether the player is a computer or not
            RETURNS: True if the player is a computer. False if they're not
            SIDE: writes to the terminal and takes input
        -}
        getComInput :: IO Bool
        getComInput = do
            putStrLn "is this player a computer?(y/n)"
            cpu <- getLine
            if cpu == "y" || cpu == "Y"
                then return True
                else if cpu == "n" || cpu == "N"
                    then return False
                    else getComInput

--tests
test0 = TestCase (assertEqual "evalscore on player that has only unset" (sumScore (generatePlayer "Test" False)) 0)
test1 = TestCase (assertEqual "one tick sumScore" (sumScore (tick (generatePlayer "Test" False) [(1,False),(1,False),(1,False),(1,False), (2,False)] Ones)) 4)
test2 = TestCase (assertEqual "Two tick sumScore" (sumScore (tick (tick (generatePlayer "Test" False) [(1,False),(1,False),(1,False),(1,False), (2,False)] Ones)[(5,False),(5,False),(4,False),(2,False),(2,False)] Twos)) 8)
test3 = TestCase (assertBool "isFinished new player"  (not $ isFinished(generatePlayer "Test" False)))
test4 = TestCase (assertBool "isFinished done player" (isFinished(Player "test" False [(Ones,0),(Twos,0),(Threes,0),(Fours,0),(Fives,0),(Sixes,0),(Pair,0),(TwoPairs,0),(ThreeOfAKind,0),(FourOfAKind,0),(FullHouse,0),(SmallLadder,0),(BigLadder,0),(Chance,0),(Yatzy,0)])))
test5 = TestCase (assertEqual "Fullhouse(3 of the same and two of the same)" (sumScore (tick (generatePlayer "Test" False) [(6, False), (6, False), (6, False), (5, False), (5, False)] FullHouse)) 28)
test6 = TestCase (assertEqual "Small straight" (sumScore (tick (tick (generatePlayer "Test" False) [(1, False), (2, False), (3, False), (4, False), (5, False)] SmallLadder) [(2, False), (3, False), (4, False), (5, False), (6, False)] BigLadder)) 35)
test7 = TestCase (assertEqual "keepDice wrong order" (keepDice [3,2,1] [(1,False),(2,False),(4,False),(3,False),(5,False)]) [(1,True),(2,True),(4,False),(3,True),(5,False)])
test8 = TestCase (assertEqual "keepDice no input" (keepDice [] [(1,False),(2,False),(4,False),(3,False),(5,False)]) [(1,False),(2,False),(4,False),(3,False),(5,False)])
test9 = TestCase (assertEqual "rerollDescision should reroll" (rerollDescision [(1,False),(1,False),(1,False),(1,False),(5,False)]) "y")
test10 = TestCase (assertEqual "rerollDescision should not reroll" (rerollDescision [(1,False),(1,False),(1,False),(1,False),(1,False)]) "n")
test11 = TestCase (assertEqual "getKeep should keep highest value" (getKeep [(3,False),(3,False),(1,False),(6,False),(6,False)]) [6,6])
test12 = TestCase (assertEqual "getKeep should keep largest amount" (getKeep [(3,False),(3,False),(2,False),(1,False),(6,False)]) [3,3])
test13 = TestCase (assertEqual "getKeep should keep highest value even when it's only single elems" (getKeep [(1,False),(2,False),(3,False),(4,False),(5,False)]) [5])
test14 = TestCase (assertEqual "selectSlot should work on non yatzy" (selectSlot (generatePlayer "Test" True) [(6,False),(6,False),(6,False),(6,False),(1,False)]) Chance)
test15 = TestCase (assertEqual "selectSlot should work for players who have pieces of the table filled out" (selectSlot (tick (generatePlayer "Test" True) [(1,False),(2,False),(3,False),(4,False),(5,False)] SmallLadder) [(1,False),(2,False),(3,False),(4,False),(5,False)]) Chance)
test16 = TestCase (assertEqual "calculateScore should work for Ones#1" (calculateScore Ones [(1,False),(1,False),(1,False),(1,False),(1,False)]) 5)
test17 = TestCase (assertEqual "calculateScore should work for Ones#2" (calculateScore Ones [(1,False),(1,False),(1,False),(3,False),(3,False)]) 3)
test18 = TestCase (assertEqual "calculateScore should work for Twos#1" (calculateScore Twos [(2,False),(2,False),(2,False),(2,False),(2,False)]) 10)
test19 = TestCase (assertEqual "calculateScore should work for Twos#2" (calculateScore Twos [(2,False),(3,False),(3,False),(3,False),(2,False)]) 4)
test20 = TestCase (assertEqual "calculateScore should work for Threes#1" (calculateScore Threes [(3,False),(3,False),(3,False),(3,False),(3,False)]) 15)
test21 = TestCase (assertEqual "calculateScore should work for Threes#2" (calculateScore Threes [(2,False),(3,False),(3,False),(3,False),(2,False)]) 9)
test22 = TestCase (assertEqual "calculateScore should work for Fours#1" (calculateScore Fours [(4,False),(4,False),(4,False),(4,False),(4,False)]) 20)
test23 = TestCase (assertEqual "calculateScore should work for Fours#2" (calculateScore Fours [(4,False),(4,False),(3,False),(4,False),(2,False)]) 12)
test24 = TestCase (assertEqual "calculateScore should work for Fives#1" (calculateScore Fives [(5,False),(5,False),(5,False),(5,False),(5,False)]) 25)
test25 = TestCase (assertEqual "calculateScore should work for Fives#2" (calculateScore Fives [(5,False),(4,False),(3,False),(4,False),(5,False)]) 10)
test26 = TestCase (assertEqual "calculateScore should work for Sixes#1" (calculateScore Sixes [(6,False),(6,False),(6,False),(6,False),(6,False)]) 30)
test27 = TestCase (assertEqual "calculateScore should work for Sixes#2" (calculateScore Sixes [(5,False),(4,False),(3,False),(4,False),(6,False)]) 6)
test28 = TestCase (assertEqual "calculateScore should work for Pairs with multiple" (calculateScore Pair [(6,False),(6,False),(2,False),(3,False),(3,False)]) 12)
test29 = TestCase (assertEqual "calculateScore should work for Pairs with threeofakind" (calculateScore Pair [(1,False),(1,False),(1,False),(4,False),(3,False)]) 2)
test30 = TestCase (assertEqual "calculateScore should work for TwoPairs with one being a threeofakind" (calculateScore TwoPairs [(1,False),(1,False),(4,False),(4,False),(1,False)]) 10)
test31 = TestCase (assertEqual "calculateScore should work for TwoPairs" (calculateScore TwoPairs [(6,False),(6,False),(5,False),(5,False),(1,False)]) 22)
test32 = TestCase (assertEqual "calculateScore should work for ThreeOfAKind" (calculateScore ThreeOfAKind [(6,False),(6,False),(6,False),(5,False),(1,False)]) 18)
test33 = TestCase (assertEqual "calculateScore should work for FourOfAKind" (calculateScore FourOfAKind [(6,False),(6,False),(6,False),(6,False),(1,False)]) 24)
test34 = TestCase (assertEqual "calculateScore should work for FullHouse#1" (calculateScore FullHouse [(3,False),(3,False),(3,False),(6,False),(6,False)]) 21)
test35 = TestCase (assertEqual "calculateScore should work for FullHouse#2" (calculateScore FullHouse [(1,False),(1,False),(1,False),(2,False),(2,False)]) 7)
test36 = TestCase (assertEqual "calculateScore should work for SmallLadder" (calculateScore SmallLadder [(1,False),(2,False),(3,False),(4,False),(5,False)]) 15)
test37 = TestCase (assertEqual "calculateScore should work for BigLadder" (calculateScore BigLadder [(6,False),(2,False),(3,False),(4,False),(5,False)]) 20)
test38 = TestCase (assertEqual "calculateScore should work for Chance#1" (calculateScore Chance [(1,False),(2,False),(3,False),(4,False),(5,False)]) 15)
test39 = TestCase (assertEqual "calculateScore should work for Chance#2" (calculateScore Chance [(6,False),(6,False),(6,False),(6,False),(6,False)]) 30)
test40 = TestCase (assertEqual "calculateScore should work for Yatzy#1" (calculateScore Yatzy [(1,False),(1,False),(1,False),(1,False),(1,False)]) 50)
test41 = TestCase (assertEqual "calculateScore should work for Yatzy#2" (calculateScore Yatzy [(2,False),(2,False),(2,False),(2,False),(2,False)]) 50)
test42 = TestCase (assertEqual "calculateScore should work for Yatzy#2" (calculateScore Yatzy [(3,False),(3,False),(3,False),(3,False),(3,False)]) 50)
test43 = TestCase (assertEqual "calculateScore should work for Yatzy#2" (calculateScore Yatzy [(4,False),(4,False),(4,False),(4,False),(4,False)]) 50)
test44 = TestCase (assertEqual "calculateScore should work for Yatzy#2" (calculateScore Yatzy [(5,False),(5,False),(5,False),(5,False),(5,False)]) 50)
test45 = TestCase (assertEqual "calculateScore should work for Yatzy#2" (calculateScore Yatzy [(6,False),(6,False),(6,False),(6,False),(6,False)]) 50)
test46 = TestCase (assertEqual "isSlotSet should recognize that a slot is set" (isSlotSet [(Ones,unset),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,unset),(Yatzy,0)] Yatzy) True)
test47 = TestCase (assertEqual "isSlotSet should recognize that a slot is not set" (isSlotSet [(Ones,unset),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,0),(Yatzy,unset)] Yatzy) False)


tests = TestList [TestLabel "test0" test0, TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6,TestLabel "test8" test8,TestLabel "test8" test8, TestLabel "test9" test9,TestLabel "test10" test10,TestLabel "test11" test11,TestLabel "test12" test12,TestLabel "test13" test13,TestLabel "test14" test14,TestLabel "test15" test15,TestLabel "test16" test16,TestLabel "test17" test17,TestLabel "test18" test18,TestLabel "test19" test19,TestLabel "test20" test20,TestLabel "test21" test21,TestLabel "test22" test22,TestLabel "test23" test23,TestLabel "test24" test24,TestLabel "test25" test25,TestLabel "test26" test26,TestLabel "test27" test27, TestLabel "test28" test28,TestLabel "test29" test29, TestLabel "test30" test30, TestLabel "test31" test31, TestLabel "test32" test32, TestLabel "test33" test33, TestLabel "test34" test34, TestLabel "test35" test35, TestLabel "test36" test36, TestLabel "test37" test37, TestLabel "test38" test38, TestLabel "test39" test39, TestLabel "test40" test40, TestLabel "test41" test41, TestLabel "test42" test42, TestLabel "test43" test43, TestLabel "test44" test44, TestLabel "test45" test45,TestLabel "test46" test46, TestLabel "test47" test47]

runTests = runTestTT tests 