{-
Modify genGameState so that it randomly generates piles containing between 1 and 20 stones.
-}


import Control.Exception
import Prelude hiding(catch)                                   
import System.Random
import System.IO


{- Move
  Represents a move. The first element of the tuple
    is the pile to move from, the second element is the amount to move.
  Invariant: The first element refers to a valid pile. The second >= 0
-}
type Move = (Int, Int) 


{- Game State
  Represents the number of stones in the three piles.
  Invariant: each element of the tuple >= 0.
-}
type GameState = (Int, Int, Int)


type Player = String

{- readMove
   Reads a move from standard input
   Returns: A move object 
   Side-effects: Reads one or more lines from standard input 
-}
readMove :: IO Move
readMove = do
  catch (do
    line <- getLine 
    evaluate (read line))  -- evaluate required to force conversion of line to Move
    ((\_ -> do   -- exception handler
       putStrLn "Invalid input. Correct format: (pileNumber,amount)"
       readMove) :: SomeException -> IO Move)

{- validMove gs m
   Determines whether a move is valid in a given game state
   Returns: True if and only if the move refers to a pile and the number 
    of elements to remove is greater than 0 and less than number of
    elements in the pile
-}
validMove :: GameState -> Move -> Bool
validMove (a, _, _) (1, n) = a >= n && n > 0
validMove (_, b, _) (2, n) = b >= n && n > 0 
validMove (_, _, c) (3, n) = c >= n && n > 0
validMove _ _ = False


{- victory gs
   Determines whether a game state is the winning position.
   Returns: True when all stone piles are empty
-}
victory :: GameState -> Bool
victory (0, 0, 0) = True
victory _ = False

{- playmove state move 
   Updates the game state after making a move
   Pre: validMove state move
   Returns: A valid game state
-}
playMove :: GameState -> Move -> GameState
playMove (a, b, c) (1, n) = (a - n, b, c)
playMove (a, b, c) (2, n) = (a, b - n, c)
playMove (a, b, c) (3, n) = (a, b, c - n)


{- gameState
   Generates a fresh game state
   Returns: A valid game state
   Side effect: None (at present)
-}
genGameState :: IO GameState
genGameState = do 
  gen <- getStdGen
  let a = randomR (1,20) gen
  gen' <- newStdGen
  let b = randomR (1,20) gen'
  gen'' <- newStdGen
  let c = randomR (1,20) gen''
  return (fst a,fst b,fst c)

{- printGameState gs
   Print a game state
   Side-effect: Displays game state to standard output
-}
printGameState :: GameState -> IO ()
printGameState (a, b, c) = do
  putStrLn $ "Pile 1 contains " ++ (show a) ++ " stones."
  putStrLn $ "Pile 2 contains " ++ (show b) ++ " stones."
  putStrLn $ "Pile 3 contains " ++ (show c) ++ " stones."


{- printMove player move
   Print a move for a given player
   Side-effect: Displays move on standard output
-}
printMove :: Player -> Move -> IO ()
printMove player (pile, amount) = putStrLn $ player ++ " removes " ++ (show amount) ++ " stones from pile " ++ (show pile)

{- displayStats
  displays the amount of times the player and computer have won
  Side-effect: Prints amount of times player and computer have won
-}
displayStats :: IO ()
displayStats = do
  contents <- readFile "wins.txt"
  let 
    len = length contents
    playerWins = length $ filter (== 'p') contents
  putStrLn ("Player has won " ++ (show playerWins) ++ " times!")
  putStrLn ("Computer has won " ++ (show (len-playerWins)) ++ " times!")
  putStrLn ""

{- main
   Run the game
   Side-effects: Quite a lot, actually
-}
main :: IO ()
main = do 
  putStrLn "Welcome to Nim."
  displayStats
  gameState <- genGameState 
  play gameState

{- play gs
   Play the game
   Pre: gs is valid and not the victory state
   Side-effect: The game interaction -- it never returns
-}
play gameState = do
  printGameState gameState
  newGameState <- playerMove gameState
  if victory newGameState then do
    putStrLn "Player won!"
    appendFile "wins.txt" "p"
    playAgain
   else do
    newNewGameState <- computerMove newGameState  
    if victory newNewGameState then do
      putStrLn "Computer won!"
      appendFile "wins.txt" "c"
      playAgain
     else
      play newNewGameState     

{- playAgain 
   Asks the player if it wants to play again
   Side-effect: Quits the game if the user enters Y and then press enter
-}
playAgain :: IO ()
playAgain = do
  putStrLn "" 
  putStrLn "Do you want to play again"
  putStrLn "Press y, then enter to play again"
  putStrLn "Press anything, then enter to play quit"
  line <- getLine
  if line == "y" 
    then main
    else return ()
        
{- playerMove gs
   Perform the player's move
   Pre: gs is valid and not the victory state
   Returns: a new game state
   Side-effect: Displays a description of the players's move
-}
playerMove :: GameState -> IO GameState
playerMove gameState = do
  putStrLn "Your move."
  move <- readMove
  printMove "Player" move
  if validMove gameState move then 
    return $ playMove gameState move
   else do
    putStrLn "Invalid Move."
    playerMove gameState

{- computerMove gs
   Perform the computer's move
   Pre: gs is valid and not the victory state
   Returns: a new game state
   Side-effect: Displays a description of the computer's move
-} 
computerMove :: GameState -> IO GameState
computerMove gameState = do 
  let move = calculateComputerMove gameState
  printMove "Computer" move
  return $ playMove gameState move
  
  
{- calculateComputerMove gs
   Calculate the (best) next move for the computer
   Returns: A valid move for gs
-}
calculateComputerMove :: GameState -> Move
calculateComputerMove (a,b,c)
                              | a > 0     = (1, a)
                              | b > 0     = (2, b)
                              | otherwise = (3, c)
       