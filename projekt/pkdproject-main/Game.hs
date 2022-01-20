module Game where 

import System.Random
import Test.HUnit
import Data.List

{-different types for the slots for the table-}
data SlotType = Ones | Twos | Threes | Fours | Fives | Sixes | Pair | TwoPairs | ThreeOfAKind | FourOfAKind | FullHouse | SmallLadder | BigLadder | Chance | Yatzy deriving (Show, Eq, Read)

{-A list of dice with a indicator on if they're to be kept or rerolled
    
    The Int represents the value of the dice
    The boolean represents if the dice is to be rerolled or not. If True the die is kept if false it is rerolled when rerolling occurs

    INVARIANT: the Int must be within the range [1,6] and the list must be of length 5
-}
type Selection = [(Int,Bool)]

{-Represents the state of a board/ledger

    The bool represents wether the board is computer controlled or not
    The (SlotType,Int) list represents the the ledger, if the int is equal to unset it is yet to be set, if it is equal to zero that value is struck otherwise it is the score

    INVARIANT: The length of the ledger must be equal to 15 and all slottypes must be included. No slotType can be repeated
-}
data Player = Player String Bool [(SlotType,Int)] deriving (Show, Eq)

--a number which represents that a slot is unset. It can be any arbitrary Int <0 or >50 in the case of standard yatzy
unset = -300


{- calculateScore slot dice
   Calculates the score for the SlotType slot with the dice rolled dice 
   Returns: the score that the roll gives for the given slot
   PRE: dice satisfies the invariant
   EXAMPLE: calculateScore Yatzy [(6,False),(6,False),(6,False),(6,False),(6,False)]) == 50
            calculateScore Chance [(6,False),(6,False),(6,False),(6,False),(6,False)]) == 30
-}
calculateScore :: SlotType -> Selection -> Int
calculateScore Ones [] = 0
calculateScore Ones ((x, _) : xs)
--Variant: length dice
  | x == 1 = 1 + calculateScore Ones xs
  | otherwise = calculateScore Ones xs
calculateScore Twos [] = 0
calculateScore Twos ((x, _) : xs)
--Variant: length dice
  | x == 2 = 2 + calculateScore Twos xs
  | otherwise = calculateScore Twos xs
calculateScore Threes [] = 0
calculateScore Threes ((x, _) : xs)
--Variant: length dice
  | x == 3 = 3 + calculateScore Threes xs
  | otherwise = calculateScore Threes xs
calculateScore Fours [] = 0
calculateScore Fours ((x, _) : xs)
--Variant: length dice
  | x == 4 = 4 + calculateScore Fours xs
  | otherwise = calculateScore Fours xs
calculateScore Fives [] = 0
calculateScore Fives ((x, _) : xs)
--Variant: length dice
  | x == 5 = 5 + calculateScore Fives xs
  | otherwise = calculateScore Fives xs
calculateScore Sixes [] = 0
calculateScore Sixes ((x, _) : xs)
--Variant: length dice
  | x == 6 = 6 + calculateScore Sixes xs
  | otherwise = calculateScore Sixes xs
calculateScore Pair dice 
  |length pairlist >= 1 = head pairlist * 2 
  |otherwise = 0
  where
    pairlist =  map head (filter (\x -> 2 <= length x) (group (sortBy (flip compare) (map fst dice))))
calculateScore TwoPairs [] = 0
calculateScore TwoPairs dice
  |length pairlist >= 2 = head pairlist * 2 + head (tail pairlist) * 2
  |otherwise = 0
  where
    pairlist =  map head (filter (\x -> 2 <= length x) (group (sortBy (flip compare)  (map fst dice))))
calculateScore ThreeOfAKind [] = 0
calculateScore ThreeOfAKind dice
  | length pairlist >= 1 = head pairlist *3
  | otherwise = 0
  where
    pairlist =  map head (filter (\x -> 3 <= length x) (group (sort (map fst dice))))
calculateScore FourOfAKind [] = 0
calculateScore FourOfAKind dice
  | length pairlist >= 1 = head pairlist * 4
  | otherwise = 0
  where
    pairlist =  map head (filter (\x -> 4 <= length x) (group (sort (map fst dice))))
calculateScore FullHouse [] = 0
calculateScore FullHouse dice
  |groupedList == [3,2] || groupedList == [2,3] = sum numbers
  |otherwise = 0
  where
    groupedList = map length (group (sort numbers))
    numbers = map fst dice
calculateScore SmallLadder [] = 0
calculateScore SmallLadder dice
  | (calculateScore Ones dice == 1) && (calculateScore Twos dice == 2) && (calculateScore Threes dice == 3) && (calculateScore Fours dice == 4) && (calculateScore Fives dice == 5) = 15
  | otherwise = 0
calculateScore BigLadder [] = 0
calculateScore BigLadder dice
  |  (calculateScore Twos dice == 2) && (calculateScore Threes dice == 3) && (calculateScore Fours dice == 4) && (calculateScore Fives dice == 5) && (calculateScore Sixes dice == 6) = 20
  | otherwise = 0
calculateScore Chance [] = 0
calculateScore Chance dice = (calculateScore Ones dice) + (calculateScore Twos dice) + (calculateScore Threes dice) + (calculateScore Fours dice) + (calculateScore Fives dice) + (calculateScore Sixes dice)
calculateScore Yatzy [] = 0
calculateScore Yatzy dice
  | length (group $ sort $ map fst dice) == 1 = 50
  | otherwise = 0



{-generateSelection i
  creates an initial roll with i dice
  RETURNS: a Selection with i entries of pseudorandom numbers
  SIDE : modifies global rng
-}
generateSelection ::Int -> IO Selection
--Variant: i
generateSelection 0 = return []
generateSelection i = do 
    r <- randomRIO (1,6)
    rs <- generateSelection (i-1)
    return ((r,False):rs)

{-generatePlayer name cpu
  RETURNS: a player Named name and with the computer controlled indicator cpu 
  PRE: name is finite
  EXAMPLE: generatePlayer "Name" False == Player "Name" False [(Ones,unset), (Twos,unset),(Threes,unset), (Fours,unset), (Fives,unset), (Sixes,unset), (Pair,unset), (TwoPairs,unset), (ThreeOfAKind,unset), (FourOfAKind,unset), (FullHouse,unset), (SmallLadder,unset), (BigLadder,unset), (Chance,unset), (Yatzy,unset)]
-}
generatePlayer :: String -> Bool -> Player
generatePlayer s b = Player s b [(Ones,unset), (Twos,unset),(Threes,unset), (Fours,unset), (Fives,unset), (Sixes,unset), (Pair,unset), (TwoPairs,unset), (ThreeOfAKind,unset), (FourOfAKind,unset), (FullHouse,unset), (SmallLadder,unset), (BigLadder,unset), (Chance,unset), (Yatzy,unset)]


{-tick p sel slot
  updates slot in p with the value from sel
  RETURNS: a player with an updated ledger
  PRE: the given slot is not already set and exists in p 
  EXAMPLES: tick (generatePlayer "Test" False) [(6,False),(6,False),(6,False),(6,False),(6,False)] Yatzy == Player "Test" False [(Ones,unset), (Twos,unset),(Threes,unset), (Fours,unset), (Fives,unset), (Sixes,unset), (Pair,unset), (TwoPairs,unset), (ThreeOfAKind,unset), (FourOfAKind,unset), (FullHouse,unset), (SmallLadder,unset), (BigLadder,unset), (Chance,unset), (Yatzy,50)]
            tick (generatePlayer "Test" False) [(6,False),(6,False),(6,False),(6,False),(1,False)] Yatzy == Player "Test" False [(Ones,unset), (Twos,unset),(Threes,unset), (Fours,unset), (Fives,unset), (Sixes,unset), (Pair,unset), (TwoPairs,unset), (ThreeOfAKind,unset), (FourOfAKind,unset), (FullHouse,unset), (SmallLadder,unset), (BigLadder,unset), (Chance,unset), (Yatzy,0)]
-}
tick :: Player -> Selection -> SlotType -> Player
tick (Player name cpu l) s target 
  |isSlotSet l target = error "Slot is already set"
  |otherwise = Player name cpu (updateLedger l s target)
  where
    {-updateLedger ledger dice slot
      Updates the slot in ledger which corresponds to slot with the value that it should give dice
      RETURNS: an updated verision of ledger with slot changed to be the value that corresponds to dice and slot
      PRE: slot appears in ledger
      EXAMPLES: updateLedger [(Ones,unset),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,unset),(Yatzy,unset)] [(1,False),(1,False),(1,False),(1,False),(1,False)] Ones == [(Ones,5),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,unset),(Yatzy,unset)]  
    -}
    updateLedger :: [(SlotType, Int)] -> Selection -> SlotType -> [(SlotType, Int)]
    updateLedger ((slot,i):xs) s target
    --Variant: length ledger
      |slot == target = (slot, calculateScore target s) : xs
      |otherwise = (slot, i): updateLedger xs s target

{-isSlotSet slots reference 
  checks if reference is set in slots
  RETURNS: True if reference is set in slots
  PRE: reference must exist in slots
  EXAMPLES: isSlotSet [(Ones,unset),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,unset),(Yatzy,0)] Yatzy == True
            isSlotSet [(Ones,unset),(Twos,unset),(Threes,unset),(Fours,unset),(Fives,unset),(Sixes,unset),(Pair,unset),(TwoPairs,unset),(ThreeOfAKind,unset),(FourOfAKind,unset),(FullHouse,unset),(SmallLadder,unset),(BigLadder,unset),(Chance,0),(Yatzy,unset)] Yatzy == False
-}
isSlotSet :: [(SlotType,Int)] -> SlotType -> Bool
isSlotSet ((slot,i):xs) reference
--Variant: length slots 
  |slot == reference = i /= unset
  |otherwise = isSlotSet xs reference

{-roll s
  rerolls s keeping all the die that are marked to be kept
  RETURNS: a Selection which has every entry with a false in the bool part rerolled to a random number
  SIDE:   modifies global rng
-}
roll :: Selection -> IO Selection
--Variant: length s
roll [] = return []
roll ((x,False):xs) = do
    r <- randomRIO (1,6)
    rs <- roll xs
    return ((r, False) : rs)
roll ((x,True):xs) = do 
    rs <- roll xs
    return ((x,True):rs)
    
{-isFinished p
  checks if a player is finished
  RETURNS: True if none of the values in the table in p is equal to unset
  EXAMPLES: isFinished (generatePlayer "Test" False) == False
            isFinished (Player "Test" False [(Ones,0), (Twos,0),(Threes,0), (Fours,0), (Fives,0), (Sixes,0), (Pair,0), (TwoPairs,0), (ThreeOfAKind,0), (FourOfAKind,0), (FullHouse,0), (SmallLadder,0), (BigLadder,0), (Chance,0), (Yatzy,0)]) == True
-}
isFinished :: Player -> Bool
isFinished (Player _ _ score) = not $ any (== (unset)) $ map snd score

{- sumScore p
  gives the value of the score of player p
  Returns: the score with 50 point bonus if they have more than 63 points in total in Ones, Twos, Threes, Fours, Fives and Sixes. unset entries count as zero 
  EXAMPLE: sumScore (Player "Test" False [(Ones,1), (Twos,8),(Threes,3), (Fours,12), (Fives,20), (Sixes,30), (Pair,unset), (TwoPairs,unset), (ThreeOfAKind,unset), (FourOfAKind,unset), (FullHouse,unset), (SmallLadder,unset), (BigLadder,unset), (Chance,unset), (Yatzy,unset)]) == 124
           sumScore (generatePlayer "Test" False) == 0
-}
sumScore :: Player -> Int
sumScore (Player _ _ tabel) = sum [x | x <- map snd tabel, x /= (unset)] + if oneThroughSix > 63 then 50 else 0
  where
    oneThroughSix = sum  [x | x <- take 6 $ map snd tabel, x /= (unset)]

