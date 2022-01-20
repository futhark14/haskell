module ComputerControl where
import Data.Function (on)
import Data.List (sort, sortBy, group, maximumBy)
import Game


{-rerollDescision s
    decides wether to reroll s
    RETURNS: "n" if s is a yhatzy, otherwise "y"
    PRE: s fulfils the Invariant
    EXAMPLES: rerollDescision [(6,False),(6,False),(6,False),(6,False),(6,False)] == "n"
              rerollDescision [(6,False),(6,False),(6,False),(6,False),(1,False)] == "y"  
-}
rerollDescision :: Selection -> String
rerollDescision s = if length (group $ map fst s) /= 1 then "y" else "n" 

{-getKeep s
    Selects which dice to keep
    RETURNS: a list containing the dice to keep. It will always prioritize higher numbers and aim for a yatzy
    EXAMPLES: getKeep [(3,False),(3,False),(1,False),(2,False),(6,False)] == [3,3]
              getKeep [(3,False),(3,False),(1,False),(6,False),(6,False)] == [6,6]
-}
getKeep :: Selection -> [Int]
getKeep s = maximumBy (compare `on` length) (group (sort (map fst s)))
{-selectSlot p s
    chooses the slot which will give the most score 
    RETURNS: the legal SlotType that will give the most score based on s and what is available in p's table
    PRE: s satisfies it's invariant
    EXAMPLES: selectSlot (generatePlayer "Test" True) [(6,False),(6,False),(6,False),(6,False),(1,False)] == Chance
              selectSlot (generatePlayer "Test" True) [(1,False),(2,False),(3,False),(4,False),(5,False)] == SmallLadder
              selectSlot (tick (generatePlayer "Test" True) [(1,False),(2,False),(3,False),(4,False),(5,False)] SmallLadder) [(1,False),(2,False),(3,False),(4,False),(5,False)] == Chance        
-}
selectSlot :: Player -> Selection -> SlotType
selectSlot (Player _ _ table) s = fst $ head scores
    where
        scores = sortBy (flip compare `on` snd) (map (\(a,b) -> (a,calculateScore a s)) (filter (\(a,b) -> b ==unset) table))

        