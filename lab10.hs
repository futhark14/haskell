data Suits = Diamond | Club | Heart | Spade  deriving (Eq, Ord, Enum, Show)

data Ranks = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)

data Card = Card Suits Ranks 

greaterCard :: Card -> Card -> Bool
greaterCard (Card s1 r1) (Card s2 r2) | r1 > r2 = True
                                      | r1 == r2 && s1 >= s2 = True
                                      | otherwise = False



(+?) :: Maybe Int -> Maybe Int -> Maybe Int
(+?) (Just a) (Just b)  = Just (a + b)  
(+?) Nothing Nothing = Nothing
(+?) (Just a) Nothing = Nothing
(+?) Nothing (Just b) = Nothing


(/?) :: Maybe Int -> Maybe Int -> Maybe Int
(/?) (Just a) (Just b) | b == 0 = Nothing
                       | otherwise = Just (a `div` b)
(/?) _ _ = Nothing