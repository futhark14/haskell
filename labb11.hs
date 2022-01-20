data FamilyTree a b = Void
                | Man (FamilyTree a b) a b (FamilyTree a b)
                | Woman (FamilyTree a b) a b (FamilyTree a b)

oldestWoman :: FamilyTree -> Maybe (String,Int)
oldestWoman _ Void = Void
oldestWoman x = (a,b)
oldestWoman (x:xs) = x


data FamilyTree = Void
                | Man (FamilyTree a Int) a Int (FamilyTree a Int)
                | Woman (FamilyTree a Int) a Int (FamilyTree a Int)

oldestWoman :: FamilyTree -> Maybe (String,Int)
oldestWoman Void = Nothing
      --      | Man _ = Nothing
         --   | Woman (a,b) = 






women :: [(a,b)] -> (a,b)
women x = (a,b)
women (x:xs) = x + women xs

menAndWomen :: [(a,b)] -> FamilyTree
menAndWomen _ Void = Void
menAndWomen 


compareMaybe :: Maybe (String,Int) -> Maybe (String,Int) -> Maybe (String,Int)
compareMaybe (g, Nothing) (h, Just y) = Just(h,y)
compareMaybe (g, Just x) (h, Nothing) = Just(g,x)
compareMaybe (Just (g, x)) (Just (h, Just y)) 
 |  x < y =  Just (g,x)
 | otherwise = Just (h,y)
oldestWoman :: FamilyTree -> Maybe (String,Int)
oldestWoman Void = Nothing
oldestWoman Man _ _ t1 t2 = compareMaybe (oldestWoman t1) (oldestWoman t2)
oldestWoman Woman a b t1 t2 = compareMaybe a b (compareMaybe (oldestWoman t1) (oldestWoman t2))



data Direction = L | R deriving (Show)  
type Directions = [Direction]  
  
changeToP :: Directions-> Tree Char -> Tree Char  
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
changeToP [] (Node _ l r) = Node 'P' l r  


import Data.Foldable as F

data Family a = Single a | Couple a a [Family a]
    deriving Show -- useful for ghci

isCouple :: Family a -> Bool
isCouple (Couple _ _ _) = True
isCouple _ = False

isFamily :: Eq a => a -> Family a -> Bool
isFamily n (Single a) = n == a
isFamily n (Couple a b _) = n == a || n == b

q1 :: Family String
q1 = Couple "Dan" "Doris" [Couple "Peggy" "Jack" [Single "Jennifer", Single "Lillian", Single "Tony"], Couple "Christine" "Paul" [], Couple "Phil" "Jill" [Single "Scula", Single "Kenton", Single "David", Single "Elizabeth"]]

q2 :: String -> Maybe [Family String]
q2 n = q2' n [q1]

q2' :: Eq a => a -> [Family a] -> Maybe [Family a]
q2' n f = case find (isFamily n) f of
    Just (Single _) -> Just []
    Just (Couple _ _ c) -> Just c
    _ -> case filter isCouple f of
        [] -> Nothing
        cs -> q2' n $ F.concatMap (\(Couple _ _ c) -> c) cs

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  