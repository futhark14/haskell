data BSTree = Void 
              | Node BSTree Int BSTree deriving (Show)

{- delete BSTree int
    Deletes the int from the tree and retains it form
    RETURNS: a tree but with int deleted from it
    EXAMPLES: (Node (Node Void 4 Void) 12 (Node Void 16 Void)) 12 == Node Void 4 (Node Void 16 Void)
-}
delete :: BSTree -> Int -> BSTree
delete Void _ = Void
delete (Node Void m Void) x
  | x == m    = Void
  | otherwise = (Node Void m Void)
delete (Node Void m r) x
  | x == m    = r
  | otherwise = delete r x
delete (Node l m Void) x
  | x == m    = (Node (bDelete l $ findLargestLabel l) (findLargestLabel l) Void)
  | otherwise = delete l x
delete (Node l m r) x 
  | x == m    = (Node (bDelete l $ findLargestLabel l) (findLargestLabel l) r)
  | x < m     = (Node (delete l x) m r)
  | otherwise = (Node l m (delete r x))

{- findLargestLabel BSTree 
    Finds the largest label in the binary tree
    RETURNS: Largest Int in the binary tree
    EXAMPLES: (Node Void 12 Void) == 12
              (Node Void 12 (Node Void 16 Void)) == 16
-}
findLargestLabel :: BSTree -> Int
findLargestLabel (Node l m Void) = m
findLargestLabel (Node _ m r) = findLargestLabel r

bDelete :: BSTree -> Int -> BSTree
bDelete Void _ = Void
bDelete t@(Node l x r) y
  | x == y    = bDeleteRoot t
  | otherwise = (Node (bDelete l y) x (bDelete r y))
  where
    bDeleteRoot :: BSTree -> BSTree
    bDeleteRoot (Node Void _ Void) = Void
    bDeleteRoot (Node t x Void)    = bDeleteRoot (Node Void x t)
    bDeleteRoot (Node l _ r@(Node rl x rr)) = Node l x $ bDeleteRoot r 


--(Node (Node (Node (Node Void 2 Void) 5 (Node Void 6 Void)) 7 (Node Void 8 Void)) 9 (Node Void 12 (Node (Node Void 15 Void) 17 Void)))