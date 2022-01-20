-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------




{- characterCounts s
   PURPOSE: To convert a given string into the datatype Table. The Table consists of tuples, with every unique character accompanied with the amount of times it occurs in the given string.  
   PRE: none
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   SIDE EFFECTS:
   EXAMPLES: characterCounts "hej" = T [('j',1),('e',1),('h',1)]
 -}
characterCounts :: String -> Table Char Int
characterCounts [] = Table.empty
characterCounts s = Table.insert (characterCounts (tail s))  (head s) (count s (head s))
-- går hela vägen ner till basfallet och bygger sedan upp tablet



{- count s c
   PURPOSE: Counting the amount of times a character occurs in a String
   PRE: none
   RETURNS: An Int corresponding to the amount of times the character c occurs in the string s
   SIDE EFFECTS:
   EXAMPLES: count "ggrdfff" 'f' = 3
 -}
count :: String -> Char -> Int
count [] _ = 0
count ord p | (head ord) == p = 1 + count (tail ord) p
            | otherwise       = 0 + count (tail ord) p
----------------------------------------------------------------------------------------


  
{- ... description of what the data type represents ... 
   ... description of how the datatype represents data ...
   INVARIANT: ... a predicate on elements of the datatype that the code
               preserves at all times ...
-}
data HuffmanTree = Leaf (Char, Int)
                 | Node HuffmanTree HuffmanTree Int
                 deriving Show



{- huffmanTree t
   PURPOSE: To, from a table, create a huffmantree-------------------------------------------------
   PRE:  t maps each key to a positive value, t cannot be empty
   RETURNS: a Huffman tree based on the character counts in t
   SIDE EFFECTS:
   EXAMPLES: huffmanTree (T [('j',1),('e',1),('h',1)]) = Node (Leaf ('e',1)) (Node (Leaf ('j',1)) (Leaf ('h',1)) 2) 3
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = atLeast(toQueue t)



{- atLeast pq
   PURPOSE: To, from a priorityQueue consisting unsorted nodes and leafs (huffmantrees), create a completely sorted huffmanTree 
   PRE: none
   RETURNS: A complete conversion from an unsorted priorityqueue pq, consisting of nodes and leafs, to a completely sorted huffmantree
   SIDE EFFECTS:
   EXAMPLES: atLeast (BinoHeap [Node 0 1 (Leaf ('H',1)) [],Node 1 1 (Leaf ('j',1)) [Node 0 1 (Leaf ('e',1)) []]]) = Node (Leaf ('e',1)) (Node (Leaf ('j',1)) (Leaf ('H',1)) 2) 3
 -}
atLeast :: PriorityQueue HuffmanTree -> HuffmanTree
atLeast pq 
      | PriorityQueue.is_empty (snd(PriorityQueue.least pq)) == True = fst(fst(PriorityQueue.least pq))
      | otherwise = atLeast(toTree pq)

      
{- toQueue table
   PURPOSE: To put every tuple within a table into a priorityqueue of unsorted huffmantrees (nodes and leafs).  
   PRE:
   RETURNS: A priorityQueue with Leafs and nodes corresponding to the tuples in table
   SIDE EFFECTS:
   EXAMPLES: toQueue T [('j',1),('e',1),('h',1)] = BinoHeap [Node 0 1 (Leaf ('h',1)) [],Node 1 1 (Leaf ('j',1)) [Node 0 1 (Leaf ('e',1)) []]]
 -}
toQueue :: Table Char Int -> PriorityQueue HuffmanTree 
toQueue table = Table.iterate (table) toQueueAux (PriorityQueue.empty)


{- toQueueAux pq (a,x)
   PURPOSE: Putting tuples into leafs, which are then put in an originally empty priorityQueue
   PRE: 
   RETURNS: A priorityQueue with Leafs and nodes corresponding to the tuples in table
   SIDE EFFECTS:
   EXAMPLES: toQueueAux BinoHeap [] T [('j',1),('e',1),('h',1)] = BinoHeap [Node 0 1 (Leaf ('h',1)) [],Node 1 1 (Leaf ('j',1)) [Node 0 1 (Leaf ('e',1)) []]]
 -}
toQueueAux :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
toQueueAux pq (a,x) = PriorityQueue.insert pq (Leaf (a,x), x)


------ INTE KLAR MED KOMMENTAR
{- toTree pq
   PURPOSE: The given huffmantree within the priorityqueue is unsorted, toTree finds the two leafs or nodes with the lowest value and combines them into a new node, which is then replaced into the priorityqueue.
   PRE:
   RETURNS: The huffmantree in pq, but with two of its nodes and/or leafs combined into a node
   SIDE EFFECTS:
   EXAMPLES: toTree (BinoHeap [Node 0 1 (Leaf ('h',1)) [],Node 1 1 (Leaf ('j',1)) [Node 0 1 (Leaf ('e',1)) []]]) = BinoHeap [Node 1 1 (Leaf ('e',1)) [Node 0 2 (Node (Leaf ('j',1)) (Leaf ('h',1)) 2) []]]
 -}
toTree :: PriorityQueue HuffmanTree -> PriorityQueue HuffmanTree
toTree pq
     | PriorityQueue.is_empty (snd(PriorityQueue.least pq)) == True = pq
     | otherwise = PriorityQueue.insert (snd(PriorityQueue.least(snd(PriorityQueue.least pq)))) ((Node (fst(fst(PriorityQueue.least pq))) (fst(fst(PriorityQueue.least(snd(PriorityQueue.least pq))))) ((snd(fst(PriorityQueue.least pq))) + (snd(fst(PriorityQueue.least(snd(PriorityQueue.least pq))))))), ((snd(fst(PriorityQueue.least pq))) + (snd(fst(PriorityQueue.least(snd(PriorityQueue.least pq))))))) 




{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
--codeTable (Leaf (a,x)) = Table.insert Table.empty a [True]
codeTable h = toTable Table.empty (findLeaf h [])

--codeTablee (Node a b x) = Table.empty Table.insert 




findLeaf :: HuffmanTree -> BitCode -> [(Char, BitCode)]
findLeaf (Leaf (a,y)) acc = [(a, acc)]
findLeaf (Node lt rt int) acc = (findLeaf lt (acc++[False])) ++ (findLeaf rt (acc++[True]))

toTable :: Table Char BitCode -> [(Char, BitCode)] -> Table Char BitCode
toTable table [] = table 
toTable table ((a,b):xs) = toTable (Table.insert table a b) xs 
 


--extra argument som ör table, rekursiva kallet stoppar vi in i table


{------------------------------------------------------------------------------IDÉ IDÉ IDÉ IDÉ IDÉ
codeTable :: HuffmanTree -> Table Char BitCode
codeTable Void = Table.empty
codeTable table = Table.
-}

  

      --     | lt == Leaf = Table.insert lt ++ findLeaf höger
{-
codeTablee (Leaf (a, x) = Table.insert ('h', --LISTA MED TRUE FALSE
codeTablee (Node (Node a b x) (Node c d y) z) = codeTablee ((Node a b x)) -- SKICKA MED LISTA T/F 
codeTablee (Node (Leaf (a,x)) (Node b c y) z) = codeTablee (Leaf (a,x)) -- SKICKA MED LISTA T/F

..PURPOSE: To convert a string into a tuple that consists of the huffmantree corresponding to the string, and the bitcode for the huffmantree

-}

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress [] = ((Leaf ('a', 0)), [])
compress s = (huffmanTree (characterCounts s), toBitCode s s)


toBitCode :: String -> String -> BitCode -- (HuffmanTree, BitCode)
toBitCode [] _ = []
toBitCode str same = fromJust (Table.lookup (codeTable(huffmanTree(characterCounts same))) (head str)) ++ toBitCode (tail str) same 

                                                      
fromJust :: Maybe a -> a
fromJust (Just a) = a

{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress (Leaf (a,x)) [] = replicate x a
decompress h bits = unpacker h h [] bits


unpacker :: HuffmanTree -> HuffmanTree -> String -> BitCode -> String
unpacker (Leaf (a,z)) same x [] = unpacker same same (a:x) []
unpacker _ same str [] = reverse str
unpacker (Leaf (a,z)) same x bits = unpacker same same (a:x) bits
unpacker (Node l r int) same str bits
      | head bits == True  = unpacker r same str (drop 1 bits)
      | head bits == False = unpacker l same str (drop 1 bits)




--unpacker (Leaf (a,z)) same (x:xs) [] =  unpacker same same (a:x:xs) []  
--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6] 
