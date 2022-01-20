-- Emil Engelin and Erdem Garip
-- TODO: Ändra length xs till t.ex. length words (KLART?)
-- TODO: newline efter let och in (KLART?)
-- TODO: Lätt dokumentation på komplicerade let ins (KLART?)

-- DO NOT MODIFY THE FOLLOWING LINE
module CompLing (wordCount, adjacentPairs, pairsCount, neighbours, mostCommonNeighbour) where

import PandP      -- provide sample text to play with (variable austin)
import Test.HUnit -- provides testing framework

-- DO NOT CHANGE THESE TYPES
type Sentence = [String]

type Document = [Sentence]

type WordTally = [(String, Int)]

type Pairs = [(String, String)]

type PairsTally = [((String, String), Int)]

-- DO NOT CHANGE THE TYPE SIGNATURES FOR THESE FUNCTIONS

{- wordCount document
  Computes word tallies found in document.
  RETURNS: word tallies from words found in document.
  EXAMPLES: wordCount []                                                                   == []
            wordCount [["a", "b", "c"], ["a"]]                                             == [("a", 2), ("c", 1), ("b", 1)]
            wordCount [["a", "rose", "is", "a", "rose"], ["but", "so", "is", "a", "rose"]] == [("rose", 3), ("so", 1), ("a", 3), ("is", 2), ("but", 1)]
-}
wordCount :: Document -> WordTally
wordCount document =
  {- countWords words wordTallies -}
  let 
    countWords [] wordTallies = wordTallies
    -- VARIANT: length words
    countWords (w:ws) wordTallies = countWords ws (wordCountAux w wordTallies [])
  in
    countWords [word | sentence <- document, word <- sentence] []

{- wordCountAux word wordTallies acc
  Increments the count of the tally containing word or adds a new word
  tally if no tally with an equal word existed before.
  RETURNS: a copy of wordTallies with either one of the word tallies having had their count
           incremented by 1 or a new word tally having been added to the copy.
  EXAMPLES: wordCountAux "a" [] []         == [("a", 1)]
            wordCountAux "a" [("a", 1)] [] == [("a", 2)]
            wordCountAux "a" [("b", 1)] [] == [("a", 1), ("b", 1)]
-}
wordCountAux :: String -> WordTally -> WordTally -> WordTally
-- We found no instance of word, add a new tally:
wordCountAux word [] acc = (word, 1):acc
-- VARIANT: length wordTallies
wordCountAux word ((tallyWord, tallyCount):wts) acc
  | tallyWord == word =
    ((word, tallyCount + 1) : acc) ++ wts
  | otherwise =
    wordCountAux word wts ((tallyWord, tallyCount):acc)

{-
  Computes adjecent word pairs of document, that is, every word k and k+1.
  RETURNS: word pairs where each pair contains a word and its successor.
  EXAMPLE: adjacentPairs []                                                            == []
           adjacentPairs [["time"], ["not"]]                                           == []
           adjacentPairs [["time", "for"], ["not"]]                                    == [("time", "for")]
           adjacentPairs [["time", "for", "a", "break"], ["not", "for", "a", "while"]] == [("time", "for"), ("for", "a"), ("a", "break"), ("not", "for"), ("for", "a"), ("a", "while")]
-}
adjacentPairs :: Document -> Pairs
adjacentPairs document =
  {- group sentence pairs -}
  let
    group [] pairs  = pairs
    group [_] pairs = pairs
    -- VARIANT: length sentence
    group (w0:w1:ws) pairs = group (w1:ws) ((w0, w1):pairs)
  in
    [pair | sentence <- document, pair <- reverse (group sentence [])]

{- initialPairs document
  Computes a list containing the first word pair of each sentence in document.
  RETURNS: the first word pairs, one from each sentence of document,
           iff said sentence contains at least 2 words.
  EXAMPLES: initialPairs []                            == []
            initialPairs [["a"], ["b"]]                == []
            initialPairs [["a"], ["b", "c"]]           == [("b", "c")]
            initialPairs [["a", "b"], ["c", "d", "e"]] == [("a", "b"), ("c", "d")]
-}
initialPairs :: Document -> Pairs
initialPairs document =
  {- initial sentence -}
  -- VARIANT: length sentence
  let initial (w0:w1:_) = (w0, w1)
  in [initial sentence | sentence <- document, length sentence >= 2]

{- finalPairs document
  Computes a list containing the last word pair of each sentence in document.
  RETURNS: the last word pairs, one from each sentence of document,
           iff said sentence contains at least 2 words.
  EXAMPLES: finalPairs []                            == []
            finalPairs [["a"], ["b"]]                == []
            finalPairs [["a"], ["b", "c"]]           == [("b", "c")]
            finalPairs [["a", "b"], ["c", "d", "e"]] == [("a", "b"), ("d", "e")]
-}
finalPairs :: Document -> Pairs
finalPairs document =
  {- final sentence -}
  let
    final [w0, w1] = (w0, w1)
    -- VARIANT: length sentence
    final (_:ws) = final ws
  in
    [final sentence | sentence <- document, length sentence >= 2]

{- pairsCount pairs
  Takes word pairs and computes the occurance of equal word pairs where (x, y) and (y, x) are regarded as the same pair.
  RETURNS: word pair tallies with the Int in each tuple representing the amount of times the word pair occurred together
           (where (x, y) == (y, x) and x and y are words).
  EXAMPLE: pairsCount [("big","bear"),("bear","big"),("big","dog")]                     == [(("big", "dog"), 1), (("big", "bear"), 2)]
           pairsCount [("ball", "rack"), ("rack", "ball"), ("rack", "loft")]            == [(("rack", "loft"), 1), (("ball", "rack"), 2)]
           pairsCount [("stun", "meteor"), ("warrior", "desperate"), ("prick", "stun")] == [(("prick", "stun"), 1), (("stun", "meteor"), 1), (("warrior", "desperate"), 1)]
-}
pairsCount :: Pairs -> PairsTally
pairsCount pairs =
  {- count pairs tallies -}
  let
    count [] tallies = tallies
    -- VARIANT: length pairs
    count (p:ps) tallies = count ps (pairsCountAux p tallies [])
  in
    count pairs []

{- pairsCountAux wordPair wordPairTallies acc
  Tries to find a pair in wordPairTallies which equals wordPair where a word pair of (x, y) is the same as (y, x).
  If none was found, a new word pair tally will have been added to wordPairTallies.
  RETURNS: a copy of wordPairTallies where either an existing word pair tallies count has
           been incremented by one or a new word pair tally have been added to the copy.
  EXAMPLES: pairsCountAux ("foo", "bar") [(("foo", "baz"), 1)] []                      == [(("foo", "bar"), 1),(("foo", "baz"), 1)]
            pairsCountAux ("foo", "bar") [(("foo", "bar"), 1), (("foo", "baz"), 1)] [] == [(("foo", "bar"), 2),(("foo", "baz"), 1)]
            pairsCountAux ("foo", "bar") [(("bar", "foo"), 1), (("foo", "baz"), 1)] [] == [(("bar", "foo"), 2),(("foo", "baz"), 1)]
-}
pairsCountAux :: (String, String) -> PairsTally -> PairsTally -> PairsTally
-- We found no instance of the pair, add a new one:
pairsCountAux wordPair [] acc = (wordPair, 1):acc
-- VARIANT: length wordPairTallies
pairsCountAux (s0, s1) (((w0, w1), pairCount):wpts) acc
  | (w0 == s0 && w1 == s1) || (w0 == s1 && w1 == s0) =
    (((w0, w1), pairCount + 1):wpts) ++ acc
  | otherwise =
    pairsCountAux (s0, s1) wpts (((w0, w1), pairCount):acc)

{- neighbours wordPairTallies word
  Gathers all neighbouring words of word along with the word pair tallies count found in wordPairTallies.
  RETURNS: word tallies where each word tally contains the neighbour of
           word and the number of times it was found to be a neighbour of word.
  EXAMPLES: neighbours [] "foo"                                         == []
            neighbours [(("foo", "bar"), 1)] "baz"                      == []
            neighbours [(("foo", "bar"), 1), (("foo", "baz"), 3)] "foo" == [("baz", 3), ("bar", 1)]
            neighbours [(("foo", "bar"), 1), (("foo", "baz"), 3)] "bar" == [("foo", 1)]
-}
neighbours :: PairsTally -> String -> WordTally
neighbours wordPairTallies word =
  {- search wordPairTallies wordTallies -}
  let
    search [] wordTallies = wordTallies
    -- VARIANT: length wordPairTallies
    search (((w0, w1), count):wpts) wordTallies
      | w0 == word || w1 == word =
        search wpts ((if w0 == word then w1 else w0, count):wordTallies)
      | otherwise =
        search wpts wordTallies
  in
    search wordPairTallies []

{- mostCommonNeighbour wordPairTallies word
  Finds the most commonly paired word neighbour of word in wordPairTallies.
  RETURNS: the most common paired word neighbour of word found in wordPairTallies
           or Nothing if word was not found in wordPairTallies.
  EXAMPLES: mostCommonNeighbour [] "foo"                                         == Nothing
            mostCommonNeighbour [(("foo", "bar"), 3), (("foo", "baz"), 1)] "foo" == Just "bar"
            mostCommonNeighbour [(("foo", "bar"), 3), (("foo", "baz"), 4), (("min", "din"), 1)] "foo" == Just "baz"
-}
mostCommonNeighbour :: PairsTally -> String -> Maybe String
mostCommonNeighbour wordPairTallies word =
  {- mostCommon wordTallies hasPair pair -}
  let
    mostCommon [] True pair = Just (fst pair)
    mostCommon [] False _   = Nothing
    -- VARIANT: length wordTallies
    mostCommon ((w, c):wts) hasPair pair
      | c > snd pair =
        mostCommon wts True (w, c)
      | otherwise =
        mostCommon wts hasPair pair
  in
    mostCommon (neighbours wordPairTallies word) False ("", minBound :: Int)

-- Test Cases
-- feel free to add other test cases here. an independent set of
-- test cases will be used when grading your code

-- wordCount
test1 = TestCase $ assertEqual "wordCount []" [] (wordCount [])

test2 = TestCase $ assertBool "wordCount [[\"a\",\"b\"],[\"a\"]]" (elem ("a", 2) (wordCount [["a", "b"], ["a"]]))

-- adjacentPairs, initialPairs, finalPairs
test3 = TestCase $ assertEqual "adjacentPairs [[\"foo\"],[\"bar\"]]" [] (adjacentPairs [["foo"], ["bar"]])

test3a = TestCase $ assertEqual "initialPairs" [("a", "b")] (initialPairs [["a", "b", "a"], ["c"]])

test3b = TestCase $ assertEqual "finalPairs" [("b", "a")] (finalPairs [["a", "b", "a"], ["c"]])

-- pairsCount
test4 =
  TestCase $
    assertBool
      "pairsCount simple"
      (elem (("a", "b"), 2) (pairsCount [("a", "b"), ("c", "d"), ("a", "b")]))

test5 =
  TestCase $
    assertBool
      "pairsCount tricky"
      ( let x = pairsCount (adjacentPairs [["a", "b", "a"], ["c"]])
         in elem (("a", "b"), 2) x || elem (("b", "a"), 2) x
      )

-- neighbours
test6 =
  TestCase $
    assertEqual
      "neighbours left"
      [("b", 2)]
      (neighbours [(("a", "b"), 2), (("c", "d"), 1)] "a")

test7 =
  TestCase $
    assertEqual
      "neighbours left"
      [("a", 2)]
      (neighbours [(("a", "b"), 2), (("c", "d"), 1)] "b")

-- mostCommonNeighbour
test8 =
  TestCase $
    assertEqual
      "mostCommonNeighbour text \"the\""
      (Just "fun")
      (mostCommonNeighbour input "the")
  where
    input = [(("the", "fun"), 4), (("the", "foot"), 3), (("dog", "power"), 2)]

test9 =
  TestCase $
    assertEqual
      "mostCommonNeighbour text \"spam\""
      Nothing
      (mostCommonNeighbour input "spam")
  where
    input = [(("the", "fun"), 4), (("the", "foot"), 3), (("dog", "power"), 2)]

-- testing the PandP.austin text
test10 =
  TestCase $
    assertEqual
      "mostCommonNeighbour of \"bennet\""
      (Just "mr")
      (mostCommonNeighbour (pairsCount $ adjacentPairs $ austin) "bennet")

-- for running all the tests (type "runtests" within ghci --- without the quotes)
runtests = runTestTT $ TestList [test1, test2, test3, test3a, test3b, test4, test5, test6, test7, test8, test9, test10]
