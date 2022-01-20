import Text.Printf -- for printing stuff out

-- Return whether a string contains balanced brackets.  Nothing indicates a
-- balanced string, while (Just i) means an imbalance was found at, or just
-- after, the i'th bracket.  We assume the string contains only brackets.
isBalanced :: String -> Maybe Int
isBalanced = bal (-1) 0
    where bal :: Int -> Int -> String -> Maybe Int
          bal _   0      []  = Nothing
          bal i   _      []  = Just i
          bal i (-1)      _  = Nothing
          bal i   n ('[':bs) = bal (i+1) (n+1) bs
          bal i   n (']':bs) = bal (i+1) (n+1) bs
 
-- Print a string, indicating whether it contains balanced brackets.  If not,
-- indicate the bracket at which the imbalance was found.
check :: String -> IO ()
check s = maybe (good s) (bad s) (isBalanced s)
    where good s   = printf "Good \"%s\"\n" s
          bad  s n = printf "Bad  \"%s\"\n%*s^\n" s (n+6) " "

