class Size a where 
  size :: a -> Int

instance Size Int where
  size _ = 4

instance Size Integer where
  size n = ceiling (logBase 2 (fromInteger n) / 8.0)

instance Size Char where
  size _= 2

instance Size () where
  size _= 1

instance Size Bool where
  size _= 1

instance Size a => Size [a] where
  size [] = 1
  size (x:xs) = size x + 1 + size xs

instance newtype F a = F (Int, a)
  data Tree a = Leaf a | Branch (Tree a) (Tree a)
  (a,b)
  (a,b,c)