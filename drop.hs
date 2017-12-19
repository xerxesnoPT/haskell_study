mydrop n xs = if n<=0 || null xs
              then xs
              else mydrop (n - 1) (tail xs)

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

returnList (Cons a as) = a : returnList as
returnList Nil = []


data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


data MyTree b = Node b (Maybe (MyTree b)) (Maybe (MyTree b)) 
                deriving (Show)


