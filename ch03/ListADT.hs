data List a = Cons a (List a)
            | Nil
              deriving (Show)
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

first (Cons x _) = x
consList (Cons x xs) = (x:(consList xs))
consList Nil = []
