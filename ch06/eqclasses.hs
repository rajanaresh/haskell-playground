class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

add :: Int -> Int -> Int
add x y = x + y

data Color = Red | Blue | Green deriving (Show)
                          

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isEqual2 x y = not (isNotEqual2 x y)
  
  isNotEqual2 :: a -> a -> Bool
  isNotEqual2 x y = not (isEqual2 x y)

instance BasicEq2 Color where
  isEqual2 Red Red = True
  isEqual2 Blue Blue = True
  isEqual2 Green Green = True
  isEqual2 _ _  = False
  