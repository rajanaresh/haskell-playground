myDrop :: Int -> [a] -> [a]
myDrop n l = if n <= 0 || null l
             then l
             else myDrop (n - 1) (tail l)

isOdd n = mod n 2 == 1

lastButOne :: [a] -> a
lastButOne l = if null (tail (tail l))
               then head l
               else lastButOne (tail l)
                    