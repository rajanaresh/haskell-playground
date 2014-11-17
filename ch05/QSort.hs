sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = (sort ys) ++ [x] ++ (sort zs)
  where ys = [a | a <- xs, a<=x]
        zs = [b | b <- xs, a>x]

