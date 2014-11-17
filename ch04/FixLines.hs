import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = fixLines
        
splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest) -> splitLines rest
    ('\n':rest) -> splitLines rest
    _ -> []
  where (pre, suf) = break isLineTerminator cs

isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)

--Should be in a Matrix type with rank and other functionality, not going to check any of that here
transpose :: [[a]] -> [[a]]
transpose m
  | any null m = []
  | True = map head m : transpose (map tail m)
