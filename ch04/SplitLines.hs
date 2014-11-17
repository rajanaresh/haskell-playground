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