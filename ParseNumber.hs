module ParseNumber where
import PlDataType

  -- | nextChar == '-' = case negativeResult of
  --   Error err -> Error err
  --   Content content -> Content (Number (read (nextChar:show(fst content)) :: Integer), snd content)

-- parseNegativeNumber Function
parseNegativeNumber :: String -> ThrowError (SchemeVal, RestString)
parseNegativeNumber ps
  | nextChar == '-' = case negativeResult of
    Error err -> Error err
    Content content -> Content (Number (read (nextChar:show(fst content)) :: Integer), snd content)
  | otherwise = Error Possible
    where
      (nextChar:restStr) = spaceRemover ps
      negativeResult = parseNumber restStr
      
-- parseNumber Function
parseNumber :: String -> ThrowError (SchemeVal, RestString)
parseNumber ps
  | isDigit nextChar = case parseResult of
    Error err -> Error err
    Content content -> Content (Number (read (nextChar:fst content) :: Integer), snd content)
  | otherwise = Error Possible
  where
    (nextChar:restStr) = spaceRemover ps
    parseResult = parseNumberStr restStr
    
-- parseNumberStr Function
parseNumberStr :: String -> ThrowError (String, RestString)
parseNumberStr [] = Content ("", "")
parseNumberStr (x:xs)
  | charParseOr [isSpace, isCap] x = Content ("", x:xs)
  | isDigit x = case nextResult of
    Error _ -> nextResult
    Content content -> Content (x:(fst content), snd content)
  | otherwise = Error Possible
    where
      nextResult = parseNumberStr xs
  