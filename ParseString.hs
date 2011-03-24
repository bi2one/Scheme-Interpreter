module ParseString where
import PlDataType

-- parseString Function
parseString :: String -> ThrowError (SchemeVal, RestString)
parseString ps = case removeResult of
  (nextChar:restStr) -> if nextChar == '\"' 
                           then case parseStringInner restStr of
                             Error err -> Error err
                             Content content -> Content $ (String (fst content), snd content)
                           else Error $ Possible
  otherwise -> Error $ Space
  where
    removeResult = spaceRemover ps
        
      
-- parseStringInner Function
parseStringInner :: String -> ThrowError (String, RestString)
parseStringInner [] = Error $ Default "Parse error: Close double quote."
parseStringInner (x:xs)
  | x == '\"' = Content ("", xs)
  | charParseOr [isDigit, isLetter, isSymbol, isSpace, isCap] x =
    case nextResult of
      Error _ -> nextResult
      Content content -> Content $ (x:(fst content), snd content)
  | otherwise = Error $ ParseError [x]
      where
        nextResult = parseStringInner xs