module ParseAtom where
import PlDataType

-- parseAtom Function
parseAtom :: String -> ThrowError (SchemeVal, RestString)
parseAtom ps
  | charParseOr [isSymbol, isLetter] nextChar && nextChar /= '\'' =
    case restResult of
      Error err -> Error err
      Content content -> Content (Atom $ nextChar:(fst content), snd content)
  | otherwise = Error $ Possible
      where
        (nextChar:restStr) = spaceRemover ps
        restResult = parseAtomInner restStr
                
-- parseAtomInner Function
parseAtomInner :: String -> ThrowError (String, RestString)
parseAtomInner [] = Content ("", "")
parseAtomInner (x:xs)
  | charParseOr [isSpace, isCap] x = Content ("", x:xs)
  | charParseOr [isLetter, isSymbol, isDigit] x = case nextResult of
    Error _ -> nextResult
    Content content -> Content (x:(fst content), snd content)
  | otherwise = Error $ ParseError [x]
    where
      nextResult = parseAtomInner xs