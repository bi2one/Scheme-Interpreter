module PlParser where
import ParseString (parseString)
import ParseAtom (parseAtom)
import ParseNumber (parseNumber, parseNegativeNumber)
import PlDataType

-- startParse
startParse :: String -> ThrowError (SchemeVal, RestString)
startParse xs = parseOr [parseString, parseNegativeNumber, parseNumber, parseAtom, parseList, parseQuote] xs

-- parseList function
parseList :: String -> ThrowError (SchemeVal, RestString)
parseList ps = case nextChar of
  '(' -> case innerResult of
    Error err -> Error err
    Content content -> if isConsPossible $ fst content 
                       then Content (DottedList (init (init contentFst)) (last contentFst), snd content)
                       else Content (List contentFst, snd content)
                         where
                           contentFst = fst content
  otherwise -> Error Possible
  where
    (nextChar:restStr) = spaceRemover ps
    innerResult = parseListInner restStr
    isConsPossible :: [SchemeVal] -> Bool
    isConsPossible [Atom ".", val] = True
    isConsPossible (x:xs) = isConsPossible xs
    isConsPossible _ = False
    
-- parseListInner function
parseListInner :: String -> ThrowError ([SchemeVal], RestString)
parseListInner [] = Error $ Default "Syntax error: Please match parenthesis."
parseListInner xs = case nextChar of
  ')' -> Content ([], restStr)
  otherwise -> case nextResult of
    Error err -> Error err
    Content content -> case nextNextResults of
      Error _ -> nextNextResults
      Content content' -> Content ((fst content):(fst content'), snd content')
  where
    (nextChar:restStr) = xs
    nextResult = startParse xs
    (Content content) = nextResult
    nextNextResults = parseListInner $ spaceRemover $ snd content
    
-- parseQuote function
parseQuote :: String -> ThrowError (SchemeVal, RestString)
parseQuote ps = case nextChar of
  '\'' -> case nextResult of
    Error err -> Error err
    Content content -> Content (List [Atom "quote", fst content], snd content)
  otherwise -> Error Possible
  where
    (nextChar:restStr) = spaceRemover ps
    nextResult = startParse restStr