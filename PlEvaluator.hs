module PlEvaluator where
import PlDataType
import Data.IORef
import Control.Monad
import HelpString
import System.Time

-----------------------------------------------------------
-- primitives
-----------------------------------------------------------
primitives :: [(String, [SchemeVal] -> ThrowError SchemeVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", carop),
              ("cdr", cdrop),
              ("cons", consop),
              ("eq?", eqvop),
              ("eqv?", eqvop)]

-----------------------------------------------------------
-- binop functions
-----------------------------------------------------------
numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> ThrowError SchemeVal
numericBinop op singleVal@[] = Error $ NumArgs 2 singleVal
numericBinop op singleVal@[_] = Error $ NumArgs 2 singleVal
numericBinop op params = do  
  -- mapResult <- mapResultLift
  case mapResultLift of
    Error err -> Error err
    Content content -> Content $ Number $ foldl1 op content
  where
    mapResultLift = mapWithThrow unpackNumLift params
    unpackNumLift x = unpackNum x

boolBinop :: (SchemeVal -> ThrowError a) -> (a -> a -> Bool) -> [SchemeVal] -> ThrowError SchemeVal
boolBinop unpacker op args 
  | argLength /= 2 = Error $ NumArgs 2 args
  | otherwise = case (argLeft, argRight) of
    (Error err, _) -> Error err
    (_, Error err) -> Error err
    (Content left, Content right) -> Content $ Bool $ op left right
    where
      argLength = length args
      argLeft = unpacker $ args !! 0
      argRight = unpacker $ args !! 1
  
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-----------------------------------------------------------
-- complex defined functions
-----------------------------------------------------------
carop :: [SchemeVal] -> ThrowError SchemeVal
carop [List (x:xs)] = Content x
carop [DottedList (x:xs) _] = Content x
carop [xs] = Error $ TypeMismatch "pair" xs
carop xs = Error $ NumArgs 1 xs

cdrop :: [SchemeVal] -> ThrowError SchemeVal
cdrop [List (x:xs)] = Content $ List xs
cdrop [DottedList [xs] x] = Content x
cdrop [DottedList (_:xs) x] = Content $ DottedList xs x
cdrop [xs] = Error $ TypeMismatch "pair" xs
cdrop xs = Error $ NumArgs 1 xs

consop :: [SchemeVal] -> ThrowError SchemeVal
consop [x1, List []] = Content $ List [x1]
consop [x, List xs] = Content $ List $ x:xs
consop [x, DottedList xs xlast] = Content $ DottedList (x:xs) xlast
consop [x1, x2] = Content $ DottedList [x1] x2
consop xs = Error $ NumArgs 2 xs

eqvop :: [SchemeVal] -> ThrowError SchemeVal
eqvop [(Bool arg1), (Bool arg2)] = Content $ Bool $ arg1 == arg2
eqvop [(Number arg1), (Number arg2)] = Content $ Bool $ arg1 == arg2
eqvop [(String arg1), (String arg2)] = Content $ Bool $ arg1 == arg2
eqvop [(Atom arg1), (Atom arg2)] = Content $ Bool $ arg1 == arg2
eqvop [(DottedList xs x), (DottedList ys y)] = eqvop [List $ x:xs, List $ y:ys]
eqvop [(List arg1), (List arg2)]
  | arg1Len /= arg2Len = Content $ Bool False
  | otherwise = Content $ Bool $ and $ map eqvPair $ zip arg1 arg2
    where
      arg1Len = length arg1
      arg2Len = length arg2
      eqvPair :: (SchemeVal, SchemeVal) -> Bool
      eqvPair (x1, x2) = case eqvop [x1, x2] of
        Error _ -> False
        Content (Bool val) -> val
eqvop [_, _] = Content $ Bool False
eqvop xs = Error $ NumArgs 2 xs

-----------------------------------------------------------
-- eval, etc functions
-----------------------------------------------------------
eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
eval env val@(String _) = return $ Content val
eval env val@(Number _) = return $ Content val
eval env val@(Bool _) = return $ Content val
eval env (List [Atom "quote", val]) = do
  return $ Content val

eval env (List [Atom "display", var]) = do
  evalResult <- eval env var
  case evalResult of
    Error err -> return $ Error err
    Content content -> case content of
      String string -> do
        putStr string
        return $ Content $ None
      error -> return $ Error $ TypeMismatch "string" error
eval env (List (Atom "display":_)) = return $ Error $ NumArgs 1 $ [Atom "display"]

eval env (List [Atom "newline"]) = do
  putStrLn ""
  return $ Content $ None
  
eval env (List [Atom "set!", Atom var, form]) = do
  formResult <- eval env form 
  case formResult of
    Error err -> return $ Error err
    Content value -> setVar env var value

eval env (List [Atom "let*", List [], body]) = eval env body
eval env (List [Atom "let*", List xs, body]) = evalInner env xs body
  where
    evalInner :: Env -> [SchemeVal] -> SchemeVal -> IOThrowsError SchemeVal
    evalInner env [] body = eval env body
    evalInner env (x:xs) body = do
      envRef <- readIORef env
      element <- unCapAtom env (unCapList x)
      case element of
        Error err -> return $ Error err
        Content content -> do
          nextEnv <- newIORef $ extentEnv content envRef
          evalInner nextEnv xs body
      where
        extentEnv elem env = elem:env
        unCapAtom :: Env -> [SchemeVal] -> IOThrowsError (String, IORef SchemeVal)
        unCapAtom env [Atom var, target] = do
          evaledTarget <- eval env target
          case evaledTarget of
            Error err -> return $ Error err
            Content content -> do
              newRef <- newIORef content
              return $ Content (var, newRef)
        unCapAtom env [xs, target] = return $ Error $ TypeMismatch "atom" xs
        unCapAtom env xs = return $ Error $ Default "Let op variable error: 2 operands needed."
        unCapList :: SchemeVal -> [SchemeVal]
        unCapList (List var) = var

eval env (List [Atom "let", List [], body]) = eval env body
eval env (List [Atom "let", List xs, body]) = do
  unCapListR <- mapWithIOThrow unCapList xs
  case unCapListR of
    Error err -> return $ Error err
    Content content -> do
      mapResult <- mapWithIOThrow unCapAtom content
      case mapResult of
        Error err -> return $ Error err
        Content content -> do
          envInner <- bindVars env content
          eval envInner body
  where
    unCapList (List xs) = return $ Content xs
    unCapList xs = return $ Error $ TypeMismatch "list" xs
    unCapAtom [Atom var, target] = do
      evaledTarget <- eval env target
      case evaledTarget of
        Error err -> return $ Error err
        Content content -> return $ Content (var, content)
    unCapAtom [xs, target] = return $ Error $ TypeMismatch "atom" xs
    unCapAtom xs = return $ Error $ Default "Let op variable error: 2 operands needed."
    
eval env (List (Atom "begin":[])) = return $ Error $ Default "Begin op variable error: least 1 operand needed."
eval env (List (Atom "begin":xs)) = do
  results <- mapWithIOThrow (eval env) xs
  case results of
    Error err -> return $ Error err
    Content content -> return $ Content $ last content
    
eval env (List [Atom "define", Atom var, form]) = do
  formResult <- eval env form
  case formResult of
    Error err -> return $ Error err
    Content value -> defineVar env var value
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
  
eval env (List (Atom "lambda" : List params : body)) = do
  makedF <- makeNormalFunc env params body
  return $ makeContent makedF
  where
    makeContent content = Content content
  
eval env (List (Atom "lambda" : DottedList params varargs : body)) = do
  makedF <- makeVarargs varargs env params body
  return $ makeContent makedF
  where
    makeContent content = Content content
    
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = do
  makedF <- makeVarargs varargs env [] body
  return $ makeContent makedF
  where
    makeContent content = Content content
  
eval env (List [Atom "if", pred, conseq, alt]) = do
  predResult <- eval env pred
  case predResult of
    Error err -> return $ Error err
    Content (Bool True) -> eval env conseq
    Content (Bool False) -> eval env alt
    
eval env (List (Atom "list":seq)) = do
  seqResult <- mapWithIOThrow (eval env) seq
  case seqResult of
    Error err -> return $ Error err
    Content content -> return $ Content $ List content

eval env (List [Atom "estimate", body]) = do
  time1 <- getClockTime
  bodyResult <- eval env body
  time2 <- getClockTime
  putStrLn ("estimate result: " ++ (show (tdSec (diffClockTimes time2 time1))) ++ "sec. " ++ (show $ convertToMili (tdPicosec (diffClockTimes time2 time1))) ++ "mili sec.")
  return bodyResult
  where
    convertToMili :: Integer -> Float
    convertToMili n = fromInteger(n) * 0.1 ^ 6
    
eval env (List (Atom "cond":xs)) = evalCondition xs
  where
    evalCondition :: [SchemeVal] -> IOThrowsError SchemeVal
    evalCondition [] = return $ Error $ Default "cond error: condition not matched."
    evalCondition (x:xs) = do
      evalResult <- evalConditionInner x
      case evalResult of
        Error Possible -> evalCondition xs
        Error err -> return $ Error err
        Content content -> return $ Content content
    
    evalConditionInner (List [Atom "else", body]) = eval env body
    evalConditionInner (List [condition, body]) = do
      conditionResult <- eval env condition
      case conditionResult of
        Error err -> return $ Error err
        Content (Bool True) -> eval env body
        Content (Bool False) -> return $ Error Possible
    evalConditionInner (List err) = return $ Error $ NumArgs 2 err
    
eval env (List (Atom "help":xs)) = do
  mapM putStrLn helpDesc
  return $ Content None
  

eval env (List (Atom func : args)) = do
  mapResult <- mapWithIOThrow (eval env) args
  funcResult <- eval env $ Atom func
  case (funcResult, mapResult) of
    (Error err, _) -> return $ Error err
    (_, Error err) -> return $ Error err
    (Content funcContent, Content mapContent) -> apply funcContent mapContent
    
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapWithIOThrow (eval env) args
  case (func, argVals) of
    (Error err, _) -> return $ Error err
    (_, Error err) -> return $ Error err
    (Content funcContent, Content argsContent) -> apply funcContent argsContent

eval env (Atom id) = do 
  bounded <- isBound env id
  if not bounded 
    then
    case id of
      -- 예외적인 상수 atom의 경우 여기서 초기 선언을 해준다.
      -- atom 초기 선언부 --
      "#t" -> return $ Content $ Bool True
      "#f" -> return $ Content $ Bool False
      otherwise -> result
    else result
  where
    result = getVar env id

eval env (DottedList head tail) = return $ Content $ DottedList head tail
  
eval env xs = return $ Error $ BadSpecialForm "Not special form." xs
  
mapWithIOThrow :: (a -> IOThrowsError b) -> [a] -> IOThrowsError [b]
mapWithIOThrow f [] = return $ Content []
mapWithIOThrow f (x:xs) = do
  evalResult <- f x
  case evalResult of
    Error err -> return $ Error err
    Content content -> do
      restListResult <- mapWithIOThrow f xs
      case restListResult of
        Error err -> return $ Error err
        Content content' -> return $ Content (content:content')

mapWithThrow :: (a -> ThrowError b) -> [a] -> ThrowError [b]
mapWithThrow f [] = Content []
mapWithThrow f (x:xs) =
  case evalResult of
    Error err -> Error err
    Content content -> case restListResult of
        Error err -> Error err
        Content content' -> Content (content:content')
  where
    evalResult = f x
    restListResult = mapWithThrow f xs

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (PrimitiveFunc func) args = return $ func args
apply (Func params varargs body closure) args =
  if paramNum /= num args && varargs == Nothing
  then return $ Error $ NumArgs paramNum args
  else do
    env <- bindVars closure $ zip params args 
    env2 <- bindVarArgs varargs env
    evalBody env2
    where
      remainingArgs = drop (length params) args
      num = toInteger . length
      paramNum = num params
      evalBody env = do
        mapResult <- mapM (eval env) body
        return $ last mapResult
      bindVarArgs arg env = case arg of
        Just argName -> bindVars env [(argName, List remainingArgs)]
        Nothing -> return env
apply (Atom func) args = case lookup func primitives of
  Nothing -> return $ Error $ NotFunction "Not a function" func
  Just content -> return $ content args
apply (Number funcNum) args = case lookup func primitives of
  Nothing -> return $ Error $ NotFunction "Not a function" func
  Just content -> return $ content args
  where
    func = show funcNum
apply (String func) args = case lookup func primitives of
  Nothing -> return $ Error $ NotFunction "Not a function" func
  Just content -> return $ content args
apply _ args = return $ Error $ Default "Apply Internal error. not catched var."
  
-----------------------------------------------------------
-- unpack functions
-----------------------------------------------------------
unpackNum :: SchemeVal -> ThrowError Integer
unpackNum (Number n) = Content n
unpackNum (String n) = let parsed = (reads n :: [(Integer, String)])
                       in 
                          if null parsed
                          then Error $ TypeMismatch "number" $ String n
                          else Content $ fst $ parsed !! 0
                                 
unpackNum (List [n]) = unpackNum n
unpackNum xs = Error $ TypeMismatch "number" xs

unpackStr :: SchemeVal -> ThrowError String
unpackStr (String s) = Content s
unpackStr (Number s) = Content $ show s
unpackStr (Bool s) = Content $ show s
unpackStr xs = Error $ TypeMismatch "string" xs

unpackBool :: SchemeVal -> ThrowError Bool
unpackBool (Bool s) = Content s
unpackBool xs = Error $ TypeMismatch "boolean" xs
