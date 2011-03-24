{-# OPTIONS -XNoMonomorphismRestriction #-}
module PlDataType where
import Data.List (isInfixOf)
import Data.IORef
import Control.Monad

-- environment -------------------------------------------
type Env = IORef [(String, IORef SchemeVal)]

type IOThrowsError a = IO (ThrowError a)

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowError a -> IOThrowsError a
liftThrows (Error err) = return $ Error err
liftThrows (Content content) = return $ Content content

extractValue :: ThrowError a -> a
extractValue (Content val) = val

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  ref <- readIORef envRef
  return $ searchValue ref
  where
    searchValue ref = case lookupResult of
      Nothing -> False
      otherwise -> True
      where
        lookupResult = lookup var ref

getVar :: Env -> String -> IOThrowsError SchemeVal
getVar envRef var = do
  ref <- readIORef envRef
  case searchResult ref of
    Nothing -> return $ Error $ UnboundVar "Getting an unbound variable" var
    Just value -> do
      buf <- readIORef value
      return $ Content buf
    where
      searchResult ref = lookup var ref
      
setVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var value = do
  ref <- readIORef envRef
  case searchResult ref of
    Nothing -> return $ Error $ UnboundVar "Setting an unbound variable" var
    Just justVal -> do
      writeIORef justVal value
      return $ Content value
    where
      searchResult ref = lookup var ref
      
defineVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var value = do
  alreadyDefined <- isBound envRef var
  if alreadyDefined
    then do
    buf <- setVar envRef var value
    return buf
    else do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return $ Content value

bindComplexFuncs :: Env -> [String] -> IO Env
bindComplexFuncs envRef names = readIORef envRef >>= extendEnv names >>= newIORef
  where
    extendEnv names env = liftM (++ env) (mapM addBinding names)
    addBinding name = do
      ref <- newIORef $ makeComplexFunc name
      return (name, ref)

bindVars :: Env -> [(String, SchemeVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
      
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal
makeComplexFunc name = PrimitiveComplexFunc name

-----------------------------------------------------------
type RestString = String

data SchemeVal = Atom String
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Number Integer
               | String String
               | Bool Bool
               | PrimitiveFunc ([SchemeVal] -> ThrowError SchemeVal)
               | PrimitiveComplexFunc {name :: String}
               | Func {params :: [String], vararg :: (Maybe String),
                       body :: [SchemeVal], closure :: Env}
               | None

data ThrowError a = Content a
                  | Error SchemeError
                  deriving (Show)
                
data SchemeError = NumArgs Integer [SchemeVal]
                 | TypeMismatch String SchemeVal
                 | ParseError String
                 | BadSpecialForm String SchemeVal
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String
                 | Possible
                 | Space

showVal :: SchemeVal -> String
showVal (PrimitiveComplexFunc name) = "<Complex Function: " ++ name ++ ">"
showVal (PrimitiveFunc _) = "<Primitive Function>"
showVal (Func _ _ _ _) = "<Defined Function>"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal None = "None"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
instance Show SchemeVal where show = showVal
                              
unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map showVal

showError :: SchemeError -> String
showError Space = ""
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (Default message) = message
showError (NumArgs expected found) = "Expected " ++ show expected 
                                     ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                          ++ ", found " ++ showVal found
showError (ParseError parseErr) = "Parse error at " ++ show parseErr

instance Show SchemeError where show = showError

letter = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbol = "`~!@#$%^&*_-+=\\|/.,;:[]{}<>?"
digit = "1234567890"
space = " \n"
cap = "()"

-- isEmpty
isEmpty :: String -> Bool
isEmpty xs
  | removeResult == "" = True
  | otherwise = False
    where
      removeResult = spaceRemover xs

-- Define basic char verifier
isCharType :: Char -> String -> Bool
isCharType x str = isInfixOf [x] str

isLetter,isSymbol,isDigit,isSpace,isCap :: Char -> Bool
isLetter x = isCharType x letter
isSymbol x = isCharType x symbol
isDigit x = isCharType x digit
isSpace x = isCharType x space
isCap x = isCharType x cap

-- Remove front space from String
spaceRemover :: String -> String
spaceRemover (x:xs) = if isInfixOf [x] space
                      then spaceRemover xs
                      else x:xs
spaceRemover [] = []

-- parseOr function: bind many parse operation.
parseOr :: [String -> ThrowError (SchemeVal, RestString)] -> String -> ThrowError (SchemeVal, RestString)
parseOr [] xs = Error $ ParseError ("front of " ++ xs)
parseOr (f:fs) ps = case r of
  Error Possible -> parseOr fs ps
  otherwise -> r
  where
    r = f ps

-- charParseOr function
charParseOr :: [Char->Bool] -> Char -> Bool
charParseOr [] _ = False
charParseOr (f:fs) x = if f x
                       then True
                       else charParseOr fs x
