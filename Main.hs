{-# OPTIONS -XNoMonomorphismRestriction #-}

module Main where
import PlParser (startParse)
import PlEvaluator
import PlDataType
import IO
import System.Time
        
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives) >>= (applyPrimitiveFunctions primitiveFunctions) >>= (flip bindComplexFuncs complexFunctionNames)
  where
    makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)
    
applyPrimitiveFunctions :: [String] -> Env -> IO Env
applyPrimitiveFunctions [] env = return env
applyPrimitiveFunctions (x:xs) env = do
  applyPrimitiveFunction x env
  applyPrimitiveFunctions xs env

applyPrimitiveFunction :: String -> Env -> IO Env
applyPrimitiveFunction primF env = do
  case parseResult of
    Error _ -> return env
    Content (content, _) -> do
      evalResult <- eval env content
      case evalResult of
        Error _ -> return env
        Content content -> return env
  where
    parseResult = startParse primF

complexFunctionNames :: [String]
complexFunctionNames = ["quote", "set!", "let", "begin", "define", "lambda", "if", "display", "newline", "cond", "let*", "estimate", "list"]

primitiveFunctions :: [String]
primitiveFunctions = [
  "(define (not x) (if x #f #t))",
  "(define (null? obj) (if (eqv? obj '()) #t #f))",
  "(define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))",
  "(define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))",
  "(define fold foldl)",
  "(define accumulate foldl)",
  "(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))",
  "(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))",
  "(define nil '())",
  "(define (abs n) (if (< n 0) (* -1 n) n))",
  "(define (or . lst) (fold || #f lst))",
  "(define (sum . lst) (fold + 0 lst))",
  "(define (product . lst) (fold * 1 lst))",
  "(define (and . lst) (fold && #t lst))",
  "(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))",
  "(define (reverse lst) (fold (flip cons) '() lst))",
  "(define (fibo n) (if (< n 3) 1 (+ (fibo (- n 1)) (fibo (- n 2)))))",
  "(define (append li1 li2) (if (null? li1) li2 (cons (car li1) (append (cdr li1) li2))))",
  "(define (flatmap proc seq) (accumulate append nil (map proc seq)))",
  "(define (enumerate-interval start end) (cond ((> start end) (list)) (else (cons start (enumerate-interval (+ start 1) end)))))"
  ]

description = [
  "==================================",
  "** Scheme interpreter **",
  "author: Jeongkyun Lee",
  "        Jihoon Ko",
  "        Mingi Kim",
  "        Jaewon Kim",
  "report: excgate@gmail.com",
  "* Type (help) to get information",
  "=================================="
  ]

main :: IO ()
main = do
  env <- primitiveBindings
  TOD clockTime semiTime <- getClockTime
  mapM putStrLn description
  mainLoop env
  where
    mainLoop :: Env -> IO()
    mainLoop env = do
      putStr "]=> " >> hFlush stdout
      inpStr <- getLine
      totalPrint env inpStr
      mainLoop env
      
    putResult :: Env -> ThrowError SchemeVal -> IO()
    putResult env (Error err) = putStrLn $ show err
    putResult env (Content result) = putStrLn $ "Return Result: " ++ (showVal result)
          
    totalPrint :: Env -> String -> IO()
    totalPrint env str = case str' of
      "" -> putStr ""
      otherwise -> do
        case startParse str' of
          Error err -> putStrLn $ show err
          Content (result, restStr) -> 
            do
              -- putStrLn $ show result
              evalResult <- eval env result
              putResult env evalResult
      where
        str' = spaceRemover str
