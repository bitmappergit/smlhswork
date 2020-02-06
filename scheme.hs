{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import System.Environment
import IO hiding (try)
import Data.IORef
import Numeric
import Control.Applicative hiding ((<|>), many)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

infixl 4 <~>
(<~>) :: Functor a => (b -> c) -> (d -> a b) -> d -> a c
(<~>) a b = fmap a . b

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc env params body = makeFunc Nothing env params body
makeVarargs varargs env params body = (makeFunc . Just . showVal) varargs env params body

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env val@(Character _) = return val
eval env (Atom id)      = getVar env id
eval env (List [Atom "load", String filename]) =
  do load filename >>= last <~> mapM (eval env)
     return $ Bool True
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip params args) >>=
          bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env =
          case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop   (+))
             ,("-",         numericBinop   (-))
             ,("*",         numericBinop   (*))
             ,("/",         numericBinop   div)
             ,("mod",       numericBinop   mod)
             ,("quot",      numericBinop  quot)
             ,("rem",       numericBinop   rem)
             ,("=",         numBoolBinop  (==))
             ,("<",         numBoolBinop   (<))
             ,(">",         numBoolBinop   (>))
             ,("/=",        numBoolBinop  (/=))
             ,(">=",        numBoolBinop  (>=))
             ,("<=",        numBoolBinop  (<=))
             ,("&&",        boolBoolBinop (&&))
             ,("||",        boolBoolBinop (||))
             ,("string=?",  strBoolBinop  (==))
             ,("string?",   strBoolBinop   (>))
             ,("string<=?", strBoolBinop  (<=))
             ,("string>=?", strBoolBinop  (>=))
             ,("car",       car)
             ,("cdr",       cdr)
             ,("cons",      cons)
             ,("eq?",       eqv)
             ,("eqv?",      eqv)
             ,("equal?",    equal)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: ([Char] -> [Char] -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                         then throwError $ TypeMismatch "number" $ String n
                         else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum
                     ,AnyUnpacker unpackStr
                     ,AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: [Char] -> Either LispError LispVal
readExpr input = readOrThrow parseExpr input

readExprList :: [Char] -> Either LispError [LispVal]
readExprList input = readOrThrow (endBy parseExpr spaces) input

escapedChars :: Parser Char
escapedChars =
  do char '\\'
     x <- oneOf "\\\"nrt"
     return $ case x of
       '\\' -> x
       '"'  -> x
       'n'  -> '\n'
       'r'  -> '\r'
       't'  -> '\t'

parseString :: Parser LispVal
parseString =
  do char '"'
     x <- many $ escapedChars <|> noneOf "\"\\"
     char '"'
     return $ String x

parseAtom :: Parser LispVal
parseAtom =
  do first <- letter <|> symbol
     rest <- many $ letter <|> digit <|> symbol
     let atom = first : rest
     return $ Atom atom

parseChar :: Parser LispVal
parseChar =
  do try $ string "#\\"
     x <- parseCharName <|> anyChar
     return $ Character x

parseCharName :: GenParser Char a Char
parseCharName =
  do x <- try (string "space" <|> string "newline")
     case x of
       "space" -> do return ' '
       "newline" -> do return '\n'

parseBool :: Parser LispVal
parseBool =
  do string "#"
     x <- oneOf "tf"
     return $ case x of
                't' -> Bool True
                'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber =
  do num <- parseDigital1
        <|> parseDigital2
        <|> parseHex
        <|> parseOct
        <|> parseBin
     return $ num

parseDigital1 :: Parser LispVal
parseDigital1 =
  do x <- many1 digit
     (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 =
  do try $ string "#d"
     x <- many1 digit
     (return . Number . read) x

parseHex :: Parser LispVal
parseHex =
  do try $ string "#x"
     x <- many1 hexDigit
     return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct =
  do try $ string "#o"
     x <- many1 octDigit
     return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin =
  do try $ string "#b"
     x <- many1 (oneOf "10")
     return $ Number (bin2dig x)

oct2dig :: Integral a => [Char] -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: Integral a => [Char] -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: String -> Integer
bin2dig = bin2dig' 0
  where bin2dig' digint "" = digint
        bin2dig' digint (x:xs) =
          let old = 2 * digint + (if x == '0' then 0 else 1)
          in bin2dig' old xs



parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
  do head <- endBy parseExpr spaces
     tail <- char '.' >> spaces >> parseExpr
     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted =
  do char '\''
     x <- parseExpr
     return $ List [Atom "quote", x]

parseUnquote :: Parser LispVal
parseUnquote =
  do char ','
     x <- parseExpr
     return $ List [Atom "unquote", x]

parseBackquoted :: Parser LispVal
parseBackquoted =
  do char '`'
     x <- parseUnquote
     return $ List [Atom "quasiquote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
  <|> parseString
  <|> try parseNumber
  <|> try parseBool
  <|> try parseChar
  <|> parseQuoted
  <|> parseBackquoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2)
  && (and $ map eqvPair $ zip arg1 arg2)
  where eqvPair (x1, x2) = case eqv [x1, x2] of
                             Left err -> False
                             Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

doUntil :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
doUntil pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> doUntil pred prompt action

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                            ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>=
    hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= doUntil (== "quit") (readPrompt "~> ") . evalAndPrint

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env
                    }

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showVal :: LispVal -> String
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords args ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (String      contents) = "\"" ++ contents ++ "\""
showVal (Character      value) = show value
showVal (Atom            name) = name
showVal (Number      contents) = show contents
showVal (Bool            True) = "#t"
showVal (Bool           False) = "#f"
showVal (List        contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "unbound variable" var)
           (liftIO . readIORef)
           (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value =
  do env <- liftIO $ readIORef envRef
     maybe (throwError $ UnboundVar "unbound variable" var)
           (liftIO . (flip writeIORef value))
           (lookup var env)
     return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value =
  do alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef $ (var, valueRef):env
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = (++ env) <$> (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc)
               ,("open-input-file", makePort ReadMode)
               ,("open-output-port", makePort WriteMode)
               ,("close-input-port", closePort)
               ,("close-output-port", closePort)
               ,("read", readProc)
               ,("write", writeProc)
               ,("read-contents", readContents)
               ,("read-all", readAll)
               ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = Port <~> liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <~> liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

printVal :: LispVal -> IO ()
printVal = putStrLn . showVal

main :: IO ()
main = do
  args <- getArgs
  if null args
     then runRepl
     else runOne $ args
