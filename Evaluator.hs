module Evaluator
(	showVal
,	eval
,	apply
,	LispVal(..)
,	LispError(..)
,	showError
,	ThrowsError
,	trapError
,	extractValue
,	module Control.Monad.Error
,	Env
,	nullEnv
,	runIOThrows
,	liftThrows)
where
import Control.Monad.Error
import Text.Parsec.Error
import Data.IORef
import qualified Data.Vector as V


data LispVal =    Atom String
				| List [LispVal]
				| DottedList [LispVal] LispVal
				| Int Integer
				| Float Float
				| Double Double
				| String String
				| Bool Bool 
				| Vector (V.Vector LispVal)
				| Char Char 
			deriving (Eq)



showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Int contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float contents) = show contents
showVal (Double contents) = show contents
showVal (Vector contents) = show contents
showVal (Char c) = [c]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Int _) = return val
eval _ val@(Float _) = return val
eval _ val@(Double _) = return val
eval _ val@(Bool _) = return val
eval env (Atom a) = getVar env a
eval _ (List [Atom "quote", val]) = return val
eval env (List (Atom "cond":xs)) = condEvaluator env xs
eval env (List (Atom "if":xs)) = ifEvaluator env xs
eval env (List (Atom "case":x:xs)) = caseEvaluator env x xs
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func		
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--conditionals :: String -> [LispVal] -> ThrowsError LispVal
--conditionals "cond" xs = condEvaluator xs
--conditionals "if" xs = ifEvaluator xs
--conditionals "case" (x:xs) = caseEvaluator x xs
--conditionals "case" x = throwError $ TypeMismatch "pair" (List x)
--conditionals badFunction _ = throwError $ NotFunction "Conditionals --> Not a function" badFunction

condEvaluator :: Env -> [LispVal] -> IOThrowsError LispVal
condEvaluator env ((List (Atom "else":conseq:_)):_) = eval env conseq
condEvaluator env ((List (cond:conseq:_)):xs) = do
	bool <- eval env cond >>= return . extractValue . unpackBool
	if bool 
		then eval env conseq 
		else condEvaluator env xs
condEvaluator _ (x:xs) = throwError $ TypeMismatch "Pair " x
condEvaluator _ [] = throwError $ UnboundVar "No catchall " "else"

ifEvaluator :: Env -> [LispVal] -> IOThrowsError LispVal
ifEvaluator env (pred:conseq:alt:_) = do
	result <- eval env pred
	case result of
		Bool False -> eval env alt
		Bool True -> eval env conseq
		otherwise -> throwError $ TypeMismatch "Bool" result
ifEvaluator _ xs = throwError $ NumArgs 3 xs


caseEvaluator :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
caseEvaualotr env _ ((List (Atom "else":result:_)):_) = eval env result
caseEvaluator env subject ((List ((List x):result:_)):xs) = if (elem subject x) then eval env result else caseEvaluator env subject xs
caseEvaluator _ _ (x:xs) = throwError $ TypeMismatch "Pair" x
caseEvaluator _ _ [] = throwError $ UnboundVar "No catchall " "else"

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
						($ args)
						(lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
			  ("-", numericBinop (-)),
			  ("*", numericBinop (*)),
			  ("/", numericBinop div),
			  ("mod", numericBinop mod),
			  ("quotient", numericBinop quot),
			  ("remainder", numericBinop rem),
			  ("string?", checker "string"),
			  ("symbol?", checker "symbol"),
			  ("int?", checker "int"),
			  ("bool?", checker "bool"),
			  ("vector?", checker "vector"),
			  ("list?", checker "list"),
			  ("char?", checker "char"),
			  ("float?", checker "float"),
			  ("double?", checker "double"),
			  ("null?", checker "null"),
			  ("contains-Float?", checker "contains-Float"),
			  ("contains-Double?", checker "contains-Double"),
			  ("symbol->string", converter "symbol->string"),
			  ("string->symbol", converter "string->symbol"),
			  ("=", numBoolBinop (==)),
			  ("<", numBoolBinop (<)),
			  (">", numBoolBinop (>)),
			  ("/=", numBoolBinop (/=)),
			  (">=", numBoolBinop (>=)),
			  ("<=", numBoolBinop (<=)),
			  ("&&", boolBoolBinop (&&)),
			  ("||", boolBoolBinop (||)),
			  ("string=?", strBoolBinop (==)),
			  ("string<?", strBoolBinop (<)),
			  ("string>?", strBoolBinop (>)),
			  ("string<=?", strBoolBinop (<=)),
			  ("string>=?", strBoolBinop (>=)),
			  ("car", listOps "car"),
			  ("cdr", listOps "cdr"),
			  ("cons", listOps "cons"),
			  ("eqv", equality "eqv"),
			  ("string", stringOps "string"),
			  ("make-string", stringOps "make-string"),
			  ("string-length", stringOps "string-length"),
			  ("string-ref", stringOps "string-ref"),
			  ("substring", stringOps "substring"),
			  ("string-append", stringOps "string-append")]
			  --("set!", environment "set!"),
			  --("define", environment "define")]
			  --("cond", conditionals "cond"),
			  --("if", conditionals "if"),
			  --("case", conditionals "case")]

stringOps :: String -> [LispVal] -> ThrowsError LispVal
stringOps "string" = lispString
stringOps "make-string" = makeString 
stringOps "string-length" = stringLength
stringOps "string-ref" = stringRef
stringOps "substring" = substring
stringOps "string-append" = stringAppend 
stringOps badFunction = \_ -> throwError $ NotFunction "Invalid Function -> StringOps" badFunction

lispString :: [LispVal] -> ThrowsError LispVal
lispString xs = return $ String $ map (extractValue . unpackChar) xs

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend ((String s1):(String s2):xs) = stringAppend ((String (s1 ++ s2)):xs)
stringAppend ((String s):_) = return $ String s
stringAppend (x:xs) = throwError $ TypeMismatch "String" x
stringAppend [] = throwError $ NumArgs 2 []

makeString :: [LispVal] -> ThrowsError LispVal
makeString ((Int i):(Char c):_) = return $ String $ replicate (fromInteger i) c
makeString ((Int i):_) = return $ String $ replicate (fromInteger i) '\n'
makeString (x:xs) = throwError $ TypeMismatch "Int" x
makeString [] = throwError $ NumArgs 1 []

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength ((String s):_) = return $ Int $ fromIntegral $ length s
stringLength (x:xs) = throwError $ TypeMismatch "String" x
stringLength [] = throwError $ NumArgs 1 []

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef ((String s):(Int i):_) = return $ Char $ s !! (fromInteger i)
stringRef ((String _):y:_) = throwError $ TypeMismatch "Int" y
stringRef (x:(Int _):_) = throwError $ TypeMismatch "String" x
stringRef xs = throwError $ NumArgs 2 xs

substring :: [LispVal] -> ThrowsError LispVal
substring ((String s):(Int start):(Int end):_) = return $ String $ take (fromIntegral (end - start)) $ drop (fromIntegral start) s
substring ((String _):y:z:_) = throwError $ TypeMismatch "Int, Int" $ List [y,z]
substring list@(x:xs) = throwError $ NumArgs 3 list

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Int . foldl1 op
--numericBinop op params
--	|	containsDouble = return $ Double $ foldl1 op $ map (\x -> if (boolExtractor . extractValue . checker "double" x) then unpackNum x else (fromInteger (unpackNum x) :: Double)) params
--	| 	otherwise = return $ Int $ foldl1 op $ map unpackNum params
--	where
--		containsDouble = (boolExtractor . extractValue . checker "contains-Double") params



boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
							 then throwError $ NumArgs 2 args
							 else do 
								left <- unpacker $ args !! 0
								right <- unpacker $ args !! 1
								return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


checker :: String -> [LispVal] -> ThrowsError LispVal
checker "contains-Double" xs = contains (boolExtractor . (typeChecker "double")) xs
checker "contains-Float" xs = contains (boolExtractor . (typeChecker "float")) xs
checker op (x:xs) = typeChecker op x
checker "null" [] = return $ Bool True
checker _ _ = throwError $ NotFunction "invalid checker!" "checker"

typeChecker :: String -> LispVal -> ThrowsError LispVal
typeChecker "string" (String _) = return $ Bool True
typeChecker "symbol" (Atom _) = return $ Bool True
typeChecker "int" (Int _) = return $ Bool True
typeChecker "bool" (Bool _) = return $ Bool True
typeChecker "vector" (Vector _) = return $ Bool True
typeChecker "list" (List _) = return $ Bool True
typeChecker "char" (Char _) = return $ Bool True
typeChecker "double" (Double _) = return $ Bool True
typeChecker "float" (Float _) = return $ Bool True
typeChecker "null" (List []) = return $ Bool True
typeChecker _ _ = throwError $ NotFunction "invalid checker!" "typeChecker"

converter :: String -> [LispVal] -> ThrowsError LispVal
converter op (x:xs) = typeConverter op x
converter _ [] = throwError $ NumArgs 2 []

typeConverter :: String -> LispVal -> ThrowsError LispVal
typeConverter "symbol->string" (Atom a) = return $ String a
typeConverter "symbol->string" l = throwError $ TypeMismatch "symbol" l
typeConverter "string->symbol" (String a) = return $ Atom a
typeConverter "string->symbol" l = throwError $ TypeMismatch "string" l
typeConverter _ _ = throwError $ NotFunction "Invalid converter" "typeConverter"


boolExtractor :: ThrowsError LispVal -> Bool
boolExtractor = helper . extractValue
	where
		helper (Bool a) = a
		helper _ = True


contains :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
contains predicate (x:xs) = if (predicate x) 
	then return $ Bool True 
	else contains predicate xs
contains _ [] = return $ Bool False






--Unpackers
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Int n) = return n
unpackNum (String n) = let parsed = reads n in 
						  if null parsed 
							then throwError $ TypeMismatch "number" $ String n
							else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum (Float n) = return $ round n
unpackNum (Double n) = return $ round n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Int s) = return $ show s
unpackStr (Float s) = return $ show s
unpackStr (Double s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar



--List Operations
listOps :: String -> [LispVal] -> ThrowsError LispVal
listOps "car" xs = car xs
listOps "cdr" xs = cdr xs
listOps "cons" xs = cons xs
listOps _ _ = throwError $ NotFunction "Invalid list operation" "listOps"

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (x:xs) _] = return $ List xs
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ List [x1,x2]
cons badArgList = throwError $ NumArgs 2 badArgList





--Equality
equality :: String -> [LispVal] -> ThrowsError LispVal
equality "eqv" xs = eqv xs
equality _ _ = throwError $ NotFunction "Invalid equalitiy" "equality"

eqv :: [LispVal] -> ThrowsError LispVal
eqv [a,b] = return $ Bool $ a == b
--eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
--eqv [(Int arg1), (Int arg2)] = return $ Bool $ arg1 == arg2
--eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
--eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
--eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
--eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
--	where eqvPair (x1, x2) = case eqv [x1, x2] of
--								Left err -> False
--								Right (Bool val) -> val
--eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList





--Error handling
data LispError = NumArgs Integer [LispVal]
			   | TypeMismatch String LispVal
			   | Parser ParseError
			   | BadSpecialForm String LispVal
			   | NotFunction String String
			   | UnboundVar String String
			   | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
								  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
									   ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
	 noMsg = Default "An error has occurred"
	 strMsg = Default


type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val































--Environment dispatch
--environment :: String -> Env -> [LispVal] -> ThrowsError LispVal
--environment "set!"  env = setEvaluator env
--environment "define" env = defineEvaluator env


--setEvaluator :: Env -> [LispVal] -> ThrowsError LispVal
--setEvaluator env [Atom var,form] = eval env form >>= setVar env var
--setEvaluator _ (x:y:xs) = throwError $ TypeMismatch "Atom" x
--setEvaluator _ list = throwError $ NumArgs 2 list


--defineEvaluator :: Env -> [LispVal] -> ThrowsError LispVal
--defineEvaluator [Atom var,form] = eval env form >>= setVar env var
--defineEvaluator _ (x:y:xs) = throwError $ TypeMismatch "Atom" x
--defineEvaluator _ list = throwError $ NumArgs 2 list




--Environment backend
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
getVar envRef var  = do 
	env <- liftIO $ readIORef envRef
	maybe 
		(throwError $ UnboundVar "Getting an unbound variable" var)
		(liftIO . readIORef)
		(lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
	env <- liftIO $ readIORef envRef
	maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
			(liftIO . (flip writeIORef value))
			(lookup var env)
	return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
	alreadyDefined <- liftIO $ isBound envRef var 
	if alreadyDefined 
		then setVar envRef var value >> return value
		else liftIO $ do 
			valueRef <- newIORef value
			env <- readIORef envRef
			writeIORef envRef ((var, valueRef) : env)
			return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
	where 
		extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
		addBinding (var, value) = do 
									ref <- newIORef value
									return (var, ref)







