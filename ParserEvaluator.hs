{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module ParserEvaluator
(   readExpr
,   eval
,   LispVal(..)
,   Env
,   runIOThrows
,   liftThrows
,   primitiveBindings
,   bindVars
,   load)
where
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import Control.Monad
import System.IO
import Control.Monad.Error
import Data.IORef
import Control.Applicative hiding ((<|>), many)
import Numeric
import Data.Ratio
import GHC.Float
import qualified Data.Vector as V

parseNumber :: Parser LispVal
parseNumber =   try parseOctal
            <|> try parseBin
            <|> try parseHex
            <|> parseDecimal

baseRead :: Num a => Int -> String -> a
baseRead base numstring = fromIntegral $ vectorBaseRead base (V.fromList $ map digitToInt numstring)

vectorBaseRead :: Num a => a -> V.Vector a -> a
vectorBaseRead base numstring = sum $ map (\i -> let from = fromIntegral i in numstring V.! from * base ^ from) [0..V.length numstring - 1]

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many processChar
                char '"'
                return $ String x

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    Char <$> anyChar

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

processChar :: Parser Char
processChar =
        noneOf "\"\n"
    <|> try (string "\"\"" >> return '"')
    <|> try (eol >> return '\n')


parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                            "#t"    -> Bool True
                            "#f"    -> Bool False
                            _       -> Atom atom


--naturalOrFloat = P.naturalOrFloat lexer
data Sign      = Positive | Negative

applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate

sign  :: Parser Sign
sign  =  do
            char '-'
            return Negative
     <|> do
            char '+'
            return Positive
     <|> return Positive

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

number :: Parser String
number = many1 digit

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

integer :: Parser String
integer = plus <|> minus <|> number

double :: Parser Double
double = rd <$> integer <++> decimal <++> exponent
    where rd       = read :: String -> Double
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

parseDecimal :: Parser LispVal
parseDecimal = do
  n <- double
  let rounded = round n
  if n == fromIntegral rounded
     then return $ Int rounded
     else return $ Double n

parseBin :: Parser LispVal
parseBin = do
    string "#b"
    s <- sign
    numstring <- many $ oneOf "01"
    return $ Int $ applySign s $ baseRead 2 numstring

parseOctal :: Parser LispVal
parseOctal = do
    string "#o"
    s <- sign
    numString <- many (oneOf ['1'..'7'])
    (return . Int . applySign s . fst . head . readOct) numString

parseHex :: Parser LispVal
parseHex = do
    string "#h"
    s <- sign
    numString <- many (oneOf $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f'])
    (return . Int . applySign s . fst . head . readHex) numString

symbol :: Parser Char
symbol = oneOf "'!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 (satisfy isSpace)

parseList :: Parser LispVal
parseList = do
    char '('
    x <- liftM List $ sepBy parseExpr spaces
    char ')'
    return x

parseDottedList :: Parser LispVal
parseDottedList = do
    char '('
    lhead <- endBy parseExpr spaces
    ltail <- char '.' >> spaces >> parseExpr
    char ')'
    return $ DottedList lhead ltail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

--parseVector :: Parser LispVal
--parseVector = do
--  char '#'
--  x <- parseExpr
--  case x of
--      List xs -> return $ Vector (V.fromList xs)
--      DottedList xs x -> return $ Vector (V.snoc (V.fromList xs) x)
--      _ -> error "Tried to construct a vector from non-list"

parseExpr :: Parser LispVal
parseExpr = try parseChar
        <|> try parseNumber
        <|> try parseString
        <|> try parseQuoted
--      <|> try parseVector
        <|> try parseAtom
        <|> try parseList
        <|> try parseDottedList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

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
                | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
                | IOFunc ([LispVal] -> IOThrowsError LispVal)
                | Port Handle
                | Func {params :: [String], vararg :: (Maybe String),
                        body :: [LispVal], closure :: Env}
                | Macro {params :: [String], varArg :: (Maybe String),
                         body :: [LispVal], closure :: Env}
--              | Macro [LispVal]

instance Num LispVal where
    (Int i1) + (Int i2) = Int $ i1 + i2
    (Int i1) + (Float f2) = Float $ fromInteger i1 + f2
    (Int i1) + (Double d2) = Double $ fromInteger i1 + d2
    (Float f1) + (Float f2) = Float $ f1 + f2
    (Float f1) + (Int i2) = Float $ f1 + fromInteger i2
    (Float f1) + (Double d2) = Double $ float2Double f1 + d2
    (Double d1) + (Double d2) = Double $ d1 + d2
    (Double d1) + (Int i2) = Double $ d1 + fromInteger i2
    (Double d1) + (Float f2) = Double $ d1 + float2Double f2
    l1 + l2 = Int $ (extractValue $ unpackNum l1) + (extractValue $ unpackNum l2)

    (Int i1) * (Int i2) = Int $ i1 * i2
    (Int i1) * (Float f2) = Float $ fromInteger i1 * f2
    (Int i1) * (Double d2) = Double $ fromInteger i1 * d2
    (Float f1) * (Float f2) = Float $ f1 * f2
    (Float f1) * (Int i2) = Float $ f1 * fromInteger i2
    (Double d1) * (Double d2) = Double $ d1 * d2
    (Double d1) * (Int i2) = Double $ d1 * fromInteger i2
    (Double d1) * (Float f2) = Double $ d1 * float2Double f2
    l1 * l2 = Int $ (extractValue $ unpackNum l1) * (extractValue $ unpackNum l2)

    abs (Int i)         = Int $ abs i
    abs (Float f)       = Float $ abs f
    abs (Double d)      = Double $ abs d
    abs l               = Int $ abs $ extractValue $ unpackNum l
    --signum (Int i)        = signum i
    --signum (Float f)  = signum f
    --signum (Double d)     = signum d
    signum _            = undefined
    fromInteger n       = Int n
    negate n            = n * Int (-1)


instance Fractional LispVal where
    (Int i1) / (Int i2) = Double $ (fromInteger i1) / (fromInteger i2)
    (Int i1) / (Float f2) = Float $ (fromInteger i1) / f2
    (Int i1) / (Double d2) = Double $ (fromInteger i1) / d2
    (Float f1) / (Int i2) = Float $ f1 / (fromInteger i2)
    (Float f1) / (Float f2) = Float $ f1 / f2
    (Float f1) / (Double d2) = Double $ float2Double f1 / d2
    (Double d1) / (Int i2) = Double $ d1 / (fromInteger i2)
    (Double d1) / (Float f2) = Double $ d1 / float2Double f2
    (Double d1) / (Double d2) = Double $ d1 / d2
    _ / _ = undefined
    fromRational rat = Double $ (fromIntegral . numerator) rat / (fromIntegral . denominator) rat

instance Ord LispVal where
    (Int i1) < (Int i2) = i1 < i2
    (Int i1) < (Double d2) = fromInteger i1 < d2
    (Double d1) < (Int i2) = d1 < fromInteger i2
    (Double d1) < (Double d2) = d1 < d2
    _ < _ = error "tried to order non-numbers"

eqLispVal :: LispVal -> LispVal -> Bool
eqLispVal v1 v2 = boolExtractor $ eqv [v1,v2]

instance Eq LispVal where (==) = eqLispVal


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Int contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float contents) = show contents
showVal (Double contents) = show contents
--showVal (Vector contents) = show contents
showVal (Char c) = [c]
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList lhead ltail) = "(" ++ unwordsList lhead ++ " . " ++ showVal ltail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
    (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++
    ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


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
--eval env form@(List (Atom "case" : key : clauses)) =
--  if null clauses
--      then throwError $ BadSpecialForm "no true clause in case expression: " form
--      else case head clauses of
--              List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
--              List ((List datums) : exprs) -> do
--                  result <- eval env key
--                  equality <- mapM (\x -> eqv [result, x]) datums
--                  if Bool True ∈ equality
--                      then mapM eval env exprs >>= return . last
--                      else eval env $ List (Atom "case" : key : tail clauses)
--              _   -> liftThrows . throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
--eval env (List (Atom "defmacro" : List (Atom var : params) : body)) = makeNormalMacro env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
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
condEvaluator _ (x:_) = throwError $ TypeMismatch "Pair " x
condEvaluator _ [] = throwError $ UnboundVar "No catchall " "else"

ifEvaluator :: Env -> [LispVal] -> IOThrowsError LispVal
ifEvaluator env (pred:conseq:alt:_) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        _ -> throwError $ TypeMismatch "Bool" result
ifEvaluator _ xs = throwError $ NumArgs 3 xs


--caseEvaluator :: Env -> LispVal -> IOThrowsError LispVal
--caseEvaluator env form@(List (Atom "case":key:clauses)) =
--  if null clauses
--      then throwError $ BadSpecialForm "no true clause in case expression: " form
--      else case head clauses of
--              List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
--              List ((List datums) : exprs) -> do
--                  result <- eval env key
--                  equality <- mapM (\x -> eqv [result, x]) datums
--                  if Bool True ∈ equality
--                      then mapM eval env exprs >>= return . last
--                      else eval env $ List (Atom "case" : key : tail clauses)
--              _   -> liftThrows . throwError $ BadSpecialForm "ill-formed case expression: " form

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply (Macro params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else do
            newList <- evalBody closure
            (liftIO . bindVars closure . zip params) args >>= bindVarArgs varargs >>= flip eval newList
        --else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env
apply (IOFunc func) args = func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                                ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [  ("+", numericNumOp (+)),
                ("-", numericNumOp (-)),
                ("*", numericNumOp (*)),
                ("/", numericFloatOp (/)),
                ("div", numericIntOp div),
                ("mod", numericIntOp mod),
                ("quotient", numericIntOp quot),
                ("remainder", numericIntOp rem),
                ("string?", checker "string"),
                ("symbol?", checker "symbol"),
                ("int?", checker "int"),
                ("bool?", checker "bool"),
                --("vector?", checker "vector"),
                ("list?", checker "list"),
                ("char?", checker "char"),
                ("float?", checker "float"),
                ("double?", checker "double"),
                ("null?", checker "null"),
                ("contains-Float?", checker "contains-Float"),
                ("contains-Double?", checker "contains-Double"),
                ("symbol->string", converter "symbol->string"),
                ("string->symbol", converter "string->symbol"),
                ("<", boolOp (<)),
                (">", boolOp (>)),
                ("/=", boolOp (/=)),
                (">=", boolOp (>=)),
                ("<=", boolOp (<=)),
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
                ("=", equality "eqv"),
                ("eq?", equality "eqv"),
                ("eqv?", equality "eqv"),
                ("equal?", equality "equal"),
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
stringAppend (x:_) = throwError $ TypeMismatch "String" x
stringAppend [] = throwError $ NumArgs 2 []

makeString :: [LispVal] -> ThrowsError LispVal
makeString ((Int i):(Char c):_) = return $ String $ replicate (fromInteger i) c
makeString ((Int i):_) = return $ String $ replicate (fromInteger i) '\n'
makeString (x:_) = throwError $ TypeMismatch "Int" x
makeString [] = throwError $ NumArgs 1 []

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength ((String s):_) = return $ Int $ fromIntegral $ length s
stringLength (x:_) = throwError $ TypeMismatch "String" x
stringLength [] = throwError $ NumArgs 1 []

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef ((String s):(Int i):_) = return $ Char $ s !! (fromInteger i)
stringRef ((String _):y:_) = throwError $ TypeMismatch "Int" y
stringRef (x:(Int _):_) = throwError $ TypeMismatch "String" x
stringRef xs = throwError $ NumArgs 2 xs

substring :: [LispVal] -> ThrowsError LispVal
substring ((String s):(Int start):(Int end):_) = return $ String $ take (fromIntegral (end - start)) $ drop (fromIntegral start) s
substring ((String _):y:z:_) = throwError $ TypeMismatch "Int, Int" $ List [y,z]
substring list@(_:_) = throwError $ NumArgs 3 list

numericIntOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericIntOp _            []  = throwError $ NumArgs 2 []
numericIntOp _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericIntOp op params = mapM unpackNum params >>= return . Int . foldl1 op

numericNumOp :: MonadError LispError m => (LispVal -> LispVal -> LispVal) -> [LispVal] -> m LispVal
numericNumOp _            []  = throwError $ NumArgs 2 []
numericNumOp _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericNumOp op params = return $ foldl1 op params

numericFloatOp :: MonadError LispError m => (LispVal -> LispVal -> LispVal) -> [LispVal] -> m LispVal
numericFloatOp _            []  = throwError $ NumArgs 2 []
numericFloatOp _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericFloatOp op params = return $ foldl1 op params

boolOp :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp op args =    if length args /= 2
                    then throwError $ NumArgs 2 args
                    else do
                        let left = args !! 0
                        let right = args !! 1
                        return $ Bool $ left `op` right

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                left <- unpacker $ args !! 0
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool


checker :: String -> [LispVal] -> ThrowsError LispVal
checker "contains-Double" xs = contains (boolExtractor . (typeChecker "double")) xs
checker "contains-Float" xs = contains (boolExtractor . (typeChecker "float")) xs
checker op (x:_) = typeChecker op x
checker "null" [] = return $ Bool True
checker _ _ = throwError $ NotFunction "invalid checker!" "checker"

typeChecker :: String -> LispVal -> ThrowsError LispVal
typeChecker "string" (String _) = return $ Bool True
typeChecker "symbol" (Atom _) = return $ Bool True
typeChecker "int" (Int _) = return $ Bool True
typeChecker "bool" (Bool _) = return $ Bool True
--typeChecker "vector" (Vector _) = return $ Bool True
typeChecker "list" (List _) = return $ Bool True
typeChecker "char" (Char _) = return $ Bool True
typeChecker "double" (Double _) = return $ Bool True
typeChecker "float" (Float _) = return $ Bool True
typeChecker "null" (List []) = return $ Bool True
typeChecker _ _ = throwError $ NotFunction "invalid checker!" "typeChecker"

converter :: String -> [LispVal] -> ThrowsError LispVal
converter op (x:_) = typeConverter op x
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
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
            do
                unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)



--List Operations
listOps :: String -> [LispVal] -> ThrowsError LispVal
listOps "car" xs = car xs
listOps "cdr" xs = cdr xs
listOps "cons" xs = cons xs
listOps _ _ = throwError $ NotFunction "Invalid list operation" "listOps"

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:xs) _] = return $ List xs
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
equality "equal" xs = equal xs
equality _ _ = throwError $ NotFunction "Invalid equalitiy" "equality"

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                     (all eqvPair $ zip arg1 arg2)
        where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left _ -> False
                                    Right (Bool val) -> val


eqv :: [LispVal] -> ThrowsError LispVal
--eqv [a,b] = return $ Bool $ a == b
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Int arg1), (Int arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Int arg2)] = return $ Bool $ (read arg1) == arg2
eqv [(Int _), (Atom arg2)] = return $ Bool $ (read arg2) == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList




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

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "couldn't extract value from LispError"

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

--Function creation
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal

--makeMacro varargs env params body =

--IO
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

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
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll (x:_) = liftThrows $ throwError $ TypeMismatch "String" x
readAll [] = liftThrows $ throwError $ NumArgs 1 []
