module Parser 
(	readExpr
,	module Evaluator)
where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System.IO
import Control.Applicative hiding ((<|>), many)
import Numeric
import qualified Data.Vector as V
import Evaluator



 
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
eol =	try (string "\n\r")
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
							"#t"	-> Bool True
							"#f"	-> Bool False
							_		-> Atom atom

parseNumber :: Parser LispVal
parseNumber = 	try parseOctal
-- 			<|> try parseBin
			<|> parseHex
-- 			<|> try parseFloat
			<|>	parseInt
			

parseInt :: Parser LispVal
parseInt = many1 digit >>= return . Int . read

parseBin :: Parser String
parseBin = do
	string "#b"
	many (oneOf "01")

parseOctal :: Parser LispVal
parseOctal = do
	string "#o"
	many (oneOf ['1'..'7'])
	>>= return . Int . fst . head . readOct

parseHex :: Parser LispVal
parseHex = do
	string "#h"
	many (oneOf (['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f'])) 
	>>= return . Int . fst . head . readHex

--parseFloat :: Parser LispVal
--parseFloat = do
--	string "#f"
--	x <- many (try (char '.') <|> digit)
--	return . Float $ (read x :: Float)

--parseDouble :: Parser LispVal
--parseDouble = do
--	string "#d"
--	x <- many (try (char '.') <|> digit)
--	return . Double $ (read x :: Double)

symbol :: Parser Char
symbol = oneOf "'!#$%&|*+-/:<=>?@^_~"
 
spaces :: Parser ()
spaces = skipMany1 space
 
parseList :: Parser LispVal
parseList = do
	char '('
	x <- liftM List $ sepBy parseExpr spaces
	char ')'
	return x

parseDottedList :: Parser LispVal
parseDottedList = do
	char '('
	head <- endBy parseExpr spaces
	tail <- char '.' >> spaces >> parseExpr
	char ')'
	return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
	char '\''
	x <- parseExpr
	return $ List [Atom "quote", x]
		
parseVector :: Parser LispVal
parseVector = do
	char '#'
	x <- parseExpr
	case x of 
		List xs -> return $ Vector (V.fromList xs)
		DottedList xs x -> return $ Vector (V.snoc (V.fromList xs) x)
		_ -> error "Tried to construct a vector from non-list"

--parseQuasiList :: Parser LispVal
--parseQuasiList = do
--	char '`'
--	sepBy parseQuasiExpr spaces >>= return . List

--parseQuasiExpr :: Parser LispVal
--parseQuasiExpr = parseCommaExpr <|> parseExpr

--parseCommaExpr :: Parser LispVal
--parseCommaExpr = do
--	char ','
--	parseExpr >>= return . eval

parseExpr :: Parser LispVal
parseExpr = try parseChar
		<|> try parseNumber
		<|> try parseString
		<|> try parseQuoted
		<|> try parseVector
		<|> try parseAtom
		<|>	try parseList
		<|> try parseDottedList


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val



