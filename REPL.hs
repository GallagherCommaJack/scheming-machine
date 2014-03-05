import Control.Monad
import System.IO
import System.Environment
import ParserEvaluator

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
	result <- prompt
	if pred result
	then return ()
	else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
	env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
	(runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
		>>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
	env <- primitiveBindings
	runIOThrows $ liftM show $ eval env (List [Atom "load", String "stdlib.scm"])
	until_ (== "quit") (readPrompt "λ>>> ") . evalAndPrint $ env

main :: IO ()
main = do
	args <- getArgs
	if null args
		then runRepl
		else runOne $ args

