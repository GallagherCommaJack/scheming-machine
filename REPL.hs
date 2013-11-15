import Control.Monad
import System.IO
import System.Environment
import Parser



--shell :: IO ()
--shell = getLine >>= print . eval . readExpr
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

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "λ>>> ") . evalAndPrint

--shell :: IO ()
--shell = do
--	line <- getLine
--	evaled <- return $ liftM show $ readExpr line >>= eval
--	putStrLn $ extractValue $ trapError evaled

--main :: IO()
--main = forever shell

main :: IO ()
main = do 
	args <- getArgs
	case length args of
		0 -> runRepl
		1 -> runOne $ args !! 0
		otherwise -> putStrLn "Program takes only 0 or 1 argument"
