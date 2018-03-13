module Main where
import Tokens
import Grammar
import Interpreter

import Data.Either
import System.Environment
import Control.Exception
import Control.Monad
import System.Exit


main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> interpretFile f
            _      -> interpretFile "p1.cql"
            -- _      -> do putStrLn "\nInvalid input arguments, correct usage is:\n\t myinterpreter <input>\n"
            --              exitFailure

interpretFile :: FilePath -> IO ()
interpretFile f = readFile f >>= interpretString

interpretString :: String -> IO () 
interpretString dat = do
    -- Lex input into tokens
    let lexerResult = lexString dat
    exitIfError lexerResult
    -- No lex error so use tokens as input for parser
    let tokens = fromRight [] lexerResult
    --putStrLn $ show tokens
    parseResult <- runParse tokens
    exitIfError parseResult
    -- No parse error so use AST for interpretation 
    let ast = fromRight (Prog [] []) parseResult
    --putStrLn $ show ast
    interResult <- runInterpreter ast
    exitIfError interResult
    -- No interpret error
    let output = fromRight "" interResult
    putStrLn output
    -- output <- runInterpreter ast
    -- putStrLn output -- FIXME: Do interpretation

exitIfError :: Show a => Either a b -> IO ()
exitIfError res = when (isLeft res) $ die (getError res)

getError :: Show a => Either a b -> String
getError (Left e) = show $ e
getError (Right _) = "No error"