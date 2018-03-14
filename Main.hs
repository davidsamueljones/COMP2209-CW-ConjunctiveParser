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
            _      -> interpretFile "test.cql"
            -- _      -> do putStrLn "\nInvalid input arguments, correct usage is:\n\t myinterpreter <input>\n"
            --              exitFailure

interpretFile :: FilePath -> IO ()
interpretFile f = readFile f >>= interpretString

interpretString :: String -> IO () 
interpretString dat = do
    -- * Parse and lex together * --
    let parseResult = runLexAndParse dat
    exitIfError parseResult
    -- * No parse error so use AST for interpretation * --
    let ast = fromRight (Prog [] []) parseResult
    interResult <- runInterpreter ast
    exitIfError interResult
    -- * Interpretation finished successfully * --

exitIfError :: Show a => Either a b -> IO ()
exitIfError res = when (isLeft res) $ die (getError res)

getError :: Show a => Either a b -> String
getError (Left e) = show $ e
getError (Right _) = "No error"



