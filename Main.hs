module Main where
import Grammar
import Interpreter (runInterpreter)

import Data.Either        (fromRight, isLeft)
import System.Environment (getArgs)
import Control.Monad      (when)
import System.Exit        (die)

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



