module Main where
import Grammar
import Interpreter

import Data.Either        (fromLeft, fromRight, isLeft)
import System.Environment (getArgs)
import Control.Monad      (when)
import System.Exit        (exitFailure, die)

main :: IO ()
main = do args <- getArgs
          case args of
            [f] -> interpretFile f
            _      -> do putStrLn "\nInvalid input arguments, correct usage is:\n\t myinterpreter <input>\n"
                         exitFailure

interpretFile :: FilePath -> IO ()
interpretFile f = readFile f >>= interpretString

interpretString :: String -> IO () 
interpretString dat = do
    -- * Parse and lex together * --
    let parseResult = runLexAndParse dat
    when (isLeft parseResult) $ die (fromLeft "" parseResult)
    -- * No parse error so use AST for interpretation * --
    let ast = fromRight (Prog [] []) parseResult
    interResult <- runInterpreter ast
    when (isLeft interResult) $ do
      printExStack (fromLeft IEUnknown interResult);
      exitFailure
    -- * Interpretation finished successfully * --