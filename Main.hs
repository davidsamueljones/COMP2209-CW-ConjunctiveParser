module Main where
import Tokens
import Grammar
import Data.Either.Compat
import Control.Exception
import Control.Monad
import System.Exit

main :: IO ()
main =   do 
    dat <- readFile "test_files/test2.txt"
    -- Lex input into tokens
    let lexerResult = lexString dat
    exitIfError lexerResult
    -- No lex error so use tokens as input for parser
    let tokens = fromRight [] lexerResult
    parseResult <- runParse tokens
    exitIfError parseResult
    -- No parse error so use AST for interpretation 
    let parsed = fromRight (Prog [] []) parseResult
    print parsed -- FIXME: Do interpretation


exitIfError :: Show a => Either a b -> IO ()
exitIfError res = when (isLeft res) $ do 
        die $ getError res

getError :: Show a => Either a b -> String
getError (Left e) = show $ e
getError (Right _) = "No error"
