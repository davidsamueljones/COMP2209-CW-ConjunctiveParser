module Main where
import Tokens
import Grammar
import Interpreter

import Control.Exception
import Control.Monad
import System.Exit   

main :: IO ()
main =   do 
  dat <- readFile "test_files/test3.txt"
  -- Lex input into tokens
  let lexerResult = lexString dat
  exitIfError lexerResult
  -- No lex error so use tokens as input for parser
  let tokens = Main.fromRight [] lexerResult
  parseResult <- runParse tokens
  exitIfError parseResult
  -- No parse error so use AST for interpretation 
  let ast = Main.fromRight (Prog [] []) parseResult
  --let output = runInterpreter ast
  --putStrLn output -- FIXME: Do interpretation
  putStrLn $ show ast


exitIfError :: Show a => Either a b -> IO ()
exitIfError res = when (Main.isLeft res) $ die (getError res)

getError :: Show a => Either a b -> String
getError (Left e) = show $ e
getError (Right _) = "No error"

--Manually add Either functions since they aren't available on all haskell versions   
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.Either.html#Either

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x (Right _) = x
