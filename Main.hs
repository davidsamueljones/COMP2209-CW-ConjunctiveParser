module Main where
import Tokens
import Grammar
import Data.Either
import Control.Exception


main :: IO ()
main =   do dat <- readFile "test_files/test.txt"
            let tokens = lexString dat
            let tokens' = fromRight [] tokens
            parse <- try(evaluate (parseCalc tokens')) :: IO (Either ParseException Prog)
            case parse of
                Left (ParseException (AlexPn charOffset lineNum columnNum)) -> putStrLn $ "Parse Error at line '" ++ show lineNum ++ "', column '" ++ show columnNum ++ "'"
                Right expression -> print expression