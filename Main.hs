module Main where
import Tokens
import Grammar
import Data.Either

main :: IO ()
main =   do dat <- readFile "test_files/test.txt"
            let tokens = lexString dat
            -- TODO: head causes issues on parse error
            let parse = parseCalc (head (rights [tokens]))
            print parse
