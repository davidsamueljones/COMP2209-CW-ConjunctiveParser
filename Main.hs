module Main where
import Tokens
import Grammar
import Data.Either

main :: IO ()
main =   do dat <- readFile "test_files/test.txt"
            let tokens = lexString dat
            error "nfnjsndjf"
            let tokens' = fromRight [] tokens
            let parse = parseCalc tokens'
            print parse

            
