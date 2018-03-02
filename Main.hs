module Main where
import Tokens
--import Grammar

main :: IO ()
main =   do dat <- readFile "test_files/test.txt"
            let tokens = alexScanTokens dat
            --let parse = parseCalc tokens
            print tokens
