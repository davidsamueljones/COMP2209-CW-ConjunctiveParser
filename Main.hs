module Main where
import Tokens
import Grammar
import Data.Either
import Control.Exception
import Control.Monad


main :: IO ()
main =   do dat <- readFile "test_files/test.txt"
            let tokens = lexString dat
            when (isLeft tokens) $ putStr ("Lexer error: " ++ Main.fromLeft "" tokens)
            let tokens' = Main.fromRight [] tokens
            parse <- try(evaluate (parseCalc tokens')) :: IO (Either ParseException Prog)
            case parse of
                Left (ParseException (AlexPn charOffset lineNum columnNum)) -> putStrLn $ "Parse Error at line '" ++ show lineNum ++ "', column '" ++ show columnNum ++ "'"
                Right expression -> print expression

                
--Manually added these two functions since they arent available on old haskell versions              
fromRight :: b -> Either a b -> b
fromRight x (Left _) = x
fromRight _ (Right x) = x

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x) = x
fromLeft x (Right _) = x





--TODO: FIX OR REMOVE

--main :: IO ()
--main =   do dat <- readFile "test_files/test.txt"
--            let tokens = lexString dat
--            if isRight(tokens) then do
--                let token' = head (rights [tokens])
--                let parse = try(evaluate (parseCalc token')) :: IO (Either ParseException Prog)
--                case parse of
--                    Left (ParseException (AlexPn charOffset lineNum columnNum)) -> putStrLn $ "Parse Error at line '" ++ show lineNum ++ "', column '" ++ show columnNum ++ "'"
--                    Right expression -> print expression
--            else putStrLn "Lexing Error" --TODO: Improve error message
--            let tokens' = fromRight [] tokens
--            let parse = case tokens of
--                Left _ = putStrLn "Lexing Error" --TODO: Improve error message
--                Right _ = try(evaluate (parseCalc tokens)) :: IO (Either ParseException Prog)
