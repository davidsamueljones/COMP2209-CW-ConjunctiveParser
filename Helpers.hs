module Helpers where

import Data.Char (isSpace)

-----------------------------------------------------------------
-- Haskell Utilities
-----------------------------------------------------------------

-- Functional infix if statement
(?) :: Bool -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

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

--Manually add Either functions since they aren't available on all haskell versions   
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.Either.html#Either

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f