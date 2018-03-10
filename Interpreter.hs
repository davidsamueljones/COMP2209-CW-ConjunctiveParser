module Interpreter where
import Grammar
import Control.Exception
import Text.Regex (splitRegex, mkRegex)
import Data.Either.Compat
import Control.Monad
import System.Exit   

-- TODO: Just ya know, write this thing?
runInterpreter :: Show a => Prog -> [a]
runInterpreter ast = []


-- Imports a csv file into zipped columns
importTable :: FilePath -> IO (Either InterException [[String]])
importTable f = do 
    res <- try $ readFile f :: IO (Either IOError String)
    case res of
      Left e -> throw (InterExceptionImport e)
      Right dat -> do
        -- FIXME: Does not allow commas in strings, can't use lookbehind due to POSIX regex -- (?<!\\)
        let tokenise = map (splitRegex (mkRegex ",")) . lines
        return (multiZip' $ tokenise dat)

-- Zip rows into column lists, failing if columns are not all equal length
multiZip' :: [[a]] -> Either InterException [[a]]
multiZip' xss | allLensSame xss = Right (multiZip xss)
multiZip' xss | otherwise       = throw InterExceptionZip

-- Zip rows into column lists
multiZip :: [[a]] -> [[a]]
multiZip xss | maxListLen xss == 0 = []
multiZip xss = foldl (++) [] line : multiZip rest
    where line = map (take 1) xss
          rest = map (drop 1) xss

-- Find the longest row in a list
maxListLen :: [[a]] -> Int
maxListLen xss = maximum $ map length xss

-- Get length of all rows, true if all equal
allLensSame :: [[a]] -> Bool
allLensSame xss = allValsSame $ map length xss

-- True if all values in list are equal 
allValsSame :: Eq a =>[a] -> Bool
allValsSame xs = all (== head xs) (tail xs)


-----------------------------------------------------------------
-- Interpreter Exceptions
-----------------------------------------------------------------
data InterException = InterExceptionZip
                    | InterExceptionImport IOError
                    deriving (Show)

instance Exception InterException



-- Our Rules:
-- * A variable that appears under the scope of an existential quantifier is 
--   said to be bound, otherwise it is free
-- * LHS of turnstile must have all free variables 
-- * Free variables must all be in the scope of at least one relation

-- Musings:
-- * Probably want either CEK or CESK interpreter, probably don't need store 
--   in CESK but who knows...
-- * Looks like breaking down into normal form a good idea?
-- * Use HashMap package for the environment?


-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
