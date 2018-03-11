module Interpreter where
import Grammar
import Control.Exception
import Data.Either
import Control.Monad
import System.Exit   

data Table = Table {titles :: Vars,
                    columns :: [[String]]}
                    deriving Show
                    
                    
testTable = (Table ["x1","x2"] [["Pawel"],["Sobocinski"]])
testTable1 = (Table ["x3","x4"] [["Julian"],["Rathke"]])
testTable2 = (Table ["x1","x2"] [["1","1"],["2","2"]])
testTable3 = (Table ["x3","x4"] [["3","3"],["4","4"]])
testTable4 = (Table ["x1","x2"] [["1","1"],["3","2"]])
testTable5 = (Table ["x2","x3"] [["3","3","2"],["1","2","2"]])
testTable6 = (Table ["x1","x2"] [["1","1"],["2","2"]])
testTable7 = (Table ["x2","x3"] [["2","2"],["1","1"]])

-- TODO: Just ya know, write this thing?
--runInterpreter :: Show a => Prog -> [a]
--runInterpreter ast = []


interpretConjunction :: Table -> Table -> Table
interpretConjunction (Table titlesA columnsA) (Table titlesB columnsB)
--    | elementsOverlap titlesA titlesB   = (Table (titlesA ++ titlesB) (transpose ([rowA ++ [c] ++ rowB |     
    | elementsOverlap titlesA titlesB   = overlappedConj (Table titlesA columnsA) (Table titlesB columnsB)
    | otherwise                         = (Table (titlesA ++ titlesB) (transpose ([rowA ++ rowB | rowA <- transpose columnsA, rowB <- transpose columnsB]))) --TODO: Tidy up :)
    
overlappedConj :: Table -> Table -> Table
overlappedConj (Table titlesA columnsA) (Table titlesB columnsB) = (Table (otherATitles ++ keyTitles ++ otherBTitles) columns)
                             where aRows = transpose columnsA
                                   keyTitles = getOverlapping titlesA titlesB
                                   otherATitles = notOverlapping titlesA keyTitles
                                   otherBTitles = notOverlapping titlesB keyTitles
                                   columns = transpose ([aOther ++ b |a <- aRows, let aKey = removeOthersFromRow keyTitles titlesA a, let aOther = removeFromRow keyTitles titlesA a, b <- getMatchingRows aKey keyTitles (Table titlesB columnsB) ]) --TODO: Im sorry david ill shorten this

-- Helper functions
removeFromRow :: [String] -> [String] -> [String] -> [String]
removeFromRow vars titles row = [v | n <- [0..(length titles -1)],let v = row !! n, let a = titles !! n, notElem a vars]

removeOthersFromRow :: [String] -> [String] -> [String] -> [String]
removeOthersFromRow vars titles row = [v | n <- [0..(length titles -1)],let v = row !! n, let a = titles !! n,elem a vars]

elementsOverlap :: Eq a => [a] -> [a] -> Bool
elementsOverlap [] ys = False
elementsOverlap (x:xs) ys = elem x ys || elementsOverlap xs ys

getOverlapping :: Eq a => [a] -> [a] -> [a]
getOverlapping [] _ = []
getOverlapping (x:xs) ys
    | elem x ys = [x] ++ getOverlapping xs ys
    | otherwise = getOverlapping xs ys
   
notOverlapping :: Eq a => [a] -> [a] -> [a]
notOverlapping [] _ = []
notOverlapping (x:xs) ys
    | notElem x ys = [x] ++ notOverlapping xs ys
    | otherwise = getOverlapping xs ys

getColumns :: Table -> [[String]]
getColumns (Table _ cs) = cs

getTitles :: Table -> [String]
getTitles (Table ts _) = ts

containsColumn :: Var -> Table -> Bool
containsColumn name (Table ts _) = elem name ts

findColumn :: Var -> Table -> [String]
findColumn var (Table [] []) = []
findColumn var (Table (t:ts) (c:cs))
    | (var == t) && (notElem var ts)    = c
    | (var == t)                        = [] -- TODO: decide how to handle this case of repeated element names within table e.g. T(x1,x2,x2)   
    | (notElem var ts)                  = []
    | otherwise                         = findColumn var (Table ts cs)
    
findColumns :: [Var] -> Table -> [[String]]
findColumns vars table = [ k | a <- vars, let k = findColumn a table]
    
findNotColumn :: Var -> Table -> Table
findNotColumn var (Table ts cs) = (Table [a | a <- ts, a /= var] (findNotColumn' var (Table ts cs)))

findNotColumn' :: Var -> Table -> [[String]]
findNotColumn' _ (Table [] []) = []
findNotColumn' var (Table (t:ts) (c:cs)) 
    | (var == t)    = findNotColumn' var (Table ts cs)
    | otherwise     = [c] ++ findNotColumn' var (Table ts cs)
    
findNotColumns :: [Var] -> Table -> Table
findNotColumns vars table = foldr findNotColumn table vars
    
splitOnColumn :: Var -> Table -> (Table, Table)
splitOnColumn var (Table ts cs) = ((Table leftTitles leftColumns),(Table rightTitles rightColumns))
    where splitIndex = getTitleIndex var (Table ts cs)
          splitTitles = splitAt splitIndex ts
          splitColumns = splitAt splitIndex cs
          leftTitles = fst splitTitles
          rightTitles = tail (snd splitTitles)
          leftColumns = fst splitColumns
          rightColumns = tail (snd splitColumns)
          
getTitleIndex :: Var -> Table -> Int
getTitleIndex _ (Table [] []) = 0 -- TODO: Handle properly?
getTitleIndex var (Table (t:ts) (c:cs))
    | var == t      = 0
    | otherwise     = 1 + getTitleIndex var (Table ts cs)
    
getMatchingRows :: [Var] -> [Var] -> Table -> [[String]]
getMatchingRows keyVars keyTitles table = getMatchingRows' keyVars keyRows otherRows
                where keyColumns = findColumns keyTitles table
                      otherColumns = getColumns(findNotColumns keyTitles table )
                      keyRows = transpose keyColumns
                      otherRows = transpose otherColumns
    
getMatchingRows' :: [Var] -> [[String]] -> [[String]] -> [[String]]
getMatchingRows' _ [] [] = []
getMatchingRows' ks (kv:kvs) (r:rs)
    | ks == kv   = [kv ++ r] ++ getMatchingRows' ks kvs rs
    | otherwise = getMatchingRows' ks kvs rs
getMatchingRows' _ _ _ = [] --If all variables are in key variables
    
--getMatchingRows :: Var -> Var -> Table -> [[String]]
--getMatchingRows varName titleName (Table titles columns)
--    | notElem titleName titles = (Table titles [])
--    | otherwise                = (getMatchingRows' varName varColumn (transpose leftColumns) (transpose rightColumns))
--    where split = splitOnColumn varName (Table titles columns)
--          leftColumns = getColumns(fst split)
--          rightColumns = getColumns(snd split)
--          varColumn = findColumn varName (Table titles columns)         
          
--getMatchingRows' :: Var -> [String] -> [[String]] -> [[String]] -> [[String]]
--getMatchingRows' _  [] [] [] = []
--getMatchingRows' var (c:cs) (l:ls) (r:rs) --Left and right of column to match
--    | var == c      = [l ++ [c] ++ r] ++ getMatchingRows' var cs ls rs
--    | otherwise     = getMatchingRows' var cs ls rs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs 
    | length (head xs) >1 = [(map head xs)] ++ transpose (map tail xs)
    | otherwise = [(map head xs)]


-----------------------------------------------------------------
-- Table Importer
-----------------------------------------------------------------

-- Imports a csv file into zipped columns
importTable :: FilePath -> IO (Either InterException [[String]])
importTable f = do 
    res <- try $ readFile f :: IO (Either IOError String)
    case res of
      Left e -> throw (InterExceptionImport e)
      Right dat -> do
        -- FIXME: Does not allow commas in strings, can't use lookbehind due to POSIX regex -- (?<!\\)
        let tokenise = map (parseCSVLine) . lines
        return (multiZip' $ tokenise dat)


-----------------------------------------------------------------
-- 'Simple' CSV Line Parser
-----------------------------------------------------------------
-- Alternative to regex based approaches, which would be far 
-- easier, yet require unavailable libraries. Allows for CSV lines
-- to be split with any characters between commas; this includes
-- commas provided they are escaped with a backslash.


-- FIXME: This was pretty dumb, maybe a better way would be to do a split
-- by ',' using splitOn from Data.List.Split, then iterate over created arrays and
-- if there is a 'free' backslash, as in last element of array not directly 
-- preceeded by another backslash the slash is removed, a comma is placed and the two
-- arrays are rejoined. Not speed efficient but probably better... 

parseCSVLine :: String -> [String]
parseCSVLine xs = processEscapes (csvSplit xs) csvEscapes 

csvSplit :: String -> [ String ]
csvSplit [] = []
csvSplit xs = (fst (split xs)) : csvSplit (drop 1 (snd (split xs)))

csvEscapes :: [(String, String)]
csvEscapes = [("\\,", ",")]

-- FIXME: Relies on replace which isn't included normally...
processEscapes :: Eq a => [a] -> [(a, a)] -> [a]
processEscapes xs []         = xs
processEscapes xs ((x,y):es) = processEscapes (replace x y xs) es

split :: String -> (String, String)
split xs = mapTuple (fmap snd) (rawSplit xs)

rawSplit :: String -> ([(Char, Char)], [(Char, Char)])
rawSplit xs = span (not . csvDelimiter) (adjs xs)

adjs :: String -> [(Char, Char)]
adjs xs = zip ('\0' : init xs) xs

csvDelimiter :: (Char, Char) -> Bool
csvDelimiter ('\\', ',') = False
csvDelimiter (_   , ',') = True
csvDelimiter  _          = False

-- Helper functions
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- FIXME: Does nothing...
replace :: Eq a => a -> a -> [a] -> [a]
replace x y xs = xs

--replace old new = intercalate new . splitOn old
-- ^^^ Data.String.Utils

-----------------------------------------------------------------
-- Rows -> Columns
-----------------------------------------------------------------

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



-----------------------------------------------------------------
-- TODO!!!
-----------------------------------------------------------------

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

