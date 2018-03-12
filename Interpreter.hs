module Interpreter where
import Grammar
import Control.Exception
import Data.Either
import Control.Monad
import System.Exit  
import Data.List
import Data.Maybe 

type ColumnTable = [Column] 
type RowTable = [Row]
type ColumnData = [String]
type RowData = [String]
type ColumnID = Var

data Column = Column
            {
               columnID :: ColumnID,
               columnData :: ColumnData
            } deriving Show
            
data Row = Row
            {
               columnIDs :: [ColumnID],
               rowData :: RowData
            } deriving Show
            
testColumn= [(Column "x1" ["Pawel"]),(Column "x2" ["Sobocinski"])]
testColumn1= [(Column "x3" ["Julian"]),(Column "x4" ["Rakthe"])]

testColumn2= [(Column "x1" ["1","1"]),(Column "x2" ["3","3"])]
testColumn3= [(Column "x3" ["3","3"]),(Column "x4" ["4","4"])]

testColumn4= [(Column "x1" ["1","1"]),(Column "x2" ["3","2"])]
testColumn5= [(Column "x2" ["3","3","2"]),(Column "x3" ["1","2","2"])]

--testTable1 = (Column["x3","x4"] [["Julian"],["Rathke"]])
--testTable2 = (Column["x1","x2"] [["1","1"],["2","2"]])
--testTable3 = (Column["x3","x4"] [["3","3"],["4","4"]])
--testTable4 = (Column["x1","x2"] [["1","1"],["3","2"]])
--testTable5 = (Column["x2","x3"] [["3","3","2"],["1","2","2"]])
--testTable6 = (Column["x1","x2"] [["1","1"],["2","2"]])
--testTable7 = (Column["x2","x3"] [["2","2"],["1","1"]])

-- TODO: Just ya know, write this thing?
--runInterpreter :: Show a => Prog -> [a]
--runInterpreter ast = []


conjunction :: [Column] -> [Column] -> [Column]
conjunction c1 c2 = removeDupCols $ transposeRow combined
                  where r1 = transposeCol c1
                        r2 = transposeCol c2
                        ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
                        vars = getDupCols ids
                        combined = [(Row ids (rowData a ++ rowData b))| a <- r1, b <- r2, sameVars vars a b]
                     
combine :: [Row] -> [Row] -> [Row]
combine r1 r2 = [(Row ids (a ++ b))| a <- map rowData r1, b <- map rowData r2]
              where ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
                        
sameVars :: [Var] -> Row -> Row -> Bool
sameVars vars row1 row2 = foldr (&&) True [a | b <- vars, let a = sameVar b row1 row2]
            
sameVar :: Var -> Row -> Row -> Bool
sameVar var row1 row2 = (getVar var row1) == (getVar var row2)

getVar :: Var -> Row -> Var
getVar var (Row columnIDs rowData)
  | elem var columnIDs = rowData !! fromJust (elemIndex var columnIDs)
  | otherwise          = error ("getVar called with no element")--TODO Exception
 
getDupCols :: [ColumnID] -> [Var]
getDupCols [] = []
getDupCols (x:xs)
    | elem x xs = [x] ++ getDupCols xs
    | otherwise = getDupCols xs

removeDupCols :: [Column] -> [Column]
removeDupCols [] = []
removeDupCols (x:xs)
  | elem id idList = removeDupCols xs
  | otherwise = [x] ++ removeDupCols xs
  where idList = map columnID xs
        id = columnID x

transposeCol :: [Column] -> [Row]
transposeCol t = transposeCol' (map columnID t) (map columnData t)
transposeCol' :: [ColumnID] -> [[String]] -> [Row]
transposeCol' _ [] = []
transposeCol' ids columns 
  | (length $ head columns) > 1 = [(Row ids row)] ++ transposeCol' ids tailColumns
  | otherwise          = [(Row ids row)]

  where row = map head columns
        tailColumns = map tail columns
  
transposeRow :: [Row] -> [Column]
transposeRow (t:ts) = transposeRow' (columnIDs t) (map rowData (t:ts))
transposeRow' :: [ColumnID] -> [[String]] -> [Column]
transposeRow' _ [] = []
transposeRow' (x:xs) rows
  | xs == []  = [(Column x column)]
  | otherwise =[(Column x column)] ++ transposeRow' xs tailRows
  where column = map head rows
        tailRows = map tail rows

colStringArr :: [Column] -> [[String]]
colStringArr t = map columnData t

rowStringArr :: [Row] -> [[String]]
rowStringArr t = map rowData t



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

