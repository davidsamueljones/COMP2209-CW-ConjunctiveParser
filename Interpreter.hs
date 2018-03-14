module Interpreter where
import Grammar

import Control.Exception
import Control.Monad
import System.Exit  
import Data.List
import Data.Maybe
import Data.Either
import Data.Char (isSpace)

-----------------------------------------------------------------
-- Interpreter Types
-----------------------------------------------------------------

-- Environment 'stack' used during interpretation holding the
-- tables available for lookup, the state of table currently in
-- the stack. The variables bound in the scope.
data Env  = Env 
          {
            storedTables  :: [StoredTable],
            tableState    :: Table,
            boundVars     :: Vars
          }
          deriving Show

-- Empty environment definition
initEnv :: Env
initEnv = Env [] (Table [] [[]]) []

-- Raw table with identifier for lookups 
type TableData    = [[String]]
data StoredTable  = StoredTable 
                  {
                    storedTableID   :: TableID,
                    storedTableData :: TableData 
                  }
                  deriving (Show, Eq)

-- Table with data associated to column variables
-- It is not enforced that data is stored in rows or columns
data Table = Table
              {
                columnVars   :: Vars,
                tableData    :: TableData
              }
              deriving (Show, Eq)

-- REMOVE THESE
-- Table held in column form (with column variables)
type ColumnTable = [Column]
data Column = Column { columnID :: Var, columnData :: [String] }
            deriving (Show, Eq)

-- Table held in row form (with column variables)
type RowTable = [Row]     
data Row = Row { columnIDs :: Vars, rowData :: [String] }
         deriving (Show, Eq)

-----------------------------------------------------------------
-- Interpreter Control
-----------------------------------------------------------------

-- Process a program, starting with imports and then statements
-- This is built up in a global environment so all queries have
-- knowledge of all base tables and all queries have knowledge
-- of previous stored queries.
runInterpreter :: Prog -> IO (Either InterException String)
runInterpreter (Prog is stmts) = do
  -- Initiate environment with imported base tables
  resImports <- evalImports initEnv is
  case resImports of
    Left e -> throw e -- rethrow up stack
    Right envImports -> do
      -- No import errors, do queries
      resStmts <- evalStmts envImports stmts
      -- TODO : Error handling
      return (Right "")

-- Process imports by importing data and creating base tables
-- in environment
evalImports :: Env -> Imports -> IO (Either InterException Env)
evalImports env []     =  return (Right env)
evalImports env (t:ts) = case t of
  Import p i -> do
    -- TODO: Error for table already exists
    res <- importTable p
    case res of
      Left e -> throw e -- rethrow up stack
      Right dat -> do
        let imported = StoredTable i dat
        let updatedEnv = env {storedTables = (imported:(storedTables env))}
        evalImports updatedEnv ts

-- Process statements, placing updates in environment
evalStmts :: Env -> Stmts -> IO (Either InterException Env) 
evalStmts env []        = return (Right env)
evalStmts env (s:ss) = do
    res <- evalStmt env s
    case res of
      Left e -> throw e -- rethrow up stack
      Right newEnv -> do
        evalStmts newEnv ss

-- Process statement, updating environment respectively
evalStmt :: Env -> Stmt -> IO (Either InterException Env)
evalStmt env s = case s of 
  (Query store vs e) -> do
    res <- evalExp env e
    case res of 
      Left e -> throw e -- rethrow up stack
      Right env' -> do
        let res' = makeOutputTable vs (tableState env')
        case res' of
          Left e -> throw e -- rethrow up stack
          Right table -> do
            case store of
              Nothing -> do
                -- Print table but do not store it for later use
                printTable table
                return (Right env)
              Just i -> do
                -- Store table for later use, not printing it
                let store = StoredTable i table -- TODO, throw store error if table exists
                let updatedEnv = env {storedTables = (store:(storedTables env))}
                return (Right updatedEnv)

  (Print t) -> do
    let res = lookupTableData t (storedTables)
    case res of 
      Left e -> throw e -- rethrow up stack
      Right table -> do
        printTable table
        return (Right env)

-- Process expression, updating environment respectively
evalExp :: Env -> Exp -> IO (Either InterException Env) 
evalExp env e = case e of

  Conjunction lExp rExp -> do
    lRes <- evalExp env lExp
    case lRes of 
      Left e -> throw e -- rethrow up stack
      Right lEnv -> do
        rRes <- evalExp lEnv rExp
        case rRes of 
          Left e -> throw e -- rethrow up stack
          Right rEnv -> do
            let joinedTable = conjunction (tableState lEnv) (tableState rEnv)
            let newEnv = rEnv {tableState = joinedTable} 
            return (Right newEnv)
            
  Equality v1 v2 -> do
    let currentTable = tableState env 
    let newTable = equality currentTable v1 v2
    case newTable of
      Left e -> throw e -- throw up the stack (IEVarNotFound)
      Right table -> do
        let newEnv = env {tableState = table}
        return (Right newEnv)
    
  Lookup t vs -> do
    let res = lookupTableData t (storedTables env)
    case res of
      Left e -> throw e -- rethrow up stack
      Right dat -> do 
        let res' = makeTable vs dat
        case res' of
          Left e -> throw e -- rethrow up stack
          Right table -> do 
            let mergedTable = mergeColumns table
            let newEnv = env {tableState = mergedTable}
            return (Right newEnv)

  ExQual vs e -> do
    let res = addBoundVariables vs env
    case res of
      Left e -> throw e -- rethrow up stack
      Right envWithBounds -> do
        res' <- evalExp envWithBounds e
        case res' of
          Left e -> throw e -- rethrow up stack
          Right newEnv -> return (Right newEnv)

-----------------------------------------------------------------
-- Conjunction
-----------------------------------------------------------------

conjunction :: ColumnTable -> ColumnTable -> ColumnTable
conjunction c1 c2 = removeDupCols $ row2col combined
  where r1 = col2row c1
        r2 = col2row c2
        ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
        vars = getDupCols ids
        combined = [(Row ids (rowData a ++ rowData b))| a <- r1, b <- r2, sameVars vars a b]
                     
combine :: RowTable -> RowTable -> RowTable
combine r1 r2 = [(Row ids (a ++ b))| a <- map rowData r1, b <- map rowData r2]
  where ids = (columnIDs $ head r1) ++ (columnIDs $ head r2)
                        
sameVars :: [Var] -> Row -> Row -> Bool
sameVars vars row1 row2 = and [a | b <- vars, let a = sameVar b row1 row2]
            
sameVar :: Var -> Row -> Row -> Bool
sameVar var row1 row2 = (getVar var row1) == (getVar var row2)

getVar :: Var -> Row -> Var
getVar var (Row columnIDs rowData)
  | elem var columnIDs = rowData !! fromJust (elemIndex var columnIDs)
  | otherwise          = ""

-----------------------------------------------------------------
-- Equality
-----------------------------------------------------------------

equality :: ColumnTable -> Var -> Var -> Either InterException ColumnTable
equality table v1 v2
  | elem v1 ids && elem v2 ids = Right (row2col [a | a <- rows, getVar v1 a == getVar v2 a])
  | otherwise                  = throw IEVarNotFound
  where rows = col2row table
        ids = map columnID table

-----------------------------------------------------------------
-- Lookup
-----------------------------------------------------------------

-- Retrieve a given table from memory 
lookupTableData :: Var -> [StoredTable] -> (Either InterException TableData) 
lookupTableData t ts = do
  let lkup = find (\x -> storedTableID x == t) ts
  case lkup of
    Nothing -> throw $ IETableNotFound t
    Just (StoredTable _ dat) -> return dat

-- Using a row table input and column data, make a table
makeTable :: Vars -> TableData -> (Either InterException Table)
makeTable vs rows | length vs > length cols = throw IETooManyVars
                  | length vs < length cols = throw IENotEnoughVars
                  | otherwise               = return $ Table vs rows
                  where cols = transpose rows

--Merges columns of a table in cases of repeated vars in columnIds)
mergeColumns :: ColumnTable -> ColumnTable
mergeColumns cs
  | getDupCols ids == [] = cs
  | otherwise            = removeDupCols $ row2col [row|row <- rs, repeatsAreEqual ids (rowData row)]
  where ids = map columnID cs
        rs = col2row cs
        repeatsAreEqual :: [Var] -> [Var] -> Bool
        repeatsAreEqual [] [] = True
        repeatsAreEqual (x:xs) (y:ys)
          | elem x xs = (y == ys !! fromJust (elemIndex x xs)) && repeatsAreEqual xs ys
          | otherwise = repeatsAreEqual xs ys

-----------------------------------------------------------------
-- ExQual
-----------------------------------------------------------------

addBoundVariables :: Vars -> Env -> (Either InterException Env)
addBoundVariables []     env = Right (env)
addBoundVariables (v:vs) env = case (find (== v) (boundVars env)) of
  Nothing ->  case (find (== v) (columnVars (tableState env))) of
                Nothing -> let newEnv = env {boundVars = (v:(boundVars env))} in
                           addBoundVariables vs newEnv
                Just _  -> throw IEVarExistsNotBound
  Just _  ->  throw IEVarAlreadyBound 

-----------------------------------------------------------------
-- Query Table Output 
-----------------------------------------------------------------

-- Gets the requested columns in the requested order, sorting rows lexicographically
makeOutputTable :: Vars -> Vars -> Table -> (Either InterException TableData) 
makeOutputTable outVars boundVars table = do
  let notVarOutputs = [v | v <- outVars, not (v `elem` (columnVars table))]
  case notVarOutputs of
    (v:_) -> throw $ IEOutputVarNotExist v
    [] -> do
      -- All output variables in table
      let boundOutputs = [v | v <- outVars, v `elem` boundVars]
      case boundOutputs of
        (v:_) -> throw $ IEOutputVarBound v
        [] -> do
          -- All output variables are not bound
          let freeVariables = filter (\x -> not (x `elem` boundVars)) (columnVars table)
          let missingVariables = [v | v <- freeVariables, not (v `elem` outVars)]
          case missingVariables of
            (v:_) -> throw $ IEVarNotInOutput v
            [] -> do
            -- All free variables are in output
            let columns  = transpose (tableData table) 
            let filtered = filter (\x -> fst x `elem` outVars) (zip (columnVars table) columns)
            let ordered  = sortOn (\x -> lookup (fst x) (zip outVars [0..])) filtered 
            let rows     = transpose (map snd ordered)
            let sorted   = sortOn (\r -> concat r) rows
            return sorted                 

-----------------------------------------------------------------
-- Table Importer
-----------------------------------------------------------------

-- Imports a csv file into zipped columns
importTable :: FilePath -> IO (Either InterException TableData)
importTable f = do 
  res <- try $ readFile f :: IO (Either IOError String)
  case res of
    Left e -> throw (IEReadError e)
    Right dat -> do
      let tokenise = map parseCSVLine . lines
      return (tokenise dat)


parseCSVLine :: String -> [String]
parseCSVLine [] = []
parseCSVLine xs = map trim (splitOnComma xs)

splitOnComma :: String -> [String]
splitOnComma xs = splitOnComma' xs []

-- Split on commas allowing escapes with backslashes (alternative to regex lookback)
splitOnComma' :: String -> String -> [String]
splitOnComma' []         buf = [reverse buf]
splitOnComma' (x1:x2:xs) buf | x1 == ','  && x2 == ',' = reverse buf : [] : splitOnComma' xs  []
splitOnComma' (x1:x2:xs) buf | x1 /= '\\' && x2 == ',' = reverse (x1:buf) : splitOnComma' xs  []
splitOnComma' (x1:x2:xs) buf | x1 == '\\'              = splitOnComma' (xs) (x2:buf)
splitOnComma' (x1:xs)    buf | x1 == ','               = reverse (buf) : splitOnComma' xs  []
splitOnComma' (x1:xs)    buf | otherwise               = splitOnComma' (xs) (x1:buf)

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

-----------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------

-- Find columns with the same ID (var name)
getDupCols :: Vars -> Vars
getDupCols [] = []
getDupCols (x:xs)
    | elem x xs = [x] ++ getDupCols xs
    | otherwise = getDupCols xs

-- Remove columns with the same ID (var name)
removeDupCols :: ColumnTable -> ColumnTable
removeDupCols [] = []
removeDupCols (x:xs)
  | elem id idList = removeDupCols xs
  | otherwise = [x] ++ removeDupCols xs
  where idList = map columnID xs
        id = columnID x

-- Convert a table in column form into row form 
col2row :: ColumnTable -> RowTable
col2row t = col2row' (map columnID t) (map columnData t)
col2row' :: Vars -> Table -> RowTable
col2row' _ [] = []
col2row' ids columns 
  | (length $ head columns) > 1 = [(Row ids row)] ++ col2row' ids tailColumns
  | otherwise          = [(Row ids row)]
  where row = map head columns
        tailColumns = map tail columns

-- Convert a table in row form into column form 
row2col :: RowTable -> ColumnTable
row2col [] = []
row2col (t:ts) = row2col' (columnIDs t) (map rowData (t:ts))
row2col' :: Vars -> Table -> ColumnTable
row2col' _ [] = []
row2col' (x:xs) rows
  | xs == []  = [(Column x column)]
  | otherwise = [(Column x column)] ++ row2col' xs tailRows
  where column = map head rows
        tailRows = map tail rows

-- Transpose but fails if lists are not all equal length
transpose' :: [[a]] -> Either InterException [[a]]
transpose' xss | allLensSame xss = Right (transpose xss)
transpose' xss | otherwise       = throw IEUnequalLists

-- Get length of all rows, true if all equal
allLensSame :: [[a]] -> Bool
allLensSame xss = allValsSame $ map length xss

-- True if all values in list are equal 
allValsSame :: Eq a =>[a] -> Bool
allValsSame xs = all (== head xs) (tail xs)

table2csv :: Table -> String
table2csv xs = unlines ( map (intercalate ",") rows)
             where rows = transpose xs
                   
printTable :: Table -> IO ()
printTable t = putStrLn $ table2csv t


-----------------------------------------------------------------
-- Interpreter Exceptions
-----------------------------------------------------------------
data InterException = IEImport InterException
                    | IEQuery InterException
                    | IEUnequalLists
                    | IEReadError IOError
                    | IETableNotFound TableID
                    | IETooManyVars
                    | IENotEnoughVars
                    | IEVarNotFound
                    | IEVarAlreadyBound
                    | IEVarExistsNotBound
                    | IEUnknown
                    deriving (Show)

instance Exception InterException

-----------------------------------------------------------------
-- TODO!!!
-----------------------------------------------------------------

testColumn=  [(Column "x1" ["Pawel"]),(Column "x2" ["Sobocinski"])]
testColumn1= [(Column "x3" ["Julian"]),(Column "x4" ["Rakthe"])]

testColumn2= [(Column "x1" ["1","1"]),(Column "x2" ["3","3"])]
testColumn3= [(Column "x3" ["3","3"]),(Column "x4" ["4","4"])]

testColumn4= [(Column "x1" ["1","1"]),(Column "x2" ["3","2"])]
testColumn5= [(Column "x2" ["3","3","2"]),(Column "x3" ["1","2","2"])]

testColumn6= [(Column "x1" ["Sofiane","Tadic","Guido"]),(Column "x2" ["Boufal","Tadic","Carillo"])]
testColumn7= [(Column "x2" ["Boufal","Carillo"]),(Column "x3" ["Maserati","Ferrari"])]

-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

