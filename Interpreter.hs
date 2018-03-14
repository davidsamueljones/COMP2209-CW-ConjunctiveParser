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
            storedTables :: [StoredTable],
            tableState :: Table,
            boundVars :: Vars
          }
          deriving Show

-- Empty environment definition
initEnv :: Env
initEnv = Env [] (Table [] [[]]) []

-- Raw table with identifier for lookups 
data TableStore  = TableStore TableID Table deriving (Show, Eq)
data StoredTable  = StoredTable 
                  {
                    storedTableID   :: TableID,
                    storedTableData :: TableData 
                  }
                  deriving (Show, Eq)

-- Table with data associated to column variables
-- It is not enforced that data is stored in rows or columns
type Row = [String]
type TableData = [[String]]    
data Table = Table { columnVars :: Vars, tableData :: TableData }
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
        let res' = makeOutputTable vs (boundVars env') (tableState env')
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
    let res = lookupTableData t (storedTables env)
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

conjunction :: Table -> Table -> Table
conjunction r1 r2 = removeDupCols combined
  where ids1 = (columnVars r1)
        ids2 = (columnVars r2)
        ids = ids1 ++ ids2
        vars = getDupCols ids
        combined = (Table ids [a ++ b| a <- tableData r1, b <- tableData r2, sameVars vars ids1 ids2 a b])
                     
combine :: Table -> Table -> Table
combine r1 r2 = (Table ids [a ++ b| a <- tableData r1, b <- tableData r2])
  where ids = (columnVars r1) ++ (columnVars r2)
                        
sameVars :: [Var] -> [String] -> [String] -> Row -> Row -> Bool
sameVars vars ids1 ids2 row1 row2 = and [a | b <- vars, let a = sameVar b ids1 ids2 row1 row2]
            
sameVar :: Var -> [String] -> [String] -> Row -> Row -> Bool
sameVar var ids1 ids2 row1 row2 = (getVar var ids1 row1) == (getVar var ids2 row2)

getVar :: Var -> [String] -> Row -> Var
getVar var ids row
  | elem var ids = row !! fromJust (elemIndex var ids)
  | otherwise          = "" --TODO: Handle this case if required

-----------------------------------------------------------------
-- Equality
-----------------------------------------------------------------

equality :: Table -> Var -> Var -> Either InterException Table
equality table v1 v2
  | elem v1 ids && elem v2 ids = Right ((Table ids [a | a <- rows, getVar v1 ids a == getVar v2 ids a]))
  | otherwise                  = throw IEVarNotFound
  where rows = tableData table
        ids = columnVars table

-----------------------------------------------------------------
-- Lookup
-----------------------------------------------------------------

--Merges columns of a table in cases of repeated vars in columnVars
mergeColumns :: Table -> Table
mergeColumns t
  | getDupCols ids == [] = t
  | otherwise            = removeDupCols (Table ids [row|row <- rs, repeatsAreEqual ids row])
  where ids = columnVars t
        rs = tableData t
        repeatsAreEqual :: [Var] -> [Var] -> Bool
        repeatsAreEqual [] [] = True
        repeatsAreEqual (x:xs) (y:ys)
          | elem x xs = (y == ys !! fromJust (elemIndex x xs)) && repeatsAreEqual xs ys
          | otherwise = repeatsAreEqual xs ys


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
      return (Right (tokenise dat))              
                 
-----------------------------------------------------------------
-- 'Simple' CSV Line Parser
-----------------------------------------------------------------
-- Allows for CSV lines to be split with any characters between commas; 
-- this includes commas provided they are escaped with a backslash. 
-- Backslashes must also be escaped for this reason. Escapes also allow 
-- forced white space at the beginning of fields FIXME!!!. All other escapes 
-- are ignored. 


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
removeDupCols :: Table -> Table
removeDupCols rowTab = (Table newIds newRows)
                     where ids = columnVars rowTab
                           rows = tableData rowTab
                           newRows = [b| a<- rows, let b = removeDupCols' ids a ]
                           newIds = nub ids
                           
removeDupCols' :: [String] -> [String] -> [String]
removeDupCols' [] [] = []
removeDupCols' (i:is) (r:rs) 
  | elem i is = removeDupCols' is rs
  | otherwise = [r] ++ removeDupCols' is rs

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

table2csv :: TableData -> Maybe String
table2csv [[]] = Nothing
table2csv xs   = Just  $ unlines (map (intercalate ",") xs)
                   
printTable :: TableData -> IO ()
printTable t = let res = table2csv t in
  case res of
    Nothing -> return ()
    Just dat -> putStr dat

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
                    | IEVarNotInOutput Var
                    | IEOutputVarBound Var
                    | IEOutputVarNotExist Var
                    | IEUnknown
                    deriving (Show)

instance Exception InterException

-----------------------------------------------------------------
-- TODO!!!
-----------------------------------------------------------------


-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

