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

data Env  = Env 
          {
            storedTables  :: [TableStore],
            tableState  :: Table,
            boundVars   :: Vars
          }
          deriving Show

-- Empty environment definition
initEnv :: Env
initEnv = Env [] [] []

-- Raw table with identifier for lookups 
data TableStore  = TableStore TableID Table deriving (Show, Eq)

-- Table held in column form (with column variables)
--type ColumnTable = [Column]
--data Column = Column { columnID :: Var, columnData :: [String] }
--            deriving (Show, Eq)

-- Table held in row form (with column variables)
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
        let imported = TableStore i dat
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
                printTable table
                return (Right env)
              Just i -> do
                let store = TableStore i table -- TODO, throw store error if table exists
                let updatedEnv = env {storedTables = (store:(storedTables env))}
                return (Right updatedEnv)

  (Print t) -> return (Right env) --TODO

-- Process expression, updating environment respectively -- FIXME FINISH
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
        let res' = assignColumnVars vs dat
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


-- TODO: 
-- * Conjunction like thing of variables and table (taking away bound variables)
-- * Sort lexicographically?
-- * Order as outputs
-- * Throw errors for: * LHS free variables not using all free variables
--                     * LHS has bound variables in

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

lookupTableData :: Var -> [TableStore] -> (Either InterException Table) 
lookupTableData x []                      = throw $ IETableNotFound x
lookupTableData x ((TableStore y dat):ts) | x == y    = Right dat
lookupTableData x (_:ts)                  | otherwise = lookupTableData x ts

assignColumnVars :: Vars -> Table -> (Either InterException Table)
assignColumnVars = assignColumnVars' []

assignColumnVars' :: Table -> Vars -> Table -> (Either InterException Table)
assignColumnVars' os []     []     = Right os
assignColumnVars' _  []     _      = throw IENotEnoughVars
assignColumnVars' _  _      []     = throw IETooManyVars
--assignColumnVars' os (v:vs) (t:ts) = assignColumnVars' ((Column v t):os) vs ts

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

-----------------------------------------------------------------
-- ExQual
-----------------------------------------------------------------

addBoundVariables :: Vars -> Env -> (Either InterException Env)
addBoundVariables []     env = Right (env)
addBoundVariables (v:vs) env = case (find (== v) (boundVars env)) of
  Nothing ->  case (find (== v) []) of -- FIXME Make check use column headings
                Nothing -> let newEnv = env {boundVars = (v:(boundVars env))} in
                           addBoundVariables vs newEnv
                Just _  -> throw IEVarExistsNotBound
  Just _  ->  throw IEVarAlreadyBound 

-----------------------------------------------------------------
-- Table Importer
-----------------------------------------------------------------

-- Imports a csv file into zipped columns
importTable :: FilePath -> IO (Either InterException [[String]])
importTable f = do 
  res <- try $ readFile f :: IO (Either IOError String)
  case res of
    Left e -> throw (IEReadError e)
    Right dat -> do
      let tokenise = map parseCSVLine . lines
      return (transpose' $ tokenise dat)

-----------------------------------------------------------------
--Selecting vars from final table
-----------------------------------------------------------------

-- TODO: Doesn't check boundness?
-- Gets the requested columns in the requested order, rows sorted lexicographically
--makeOutputTable :: [Var] -> ColumnTable -> (Either InterException Table)
--makeOutputTable xs cls
--  | length columns == length xs = Right (map columnData columns)
--  | otherwise                   = throw IEVarNotFound
--  where ids = map columnID cls
--        columns = lexiSort [fst colbool| a <- xs, let colbool = getColumn a cls, snd colbool == True]
  
--lexiSort :: ColumnTable -> ColumnTable
--lexiSort a = row2col $ sorted
--           where rows = col2row a
--                 sorted = sortOn (\a -> concat $ rowData a) rows

                 
                 
-----------------------------------------------------------------
-- 'Simple' CSV Line Parser
-----------------------------------------------------------------
-- Allows for CSV lines to be split with any characters between commas; 
-- this includes commas provided they are escaped with a backslash. 
-- Backslashes must also be escaped for this reason. Escapes also allow 
-- forced white space at the beginning of fields FIXME!!!. All other escapes 
-- are ignored. 

parseCSVLine :: String -> [String]
parseCSVLine xs = csvSplit xs 

csvSplit :: String -> [ String ]
csvSplit [] = []
csvSplit xs = map trim (splitOnComma xs)

splitOnComma :: String -> [String]
splitOnComma xs = splitOnComma' xs []

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

table2csv :: Table -> String
table2csv xs = unlines ( map (intercalate ",") rows)
             where rows = tableData xs
                   
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


-- http://matt.might.net/articles/cek-machines/
-- http://matt.might.net/articles/cesk-machines/
-- https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf

