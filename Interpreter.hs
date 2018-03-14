module Interpreter where
import Grammar

import Control.Exception (try)
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
data Table = Table { columnVars :: Vars, tableData :: TableData }
         deriving (Show, Eq)

-- Unstrict identifiers for function clarity 
type Row         = [String]
type RowTable    = Table 
type Column      = [String]
type ColumnTable = Table
type TableData   = [[String]]  

-- Return type for functions that holds a given type or an interpreter Exception
type InterReturn a = Either InterException a

-----------------------------------------------------------------
-- Interpreter Control
-----------------------------------------------------------------

-- Process a program, starting with imports and then statements
-- This is built up in a global environment so all queries have
-- knowledge of all base tables and all queries have knowledge
-- of previous stored queries.
runInterpreter :: Prog -> IO (InterReturn String)
runInterpreter (Prog is stmts) = do
  -- Initiate environment with imported base tables
  resImports <- evalImports initEnv is
  case resImports of
    Left e -> throwIO (IEImport e) -- rethrow up stack
    Right envImports -> do
      -- No import errors, do queries
      resStmts <- evalStmts envImports stmts
      case resStmts of
        Left e -> throwIO (IEQuery e)
        Right _ -> return (Right "")
      -- TODO : Error handling


-- Process imports by importing data and creating base tables
-- in environment
evalImports :: Env -> Imports -> IO (InterReturn Env)
evalImports env []     =  return (pure env)
evalImports env (t:ts) = case t of
  Import p i -> do
    let findVar = find (\x -> storedTableID x == i) (storedTables env)
    case findVar of
      Just _ -> throwIO $ IETableAlreadyDefined i --If A table is already defined under that name
      Nothing -> do
        res <- importTable p
        case res of
          Left e -> throwIO e -- rethrow up stack
          Right dat -> do
            let imported = StoredTable i dat
            let updatedEnv = env {storedTables = (imported:(storedTables env))}
            evalImports updatedEnv ts

-- Process statements, placing updates in environment
evalStmts :: Env -> Stmts -> IO (InterReturn Env) 
evalStmts env []        = return (pure env)
evalStmts env (s:ss) = do
    res <- evalStmt env s
    case res of
      Left e -> throwIO e -- rethrow up stack
      Right newEnv -> do
        evalStmts newEnv ss

-- Process statement, updating environment respectively
evalStmt :: Env -> Stmt -> IO (InterReturn Env)
evalStmt env s = case s of 
  (Query store vs e) -> do
    res <- evalExp env e
    case res of 
      Left e -> throwIO e -- rethrow up stack
      Right env' -> do
        let res' = makeOutputTable vs (boundVars env') (tableState env')
        case res' of
          Left e -> throwIO e -- rethrow up stack
          Right table -> do
            case store of
              Nothing -> do
                -- Print table but do not store it for later use
                printTable table
                return (pure env)
              Just i -> do 
                -- Store table for later use, not printing it
                let findVar = find (\x -> storedTableID x == i) (storedTables env)
                case findVar of
                  Just _ -> throwIO $ IETableAlreadyDefined i
                  Nothing -> do
                    let store = StoredTable i table
                    let updatedEnv = env {storedTables = (store:(storedTables env))}
                    return (pure updatedEnv)

  (PrintTable t) -> do
    let res = lookupTableData t (storedTables env)
    case res of 
      Left e -> throwIO e -- rethrow up stack
      Right table -> do
        printTable table
        return (pure env)

  (PrintString s) -> do
    putStrLn s
    return (pure env)


-- Process expression, updating environment respectively
evalExp :: Env -> Exp -> IO (InterReturn Env) 
evalExp env e = case e of

  Conjunction lExp rExp -> do
    lRes <- evalExp env lExp
    case lRes of 
      Left e -> throwIO e -- rethrow up stack
      Right lEnv -> do
        case rExp of
        
          (Equality v1 v2) -> do 
            rRes <- evalExp lEnv (Equality v1 v2)
            case rRes of 
              Left e -> throwIO e -- rethrow up stack
              Right rEnv -> do
                return (pure rEnv)  

          (NotEquality v1 v2) -> do 
            rRes <- evalExp lEnv (NotEquality v1 v2)
            case rRes of 
              Left e -> throwIO e -- rethrow up stack
              Right rEnv -> do
                return (pure rEnv)                  
        
          rEx -> do
            rRes <- evalExp lEnv rEx
            case rRes of 
              Left e -> throwIO e -- rethrow up stack
              Right rEnv -> do
                let joinedTable = conjunction (tableState lEnv) (tableState rEnv)
                let newEnv = rEnv {tableState = joinedTable} 
                return (pure newEnv)
            
  Equality v1 v2 -> do
    let currentTable = tableState env 
    let newTable = equality currentTable v1 v2
    case newTable of
      Left e -> throwIO e -- throw up the stack (IEVarNotFound)
      Right table -> do
        let newEnv = env {tableState = table}
        return (pure newEnv)
    
            
  NotEquality v1 v2 -> do
    let currentTable = tableState env 
    let newTable = notEquality currentTable v1 v2
    case newTable of
      Left e -> throwIO e -- throw up the stack (IEVarNotFound)
      Right table -> do
        let newEnv = env {tableState = table}
        return (pure newEnv)    
    
  Lookup t vs -> do
    let res = lookupTableData t (storedTables env)
    case res of
      Left e -> throwIO e -- rethrow up stack
      Right dat -> do 
        let res' = makeTable t vs dat
        case res' of
          Left e -> throwIO e -- rethrow up stack
          Right table -> do 
            let mergedTable = mergeColumns table
            let newEnv = env {tableState = mergedTable}
            return (pure newEnv)

  ExQual vs e -> do
    let res = addBoundVariables vs env
    case res of
      Left e -> throwIO e -- rethrow up stack
      Right envWithBounds -> do
        res' <- evalExp envWithBounds e
        case res' of
          Left e -> throwIO e -- rethrow up stack
          Right newEnv -> return (pure newEnv)

-----------------------------------------------------------------
-- Conjunction
-----------------------------------------------------------------

conjunction :: RowTable -> RowTable -> RowTable
conjunction r1 r2 = removeDupCols combined
  where ids1 = (columnVars r1)
        ids2 = (columnVars r2)
        ids = ids1 ++ ids2
        vars = getDupCols ids
        combined = (Table ids [a ++ b| a <- tableData r1, b <- tableData r2, sameVars vars ids1 ids2 a b])
                     
combine :: RowTable -> RowTable -> RowTable
combine r1 r2 = (Table ids [a ++ b| a <- tableData r1, b <- tableData r2])
  where ids = (columnVars r1) ++ (columnVars r2)
                        
sameVars :: Vars -> Vars -> Vars -> Row -> Row -> Bool
sameVars vars ids1 ids2 row1 row2 = and [a | b <- vars, let a = sameVar b ids1 ids2 row1 row2]
            
sameVar :: Var -> Vars -> Vars -> Row -> Row -> Bool
sameVar var ids1 ids2 row1 row2 = (getVar var ids1 row1) == (getVar var ids2 row2)

getVar :: Var -> Vars -> Row -> Var
getVar var ids row
  | elem var ids = row !! fromJust (elemIndex var ids)
  | otherwise          = ""

-----------------------------------------------------------------
-- Equality
-----------------------------------------------------------------

equality :: RowTable -> Var -> Var -> InterReturn RowTable
equality table v1 v2
  | notElem v2 ids = throw $ IEVarNotFound v2
  | notElem v1 ids = throw $ IEVarNotFound v1
  | otherwise                  = return ((Table ids [a | a <- rows, getVar v1 ids a == getVar v2 ids a]))
  where rows = tableData table
        ids = columnVars table
        
notEquality :: RowTable -> Var -> Var -> InterReturn RowTable
notEquality table v1 v2
  | notElem v2 ids = throw $ IEVarNotFound v2
  | notElem v1 ids = throw $ IEVarNotFound v1
  | otherwise                  = return ((Table ids [a | a <- rows, getVar v1 ids a /= getVar v2 ids a]))
  where rows = tableData table
        ids = columnVars table

-----------------------------------------------------------------
-- Lookup
-----------------------------------------------------------------

--Merges columns of a table in cases of repeated vars in columnVars
mergeColumns :: RowTable -> RowTable
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
lookupTableData :: Var -> [StoredTable] -> (InterReturn TableData) 
lookupTableData t ts = do
  let lkup = find (\x -> storedTableID x == t) ts
  case lkup of
    Nothing -> throw $ IETableNotFound t
    Just (StoredTable _ dat) -> return dat

-- Using a row table input and column data, make a table
makeTable :: String -> Vars -> TableData -> (Either InterException Table)
makeTable _ vs []   = return $ Table vs []
makeTable name vs rows | length vs > length cols = throw $ IETooManyVars name
                       | length vs < length cols = throw $ IENotEnoughVars name
                       | otherwise               = return $ Table vs rows
                       where cols = transpose rows

-----------------------------------------------------------------
-- ExQual
-----------------------------------------------------------------

addBoundVariables :: Vars -> Env -> (InterReturn Env)
addBoundVariables []     env = return env
addBoundVariables (v:vs) env = case (find (== v) (boundVars env)) of
  Nothing ->  case (find (== v) (columnVars (tableState env))) of
                Nothing -> let newEnv = env {boundVars = (v:(boundVars env))} in
                           addBoundVariables vs newEnv
                Just _  -> throw $ IEVarExistsNotBound v
  Just _  ->  throw $ IEVarAlreadyBound v

-----------------------------------------------------------------
-- Table Output 
-----------------------------------------------------------------

-- Gets the requested columns in the requested order, sorting rows lexicographically
makeOutputTable :: Vars -> Vars -> RowTable -> (InterReturn TableData) 
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

-- Format table as a csv string
table2csv :: TableData -> Maybe String
table2csv [[]] = Nothing
table2csv xs   = Just  $ unlines (map (intercalate ",") xs)
 
-- Print table in csv form as a string                  
printTable :: TableData -> IO ()
printTable t = let res = table2csv t in
  case res of
    Nothing -> return ()
    Just dat -> putStr dat

-----------------------------------------------------------------
-- Table Importer
-----------------------------------------------------------------

-- Imports a csv file into zipped columns
importTable :: FilePath -> IO (InterReturn TableData)
importTable f = do 
  res <- try $ readFile f :: IO (Either IOError String)
  case res of
    Left e -> throwIO (IEReadError e)
    Right dat -> do
      let tokenise = map parseCSVLine . lines
      return (pure (tokenise dat))              
                 
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
removeDupCols :: RowTable -> RowTable
removeDupCols rowTab = (Table newIds newRows)
                     where ids = columnVars rowTab
                           rows = tableData rowTab
                           newRows = [b| a<- rows, let b = removeDupCols' ids a ]
                           newIds = nub ids
                           
removeDupCols' :: Vars -> Row -> Row
removeDupCols' [] [] = []
removeDupCols' (i:is) (r:rs) 
  | elem i is = removeDupCols' is rs
  | otherwise = [r] ++ removeDupCols' is rs

-- Transpose but fails if lists are not all equal length
transpose' :: [[a]] -> InterReturn [[a]]
transpose' xss | allLensSame xss = return (transpose xss)
transpose' xss | otherwise       = throw IEUnequalLists

-- Get length of all rows, true if all equal
allLensSame :: [[a]] -> Bool
allLensSame xss = allValsSame $ map length xss

-- True if all values in list are equal 
allValsSame :: Eq a =>[a] -> Bool
allValsSame xs = all (== head xs) (tail xs)

-----------------------------------------------------------------
-- Interpreter Exceptions
-----------------------------------------------------------------
data InterException = IEImport InterException 
                    | IEQuery InterException
                    | IEUnequalLists
                    | IEReadError IOError
                    | IETableNotFound TableID
                    | IETableAlreadyDefined TableID
                    | IETooManyVars String
                    | IENotEnoughVars String
                    | IEVarNotFound Var
                    | IEVarAlreadyBound Var
                    | IEVarExistsNotBound Var
                    | IEVarNotInOutput Var
                    | IEOutputVarBound Var
                    | IEOutputVarNotExist Var
                    | IEUnknown
                    deriving (Show)

-- Exceptions are tracked inside LHS so throw is syntactic sugar
-- for left assignment
throw :: InterException -> Either InterException b
throw x = Left x
throwIO :: InterException -> IO (Either InterException b)
throwIO x = return $ Left x
