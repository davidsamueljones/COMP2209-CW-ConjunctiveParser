{
module Grammar where
import Tokens
import Control.Exception
}

%name parseTokens
%tokentype { Token }
%error { parseError }
%token      
    ';'     { Token _ TSemicolon}
    ','     { Token _ TComma}
    '.'     { Token _ TDot}
    '='     { Token _ TEqual}
    '^'     { Token _ TConjunction}
    '<-'    { Token _ TLeftArrow}
    '('     { Token _ TLParenthesis}
    ')'     { Token _ TRParenthesis}
    '$'     { Token _ TExQual}
    import  { Token _ TImport}
    as      { Token _ TAs}
    VAR     { Token _ (TVar $$)}
    TABLE   { Token _ (TTable $$)}
    STRING  { Token _ (TString $$)}

-- TODO: Check associativity
%right '.' -- In what event does changing this even do anything?
%right '^' -- maybe left?
%%

-- Parser start: expect 0+ imports and 0+ queries
Prog     : Import Query                        { Prog $1 $2 }

-- Store imports as a list
Import   : import STRING as TABLE ';' Import   { (Import $2 $4):$6 }
         | {- empty -}                         { [] }

-- Store queries as a list
Query    : Vars '<-' Exp ';' Query             { (Query $1 $3):$5 }
         | {- empty -}                         { [] }

-- Store variables as a list
Vars     : VAR MoreVars                        { $1:$2 }
         | {- empty -}                         { [] }

MoreVars : ',' VAR MoreVars                    { $2:$3 }
         | {- empty -}                         { [] }

-- Use tree structure for expressions
Exp      : TABLE '(' Vars ')'                  { Lookup $1 $3 }
         | VAR '=' VAR                         { Equality $1 $3 }
         | '$' Vars '.' Exp                    { eqFlatten (ExQual $2 $4) }
         | Exp '^' Exp                         { Conjunction $1 $3 }

{

-----------------------------------------------------------------
-- Helper Functions
-----------------------------------------------------------------

-- Function to flatten vars from consecutive ExQual parse results 
-- into a single parse result simplifying AST
eqFlatten :: Exp -> Exp
eqFlatten e@(ExQual _ _) = (ExQual (eqVars e) (eqNext e))
  where
    eqVars :: Exp -> Vars
    eqVars (ExQual vs e@(ExQual _ _)) =  vs ++ eqVars e
    eqVars (ExQual vs _) = vs
    eqNext :: Exp -> Exp
    eqNext (ExQual vs e@(ExQual _ _)) = eqNext e
    eqNext (ExQual vs e) = e
eqFlatten e = e

-----------------------------------------------------------------
-- Error Handling
-----------------------------------------------------------------

-- Run with error handling
runParse :: Tokens -> IO (Either ParseException Prog)
runParse ts = try $ evaluate (parseTokens ts)

-- TODO: Are we sure we want to lose information with the exception
-- Possibly look into monadic approach
data ParseException = ParseException AlexPosn
instance Exception ParseException

-- TODO: Improve this error
instance Show ParseException where
  show (ParseException (AlexPn o l c)) = "Parse Error at line '" ++ show l ++ "', column '" ++ show c ++ "'"

-- FIXME: Head causes errors if reaches EOF trying to match
parseError :: Tokens -> a
parseError p = throw (ParseException (tPosition (head p))) 

-----------------------------------------------------------------
-- Parsed Datatypes
-----------------------------------------------------------------

-- Split program into imports and queries, storing separately 
data Prog  = Prog
           {
             progImports  :: Imports,
             progQueries  :: Queries
           }
          deriving (Eq, Show)

type Path  = String
type Table = String 
type Vars  = [ Var ] 
type Var   = String

type Imports = [ Import ]
data Import = Import Path Table
            deriving (Eq, Show)
 
type Queries = [ Query ]
data Query = Query Vars Exp
          deriving (Eq, Show)
          
data Exp = Conjunction Exp Exp
         | Equality Var Var
         | Lookup Table Vars
         | ExQual Vars Exp            
         deriving (Eq, Show)

}