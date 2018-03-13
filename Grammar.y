{
module Grammar where
import Tokens
import Control.Exception
}

%name parseTokens
%tokentype { Token }
%error { parseError }
%token
    '::'    { Token _ TAssign}
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
    print   { Token _ TPrint}
    VAR     { Token _ (TVar $$)}
    TABLE   { Token _ (TTable $$)}
    STRING  { Token _ (TString $$)}

-- TODO: Check associativity
%nonassoc '.'
%left '^'
%%

-- Parser start: expect 0+ imports and 0+ queries
Prog     : Imports Stmts                        { Prog $1 $2 }

-- Store imports as a list
Imports  : Import ';' Imports                  { $1:$3 } 
         | {- empty -}                         { [] }
Import   : import STRING as TABLE              { (Import $2 $4) }

Stmts    : Stmt ';' Stmts                      { $1:$3 }
         | {- empty -}                         { [] }
Stmt     : TABLE '::' Vars '<-' Exp            { (Query (Just $1) $3 $5) }
         | Vars '<-' Exp                       { (Query (Nothing) $1 $3) }
         | print TABLE                         { (Print $2) }

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
             progStmts    :: Stmts
           }
          deriving (Eq, Show)

type Path    = String
type TableID = String 
type Vars    = [ Var ] 
type Var     = String

type Imports = [ Import ]
data Import  = Import Path TableID
               deriving (Eq, Show)
 
type Stmts = [ Stmt ]
data Stmt = Query (Maybe TableID) Vars Exp
          | Print TableID
          deriving (Eq, Show)
          
data Exp = Conjunction Exp Exp
         | Equality Var Var
         | Lookup TableID Vars
         | ExQual Vars Exp            
         deriving (Eq, Show)

}