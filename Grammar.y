{
module Grammar where
import Tokens
import Control.Exception
}

%name parseCalc
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

-- TODO: Add associativity
%left
%right
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
Exp      : Exp '^' Exp                         { Conjunction $1 $3 }
         | VAR '=' VAR                         { Equality $1 $3 }
         | TABLE '(' Vars ')'                  { Lookup $1 $3 }
         | '$' VAR '.' Exp                     { ExQual $2 $4 }

{

-----------------------------------------------------------------
-- Error Handling
-----------------------------------------------------------------

data ParseException = ParseException AlexPosn deriving Show
instance Exception ParseException

parseError :: [Token] -> a
parseError p = throw (ParseException (getAlexPosn (head p)))

getAlexPosn :: Token -> AlexPosn
getAlexPosn (Token a b) = a

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
         | ExQual Var Exp            
         deriving (Eq, Show)

}