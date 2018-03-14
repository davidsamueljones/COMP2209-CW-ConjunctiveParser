{
module Grammar where
import Tokens
import Control.Exception
}

%name runParser
%tokentype { Token }

-- Connect Alex Lexer to Happy Parser 
%monad { Parser } { (>>=) } { return }
%lexer { (\x -> alexMonadScan' >>= x) } { Token _ TEOF }
%error { parseError }

%token
    '::'    { Token _ TAssign}
    ';'     { Token _ TSemicolon}
    ','     { Token _ TComma}
    '.'     { Token _ TDot}
    '=='    { Token _ TEqual}
    '!='    { Token _ TNotEqual}
    '^'     { Token _ TConjunction}
    '<-'    { Token _ TLeftArrow}
    '('     { Token _ TLParenthesis}
    ')'     { Token _ TRParenthesis}
    '$'     { Token _ TExQual}
    import  { Token _ TImport}
    as      { Token _ TAs}
    print   { Token _ TPrint}
    -- We do not use $$ notation to keep token data, unfortunate consequence is mappings
    -- start to look a bit messy
    VAR     { Token _ (TVar _)}
    TABLE   { Token _ (TTable _)}
    STRING  { Token _ (TString _)}

%nonassoc '.'
%left '^'
%%

-- Parser start: expect 0+ imports and 0+ queries
Prog     : Imports Stmts                        { Prog $1 $2 }

-- Store imports as a list
Imports  : Import ';' Imports                  { $1:$3 } 
         | {- empty -}                         { [] }
Import   : import STRING as TABLE              { (Import (tkSVal $2) (tkSVal $4)) }

-- Store statements as a list
Stmts    : Stmt ';' Stmts                      { $1:$3 }
         | {- empty -}                         { [] }
Stmt     : TABLE '::' Vars '<-' Exp            { (Query (Just (tkSVal $1)) $3 $5) }
         | Vars '<-' Exp                       { (Query (Nothing) $1 $3) }
         | print TABLE                         { (PrintTable  (tkSVal $2)) }
         | print STRING                        { (PrintString (tkSVal $2)) }

-- Store variables as a list
Vars     : VAR MoreVars                        { (tkSVal $1):$2 }
         | {- empty -}                         { [] }
MoreVars : ',' VAR MoreVars                    { (tkSVal $2):$3 }
         | {- empty -}                         { [] }

-- Use tree structure for expressions
Exp      : TABLE '(' Vars ')'                  { Lookup (tkSVal $1) $3 }
         | VAR '==' VAR                        { Equality (tkSVal $1) (tkSVal $3) }
         | VAR '!=' VAR                        { NotEquality (tkSVal $1) (tkSVal $3) }
         | '$' Vars '.' Exp                    { eqFlatten (ExQual $2 $4) }
         | Exp '^' Exp                         { Conjunction $1 $3 }

{

-----------------------------------------------------------------
-- Parser Control
-----------------------------------------------------------------

-- This parser uses an Alex user so share its state monad 
type Parser a = Alex a

-- Run lexer and parser together
runLexAndParse :: String -> Either String Prog
runLexAndParse s = do 
     let res = runAlex s (runParser)
     case res of
        Left err -> Left err
        Right prog -> return prog

-- Output for parsing errors
parseError :: Token -> Parser a
parseError tk = do
  (AlexPn _ l c) <- getCurAlexPos
  makeParseError $ "ERROR" ++ show tk ++ (show (l, c, "NO MESSSAGE")) -- TODO

makeParseError = alexError

-----------------------------------------------------------------
-- Parsing Helper Functions
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

getPos :: Token -> XY
getPos tk = let (AlexPn _ x y) = tkPos tk in (x, y)

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

type XY      = (Int, Int)
type Path    = String
type TableID = String 
type Vars    = [ Var ] 
type Var     = String

type Imports = [ Import ]
data Import  = Import Path TableID
               deriving (Eq, Show)
 
type Stmts = [ Stmt ]
data Stmt = Query (Maybe TableID) Vars Exp
          | PrintTable TableID
          | PrintString String
          deriving (Eq, Show)
      
data Exp = Conjunction Exp Exp
         | Equality Var Var
         | NotEquality Var Var
         | Lookup TableID Vars
         | ExQual Vars Exp            
         deriving (Eq, Show)

}