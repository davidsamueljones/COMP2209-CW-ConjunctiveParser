{
module Grammar where
import Tokens
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
Imports  : Import EOL Imports                  { $1:$3 } 
         | {- empty -}                         { [] }
Import   : import STRING as TABLE              { (Import (tkSVal $2) (tkSVal $4)       , getPos $1) }

-- Store statements as a list
Stmts    : Stmt EOL Stmts                      { $1:$3 }
         | {- empty -}                         { [] }
Stmt     : TABLE '::' Vars '<-' Exp            { (Query (Just (tkSVal $1)) (fst $3) $5 , getPos $1) }
         | Vars '<-' Exp                       { (Query Nothing (fst $1) $3            , getPos' $1 $2) }
         | print TABLE                         { (PrintTable  (tkSVal $2)              , getPos $1) }
         | print STRING                        { (PrintString (tkSVal $2)              , getPos $1) }

-- Store variables as a list
Vars     : VAR MoreVars                        { ((tkSVal $1):$2, getPos $1) }
         | {- empty -}                         { ([], noTokPos)  }
MoreVars : ',' VAR MoreVars                    { (tkSVal $2):$3 }
         | {- empty -}                         { [] }

-- Use tree structure for expressions
Exp      : TABLE '(' Vars ')'                  { (Lookup (tkSVal $1) (fst $3)          , getPos $1) }
         | VAR '==' VAR                        { (Equality (tkSVal $1) (tkSVal $3)     , getPos $1) }
         | VAR '!=' VAR                        { (NotEquality (tkSVal $1) (tkSVal $3)  , getPos $1) }
         | '$' Vars '.' Exp                    { (ExQual (fst $2) $4                   , getPos $1) }
         | Exp '^' Exp                         { (Conjunction $1 $3                    , snd $1) }

-- Optional end of line character
EOL      : ';'                                 { {- empty -} }
         | {- empty -}                         { {- empty -} }

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
  pos <- getCurAlexPos
  let es = errorStart pos
  let cla = tkClass tk
  case cla of
    (TAssign)       -> makeParseError $ es ++ "Incorrect use of '::' Correct use is TABLE_NAME :: VARIABLES <- QUERY"
    (TDot)          -> makeParseError $ es ++ "Incorrect use of '.' Correct use is $ VAR . EXPRESSION"
    (TSemicolon)    -> makeParseError $ es ++ "Incorrect use of ';' Correct use is at end of query or import"
    (TComma)        -> makeParseError $ es ++ "Incorrect use of ',' Correct use is inbetween Variables" 
    (TEqual)        -> makeParseError $ es ++ "Incorrect use of '==' Correct use is VAR == VAR" 
    (TNotEqual)     -> makeParseError $ es ++ "Incorrect use of '!=' Correct use is VAR != VAR"
    (TConjunction)  -> makeParseError $ es ++ "Incorrect use of '^' Correct use is EXPRESSION ^ EXPRESSION" 
    (TLeftArrow)    -> makeParseError $ es ++ "Incorrect use of '<-' Correct use is VARIABLES <- QUERY" 
    (TLParenthesis) -> makeParseError $ es ++ "Incorrect use of '(' Correct use is TABLE_NAME ( VARIABLES )" 
    (TRParenthesis) -> makeParseError $ es ++ "Incorrect use of ')' Correct use is TABLE_NAME ( VARIABLES )" 
    (TExQual)       -> makeParseError $ es ++ "Incorrect use of '$' Correct use is $ VAR . EXPRESSION" 
    (TImport)       -> makeParseError $ es ++ "Incorrect use of 'import' Correct use is import FILEPATH as TABLE_NAME;" 
    (TAs)           -> makeParseError $ es ++ "Incorrect use of 'as' Correct use is FILEPATH as TABLE_NAME;" 
    (TPrint)        -> makeParseError $ es ++ "Incorrect use of 'print' Correct use is print TABLE_NAME " 
    (TTable t)      -> makeParseError $ es ++ "Incorrect placement of Table '" ++ t ++ "'" 
    (TVar v)        -> makeParseError $ es ++ "Incorrect placement of Variable '" ++ v ++ "'"
    (TEOF)          -> makeParseError $ es ++ "Reached EOF whilst attempting to parse" 
    (t)             -> makeParseError $ es ++ "Incorrect use of token '" ++ show t ++ "'"

errorStart :: AlexPosn -> String
errorStart (AlexPn _ l c) = "Parse Error " ++ show (l, c) ++ ": "

makeParseError = alexError

-----------------------------------------------------------------
-- Parsing Helper Functions
-----------------------------------------------------------------

-- Get position of a token as XY coordinates 
getPos :: Token -> XY
getPos tk = let (AlexPn _ x y) = tkPos tk in (x, y)

-- Try and retrieve from existing position element, else get from backup 
getPos' :: (a, XY) -> Token -> XY
getPos' (_, xy) _  | xy /= noTokPos = xy 
getPos' _       tk = getPos tk

-- Position if no start position could be identified
noTokPos :: XY
noTokPos = (-1, -1)

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

type Imports = [ PImport ]
type PImport = (Import, XY)
data Import  = Import Path TableID
               deriving (Eq, Show)
 
type Stmts = [ PStmt ]
type PStmt = (Stmt, XY)
data Stmt = Query (Maybe TableID) Vars PExp
          | PrintTable TableID
          | PrintString String
          deriving (Eq, Show)
  
type PExp = (Exp, XY)    
data Exp = Conjunction PExp PExp
         | Equality Var Var
         | NotEquality Var Var
         | Lookup TableID Vars
         | ExQual Vars PExp            
         deriving (Eq, Show)

}