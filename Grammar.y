{
module Grammar where
import Tokens
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
    var     { Token _ (TVar $$)}
    table   { Token _ (TTable $$)}
    string  { Token _ (TString $$)}
 --   TEOF    { Token _ TEOF}


%%
Import: import string as table ';' Import   {Import $2 $4 $6}
      | Query                               {Query' $1}
      |                                     {ImportEmpty}
      
Query: Args '<-' Exp                    {Query $1 $3}
      
Args: var ',' Args                      {Multiple $1 $3}
    | var                               {Single $1}
    |                                   {ArgsEmpty}

Exp: Exp '^' Exp                        {Conjunction $1 $3}
   | var '=' var                        {Equality $1 $3}
   | table '(' Args ')'                 {LookupIn $1 $3}
   | '$' var '.' Exp                    {ExQual $2 $4}
{

parseError :: [Token] -> a
parseError p = error ("Parse error at location :" ++ show (getAlexPosn (head p))) --TODO: make nicer

getAlexPosn :: Token -> AlexPosn
getAlexPosn (Token a b) = a

         
data Import = Import String String Import          --TString TTable 
            | Query' Query
            | ImportEmpty 
            deriving Show
            
data Query = Query Args Exp
            deriving Show

data Args = Multiple String Args                    --TVar Args 
          | Single String                           --TVar 
          | ArgsEmpty 
          deriving Show
          
data Exp = Conjunction Exp Exp      
         | Equality String String                   --TVar TVar
         | LookupIn String Args                     --TTable Args
         | ExQual String Exp                        --TVar Exp
         deriving Show
}