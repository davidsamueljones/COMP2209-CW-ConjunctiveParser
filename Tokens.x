{
module Tokens where
}

%wrapper "posn"
$digit = [0-9]
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]

tokens :-
  $white+           ;
  \/\*.*\*\/        ;
  \/\/.*            ;
  \;                { tok (\p s -> TSemicolon p) }
  \,                { tok (\p s -> TComma p) }
  \.                { tok (\p s -> TDot p) }
  \=                { tok (\p s -> TEqual p) }  
  \^                { tok (\p s -> TConjunction p) }
  \<\-              { tok (\p s -> TLeftArrow p) }
  \(                { tok (\p s -> TLParenthesis p) }
  \)                { tok (\p s -> TRParenthesis p) }
  \"                { tok (\p s -> TQuote p) }
  \$                { tok (\p s -> TExQual p) }
  import            { tok (\p s -> TImport p) }
  as                { tok (\p s -> TAs p) }
  $lower\w*         { tok (\p s -> TVar p s) }
  $upper+           { tok (\p s -> TTable p s) }
  [^"]+             { tok (\p s -> TString p s) }
{

tok f p s = f p s

data Token =
    TSemicolon AlexPosn         |
    TComma AlexPosn             |
    TDot AlexPosn               |
    TEqual AlexPosn             |
    TConjunction AlexPosn       |
    TLeftArrow AlexPosn         |
    TLParenthesis AlexPosn      |
    TRParenthesis AlexPosn      |
    TQuote AlexPosn             |
    TExQual AlexPosn            |
    TImport AlexPosn            |
    TAs AlexPosn                |
    TVar AlexPosn String        |
    TTable AlexPosn String      |
    TString AlexPosn String     
    deriving (Eq,Show)
    
}