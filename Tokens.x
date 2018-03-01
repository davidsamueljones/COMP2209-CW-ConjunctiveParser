{
module Tokens where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]
$variable = [a-z](\w)*
$lower = [a-z]
$upper = [A-Z]
$filepath = [^"]+

tokens :-
$white+;
  \/\*.*\*\/        ;
  \;                ;
  \/\/.*            ;
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
  as                { tok (\p a -> TAs p) }
  
  $filepath         { tok (\p s -> TFilePath p s) }
  $variable         { tok (\p s -> TVar p s) }
  $upper+           { tok (\p s -> TTable p s) }
  
{
tok f p s = f p s

data Token =
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
    TFilePath AlexPosn String   |
    TVar AlexPosn String        |
    TTable AlexPosn String
    deriving (Eq,Show)
    
}