{
module Tokens where
}

%wrapper "monad"
$digit = [0-9]
$alpha = [a-zA-Z]
$lower = [a-z]
$upper = [A-Z]

tokens :-
  <0> $white+           ;
  <0> \/\*.*\*\/        ;
  <0> \/\/.*            ;
  <0> \;                { lexT TSemicolon }
  <0> \,                { lexT TComma }
  <0> \.                { lexT TDot }
  <0> \=                { lexT TEqual }  
  <0> \^                { lexT TConjunction }
  <0> \<\-              { lexT TLeftArrow }
  <0> \(                { lexT TLParenthesis }
  <0> \)                { lexT TRParenthesis } 
  <0> \"                { lexT TQuote } -- "
  <0> \$                { lexT TExQual }
  <0> import            { lexT TImport }
  <0> as                { lexT TAs }
  <0> $lower\w*         { lexT' TVar }
  <0> $upper+           { lexT' TTable }
  -- TODO: At the moment requires stripping of quotes, not ideal
  -- Implement states to deal with this
  <0> \"[^\"]+\"         { lexT' TString }  -- "

{


-----------------------------------------------------------------
-- Lexing Helper Functions
-----------------------------------------------------------------

-- Get Token from a monad wrapper input
lexT :: TokenClass -> AlexAction Token
lexT t (p, _, _, _) _ = return (Token p t)

-- Get Token (\w String) from a monad wrapper input
lexT' :: (String -> TokenClass) -> AlexAction Token
lexT' t i l = lexT (t $ getMatch i l) i l

-- EOF definition defined so EOF token uses our Token type
alexEOF :: Alex Token
alexEOF = do (p, _, _, _) <- alexGetInput
             return (Token p TEOF)

-- Extract matched string
getMatch :: AlexInput -> Int -> String
getMatch (_, _, _, s) l =  take l s


-----------------------------------------------------------------
-- Lexing Control
-----------------------------------------------------------------

-- Tokenise string using monad lexer
lexString :: String -> Either String [Token]
lexString str = runAlex str alexMonadScanAll

-- Recursively call alexMonadScan building up a list of tokens
alexMonadScanAll :: Alex [Token]
alexMonadScanAll = do
  -- Scan control loop
  let loop ts = do t@(Token p c) <- alexMonadScan; 
                   if c == TEOF then 
                     return (reverse ts) 
                   else 
                     loop (t:ts)
  -- Start loop 
  loop []

-----------------------------------------------------------------
-- Datatypes
-----------------------------------------------------------------

-- Wrapper for TokenClass that holds position 
data Token      = Token AlexPosn TokenClass
                  deriving (Eq, Show)

-- Types of token that can be lexed
data TokenClass = TSemicolon        
                | TComma             
                | TDot             
                | TEqual            
                | TConjunction       
                | TLeftArrow         
                | TLParenthesis     
                | TRParenthesis      
                | TQuote          
                | TExQual           
                | TImport        
                | TAs               
                | TVar String     
                | TTable String    
                | TString String   
                | TEOF
                  deriving (Eq, Show)
  
}

-- Helpful Links:
-- https://www.haskell.org/alex/doc/html/wrappers.html
-- https://github.com/dagit/happy-plus-alex/tree/master/src
-- http://hackage.haskell.org/package/tamarin-prover-0.4.1.0/src/interactive-only-src/Lexer.x
-- https://github.com/simonmar/alex/blob/master/examples/haskell.x

-- Notes:
-- (AlexAction Token) == (AlexInput -> Int -> Alex Token)
