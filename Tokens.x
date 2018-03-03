{
module Tokens where
}

%wrapper "monadUserState"

$digit    = [0-9]
$lower    = [a-z]
$upper    = [A-Z]
$alpha    = [$lower$upper]
$alphanum = [$lower$upper$digit]

tokens :-
  <0>    $white+           ;
  <0>    \/\*(.|\r?\n)*\*\/;
  <0>    \/\/.*            ;
  <0>    \;                { lexT  TSemicolon }
  <0>    \,                { lexT  TComma }
  <0>    \.                { lexT  TDot }
  <0>    \=                { lexT  TEqual }  
  <0>    \^                { lexT  TConjunction }
  <0>    \<\-              { lexT  TLeftArrow }
  <0>    \(                { lexT  TLParenthesis }
  <0>    \)                { lexT  TRParenthesis } 
  <0>    \$                { lexT  TExQual }
  <0>    import            { lexT  TImport }
  <0>    as                { lexT  TAs }
  <0>    $lower$alphanum*  { lexT' TVar }
  <0>    $upper+           { lexT' TTable }
  
  -- String handling
  <0>     \"               { beginString  } -- "
  <strSC> \\[\"]           { escapeString } -- "
  <strSC> \"               { finishString } -- "
  <strSC> .                { storeString  }  
  
{

-----------------------------------------------------------------
-- Lexing Helper Functions
-----------------------------------------------------------------
-- Alex :: (AlexState -> Either String (AlexState, a)) -> Alex a
-- AlexAction Token :: (AlexInput -> Int -> Alex Token)

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
-- State Control
-----------------------------------------------------------------
-- TODO: Does it matter that EOF errors do not occur when inside quotes? Can fix with SC check at alexScanUser 
-- but requires massive copying of generated code so not ideal.
-- TODO: Handle multiline comments properly?


inital_sc :: Int
inital_sc = 0;

beginString :: AlexAction Token
beginString i l = do
  alexSetStartCode strSC
  setLexerStringValue ""
  skip i l

escapeString :: AlexAction Token
escapeString i l = do
  let s = getMatch i l
  case s of
    "\\\"" -> addCharToLexerStringValue '"'
  skip i l

storeString :: AlexAction Token
storeString i l = do
  addCharToLexerStringValue (head $ getMatch i l)
  skip i l

finishString :: AlexAction Token
finishString i l = do
  alexSetStartCode inital_sc
  s <- getLexerStringValue
  lexT (TString $ reverse s) i l


alexInitUserState  :: AlexUserState
alexInitUserState   = AlexUserState
                    {
                      lexerCommentDepth  = 0,
                      lexerStringValue   = ""
                    }

-- FROM WRAPPER SITE (LINKS AT THE BOTTOM) [Currently witchcraft]
-- getLexerStringValue :: Alex String
-- getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

-- setLexerStringValue :: String -> Alex ()
-- setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

-- addCharToLexerStringValue :: Char -> Alex ()
-- addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

-- REWRITTEN [No longer witchcraft]
getLexerStringValue :: Alex String
getLexerStringValue = do
  ust <- alexGetUserState
  return (lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = do
  ust <- alexGetUserState
  alexSetUserState ust {lexerStringValue = ss}

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = do
  ust <- alexGetUserState
  let newString = c:(lexerStringValue ust)
  alexSetUserState ust {lexerStringValue = newString}


-----------------------------------------------------------------
-- Lexing Control
-----------------------------------------------------------------

-- Tokenise string using monad lexer if return data type is Left, 
-- holds error message, if is Right holds tokens
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
-- Lexing Datatypes
-----------------------------------------------------------------

data AlexUserState  = AlexUserState
                    {
                      lexerCommentDepth  :: Int,
                      lexerStringValue   :: String
                    }
                    deriving (Eq, Show)
 
-----------------------------------------------------------------
-- User Datatypes
-----------------------------------------------------------------

-- Wrapper for TokenClass that holds position 
data Token  = Token
            {
              tPosition :: AlexPosn,
              tClass    :: TokenClass
            }
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
                | TExQual           
                | TImport        
                | TAs               
                | TVar String     
                | TTable String    
                | TString String   
                | TEOF
                  deriving (Eq, Show)
  
}

-- Helpful Links (collation of random sites used):
-- https://www.haskell.org/alex/doc/html/wrappers.html
-- https://github.com/dagit/happy-plus-alex/tree/master/src
-- http://hackage.haskell.org/package/tamarin-prover-0.4.1.0/src/interactive-only-src/Lexer.x
-- https://github.com/simonmar/alex/blob/master/examples/haskell.x
-- https://github.com/alanz/Alex
-- https://leanpub.com/alexandhappy/read#leanpub-auto-monadic-parsers-and-lexers
-- https://github.com/jmoy/alexhappy/blob/master/startcode/Lexer.x
-- https://raw.githubusercontent.com/simonmar/alex/master/examples/tiger.x
-- http://lpaste.net/raw/119212
-- https://stackoverflow.com/questions/20315739/how-to-use-an-alex-monadic-lexer-with-happy
-- https://stackoverflow.com/questions/31996489/what-causes-happy-to-throw-a-parse-error
-- https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html

