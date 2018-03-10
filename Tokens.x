{
module Tokens where
}

%wrapper "monadUserState"

$digit    = [0-9]
$lower    = [a-z]
$upper    = [A-Z]
$alpha    = [$lower$upper]
$alphanum = [$lower$upper$digit]
$uppernum = [$upper$digit]
$lowernum = [$lower$digit]

tokens :-
  <0>    $white+                ;
  <0>    \/\*(.|\r?\n)*\*\/     ;
  <0>    \/\/.*                 ;
  <0>    \;                     { lexT  TSemicolon }
  <0>    \,                     { lexT  TComma }
  <0>    \.                     { lexT  TDot }
  <0>    \=                     { lexT  TEqual }  
  <0>    \^                     { lexT  TConjunction }
  <0>    \<\-                   { lexT  TLeftArrow }
  <0>    \(                     { lexT  TLParenthesis }
  <0>    \)                     { lexT  TRParenthesis } 
  <0>    \$                     { lexT  TExQual }
  <0>    import                 { lexT  TImport }
  <0>    as                     { lexT  TAs }
  <0>    $lower($alphanum|\_)*  { lexT' TVar }
  <0>    $upper($uppernum|\_)*  { lexT' TTable }
  
  -- String handling
  <0>     \"                    { beginString  } -- "
  <strSC> \\[\"]                { escapeString } -- "
  <strSC> \"                    { finishString } -- "
  <strSC> .                     { storeString  }  
  
{

-----------------------------------------------------------------
-- Generic Lexing Functions
-----------------------------------------------------------------

-- Get Token from a monad wrapper input
lexT :: TokenClass -> AlexAction Token
lexT t (p, _, _, _) _ = return (Token p t)

-- Get Token (\w String) from a monad wrapper input
lexT' :: (String -> TokenClass) -> AlexAction Token
lexT' t i l = lexT (t $ getMatch i l) i l

-- EOF definition defined so EOF token uses our Token type
-- Also used to verify that EOF is expected in lexer's current state
alexEOF :: Alex Token
alexEOF = do  
  (p, _, _, _) <- alexGetInput
  sc <- alexGetStartCode
  -- Check if lexer state expects an EOF
  (sc == strSC) ? (alexError' LEStringEOF "") $
  -- EOF okay
    return (Token p TEOF)            

-- Extract matched string
getMatch :: AlexInput -> Int -> String
getMatch (_, _, _, s) l =  take l s

-----------------------------------------------------------------
-- User State Control Functions
-----------------------------------------------------------------
-- TODO: Handle multiline comments properly?

-- Change the start code to indicate inside a string [No Token]
beginString :: AlexAction Token
beginString i l = do
  alexSetStartCode strSC
  setLexerStringValue ""
  skip i l

-- Handle escape characters behaviour [No Token]
escapeString :: AlexAction Token
escapeString i l = do
  let s = getMatch i l
  case s of
    "\\\"" -> addCharToLexerStringValue '"'
  skip i l

-- Stores a string to the state's string buffer a character at a time [No Token]
storeString :: AlexAction Token
storeString i l = do 
  addStringToLexerStringValue $ getMatch i l
  skip i l 

-- Reset the start code and store the current string buffer to a String Token 
-- using normal lexing functions (buffer is reversed)
finishString :: AlexAction Token
finishString i l = do
  alexSetStartCode initialSC
  s <- getLexerStringValue
  lexT (TString $ reverse s) i l

-----------------------------------------------------------------
-- Generic State Control Functions
-----------------------------------------------------------------

-- Identifier for initial start code
initialSC :: Int
initialSC = 0;

-- Configure the initial user state of the lexer (called by generated code)
alexInitUserState  :: AlexUserState
alexInitUserState   = AlexUserState
                    {
                      lexerCommentDepth  = 0,
                      lexerStringValue   = ""
                    }

-- Get the value stored in the lexer state's string buffer
getLexerStringValue :: Alex String
getLexerStringValue = do
  ust <- alexGetUserState
  return (lexerStringValue ust)

-- Set the string buffer in the lexer state to the specified value
setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = do
  ust <- alexGetUserState
  alexSetUserState ust {lexerStringValue = ss}

--Add a string to the lexer state's string buffer (at front in reverse)
addStringToLexerStringValue :: String -> Alex () 
addStringToLexerStringValue [] = noStateChange
addStringToLexerStringValue (s:ss) = do
  addCharToLexerStringValue s
  addStringToLexerStringValue ss

-- Add a single character to the lexer state's string buffer (at front)
addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = do
  ust <- alexGetUserState
  let ss = c:(lexerStringValue ust)
  setLexerStringValue ss

-- Leave state alone by setting it to itself
noStateChange :: Alex ()
noStateChange = alexGetUserState >>= alexSetUserState 

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
  let loop ts = do t@(Token p c) <- alexMonadScan'; 
                   if c == TEOF then 
                     return (reverse ts) 
                   else 
                     loop (t:ts)
  -- Start loop 
  loop []




-- TODO: Can we redirect everything but AlexError to the original generation?
-- Custom monad scan to make use of improved error handling
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError _ -> alexError' LENotToken ""
    AlexSkip  inp' _len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- TODO: Actually improve error messages, currently just structure
-- Custom error handling, displaying more specific error messages
alexError' :: LexerErrorType -> String -> Alex a
alexError' t m = do 
  (p, c, bs, s) <- alexGetInput
  sc <- alexGetStartCode
  us <- alexGetUserState
  case t of
    LEStringEOF -> alexError $ "EOF STRING " ++ readAlexPos p ++ (show us)
    LENotToken  -> alexError $ "Lexical error at " ++ readAlexPos p ++ (show s)


readAlexPos :: AlexPosn -> String
readAlexPos (AlexPn a l c) = (show l) ++ ":" ++ (show c)

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
type Tokens = [Token]
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


data LexerErrorType = LEStringEOF
                    | LENotToken
                    deriving (Eq, Show)
-----------------------------------------------------------------
-- Haskell Utilities
-----------------------------------------------------------------

-- Functional infix if statement
(?) :: Bool -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

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

