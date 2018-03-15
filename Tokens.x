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
  -- Whitespace handling
  <0>       $white+               ;

  -- Multi-line comment handling  
  <0>       \/\*                   { addCommentLayer } -- base case
  <commSC>  \/\*                   { addCommentLayer }
  <commSC>  \*\/                   { removeCommentLayer }
  <commSC>  (.|\r?\n)              ; -- Ignore character/new line
  
  -- Single line comment handling 
  <0>       \/\/.*                 ; -- Ignore line
  
  -- String handling
  <0>       \"                     { beginString  } -- "
  <strSC>   \\[\"n]                { escapeString } -- "
  <strSC>   \"                     { finishString } -- "
  <strSC>   (\r?\n)                ; -- Ignore line breaks
  <strSC>   .                      { storeString  }  

  -- Language handling
  <0>       \:\:                   { lexT  TAssign}
  <0>       \;                     { lexT  TSemicolon }
  <0>       \,                     { lexT  TComma }
  <0>       \.                     { lexT  TDot }
  <0>       \=\=                   { lexT  TEqual }
  <0>       \!\=                   { lexT  TNotEqual }    
  <0>       \^                     { lexT  TConjunction }
  <0>       \<\-                   { lexT  TLeftArrow }
  <0>       \(                     { lexT  TLParenthesis }
  <0>       \)                     { lexT  TRParenthesis } 
  <0>       \$                     { lexT  TExQual }
  <0>       import                 { lexT  TImport }
  <0>       as                     { lexT  TAs }
  <0>       print                  { lexT  TPrint }
  <0>       $lower($alphanum|\_)*  { lexT' TVar }
  <0>       $upper($uppernum|\_)*  { lexT' TTable }
  
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
  (sc == commSC)   ? (alexError' LECommentEOF "") $
    (sc == strSC) ? (alexError' LEStringEOF "") $
  -- EOF okay
    return (Token p TEOF)            

-- Extract matched string
getMatch :: AlexInput -> Int -> String
getMatch (_, _, _, s) l =  take l s

-----------------------------------------------------------------
-- Generic State Control Functions
-----------------------------------------------------------------

-- Identifier for normal start code
baseSC :: Int
baseSC = 0

-- Configure the initial user state of the lexer (called by generated code)
alexInitUserState  :: AlexUserState
alexInitUserState   = AlexUserState
                    {
                      lexerCommentDepth  = 0,
                      lexerStringValue   = ""
                    }

-- Leave state alone by setting it to itself
noStateChange :: Alex ()
noStateChange = alexGetUserState >>= alexSetUserState 

-- Get current position of lexer from lexer state
getCurAlexPos :: Alex AlexPosn
getCurAlexPos = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

-----------------------------------------------------------------
-- Comment State Control Functions
-----------------------------------------------------------------

-- Increment value of comment depth, setting start code respectively
addCommentLayer :: AlexAction Token
addCommentLayer i l = do
  changeLexerCommentDepth 1
  begin commSC i l

-- Decrement value of comment depth, setting start code respectively
removeCommentLayer :: AlexAction Token
removeCommentLayer i l = do
  changeLexerCommentDepth (-1)
  d <- getLexerCommentDepth
  case d of
    0 -> begin baseSC i l
    _ -> skip i l

-- Change the comment depth value stored in the lexer state by the given amount
changeLexerCommentDepth :: Int -> Alex ()
changeLexerCommentDepth x = do
  cur <- getLexerCommentDepth
  let new = cur + x
  setLexerCommentDepth new

-- Get the value stored in the lexer state's comment depth indicator
getLexerCommentDepth :: Alex Int
getLexerCommentDepth = do
  ust <- alexGetUserState
  return (lexerCommentDepth ust)

-- Set the comment depth in the lexer state to the specified value
setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth x = do
  ust <- alexGetUserState
  alexSetUserState ust {lexerCommentDepth = x}

-----------------------------------------------------------------
-- String State Control Functions
-----------------------------------------------------------------

-- Change the start code to indicate inside a string [No Token]
beginString :: AlexAction Token
beginString i l = do
  setLexerStringValue ""
  begin strSC i l

-- Handle escape characters behaviour [No Token]
escapeString :: AlexAction Token
escapeString i l = do
  let s = getMatch i l
  case s of
    "\\\"" -> addCharToLexerStringValue '"'
    "\\n"  -> addCharToLexerStringValue '\n'
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
  alexSetStartCode baseSC
  s <- getLexerStringValue
  lexT (TString $ reverse s) i l

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

-- Custom error handling, displaying more specific error messages
alexError' :: LexerErrorType -> String -> Alex a
alexError' t m = do 
  (p, c, bs, s) <- alexGetInput
  sc <- alexGetStartCode
  us <- alexGetUserState
  case t of
    LECommentEOF -> alexError $ "Lexing Error : Multi-line comment not terminated before EOF (Depth " 
        ++ (show (lexerCommentDepth us)) ++ ")"
    LEStringEOF -> alexError $ "Lexing Error : String not terminated before EOF"
    LENotToken  -> alexError $ "Lexing Error " ++ readAlexPos p ++ ": Unrecognised token" 

readAlexPos :: AlexPosn -> String
readAlexPos (AlexPn a l c) = show (l, c)

-----------------------------------------------------------------
-- Lexer Datatypes
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
              tkPos   :: AlexPosn,
              tkClass :: TokenClass
            }
            deriving (Eq, Show)

-- Helper function to getvalue of token
tkSVal :: Token -> String
tkSVal (Token _ (TVar    v)) = v
tkSVal (Token _ (TTable  v)) = v
tkSVal (Token _ (TString v)) = v
tkSVal _                     = ""

-- Types of token that can be lexed
data TokenClass = TAssign
                | TSemicolon        
                | TComma             
                | TDot             
                | TEqual
                | TNotEqual            
                | TConjunction       
                | TLeftArrow         
                | TLParenthesis     
                | TRParenthesis       
                | TExQual           
                | TImport        
                | TAs
                | TPrint               
                | TVar String     
                | TTable String    
                | TString String   
                | TEOF
                  deriving (Eq, Show)

data LexerErrorType = LEStringEOF
                    | LECommentEOF
                    | LENotToken
                    deriving (Eq, Show)

-----------------------------------------------------------------
-- Helper Utilities
-----------------------------------------------------------------

-- Functional infix if statement
(?) :: Bool -> a -> a -> a
(?) True x _ = x
(?) False _ y = y

}