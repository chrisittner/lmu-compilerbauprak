module MJTokens where

data Token p = 
      TInt p
    | TBool  p
    | TString p
    | LBrace  p
    | RBrace  p
    | LBrack  p
    | RBrack  p
    | LPar  p
    | RPar  p
    | If  p
--    | Then  p
    | Else p
    | While p
    | Print  p
    | PrintLn p
    | This p
    | Length p
    | New p 
    | Char p
    | Return p
    | Class p
    | Extends p
    | Public p
    | Static p
    | Void p
    | Main p
    | Assign  p -- =
    | OpAnd  p -- &&
    | OpEq  p -- ==
    | OpLt  p -- <
    | OpPlus  p -- +
    | OpMinus  p -- -
    | OpTimes  p -- *
    | OpDivide  p -- /
    | OpNeg  p -- !
--    | OpPoint p -- 
    | CTrue  p 
    | CFalse  p
    | Comma  p
    | Point p
    | Semicol  p
    | IVal Int p -- Integerwert, z.b. 343
    | SVal String p  -- ein String mit anführungszeichen, z.B. "jnk" ## --  "\"" (.*) # ([^\\][\"])   [^\\][\"]
    | Id String  p -- Identifier, z.b. jnlk
 deriving (Eq,Read,Show)

filterPn :: String -> String
filterPn [] = []
filterPn (' ':'(':'P':'n': s) = filterPn (tail $ dropWhile (/= ')') s)
filterPn (c:cs) = c:filterPn cs

token_pos :: (Show p) => Token p -> p
token_pos (IVal _ p) = p
token_pos (Id _  p) = p
token_pos (SVal _ p) = p
token_pos (TString p) = p
token_pos (TInt p) = p
token_pos (TBool  p) = p
token_pos (LBrace  p) = p
token_pos (RBrace  p) = p
token_pos (LBrack  p) = p
token_pos (RBrack  p) = p
token_pos (LPar  p) = p
token_pos (RPar  p) = p
token_pos (If  p) = p
token_pos (Else p) = p
token_pos (While p) = p
token_pos (Print  p) = p
token_pos (PrintLn p) = p
token_pos (This p) = p
token_pos (Length p) = p
token_pos (New p) = p
token_pos (Char p) = p
token_pos (Return p) = p
token_pos (Class p) = p
token_pos (Extends p) = p
token_pos (Public p) = p
token_pos (Static p) = p
token_pos (Void p) = p
token_pos (Main p) = p
token_pos (Assign  p) = p
token_pos (OpAnd  p) = p
token_pos (OpEq  p) = p
token_pos (OpLt  p) = p
token_pos (OpPlus  p) = p
token_pos (OpMinus  p) = p
token_pos (OpTimes  p) = p
token_pos (OpNeg p) = p
token_pos (OpDivide  p) = p
-- token_pos (OpPoint  p) = p
token_pos (CTrue  p) = p
token_pos (CFalse  p) = p
token_pos (Comma  p) = p
token_pos (Point  p) = p
token_pos (Semicol  p) = p


