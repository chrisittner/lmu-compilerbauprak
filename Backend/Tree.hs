-- Abstract Syntax for Tree Intermediate Language

-- has unbounded labels and temps (registers)
-- abstracts over
-- * run-time library calls (new, print)
-- * access to parameters, locals, and fields
-- * calls to methods
-- it is sufficient to leave memory access abstract

module Backend.Tree where

import Prelude hiding (EQ,GT,LT)
import Data.Monoid

import Backend.Names

data Exp
  = CONST { value :: Int }
  | NAME  { lab :: Label }
  | TEMP  { temp :: Temp }
  | BINOP { op :: BinOp, left :: Exp, right :: Exp }
  | MEM   { memaddr :: Exp }
  | CALL  { func :: Exp, args :: [Exp] }
  | ESEQ  { stm :: Stm, resExp :: Exp }
 deriving (Eq,Show)

data Stm
  = MOVE  { dest :: Exp, src :: Exp }
  | EXP   { exp  :: Exp }
  | JUMP  { dest :: Exp, poss :: [Label] }
  | CJUMP { rel :: RelOp, leftE :: Exp, rightE :: Exp, trueLab :: Label, falseLab :: Label }
  | SEQ   { first :: Stm, second :: Stm }
  | LABEL { label :: Label }
  | NOP
 deriving (Eq,Show)

data BinOp
  = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
 deriving (Eq,Show)

data RelOp
  = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE
 deriving (Eq,Show)

neg :: RelOp -> RelOp
neg EQ = NE
neg NE = EQ
neg LT = GE
neg GT = LE
neg LE = GT
neg GE = LT
neg ULT = UGE
neg UGT = ULE
neg ULE = UGT
neg UGE = ULT

-- swapping lhs and rhs of comparison
swap :: RelOp -> RelOp
swap EQ = EQ
swap NE = NE
swap LT = GT
swap GT = LT
swap LE = GE
swap GE = LE
swap ULT = UGT
swap UGT = ULT
swap ULE = UGE
swap UGE = ULE

data Func = Func String [String] Exp

-- lempty = LABEL "L_exception_Stm_empty"

instance Monoid Stm where
    mempty = NOP
    mappend s1 s2 | s1 == NOP = s2
                  | s2 == NOP = s1
                  | otherwise = s1 `SEQ` s2

sseq :: [Stm] -> Stm
sseq = mconcat
unsseq :: Stm -> [Stm]
unsseq (SEQ first second) = (unsseq first) ++ (unsseq second)
unsseq x = [x]
{-
sseq [s] = s
sseq (s:ss) = SEQ s (sseq ss)
-}

jump :: Label -> Stm
jump l = JUMP (NAME l) [l]

scale :: Exp -> Int -> Exp
scale (CONST i) j = CONST (i * j)
scale e j = BINOP MUL e (CONST j)

isTemp :: Exp -> Bool
isTemp (TEMP _) = True
isTemp _ = False

-- notTemp :: Exp -> Bool
-- notTemp = not . isTemp

-- an expression which does not have subexpressions is a leaf
isLeaf :: Exp -> Bool
isLeaf (CONST _) = True
isLeaf (TEMP _) = True
isLeaf (NAME _) = True
isLeaf _ = False


----------------------------------------------------------------------
-- Sums
----------------------------------------------------------------------

-- in a sum, the const part is always first
-- a difference is only at the very outside
-- normal forms:  
--  i + e1 + ... + en   (ei not constants, sums, or differences)
--  (i + es) - es'      (es a pure sum as above)

plus :: Exp -> Exp -> Exp

-- first is a const
plus (CONST i) (CONST j) = CONST $ i + j
plus (CONST 0) e = e
plus (CONST i) (BINOP PLUS (CONST j) e) = plus (CONST $ i+j) e
plus (CONST i) (BINOP MINUS (CONST j) e) = minus (CONST $ i+j) e

-- second is a const
plus e (CONST i) = plus (CONST i) e

-- both are const + x
plus (BINOP PLUS (CONST i) e) (BINOP PLUS (CONST j) e') =
   (CONST $ i + j) `plus` (e `plus` e')

{- permutations are only sound for pure expressions, so these are disabled:

-- first is a difference
plus (BINOP MINUS e1 e2) e' = BINOP MINUS (plus e1 e') e2

-- second is a difference
plus e' (BINOP MINUS e1 e2) = plus (BINOP MINUS e1 e2) e'

-}

-- default case
plus e e' = BINOP PLUS e e'


minus :: Exp -> Exp -> Exp

-- subtracting a constant
minus e (CONST i) = plus (CONST (-i)) e

minus (CONST i) (BINOP PLUS (CONST j) e) = minus (CONST (i-j)) e
minus (BINOP PLUS (CONST i) e) (BINOP PLUS (CONST j) e') =
  minus (plus (CONST (i-j)) e) e'

{- unsound for effects:

-- one exp is a difference
minus e (BINOP MINUS e1 e2) = (plus e2 e) `minus` e1  
minus (BINOP MINUS e1 e2) e = minus e1 (plus e2 e)

-}

minus e e' = BINOP MINUS e e'



times :: Exp -> Exp -> Exp
times (CONST i) (CONST j) = CONST $ i * j
times e e' = BINOP MUL e e'

divide :: Exp -> Exp -> Exp
divide (CONST i) (CONST j) = CONST $  i `div` j
divide e e' = BINOP DIV e e'

and :: Exp -> Exp -> Exp
and (CONST 0) e = CONST 0
and e (CONST 0) = CONST 0
and (CONST 1) (CONST 1) = CONST 1 
and e e' = BINOP AND e e'


-- TODO: 
-- integrate SHL
-- think about normal forms for products/shifts 
-- think about normal forms for boolean expressions
-- optimize e < e' + 1 to e <= e'



