module Backend.Canonicalize where

import Prelude hiding (EQ,GT,LT)
import Backend.Tree
import Backend.Names
import Backend.MachineSpecifics
import Control.Monad

{- Ziel: CALL und ESEQ rausliften -}
canonicalizeExp :: Exp -> ([Stm], Exp)
canonicalizeExp (BINOP op left right) =
canonicalizeExp (CALL func args) =
canonicalizeExp (ESEQ stm resExp) = case resExp of 
		(
		_ -> (canonicalizeStm stm, resExp) -- CONST NAME TEMP
canonicalizeExp (MEM addr) = MEM (canonicalizeExp addr) -- geht das?
canonicalizeExp x = x

  = CONST { value :: Int }
  | NAME  { lab :: Label }
  | TEMP  { temp :: Temp }
  | BINOP { op :: BinOp, left :: Exp, right :: Exp }
  | MEM   { memaddr :: Exp }
  | CALL  { func :: Exp, args :: [Exp] }
  | ESEQ  { stm :: Stm, resExp :: Exp }

canonicalizeStm :: Stm -> [Stm]
canonicalizeStm s@(MOVE dest src) =
canonicalizeStm s@(EXP exp) =
canonicalizeStm s@(JUMP dest poss) =
canonicalizeStm s@(CJUMP rel leftE rightE trueLab falseLab) =
canonicalizeStm s@(SEQ first second) = concat . map . canonicalizeStm $ unsseq s
canonicalizeStm s@(LABEL label) = s
canonicalizeStm s@NOP = s

  = MOVE  { dest :: Exp, src :: Exp }
  | EXP   { exp  :: Exp }
  | JUMP  { dest :: Exp, poss :: [Label] }
  | CJUMP { rel :: RelOp, leftE :: Exp, rightE :: Exp, trueLab :: Label, falseLab :: Label }
  | SEQ   { first :: Stm, second :: Stm }
  | LABEL { label :: Label }
  | NOP


