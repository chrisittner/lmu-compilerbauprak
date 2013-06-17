module Backend.Canonicalize where

import Prelude hiding (EQ,GT,LT)
import Backend.Tree
import Backend.Names
import Backend.MachineSpecifics
import Backend.DummyMachine
import Backend.Cmm
import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Identity



bufferExp :: (MachineSpecifics m a f)=> Exp -> WriterT [Stm] m Exp
bufferExp exp = case exp of 
		(CONST _) -> return exp
		(NAME _) -> return exp
		(TEMP _) -> return exp
		_ -> do 
			t <- nextTemp
			canonicalizeExp exp
			tell $ [MOVE (TEMP t) exp]
			return $ TEMP t

{- Ziel: CALL und ESEQ rausliften -}
canonicalizeExp :: (MachineSpecifics m a f) => Exp -> WriterT [Stm] m Exp

canonicalizeExp (BINOP op left right) = do
	left <- bufferExp left
	right <- canonicalizeExp right  -- bufferExp??
	return (BINOP op left right)
	
canonicalizeExp e@(CALL func args) = do
	t <- nextTemp
	tell $ [MOVE (TEMP t) e]
	return $ TEMP t
	
canonicalizeExp (ESEQ stm resExp) = do 
{-	mapM tell $ canonicalizeStm stm -}
	let stms = runIdentity . withDummyMachine $ canonicalizeStm stm
	tell stms
	canonicalizeExp resExp
	
canonicalizeExp (MEM addr) = do
	addr <- canonicalizeExp addr
	return $ MEM addr
	
canonicalizeExp x = return x


{-  = CONST { value :: Int }
  | NAME  { lab :: Label }
  | TEMP  { temp :: Temp }
  | BINOP { op :: BinOp, left :: Exp, right :: Exp }
  | MEM   { memaddr :: Exp }
  | CALL  { func :: Exp, args :: [Exp] }
  | ESEQ  { stm :: Stm, resExp :: Exp }
-}


canonicalizeStm :: (MachineSpecifics m a f) => Stm -> m [Stm]
canonicalizeStm (MOVE dest src) = do
	(dest, destStms) <- runWriterT $ canonicalizeExp dest
	(src, srcStms) <- runWriterT $ canonicalizeExp src
	return $ destStms ++ srcStms ++ [MOVE dest src] -- hier auch dest puffern?

canonicalizeStm (EXP exp) = do 
	(exp, stms) <- runWriterT $ canonicalizeExp exp
	return $ stms ++ [EXP exp]

canonicalizeStm s@(CJUMP rel leftE rightE trueLab falseLab) = do
	(leftE, leftStms) <- runWriterT $ canonicalizeExp leftE
	(rightE, rightStms) <- runWriterT $ canonicalizeExp rightE
	return $ leftStms ++ rightStms ++ [CJUMP rel leftE rightE trueLab falseLab]

canonicalizeStm s@(SEQ first second) = do
	tmp <- mapM  canonicalizeStm (unsseq s)
	return $ concat tmp

canonicalizeStm s = return [s]

{-  = MOVE  { dest :: Exp, src :: Exp }
  | EXP   { exp  :: Exp }
  | JUMP  { dest :: Exp, poss :: [Label] }
  | CJUMP { rel :: RelOp, leftE :: Exp, rightE :: Exp, trueLab :: Label, falseLab :: Label }
  | SEQ   { first :: Stm, second :: Stm }
  | LABEL { label :: Label }
  | NOP
-}
