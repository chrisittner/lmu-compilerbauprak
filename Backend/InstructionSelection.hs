module Backend.InstructionSelection where

import Backend.Tree
import Backend.Names
import Backend.MachineSpecifics
import Backend.DummyMachine
import Backend.Cmm
import Control.Monad.Trans.Writer.Strict


munchExp :: (MachineSpecifics m a f)=> Exp -> WriterT [a] m Temp

munchExp (CONST a) = do -- überhaupt nötig?
	temp <- nextTemp
	tell [ENTER (Reg temp) (Imm a)]
	return temp

munchExp (BINOP (CONST a) b) = do 
	temp <- nextTemp
	b <- munchExp b
	tell [MOV (Reg temp) (Imm a)]
	tell [ADD (Reg temp) (Reg b)]
	return temp

munchExp (BINOP b (CONST a)) = do 
	temp <- nextTemp
	b <- munchExp b
	tell [MOV (Reg temp) (Imm a)]
	tell [ADD (Reg temp) (Reg b)]
	return temp

munchExp (BINOP a b) = do 
	temp <- nextTemp
	a <- munchExp a
	b <- munchExp b
	tell [MOV (Reg temp) (Reg a)]
	tell [ADD (Reg temp) (Reg b)]
	return temp

munchExp (NAME a) = do
	lab <- mkNamedLabel a
	return lab

munchExp (TEMP a) = do
	temp <- mkNamedTemp a
	return temp

munchExp (MEM a) = do
	temp <- nextTemp
	tell [ MOV (Reg temp) (Mem (Just a) 0 Nothing) ]
	return temp

munchExp (CAll f args) = do --?
	temp <- nextTemp
	f <- munchExp f
	args <- mapM munchExp args
	tell [CALL (Reg f)]
	return temp




munchStm :: (MachineSpecifics m a f)=> Stm -> WriterT [a] m ()

munchStm (EXP (CALL f args)) =

munchStm (EXP exp) = munchExp exp >>= (\ _ -> return ())

munchStm (MOVE dest src) = do 
	dest <- munchExp dest
	src <- munchExp src
	tell [MOV (Reg dest) (Reg src)]

munchStm (JUMP dest poss) = tell [JMP dest] --?

munchStm (CJUMP rel e1 e2 trueLab falseLab) = do
	e1 <- munchExp e1
	e2 <- munchExp e2
	tell [CMP (Reg e1) (Reg e2)]
	let rel' = case rel of EQ -> E 
									| NE -> NE 
									| LT -> L
									| GT -> G
									| LE -> LE
									| GE -> GE
									| ULT -> L
									| ULE -> LE
									| UGT -> G
									| UGE -> GE
	tell [J rel' trueLab]

munchStm (LABEL lab) = do
	lab <- mkNamedLabel lab
	tell [LABEL Label]

munchStm (NOP) = tell [NOP]
	


{-
data Stm
  = MOVE  { dest :: Exp, src :: Exp }
  | EXP   { exp  :: Exp }
  | JUMP  { dest :: Exp, poss :: [Label] }
  | CJUMP { rel :: RelOp, leftE :: Exp, rightE :: Exp, trueLab :: Label, falseLab :: Label }
  | SEQ   { first :: Stm, second :: Stm }
  | LABEL { label :: Label }
  | NOP -}
