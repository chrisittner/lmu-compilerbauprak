{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Backend.InstructionSelection where

import Backend.Names
import Backend.MachineSpecifics
import Backend.X86Assem
import qualified Backend.Tree as B
import Control.Monad.Trans.Writer.Strict
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans

munchExp :: (MachineSpecifics m a f)=> B.Exp -> WriterT [X86Assem] m Operand

munchExp (B.CONST a) = return (Imm a)

munchExp (B.BINOP op a b) = do 
	t <- nextTemp
	let eax = mkNamedTemp "%eax"
	a <- munchExp a
	b <- munchExp b
	a <- bufferIfTwoMems b a
	let instr = case op of
		B.PLUS -> [OPER2 MOV (Reg t)  a, OPER2 ADD (Reg t)  b]
		B.MINUS -> [OPER2 MOV (Reg t) a, OPER2 SUB (Reg t) b]
		B.MUL -> [OPER2 MOV (Reg eax) a, OPER2 MOV (Reg t) b, OPER1 IMUL (Reg t), OPER2 MOV (Reg t) (Reg eax)]
		B.DIV -> [OPER2 MOV (Reg eax) a, OPER2 MOV (Reg t) b, OPER1 IDIV (Reg t), OPER2 MOV (Reg t) (Reg eax)]
		B.AND -> [OPER2 MOV (Reg t) a, OPER2 AND (Reg t) b]
		B.OR -> [OPER2 MOV (Reg t) a, OPER2 OR (Reg t) b]
		B.LSHIFT -> [OPER2 MOV (Reg t) a, OPER2 SAL (Reg t) b]
		B.RSHIFT -> [OPER2 MOV (Reg t) a, OPER2 SAR (Reg t) b]
{-		B.ARSHIFT -> [OPER2 SHR (Reg t) b] -- not implemented -}
		B.XOR -> [OPER2 XOR (Reg t) b]
	tell instr
	return $ Reg t

munchExp (B.TEMP a) = do
	let t = mkNamedTemp $ show a
	return (Reg t)

munchExp (B.MEM (B.BINOP B.MINUS (B.TEMP t) (B.CONST n))) = return $ Mem (Just t) 0 Nothing (Just (negate n))
munchExp (B.MEM (B.BINOP B.PLUS (B.TEMP t) (B.CONST n))) = return $ Mem (Just t) 0 Nothing (Just n)
munchExp (B.MEM (B.BINOP B.PLUS (B.CONST n) (B.TEMP t))) = return $ Mem (Just t) 0 Nothing (Just n)

munchExp (B.MEM exp) = do
	exp <- munchExp exp
	temp <- case exp of
		(Reg t) -> return t
		(Imm t) -> do
			tmp <- nextTemp
			tell [OPER2 MOV (Reg tmp) (Imm t)]
			return tmp
		m@(Mem _ _ _ _) -> do 
			tmp <- nextTemp
			tell [OPER2 MOV (Reg tmp) m]
			return tmp
	return $ Mem (Just temp) 0 Nothing Nothing





munchStm :: (MachineSpecifics m a f)=> B.Stm -> WriterT [X86Assem] m ()

munchStm (B.SEQ stm1 stm2) = munchStm stm1 >> munchStm stm2

munchStm (B.EXP (B.CALL (B.NAME lab) args)) = do
	let esp = mkNamedTemp "%esp"
	let eax = mkNamedTemp "%eax"
	args <- mapM munchExp args
	args <- mapM bufferMem args
	tell $ map (OPER1 PUSH) (reverse args) -- push args in reversed order
	tell [CALL lab]
	tell [OPER2 ADD (Reg esp) (Imm (4*(length args)))] -- reset stack pointer

munchStm (B.MOVE dest (B.CALL (B.NAME lab) args)) = do
	let esp = mkNamedTemp "%esp"
	let eax = mkNamedTemp "%eax"
	dest <- munchExp dest
	args <- mapM munchExp args
	args <- mapM bufferMem args
	tell $ map (OPER1 PUSH) (reverse args) -- push args in reversed order
	tell [CALL lab]
	tell [OPER2 ADD (Reg esp) (Imm (4*(length args)))] -- reset stack pointer
	tell [OPER2 MOV dest (Reg eax)]

munchStm (B.EXP exp) = munchExp exp >> return ()

munchStm (B.MOVE dest src) = do 
	dest <- munchExp dest
	src <- munchExp src
	src <- bufferIfTwoMems dest src
	tell [OPER2 MOV dest src]

munchStm (B.JUMP (B.NAME lab) poss) = do
	tell [JMP lab]

munchStm (B.CJUMP rel e1 e2 trueLab falseLab) = do
	e1 <- munchExp e1
	e2 <- munchExp e2
	e2 <- bufferIfTwoMems e1 e2
	tell [OPER2 CMP e1 e2]
	let rel' = case rel of 
		B.EQ -> E
		B.NE -> NE
		B.LT -> L
		B.GT -> G
		B.LE -> LE
		B.GE -> GE
{-		B.ULT -> L
		B.ULE -> LE
		B.UGT -> G
		B.UGE -> GE -- not implemented -}
	tell [J rel' trueLab]

munchStm (B.LABEL lab) = do
	tell [LABEL lab]

munchStm (B.NOP) = tell [OPER0 NOP]






-- helpers

-- if both paramters are Mems, buffer the second one into a (Reg tmp), o/w return second
bufferIfTwoMems :: (MachineSpecifics m a f) => Operand -> Operand ->  WriterT [X86Assem] m Operand
bufferIfTwoMems (Mem _ _ _ _) m@(Mem _ _ _ _) = do 
	t <- nextTemp
	tell [OPER2 MOV (Reg t) m]
	return $ Reg t
bufferIfTwoMems _ m = return m

-- replaces a Mem by (Reg tmp), tmp containing the calculated memory address (usig LEA)
bufferMem :: (MachineSpecifics m a f) => Operand ->  WriterT [X86Assem] m Operand
bufferMem m@(Mem _ _ _ _) = do
	t <- nextTemp
	tell [OPER2 LEA (Reg t) m]
	return $ Reg t
bufferMem op = return op
