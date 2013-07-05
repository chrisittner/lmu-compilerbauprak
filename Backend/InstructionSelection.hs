{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Backend.InstructionSelection where

import Backend.Names
import Backend.MachineSpecifics
import Backend.DummyMachine
import Backend.X86Machine
import qualified Backend.Tree as B
import Control.Monad.Trans.Writer.Strict
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans

munchExp ::  (MachineSpecifics m a f)=> B.Exp -> WriterT [X86Assem] m Operand

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
{-		B.ARSHIFT -> [OPER2 SHR (Reg t) b] -}
		B.XOR -> [OPER2 XOR (Reg t) b]
	tell instr
	return $ Reg t

munchExp (B.TEMP a) = do
	let t = mkNamedTemp $ show a
	return (Reg t)

munchExp (B.MEM exp) = do
	exp <- munchExp exp
	temp <- case exp of
		(Reg t) -> return t
		(Imm t) -> do
			tmp <- nextTemp
			tell [OPER2 MOV (Reg tmp) (Imm t)]
			return tmp
		m@(Mem _ _ _) -> do 
			tmp <- nextTemp
			tell [OPER2 MOV (Reg tmp) m]
			return tmp
	return $ Mem (Just temp) 0 Nothing





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
		B.ULT -> L
		B.ULE -> LE
		B.UGT -> G
		B.UGE -> GE
	tell [J rel' trueLab]

munchStm (B.LABEL lab) = do
	tell [LABEL lab]

munchStm (B.NOP) = tell [OPER0 NOP]





newtype X86MachineT m a = X86MachineT { runX86MachineT :: NameGenT m a }
   deriving (Monad, MonadNameGen, MonadTrans)

withX86Machine :: Monad m => X86MachineT m a -> m a
withX86Machine = runNameGenT . runX86MachineT

instance (Monad m) => MachineSpecifics (X86MachineT m) X86Assem DummyFrame where
  wordSize = return 4
  mkFrame name nparams =
    do paramTemps <- replicateM nparams nextTemp 
       returnTemp <- nextTemp
       return $ DummyFrame name paramTemps [] returnTemp
  codeGen (FragmentProc f b) = do 
  	assemlist <- execWriterT $ munchStm (B.sseq b)
  	return $ FragmentProc f assemlist
  allRegisters = return Nothing
  generalPurposeRegisters = return Nothing
  -- Der Typ Assem is leer, also sind folgende Definitionen sinnvoll:
  spill frame body temps = return (frame, [])
  printAssembly frags = return ""






-- helpers

-- if both paramters are Mems, buffer the second one into a (Reg tmp), o/w return second
bufferIfTwoMems :: (MachineSpecifics m a f) => Operand -> Operand ->  WriterT [X86Assem] m Operand
bufferIfTwoMems (Mem _ _ _) m@(Mem _ _ _) = do 
	t <- nextTemp
	tell [OPER2 MOV (Reg t) m]
	return $ Reg t
bufferIfTwoMems _ m = return m

-- replaces a Mem by (Reg tmp), tmp containing the calculated memory address (usig LEA)
bufferMem :: (MachineSpecifics m a f) => Operand ->  WriterT [X86Assem] m Operand
bufferMem m@(Mem _ _ _) = do 
	t <- nextTemp
	tell [OPER2 LEA (Reg t) m]
	return $ Reg t
bufferMem op = return op






