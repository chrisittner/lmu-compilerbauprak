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
import Debug.Trace


munchExp ::  (MachineSpecifics m a f)=> B.Exp -> WriterT [X86Assem] m Operand

munchExp (B.CONST a) = return (Imm a)

munchExp (B.BINOP op a b) = do 
	t <- nextTemp
	let eax = mkNamedTemp "eax"
	a <- munchExp a
	b <- munchExp b
	let instr = case op of
											B.PLUS -> [OPER2 MOV (Reg t)  a, OPER2 ADD (Reg t)  b]
											B.MINUS -> [OPER2 MOV (Reg t) a, OPER2 SUB (Reg t) b]
											B.MUL -> [OPER2 MOV (Reg eax) a, OPER1 IMUL b, OPER2 MOV (Reg t) (Reg eax)]
											B.DIV -> [OPER2 MOV (Reg eax) a, OPER1 IDIV b, OPER2 MOV (Reg t) (Reg eax)]
											B.AND -> [OPER2 MOV (Reg t) a, OPER2 AND (Reg t) b]
											B.OR -> [OPER2 MOV (Reg t) a, OPER2 OR (Reg t) b]
											B.LSHIFT -> [OPER2 MOV (Reg t) a, OPER2 SAL (Reg t) b]
											B.RSHIFT -> [OPER2 MOV (Reg t) a, OPER2 SAR (Reg t) b]
{- 											B.ARSHIFT -> [OPER2 SHR (Reg t) b] -}
											B.XOR -> [OPER2 XOR (Reg t) b]
	tell instr
	return $ Reg t

-- munchExp (B.NAME a) =

munchExp (B.TEMP a) = do
	let t = mkNamedTemp $ show a
	return (Reg t)

munchExp (B.MEM a) = do -- wirklich notwendig das in ein Register zu laden? können asmbefehle nicht auf beliebigen Operanden arbeiten?
	t <- nextTemp
	(Reg a) <- munchExp a
	tell [OPER2  MOV (Reg t) (Mem (Just a) 0 Nothing)]
	return (Reg t) 
{-munchExp (B.MEM a) = return $ Mem (Just a) 0 Nothing-}


munchStm :: (MachineSpecifics m a f)=> B.Stm -> WriterT [X86Assem] m ()

munchStm (B.SEQ stm1 stm2) = munchStm stm1 >> munchStm stm2

munchStm (B.EXP (B.CALL f args)) = do -- was passiert mit den argumenten?
	f <- munchExp f
	args <- mapM munchExp args
	tell [CALL f []]
munchStm (B.MOVE (B.TEMP a) (B.CALL f args)) = do -- was passiert mit den argumenten und woher kriegen wir den rückgabewert?
	f <- munchExp f
	args <- mapM munchExp args
	tell [CALL f []]

munchStm (B.EXP exp) = munchExp exp >> return ()

munchStm (B.MOVE dest src) = do 
	dest <- munchExp dest
	src <- munchExp src
	tell [OPER2 MOV dest src]

munchStm (B.JUMP (B.NAME lab) poss) = do
	let dest = mkLabel $ labelName lab
	tell [JMP dest]

munchStm (B.CJUMP rel e1 e2 trueLab falseLab) = do
	e1 <- munchExp e1
	e2 <- munchExp e2
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
	let lab = mkLabel $ labelName trueLab
	tell [J rel' lab]

munchStm (B.LABEL lab) = do
	let lab' = mkLabel $ labelName lab
	tell [LABEL lab']

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





