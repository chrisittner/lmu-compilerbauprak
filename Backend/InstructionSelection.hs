module Backend.InstructionSelection where

import Backend.Tree as B
import Backend.Names
import Backend.MachineSpecifics
import Backend.DummyMachine
import Backend.Cmm
import Control.Monad.Trans.Writer.Strict


munchExp :: (MachineSpecifics m a f)=> B.Exp -> WriterT [a] m Operand

munchExp (B.CONST a) = return (Imm a)

munchExp (B.BINOP op a b) = do 
	t <- nextTemp
	eax <- mkNamedTemp "eax"
	a <- munchExp a
	b <- munchExp b
	let instr = case op of B.PLUS -> [OPER2 MOV (Reg t) (Reg a), OPER2 ADD (Reg t) (Reg b)]
											| B.MINUS -> [OPER2 MOV (Reg t) (Reg a), OPER2 SUB (Reg t) (Reg b)]
											| B.MUL -> [OPER2 MOV (Reg eax) (Reg a), OPER2 IMUL (Reg b), OPER2 MOV (Reg t) (Reg eax)]
											| B.DIV -> [OPER2 MOV (Reg eax) (Reg a), OPER2 IDIV (Reg b), OPER2 MOV (Reg t) (Reg eax)]
											| B.AND -> [OPER2 MOV (Reg t) (Reg a), OPER2 AND (Reg t) (Reg b)]
											| B.OR -> [OPER2 MOV (Reg t) (Reg a), OPER2 OR (Reg t) (Reg b)]
											| B.LSHIFT -> [OPER2 MOV (Reg t) (Reg a), OPER2 SAL (Reg t) (Reg b)]
											| B.RSHIFT -> [OPER2 MOV (Reg t) (Reg a), OPER2 SAR (Reg t) (Reg b)]
{- 											| B.ARSHIFT -> [OPER2 SHR (Reg t) (Reg b)] -}
											| B.XOR -> [OPER2 XOR (Reg t) (Reg b)]
	tell instr
	return $ Reg t

-- munchExp (B.NAME a) =

munchExp (B.TEMP a) = do
	t <- mkNamedTemp a
	return (Reg t)

{-munchExp (B.MEM a) = do -- wirklich notwendig das in ein Register zu laden? können asmbefehle nicht auf beliebigen Operanden arbeiten?
	t <- nextTemp
	tell [OPER2  MOV (Reg t) (Mem (Just a) 0 Nothing)]
	return (Reg t) -}
munchExp (B.MEM a) = return $ Mem (Just a) 0 Nothing



munchStm :: (MachineSpecifics m a f)=> B.Stm -> WriterT [a] m ()

munchStm (B.SEQ stm1 stm2) = munchStm stm1 >> munchStm stm2

munchStm (B.EXP (B.CALL f args)) = do -- ?
	f <- munchExp f
	args <- mapM munchExp args
munchStm (B.MOVE (B.TEMP a) (B.CALL f args)) = do -- ?
	f <- munchExp f
	args <- mapM munchExp args
	

munchStm (B.EXP exp) = munchExp exp >> return ()

munchStm (B.MOVE dest src) = do 
	dest <- munchExp dest
	src <- munchExp src
	tell [OPER2 MOV dest src]

munchStm (B.JUMP (B.NAME lab) poss) = do
	dest <- mkNamedLabel lab
	tell [JMP dest]

munchStm (B.CJUMP rel e1 e2 trueLab falseLab) = do
	e1 <- munchExp e1
	e2 <- munchExp e2
	tell [OPER2 CMP e1 e2]
	let rel' = case rel of B.EQ -> E 
									| B.NE -> NE
									| B.LT -> L
									| B.GT -> G
									| B.LE -> LE
									| B.GE -> GE
									| B.ULT -> L
									| B.ULE -> LE
									| B.UGT -> G
									| B.UGE -> GE
	lab <- mkNamedLabel trueLab
	tell [J rel' lab]

munchStm (B.LABEL lab) = do
	lab <- mkNamedLabel lab
	tell [LABEL lab]

munchStm (B.NOP) = tell [NOP]








instance (Monad m) => MachineSpecifics (DummyMachineT m) X86Assem X86Frame where
  wordSize = return 4
  mkFrame name nparams =
    do paramTemps <- replicateM nparams nextTemp 
       returnTemp <- nextTemp
       return $ DummyFrame name paramTemps [] returnTemp
  codeGen (FragmentProc f b) = ... -- #########
  allRegisters = return Nothing
  generalPurposeRegisters = return Nothing
  -- Der Typ Assem is leer, also sind folgende Definitionen sinnvoll:
  spill frame body temps = return (frame, [])
  printAssembly frags = return ""








