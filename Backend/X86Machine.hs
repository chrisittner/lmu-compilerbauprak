{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleContexts #-}
module Backend.X86Machine where
import Backend.Names
import Backend.X86Assem
import Backend.Tree
import Backend.InstructionSelection
import Backend.MachineSpecifics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Writer.Strict
import Data.List
import Debug.Trace

data X86Frame = X86Frame { fname :: String, numParams :: Int, temps :: [Temp], numMemoryLocals :: Int } deriving Show

instance Frame X86Frame where
  name f = fname f
  params f = [ MEM (BINOP PLUS (TEMP ebp) (CONST (8 + 4*n))) | n <- [0..((numParams f)-1)] ] -- Params liegen bei x86 an (%ebp+8)+n*4
  size f = numMemoryLocals f
  allocLocal f Anywhere = do 
        t <- nextTemp 
        return (X86Frame (fname f) (numParams f) (t:(temps f)) (numMemoryLocals f),
                TEMP t)
  allocLocal f InMemory = return (X86Frame (fname f) (numParams f) (temps f) ((numMemoryLocals f)+1),
                MEM (BINOP MINUS (TEMP ebp) (CONST $ 4 * numMemoryLocals f)) )
  makeProc f body returnExp = return $ SEQ body $ MOVE (TEMP eax) returnExp



newtype X86MachineT m a = X86MachineT { runX86MachineT :: NameGenT m a }
   deriving (Monad, MonadNameGen, MonadTrans)

withX86Machine :: Monad m => X86MachineT m a -> m a
withX86Machine = runNameGenT . runX86MachineT

instance (Monad m) => MachineSpecifics (X86MachineT m) X86Assem X86Frame where
  wordSize = return 4
  mkFrame name nparams = return $ X86Frame name nparams [] 0
  allRegisters = return [eax, ebx, ecx, edx, esi, edi, esp, ebp]
  generalPurposeRegisters = return [eax, ebx, ecx, edx, esi, edi]

  codeGen (FragmentProc f b) = do 
  	assemlist <- execWriterT $ munchStm (sseq b)
  	bufferTemps <- replicateM 3 nextTemp
  	let bufferCalleeSaves = [ OPER2 MOV (Reg $ bufferTemps!!0) (Reg ebx),
  		OPER2 MOV (Reg $ bufferTemps!!1) (Reg edi),
  		OPER2 MOV (Reg $ bufferTemps!!2) (Reg esi) ]
  	let restoreCalleeSaves = [ OPER2 MOV (Reg ebx) (Reg $ bufferTemps!!0),
  		OPER2 MOV (Reg edi) (Reg $ bufferTemps!!1),
  		OPER2 MOV (Reg esi) (Reg $ bufferTemps!!2), 
  		OPER2 MOV (Reg esp) (Reg ebp),
  		OPER1 POP (Reg ebp),
  		OPER0 RET ]
  	return $ FragmentProc f (bufferCalleeSaves ++ assemlist ++ restoreCalleeSaves) 


--spill :: f -> [a] -> [Temp] -> m (f, [a])
  spill f assems temps | trace ("spill:\n" ++ show f ++ "\n" ++ show assems ++ "\n" ++ show temps ++ "\n\n") False = undefined {-%%%-}
  spill f assems temps = foldM (\ (frame,instrs) temp -> spillOne frame instrs temp) (f,assems) temps

  printAssembly fragments = return $ ".intel_syntax\n.global main\n\n" ++ (concat $ map (\ f -> fraglabel f ++ prolog f ++ functioncode f ++ epilog f ++ "\n") fragments) where
  	fraglabel :: Fragment X86Frame [X86Assem] -> String
  	fraglabel (FragmentProc f _) = name f ++ ":\n"
  	prolog :: Fragment X86Frame [X86Assem] -> String
  	prolog (FragmentProc frame instrs) = showAssems [ OPER1 PUSH (Reg ebp), OPER2 MOV (Reg ebp) (Reg esp), OPER2 SUB (Reg esp) (Imm (size frame)) ]
  	functioncode :: Fragment X86Frame [X86Assem] -> String
  	functioncode (FragmentProc _ instrs) = showAssems instrs
  	epilog :: Fragment X86Frame [X86Assem] -> String
  	epilog (FragmentProc frame instrs) = showAssems [ ] -- bereits in codeGen angehängt (um die calleeSaves zu retten)
  	showAssems :: [X86Assem] -> String
  	showAssems instrs = concat $ map (\ instr -> (show instr ++ "\n")) instrs



-- Moves single Temp to Memory: Replaces occurrences with a new temp, loads the value from memory before use, saves it to memory after defs
-- example: "add t4 1" -> ["mov t42 [ebp+12]", "add t42 1", "mov [ebp+12] t42"] (also returns the updated frame)
spillOne :: (MachineSpecifics m X86Assem f) => f -> [X86Assem] -> Temp -> m (f, [X86Assem])
spillOne f assems temp = do
	(f', newLocal) <- allocLocal f InMemory
	(newLocal, _) <- runWriterT $ munchExp newLocal -- munchExp schreibt hier nie in die Monade, übersetzt nur die Speicheraddresse
	newTemp <- nextTemp
	let assems' = mapM (tempToMemory temp newLocal newTemp) assems
	return (f', concat assems') where 
		tempToMemory :: Temp -> Operand -> Temp -> X86Assem -> [X86Assem]
		tempToMemory temp newLocal newTemp instr = loadTemp ++ [newInstr] ++ saveTemp where
			newInstr = rename instr (\ t -> if t==temp then newTemp else t)
			loadTemp = if temp `elem` (use instr) then [(OPER2 MOV (Reg newTemp) newLocal)] else []
			saveTemp = if temp `elem` (def instr) then [(OPER2 MOV newLocal (Reg newTemp))] else []



