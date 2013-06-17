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
import Control.Monad.Trans

canonicalize :: (MachineSpecifics m a f)=> Fragment f Stm -> m (Fragment f [Stm])
canonicalize (FragmentProc f stms) = do
	stms <- canonicalizeStm stms
	(blocks, label) <- blockalize stms
	stms <- trace (blocks, label)
	return $ FragmentProc f stms

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
	tell =<< lift (canonicalizeStm stm)
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










blockalize :: (MachineSpecifics m a f) => [Stm] -> m ([[Stm]], Maybe Label)

blockalize stms = do
	--assume stms to be nonempty
	let stms = foldr addJumps [last stms]  (init stms) -- add JUMPS before LABELs if neccassary
	let stms = foldl removeDeadCode [head stms] (tail stms)
	stms <- labelizeFirstBlock stms
	(stms, label) <- jumpalizeLastBlock stms
	let blocks = foldl blockalize' [] stms
	return (blocks, label)

blockalize' :: [[Stm]] -> Stm -> [[Stm]]
blockalize' blocks stm@(LABEL _) = [stm]:blocks
blockalize' (firstblock:blocks) stm = (stm:firstblock) : blocks

labelizeFirstBlock :: (MachineSpecifics m a f) => [Stm] -> m [Stm]
labelizeFirstBlock s@((LABEL _):_) = return s
labelizeFirstBlock s = do
	t <- nextLabel
	return $ (LABEL t):s

jumpalizeLastBlock :: (MachineSpecifics m a f) => [Stm] -> m ([Stm], Maybe Label)
jumpalizeLastBlock s = jumpalizeLastBlock' (reverse s)
jumpalizeLastBlock' :: (MachineSpecifics m a f) => [Stm] -> m ([Stm], Maybe Label)
jumpalizeLastBlock' s@((JUMP _ _):_) = return (s, Nothing)
jumpalizeLastBlock' s@((CJUMP _ _ _ _ _):_) = return (s, Nothing)
jumpalizeLastBlock' s = do
	l <- nextLabel
	return ((jump l):s, Just l)

addJumps :: Stm -> [Stm] -> [Stm]
addJumps stm@(JUMP _ _) acc@((LABEL _):_) = stm:acc
addJumps stm@(CJUMP _ _ _ _ _) acc@((LABEL _):_) = stm:acc
addJumps stm acc@((LABEL l):_) = stm : (jump l) : acc
addJumps stm acc = stm:acc

removeDeadCode :: [Stm] -> Stm -> [Stm]
removeDeadCode acc@((JUMP _ _):_) stm@(LABEL _) = stm:acc
removeDeadCode acc@((JUMP _ _):_) _ = acc
removeDeadCode acc@((CJUMP _ _ _ _ _):_) stm@(LABEL _) = stm:acc
removeDeadCode acc@((CJUMP _ _ _ _ _):_) _ = acc
removeDeadCode acc stm = stm:acc

trace :: (MachineSpecifics m a f) => ([[Stm]], Maybe Label) -> m [Stm]
trace _ = return [NOP]














