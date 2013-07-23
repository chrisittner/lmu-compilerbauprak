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
import Data.List

canonicalize :: (MachineSpecifics m a f)=> Fragment f Stm -> m (Fragment f [Stm])
canonicalize (FragmentProc f stms) = do
	stms <- canonicalizeStm stms
	(blocks, label) <- blockalize stms
	stms' <- trace (blocks, label)
	return $ FragmentProc f (stms')

bufferExp :: (MachineSpecifics m a f)=> Exp -> WriterT [Stm] m Exp
bufferExp exp = case exp of 
		(CONST _) -> return exp
		(NAME _) -> return exp
		(TEMP _) -> return exp
		_ -> do 
			t <- nextTemp
			exp <- canonicalizeExp exp
			tell $ [MOVE (TEMP t) exp]
			return $ TEMP t

{- Ziel: CALL und ESEQ rausliften -}
canonicalizeExp :: (MachineSpecifics m a f) => Exp -> WriterT [Stm] m Exp

canonicalizeExp (BINOP op left right) = do
	left <- bufferExp left
	right <- canonicalizeExp right
	return (BINOP op left right)
	
canonicalizeExp (CALL func args) = do
	args <- mapM canonicalizeExp args
	func <- canonicalizeExp func
	t <- nextTemp
	tell $ [MOVE (TEMP t) (CALL func args)]
	return $ TEMP t
	
canonicalizeExp (ESEQ stm resExp) = do 
	tell =<< lift (canonicalizeStm stm)
	canonicalizeExp resExp
	
canonicalizeExp (MEM addr) = do
	addr <- canonicalizeExp addr
	return $ MEM addr
	
canonicalizeExp x = return x



canonicalizeStm :: (MachineSpecifics m a f) => Stm -> m [Stm]

canonicalizeStm (MOVE dest src) = do
	(dest, destStms) <- runWriterT $ canonicalizeExp dest
	(src, srcStms) <- runWriterT $ canonicalizeExp src
	return $ destStms ++ srcStms ++ [MOVE dest src] -- hier auch dest puffern?

canonicalizeStm (EXP exp) = do 
	(exp, stms) <- runWriterT $ canonicalizeExp exp
	return $ stms ++ [EXP exp]

canonicalizeStm (CJUMP rel leftE rightE trueLab falseLab) = do
	(leftE, leftStms) <- runWriterT $ canonicalizeExp leftE
	(rightE, rightStms) <- runWriterT $ canonicalizeExp rightE
	return $ leftStms ++ rightStms ++ [CJUMP rel leftE rightE trueLab falseLab]

canonicalizeStm s@(SEQ first second) = do
	tmp <- mapM  canonicalizeStm (unsseq s)
	return $ concat tmp

canonicalizeStm s = return [s]




blockalize :: (MachineSpecifics m a f) => [Stm] -> m ([[Stm]], Maybe Label)

blockalize stms = do
	--assume stms to be nonempty
	let stms' = foldr addJumps [last stms]  (init stms) -- add JUMPS before LABELs if neccassary
	let stms'' = reverse $ foldl removeDeadCode [head stms] (tail stms')
	stms'' <- labelizeFirstBlock stms''
	(stms'', label) <- jumpalizeLastBlock stms''
	let blocks = map reverse (foldl blockalize' [] stms'')
	return (blocks, label)

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


labelizeFirstBlock :: (MachineSpecifics m a f) => [Stm] -> m [Stm]
labelizeFirstBlock stms@((LABEL _):_) = return stms
labelizeFirstBlock stms = do
	t <- nextLabel
	return $ (LABEL t):stms

jumpalizeLastBlock :: (MachineSpecifics m a f) => [Stm] -> m ([Stm], Maybe Label)
jumpalizeLastBlock stms = if isJump (last stms) then return (stms, Nothing) else do
	lab <- nextLabel
	return $ (stms ++ [jump lab], Just lab)

isJump :: Stm -> Bool
isJump (JUMP _ _) = True
isJump (CJUMP _ _ _ _ _) = True
isJump _ = False

blockalize' :: [[Stm]] -> Stm -> [[Stm]]
blockalize' blocks stm@(LABEL _) = [stm]:blocks
blockalize' (firstblock:blocks) stm = (stm:firstblock) : blocks



trace :: (MachineSpecifics m a f) => ([[Stm]], Maybe Label) -> m [Stm]
trace (blocks, Just endlabel) = trace' (reverse (last blocks)) (init blocks) >>= (\ stms -> return $ (reverse stms) ++ [LABEL endlabel])
trace (blocks, _) = trace' (reverse (last blocks)) (init blocks) >>= (\ stms -> return $ reverse stms)

trace':: (MachineSpecifics m a f) =>  [Stm] -> [[Stm]] -> m [Stm] -- tracedstms is backwards
trace' tracedstms@((JUMP (NAME lab) _):_) blocks = do
	let suitableBlock = foldl (hasLabel lab) [] blocks
	if null blocks then return tracedstms
	else	if length suitableBlock == 1
			then trace' ((reverse (head suitableBlock)) ++ tracedstms) (blocks \\ suitableBlock) -- ToMaybe: remove JUMP + following LABEL if possible (= Label not used anywhere else)
			else trace' ((reverse (head blocks)) ++ tracedstms) (tail blocks)

trace' tracedstms@((CJUMP cmp e1 e2 trueLabel falseLabel):_) blocks = do 
	let suitableBlock = foldl (hasLabel falseLabel) [] blocks
	let suitableBlock2 = foldl (hasLabel trueLabel) [] blocks
	if null blocks then return tracedstms 
	else if length suitableBlock == 1
			then trace' ((reverse (head suitableBlock)) ++ tracedstms) (blocks \\ suitableBlock)
			else if length suitableBlock2 == 1
					then trace' (reverse (head suitableBlock2) ++ ((CJUMP (neg cmp) e1 e2 falseLabel trueLabel):(tail tracedstms))) (blocks \\ suitableBlock2)
					else do 
						dummyLabel <- nextLabel
						let dummyBlock = [jump falseLabel, LABEL dummyLabel] -- Dummy Block (in reverse)
						trace' ((reverse (head blocks)) ++ dummyBlock ++ ((CJUMP cmp e1 e2 trueLabel dummyLabel):(tail tracedstms))) (tail blocks)


-- helpers
hasLabel :: Label -> [[Stm]] -> [Stm] -> [[Stm]]
hasLabel lab results block = if LABEL lab == head block then block:results else results
