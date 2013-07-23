module MJTranslate where
import Prelude hiding (EQ,GT,LT)
import qualified MJAbsSyn as F -- Frontend Syntax
import Backend.Tree -- Backend Syntax
import Backend.Names
import Backend.MachineSpecifics
import MJSymbolTreeMaker
import MJTypeChecker
import Control.Monad
-- Stm, Exp bezeichnen die Typen aus dem Backend!


getClassName :: F.Exp -> (SymbolTree, SymbolTree, SymbolTree) -> Label
getClassName objname s = getclass (typeof objname s)
objsize :: Int -> Label -> SymbolTree -> Int
objsize wordsize classname (Program classlist) = (wordsize*) . length . head $ [ varlist | (Class name varlist _) <-classlist, name == classname ]
addr :: String -> [(String, Exp)] -> Exp
addr name addrlist = head [ addr | (aname, addr) <- addrlist, aname == name ]
varName :: SymbolTree -> String
varName (Var n _) = n

class Translate x where
 translate :: (Frame f, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> x -> m [Fragment f Stm]
 translate' :: (Frame f, MachineSpecifics m a f) => x -> SymbolTree -> m [Fragment f Stm]
 translate' x s = translate (s, EmptyTree, EmptyTree) x

instance Translate F.Prg where
 translate (p,_,_) (F.Prg mainclass classlist) = do
	mainclass <- translate (p, getMainClassSymbolTree mainclass p, EmptyTree) mainclass
	classlist <- mapM (\ c -> (translate (p, getClassSymbolTree c p, EmptyTree) c)) classlist
	return $ mainclass ++ (concat classlist)

instance Translate F.MainClass where
 translate (p, c@(Class _ _ methods),_) (F.MainClass name arg stm) = do
	f <- mkFrame "main" 1
	stm <- translateStm (p,c,methods!!0) [(arg, (params f)!!0)] stm
	proc <- makeProc f stm (CONST 0)
	return [FragmentProc f proc]
	
instance Translate F.ClassDeclaration where
 translate (p, c,_) (F.ClassDeclaration classname _ varlist methodlist) = mapM (\ method -> (translateMethod (p, c, (getMethodSymbolTree method c)) method)) methodlist



translateMethod :: (Frame f, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> F.MethodDeclaration -> m (Fragment f Stm)
translateMethod s@(_, (Class classname classvars _), (Method name _ pars vars)) (F.MethodDeclaration _ _ _ _ stm returnexp) = do
	f <- mkFrame (classname ++ "$" ++ name) ((length pars)+1)
	wordsize <- wordSize
	(f, localAddrList) <- foldM (\ (frame,addrlist) name -> do (frame',addr)<- allocLocal frame Anywhere;  return (frame', addrlist++[(name,addr)])) (f,[]) (map varName vars) -- alloziere lokale variablen (der frame wird bei jedem aufruf verändert und muss deshalb sequenziell durchgereicht werden)
	let parAddrList = zip ("This":(map (\ (Var n _) -> n) pars)) (params f)
	let classAddrList = zipWith (\ (Var n _) relAddr -> (n, MEM (BINOP PLUS ((params f)!!0) (CONST relAddr))) ) classvars [0,wordsize..]
	let addrlist = localAddrList ++ parAddrList ++ classAddrList 
	stm <- (translateStm s addrlist stm)
	returnexp <- translateExp s addrlist returnexp
	proc <- makeProc f stm returnexp
	return $ FragmentProc f proc



translateStm :: (Frame f, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> ([(String, Exp)]) -> F.Stm  -> m Stm
translateStm s addrlist (F.StmList stmlist) = do 
	stmlist <- mapM (translateStm s addrlist) stmlist
	return $ sseq stmlist

translateStm s addrlist (F.IfStm condition stm1 stm2) = do 
	ltrue <- nextLabel
	lfalse <- nextLabel
	lexit <- nextLabel
	condition <- translateExp s addrlist condition
	stm1 <- translateStm s addrlist stm1
	stm2 <- translateStm s addrlist stm2
	return $ sseq [ CJUMP {rel=EQ, leftE=condition, rightE=CONST 1, trueLab=ltrue, falseLab=lfalse}, -- ToMaybe: optimieren!
		LABEL ltrue,
		stm1,
		jump lexit,
		LABEL lfalse,
		stm2,
		LABEL lexit ]

translateStm s addrlist (F.WhileStm condition stm) = do 
	ltrue <- nextLabel
	lfalse <- nextLabel
	lstart <- nextLabel
	condition <- translateExp s addrlist condition
	stm <- translateStm s addrlist stm
	return $ sseq [ LABEL lstart,
		CJUMP {rel=EQ, leftE=condition, rightE=CONST 1, trueLab=ltrue, falseLab=lfalse},
		LABEL ltrue,
		stm,
		jump lstart,
		LABEL lfalse ]

translateStm s addrlist (F.PrintStm exp) = do
	exp <- translateExp s addrlist exp
	return $ EXP (CALL {func=NAME "L_print_char", args = [exp] })

translateStm s addrlist (F.PrintLnStm exp) = do
	exp <- translateExp s addrlist exp
	return $ EXP (CALL {func=NAME "L_println_int", args = [exp] })

translateStm s addrlist (F.AssignStm name exp) = do 
	exp <- translateExp s addrlist exp
	return $ MOVE {dest=(addr name addrlist), src=exp}

translateStm s addrlist (F.ArrayAssignStm arrayname indexexp exp) = do
	wordsize <- wordSize
	indexexp <- translateExp s addrlist indexexp
	ltrue <- nextLabel
	lfalse <- nextLabel
	lexit <- nextLabel
	exp <- translateExp s addrlist exp
	let dest' = MEM (BINOP PLUS (BINOP MUL (CONST wordsize) (BINOP PLUS (CONST 1) indexexp)) (addr arrayname addrlist) )
	return $ sseq [ CJUMP {rel=LT, leftE=indexexp, rightE=MEM (addr arrayname addrlist), trueLab=ltrue, falseLab=lfalse},
		LABEL ltrue,
		MOVE {dest =dest', src=exp},
		jump lexit,
		LABEL lfalse,
		EXP $ CALL {func=NAME "L_raise", args = [(CONST 23)] }, 
		LABEL lexit ]



translateExp :: (Frame f, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> ([(String, Exp)]) -> F.Exp -> m Exp
translateExp s addrlist (F.OpExp exp1 operation exp2) = do 
	exp1 <- translateExp s addrlist exp1
	exp2 <- translateExp s addrlist exp2
	case operation of
		F.OpPlus -> return $ exp1 `plus` exp2
		F.OpMinus -> return $ exp1 `minus` exp2
		F.OpTimes -> return $ exp1 `times` exp2
		F.OpDivide -> return $ exp1 `divide`exp2
		F.OpAnd ->  do -- lazy evaluation of &&
			tmp <- nextTemp
			ltrue <- nextLabel
			lfalse <- nextLabel
			let lazyAnd = sseq [ MOVE (TEMP tmp) (CONST 0),
				CJUMP {rel=EQ, leftE=exp1, rightE=(CONST 0), trueLab=ltrue, falseLab=lfalse},
				LABEL lfalse,
				MOVE (TEMP tmp) exp2,
				LABEL ltrue]
			return $ ESEQ lazyAnd (TEMP tmp) 
		F.OpLt -> do 
			t <- nextTemp
			ltrue <- nextLabel
			lfalse <- nextLabel
			let cond = sseq [ MOVE (TEMP t) (CONST 0), 
				CJUMP {rel=LT, leftE=exp1, rightE=exp2, trueLab=ltrue, falseLab=lfalse},
				LABEL ltrue,
				MOVE (TEMP t) (CONST 1),
				LABEL lfalse ]
			return $ ESEQ cond (TEMP t)

translateExp s addrlist (F.ArrayGetExp nameexp index) = do 
	name <- translateExp s addrlist nameexp
	wordsize <- wordSize
	ltrue <- nextLabel
	lfalse <- nextLabel
	lexit <- nextLabel
	index <- translateExp s addrlist index
	return $ ESEQ  (sseq [ CJUMP {rel=LT, leftE=index, rightE=(MEM name), trueLab=ltrue, falseLab=lfalse},
		LABEL ltrue,
		NOP,
		jump lexit,
		LABEL lfalse,
		EXP $ CALL {func=NAME "L_raise", args = [(CONST 42)] },
		LABEL lexit ]) 
		(MEM (BINOP PLUS (BINOP MUL (CONST wordsize) (BINOP PLUS (CONST 1) index)) name ))

translateExp s addrlist (F.ArrayLengthExp nameexp) = do
	name <- translateExp s addrlist nameexp
	return $ MEM name

translateExp s addrlist (F.InvokeExp objname methodname args) = do
	args <- mapM (translateExp s addrlist) args
	obj <- translateExp s addrlist objname
	return $ CALL (NAME ((getClassName objname s) ++ "$" ++ methodname)) (obj:args)

translateExp _ _ (F.IntExp int) = return $ CONST int

translateExp _ _ (F.BoolExp bool) = case bool of
		True -> return $ CONST 1
		False -> return $ CONST 0

translateExp _ addrlist (F.IdentifierExp name) = return $ (addr name addrlist)

translateExp _ addrlist (F.ThisExp)  = return (addr "This" addrlist)

translateExp s addrlist (F.IntArrayDeclExp length) = do
	length <- translateExp s addrlist length
	wordsize <- wordSize
	t <- nextTemp
	return $ ESEQ (sseq [MOVE (TEMP t) CALL {func = NAME "L_halloc", args=[(BINOP MUL (BINOP PLUS (CONST 1) length) (CONST wordsize))]},
		MOVE (MEM (TEMP t)) length]) (TEMP t)

translateExp (p@(Program classes),_,_) _ (F.NewObjExp classname) = do 
	wordsize <- wordSize
	t <- nextTemp
	-- call Class constructor if it exists:
	if classname `elem` ([ map (\ (Method n _ _ _) -> n) methods | (Class name _ methods)<-classes, name == classname ] !!0) 
	then do 
		let stm = sseq [ MOVE (TEMP t) (CALL {func = NAME "L_halloc", args=[CONST (objsize wordsize classname p)]}), 
			EXP (CALL {func = NAME (classname ++ "$" ++ classname), args = [TEMP t]})	]
		return $ ESEQ stm (TEMP t)
	else do return $ CALL {func = NAME "L_halloc", args=[CONST (objsize wordsize classname p)]}

translateExp s addrlist (F.NegExp exp) = do 
	exp <- translateExp s addrlist exp
	return $ BINOP MINUS (CONST 1) exp

translateExp s addrlist (F.BracedExp exp) = translateExp s addrlist exp
