module MJTranslate where
import qualified MJAbsSyn as F -- Frontend Syntax
import Backend.Tree -- Backend Syntax
import Backend.Names
import Backend.MachineSpecifics
import MJSymbolTreeMaker
import MJTypeChecker
import Control.Monad
-- Stm, Exp bezeichnen die Typen aus dem Backend!

{- ToDo: NegExp, Konstruktoren, Optimierungen -}

getClassName :: F.Exp -> (SymbolTree, SymbolTree, SymbolTree) -> Label
getClassName objname s = getclass (typeof objname s)
objsize :: Int -> Label -> SymbolTree -> Int
objsize wordsize classname (Program classlist) = (wordsize*) . length . head $ [ varlist | (Class name varlist _) <-classlist, name == classname ]

addr :: String -> [(String, Exp)] -> Exp
addr name addrlist = head [ addr | (aname, addr) <- addrlist, aname == name ]
varName :: SymbolTree -> String
varName (Var n _) = n

class Translate x where
 translate :: (Frame f, Assem a, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> x -> m [Fragment f Stm]
 translate' :: (Frame f, Assem a, MachineSpecifics m a f) => x -> SymbolTree -> m [Fragment f Stm]
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

translateMethod :: (Frame f, Assem a, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> F.MethodDeclaration -> m (Fragment f Stm)
translateMethod s@(_, (Class classname classvars _), (Method name _ pars vars)) (F.MethodDeclaration _ _ _ _ stm returnexp) = do
	f <- mkFrame (classname ++ "$" ++ name) ((length pars)+1)
	wordsize <- wordSize
	(f, localAddrList) <- foldM (\ (frame,addrlist) name -> do (frame',addr)<- allocLocal frame Anywhere;  return (frame', addrlist++[(name,addr)])) (f,[]) (map varName vars) -- alloziere lokale variablen (der frame wird bei jedem aufruf verändert und muss deshalb sequenziell durchgereicht werden)
	let parAddrList = zip ("This":(map (\ (Var n _) -> n) pars)) (params f)
	let classAddrList = zipWith (\ (Var n _) relAddr -> (n, BINOP PLUS ((params f)!!0) (CONST relAddr)) ) classvars [0,wordsize..]
	let addrlist = localAddrList ++ parAddrList ++ classAddrList 
	stm <- (translateStm s addrlist stm)
	returnexp <- translateExp s addrlist returnexp
	proc <- makeProc f stm returnexp
	return $ FragmentProc f proc




translateStm :: (Frame f, Assem a, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> ([(String, Exp)]) -> F.Stm  -> m Stm
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
	return $ sseq [ CJUMP {rel=Backend.Tree.EQ, leftE=condition, rightE=CONST 1, trueLab=ltrue, falseLab=lfalse}, -- optimieren!
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
		CJUMP {rel=Backend.Tree.EQ, leftE=condition, rightE=CONST 1, trueLab=ltrue, falseLab=lfalse},
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
	exp <- translateExp s addrlist exp
	let dest' = BINOP PLUS (MEM (addr arrayname addrlist)) (BINOP MUL (CONST wordsize) (BINOP PLUS (CONST 1) indexexp))
	return $ MOVE {dest =dest', src=exp}




translateExp :: (Frame f, Assem a, MachineSpecifics m a f) => (SymbolTree, SymbolTree, SymbolTree) -> ([(String, Exp)]) -> F.Exp -> m Exp
translateExp s addrlist (F.OpExp exp1 operation exp2) = do 
	exp1 <- translateExp s addrlist exp1
	exp2 <- translateExp s addrlist exp2
	case operation of 
		F.OpAnd -> return $ exp1 `Backend.Tree.and` exp2
		F.OpPlus -> return $ exp1 `plus` exp2
		F.OpMinus -> return $ exp1 `minus` exp2
		F.OpTimes -> return $ exp1 `times` exp2
		F.OpDivide -> return $ exp1 `divide`exp2
		F.OpLt -> do
		t <- nextTemp
		ltrue <- nextLabel
		lfalse <- nextLabel
		let cond = sseq [ MOVE (TEMP t) (CONST 0), 
		      	CJUMP {rel=Backend.Tree.LT, leftE=exp1, rightE=exp2, trueLab=ltrue, falseLab=lfalse},
		      	LABEL ltrue,
		      	MOVE (TEMP t) (CONST 1),
		      	LABEL lfalse ]
		return $ ESEQ cond (TEMP t)

translateExp s addrlist (F.ArrayGetExp (F.IdentifierExp name) index) = do 
	wordsize <- wordSize
	index <- translateExp s addrlist index
	return $ MEM (BINOP PLUS (MEM (addr name addrlist)) (BINOP MUL (CONST wordsize) (BINOP PLUS (CONST 1) index)))

translateExp _ addrlist (F.ArrayLengthExp (F.IdentifierExp name)) = do
	return $ MEM (MEM (addr name addrlist))

translateExp s addrlist (F.InvokeExp objname methodname args) = do
	args <- mapM (translateExp s addrlist) args
	obj <- translateExp s addrlist objname
	return $ CALL (NAME ((getClassName objname s) ++ "$" ++ methodname)) (obj:args)

translateExp _ _ (F.IntExp int) = return $ CONST int

translateExp _ _ (F.BoolExp bool) = case bool of
		True -> return $ CONST 1
		False -> return $ CONST 0

translateExp _ addrlist (F.IdentifierExp name) = return $ MEM (addr name addrlist)

translateExp _ addrlist (F.ThisExp)  = return (addr "This" addrlist)

translateExp s addrlist (F.IntArrayDeclExp length) = do
	length <- translateExp s addrlist length
	wordsize <- wordSize
	return $ CALL {func = NAME "L_halloc", args=[(BINOP MUL (BINOP PLUS (CONST 1) length) (CONST wordsize))]}

translateExp (p,_,_) _ (F.NewObjExp classname) = do 
	wordsize <- wordSize
	return $ CALL {func = NAME "L_halloc", args=[CONST (objsize wordsize classname p)]}

translateExp s addrlist (F.NegExp exp) = do 
	exp <- translateExp s addrlist exp
	return  exp -- $ neg exp -- ###

translateExp s addrlist (F.BracedExp exp) = translateExp s addrlist exp

