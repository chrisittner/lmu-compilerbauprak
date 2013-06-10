module MJTypeChecker where
import MJAbsSyn
import MJSymbolTreeMaker
import Data.List (nub)

getClassSymbolTree (ClassDeclaration name _ _ _) (Program classes) = [ c | c@(Class n _ _)<-classes, n == name] !!0
getMainClassSymbolTree (MainClass name _ _) (Program classes) = [ c | c@(Class n _ _)<-classes, n == name] !!0
getMethodSymbolTree (MethodDeclaration _ name _ _ _ _) (Class _ _ methods) = [ m | m@(Method n _ _ _)<-methods, n == name ] !!0

class TypeChecker x where
 typecheck :: x -> (SymbolTree, SymbolTree, SymbolTree) -> Bool
 typecheck' :: x -> SymbolTree -> Bool
 typecheck' x s = typecheck x (s, EmptyTree, EmptyTree)
 -- The tripels elements are (1) the Program Tree, (2) the Tree of the current Class and (3) the Tree of the current Method.

instance TypeChecker Prg where
 typecheck (Prg mainclass classdecllist) (p, _, _) = typecheck mainclass (p, (getMainClassSymbolTree mainclass p) , EmptyTree) -- typecheck mainclass
	&& foldl (&&) True [ typecheck x (p, (getClassSymbolTree x p), EmptyTree) | x<-classdecllist ] -- typecheck all classes
	&& ((length . nub $ [ name | (ClassDeclaration name _ _ _)<-classdecllist ]) == length classdecllist) -- check uniqueness of classdecllist

instance TypeChecker MainClass where
 typecheck (MainClass _ _ stm) (p, c@(Class _ _ methods), _) = typecheck stm (p, c, methods!!0) -- typecheck all statements (of the only method: main)

instance TypeChecker ClassDeclaration where
 typecheck (ClassDeclaration name _ varlist methodlist) s@(p, c, _) = 
	foldl (&&) True [ typecheck method (p, c, (getMethodSymbolTree method c)) | method<-methodlist ] -- typecheck all methods
	&& (length . nub $ [ name | (MethodDeclaration _ name _ _ _ _) <-methodlist ]) == length methodlist -- check uniqueness of methodlist
	&& (length . nub $ [ name | (VarDeclaration _ name) <-varlist ]) == length varlist -- check uniqueness of varlist
	&& foldl (&&) True [typecheck var s | var<-varlist] -- typecheck all vars

instance TypeChecker MethodDeclaration where
 typecheck (MethodDeclaration typ name parlist varlist stm returnexp) s = Just typ == typeof returnexp s  -- check for matching returntype
	&& (length . nub $ [ snd x | x <-parlist ]) == length parlist -- check uniqueness of parlist
	&& (length . nub $ [ name | (VarDeclaration _ name) <-varlist ]) == length varlist -- uniqueness of varlist 
	&& typecheck stm s -- typecheck method statement(s)
	&& foldl (&&) True [typecheck (VarDeclaration t i) s | (t,i)<-parlist] -- typecheck all parameters
	&& foldl (&&) True [typecheck var s | var<-varlist] -- typecheck all vars

instance TypeChecker Stm where 
 typecheck (StmList stmlist) s = foldl (&&) True [ typecheck stm s | stm<-stmlist ] -- typecheck all statements
 typecheck (IfStm condition stm1 stm2) s = typeof condition s == Just BoolType
	&& typecheck stm1 s
	&& typecheck stm2 s
 typecheck (WhileStm condition stm) s = typeof condition s == Just BoolType
	&& typecheck stm s
 typecheck (PrintStm exp) s = typeof exp s == Just IntType
 typecheck (PrintLnStm exp) s = typeof exp s == Just IntType
 typecheck (AssignStm varname exp) s@(_, (Class _ classvars _), (Method _ _ pars vars)) = elem varname [ name | (Var name _)<-varlist ] -- check if variable is declared
	&& typeof exp s == Just vartype -- check if types match
		where varlist = vars ++ pars ++ classvars
		      vartype = [ typ | (Var name typ)<-varlist, name == varname ] !!0
 typecheck (ArrayAssignStm arrayname indexexp exp) s@(_, (Class _ classvars _), (Method _ _ pars vars)) = elem arrayname [name | (Var name IntArrayType)<-varlist] -- check if array is declared
	&& typeof indexexp s == Just IntType
	&& typeof exp s == Just IntType -- only IntArrays supported!
		where varlist = vars ++ pars ++ classvars

instance TypeChecker VarDeclaration where -- check whether the the Var type exists (only in cass of ClassType)
 typecheck (VarDeclaration (ClassType classname) _) ((Program classlist), _, _) 
	| elem classname [n | (Class n _ _)<-classlist] = True
	| otherwise = False
 typecheck _ _ = True


typeof :: Exp -> (SymbolTree, SymbolTree, SymbolTree) -> Maybe Type
-- Returns type of expression or Nothing
-- Parameter: (1) the Expression (2) a 3-tupel consisting of (p) the Program Symboltree (c) the current Class Symboltree and (m) the current Method Symboltree

typeof (IdentifierExp name) (_, (Class _ classvars _), (Method _ _ pars vars)) 
	| elem name [n | (Var n _)<-varlist] = Just ([typ | (Var n typ)<-varlist, name==n] !!0)
	| otherwise = Nothing
		where varlist = vars ++ pars ++ classvars
typeof (ArrayGetExp name index) s
	| typeof name s == Just IntArrayType && typeof index s == Just IntType = Just IntType
	| otherwise = Nothing
typeof (ArrayLengthExp name) s
	| typeof name s == Just IntArrayType = Just IntType
	| otherwise = Nothing
typeof (NewObjExp classname) ((Program classlist),_,_) = if elem classname [n | (Class n _ _)<-classlist] then Just (ClassType classname) else Nothing
typeof (NegExp boolexp) s = if typeof boolexp s == Just BoolType then Just BoolType else Nothing
typeof (BoolExp bool) _ = Just BoolType
typeof (IntExp int) _ = Just IntType
typeof (IntArrayDeclExp length) _ = Just IntArrayType
typeof (ThisExp) (_, (Class name _ methods), _)
	| length [m | (Method m typ pars vars)<-methods, m=="main", typ==VoidType, length vars == 0, length pars ==1, length [p | (Var p StringArrayType)<-pars] == 1  ] > 0 = Nothing  -- return Nothing if in main method (static!->no "this" allowed)
	| otherwise = Just (ClassType name)
typeof (BracedExp exp) s = typeof exp s
typeof (OpExp exp1 operator exp2) s
	| elem operator [OpPlus, OpMinus, OpTimes, OpDivide]
		&& typeof exp1 s == Just IntType
		&& typeof exp1 s == typeof exp2 s = Just IntType
	| operator == OpAnd
		&& typeof exp1 s == Just BoolType
		&& typeof exp2 s == typeof exp1 s = Just BoolType
	| operator == OpLt
		&& typeof exp1 s == Just IntType
		&& typeof exp2 s == typeof exp1 s = Just BoolType
	| otherwise = Nothing
-- Methodenaufruf: Prüfen ob Objekt existiert und die zugehörige Klasse die Methode besitzt und ob die Zahl der Parameter stimmt und ob alle Argumente existieren und alle den korrekten Typ haben.
typeof (InvokeExp objname methodname args) s@((Program classlist), (Class _ classvars classmethods), (Method _ _ pars vars))
	| isobj objtype
		&& elem methodname methodnamelist
		&& (length args) ==  (length methodargs)
		&& foldl (&&) True (zipWith (==) argtypes methodargtypes) = (Just methodtype)
	| otherwise = Nothing
		where varlist = vars ++ pars ++ classvars
		      objtype = typeof objname s
		      objclass = getclass objtype
		      methodlist = [methods | (Class n _ methods)<-classlist, n==objclass] !!0
		      methodnamelist = [ name | (Method name _ _ _)<-methodlist ]
		      (methodargs, methodtype) = [(args, typ) | (Method n typ args _)<-methodlist, n==methodname] !!0
		      argtypes = [typeof arg s | arg<-args]
		      methodargtypes = [Just typ | (Var _ typ)<-methodargs]
typeof _ _ = Nothing

isobj (Just (ClassType _)) = True
isobj _ = False
getclass (Just (ClassType c)) = c



