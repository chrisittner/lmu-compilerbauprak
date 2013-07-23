module MJAbsSyn where

data Type = IntType | BoolType | IntArrayType | ClassType String | StringArrayType | VoidType deriving (Show, Eq, Ord)
type Identifier = String

data Prg = Prg MainClass [ClassDeclaration] deriving (Show, Eq, Ord)
data MainClass = MainClass Identifier Identifier Stm deriving (Show, Eq, Ord)
data ClassDeclaration = ClassDeclaration Identifier Identifier [VarDeclaration] [MethodDeclaration] deriving (Show, Eq, Ord)
data VarDeclaration = VarDeclaration Type Identifier deriving (Show, Eq, Ord)
data MethodDeclaration = MethodDeclaration Type Identifier [(Type, Identifier)] [VarDeclaration] Stm Exp deriving (Show, Eq, Ord)
data OpCode = OpAnd |  OpPlus |  OpMinus |  OpTimes | OpDivide |  OpLt  deriving (Show, Eq, Ord)

data Stm = StmList [Stm]
	| IfStm Exp Stm Stm 
	| WhileStm Exp Stm
	| PrintStm Exp
	| PrintLnStm Exp
	| AssignStm Identifier Exp
	| ArrayAssignStm Identifier Exp Exp
 deriving (Show, Eq, Ord)

data Exp = OpExp Exp OpCode Exp 
	| ArrayGetExp Exp Exp
	| ArrayLengthExp Exp
	| InvokeExp Exp Identifier [Exp]
	| IntExp Int
	| BoolExp Bool
	| IdentifierExp Identifier
	| ThisExp
	| IntArrayDeclExp Exp
	| NewObjExp Identifier
	| NegExp Exp
	| BracedExp Exp
 deriving (Show, Eq, Ord)
