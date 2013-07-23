{
module MJParse where
import MJLex
import MJTokens
import MJAbsSyn
}

%name parse
%tokentype { (Token AlexPosn) }

%name parse

%token

  "int"    	{ TInt _ }
  "boolean" { TBool _ }
  "String"	{ TString _ }
  "public"  { Public _ }
  "static"  { Static _ }
  "void"    { Void _ }
  "main"    { Main p}
  "class"   { Class _ }
  "extends" { Extends _ }
  "return"  { Return _ }
  "length"  { Length _ }
  "new"     { New _ }
  "this"    { This _ }
  "char"    { Char _ }
  "{"				{ LBrace _ }
  "}"	      { RBrace _ }
  "["	      { LBrack _ }
  "]"	      { RBrack _ }
  "("	      { LPar _ }
  ")"	      { RPar _ }
  "if"      { If _ }
  "else"	  { Else _ }
  "while"   { While _ }
  "print"  { Print _ }
  "println" { PrintLn _ }
  "="       { Assign _ } 
  "&&"      { MJTokens.OpAnd _ } 
--  "=="      { OpEq _ } 
  "<"       { MJTokens.OpLt _ } 
  "+"       { MJTokens.OpPlus _ }
  "-"       { MJTokens.OpMinus _ } 
  "*"       { MJTokens.OpTimes _ }
  "/"       { MJTokens.OpDivide _ }
  "!"			  { OpNeg _ }
  "true"    { CTrue _ }
  "false"   { CFalse _ }
  ","	      { Comma _ }
  "."				{ Point _ }
  ";"	      { Semicol _ }
  integer   { IVal $$ _ }
  identifier { Id $$ _ }
--  string    { SVal $$ _ }



%left "&&"
%left "<"
%left "+" "-"
%left "*" "/" 
%right "!"
%left "["
%right "."

-- %left "(" ")" "{" "}" "]"

%%

Prg :: { Prg }
Prg: MainClass ClassDeclarationList { Prg $1 $2 }

MainClass :: { MainClass }
MainClass: "class" identifier "{" "public" "static" "void" "main" "(" "String" "[" "]" identifier ")" "{" StmListStm "}" "}" { MainClass $2 $12 $15 }

ClassDeclaration :: { ClassDeclaration }
ClassDeclaration: "class" identifier "{" VarDeclarationList  MethodDeclarationList "}" { ClassDeclaration $2 "" $4 $5 }
| "class" identifier "extends" identifier "{" VarDeclarationList  MethodDeclarationList "}" { ClassDeclaration $2 $4 $6 $7 }

ClassDeclarationList :: { [ClassDeclaration] }
ClassDeclarationList: { [] }
| ClassDeclaration ClassDeclarationList { $1:$2 }

VarDeclaration :: { VarDeclaration }
VarDeclaration: Type identifier ";" { VarDeclaration $1 $2 }

VarDeclarationList :: { [VarDeclaration] }
VarDeclarationList: { [] }
| VarDeclarationList VarDeclaration { $1 ++ [$2] }

MethodDeclaration :: { MethodDeclaration }
MethodDeclaration: "public" Type identifier "(" ")" "{" VarDeclarationList StmListStm "return" Exp ";" "}" 	{ MethodDeclaration $2 $3 [] $7 $8 $10 }
| "public" Type identifier "(" ParameterList ")" "{" VarDeclarationList StmListStm "return" Exp ";" "}"	 		{ MethodDeclaration $2 $3 $5 $8 $9 $11 }

MethodDeclarationList :: { [MethodDeclaration] }
MethodDeclarationList: { [] }
| MethodDeclaration MethodDeclarationList { $1:$2 }

ParameterList :: { [(Type, Identifier)] }
ParameterList: Type identifier { [($1, $2)] }
| Type identifier "," ParameterList  { ($1, $2):$4 }

Type :: { Type }
Type: "int" "[" "]" { IntArrayType }
| "int" 						{ IntType }
| "boolean"					{ BoolType }
| identifier 				{ ClassType $1 }

-- Identifier:: { Identifier }
-- Identifier: identifier { $1 }


Stm :: { Stm }
Stm:  "{" StmList "}"                   						{ StmList $2 }
| "while" "(" Exp ")" Stm           								{ WhileStm $3 $5 }
| "if" "(" Exp ")" Stm "else" Stm   								{ IfStm $3 $5 $7 }
|  "print" "(" "(" "char" ")" Exp ")" ";"  					{ PrintStm $6 }
|  "println" "(" Exp ")" ";"       									{ PrintLnStm $3 }
|  identifier "=" Exp ";"            								{ AssignStm $1 $3 }
|  identifier "[" Exp "]" "=" Exp ";"               { ArrayAssignStm $1 $3 $6 }


StmListStm :: { Stm }
StmListStm : StmList { StmList $1 }

StmList :: { [Stm] }
StmList: { [] }
| Stm StmList { $1:$2 }


Exp :: { Exp }
Exp: identifier     									{ IdentifierExp $1 }
| integer           									{ IntExp $1 }
| Exp "&&" Exp     										{ OpExp $1 MJAbsSyn.OpAnd $3 }
| Exp "<" Exp     										{ OpExp $1 MJAbsSyn.OpLt $3 }
| Exp "+" Exp     										{ OpExp $1 MJAbsSyn.OpPlus $3 }
| Exp "-" Exp     										{ OpExp $1 MJAbsSyn.OpMinus $3 }
| Exp "*" Exp     										{ OpExp $1 MJAbsSyn.OpTimes $3 }
| Exp "/" Exp     										{ OpExp $1 MJAbsSyn.OpDivide $3 }
| Exp "[" Exp "]" 										{ ArrayGetExp $1 $3 }
| Exp "." "length" 										{ ArrayLengthExp $1 }
| Exp "." identifier "(" ")" 					{ InvokeExp $1 $3 [] }
| Exp "." identifier "(" ExpList ")"  { InvokeExp $1 $3 $5 }
| "true" 															{ BoolExp True }
| "false" 														{ BoolExp False }
| "this" 															{ ThisExp }
| "new" "int" "[" Exp "]" 						{ IntArrayDeclExp $4 }
| "new" identifier "(" ")" 						{ NewObjExp $2 }
| "!" Exp 														{ NegExp $2 }
| "(" Exp ")" 												{ BracedExp $2 }

ExpList :: { [Exp] }
ExpList:  Exp					{ [$1] }
| Exp "," ExpList  	{ $1:$3 }


{
happyError :: [Token AlexPosn] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  (tk:_) -> "line " ++ show l ++ ", column " ++ show c ++ " (token " ++ filterPn (show tk) ++ ")"
			where AlexPn _ l c = token_pos tk
}
