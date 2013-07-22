{
module MJLex where
import MJTokens
}
%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+;
  "//".*;
  "/*".*"*/";
  "int"    	{ \ p s  -> TInt p }
  "boolean" { \ p s  -> TBool p }
  "String"	{ \ p s  -> TString p }
  "public"  { \ p s -> Public p }
  "static"  { \ p s -> Static p }
  "void"    { \ p s -> Void p }
  "main"    { \ p s -> Main p}
  "class"   { \ p s -> Class p }
  "extends" { \ p s -> Extends p }
  "return"  { \ p s -> Return p }
  "length"  { \ p s -> Length p }
  "new"     { \ p s -> New p }
  "this"    { \ p s -> This p }
  "char"    { \ p s -> Char p }
  "{"				{ \ p s  -> LBrace p }
  "}"	      { \ p s  -> RBrace p }
  "["	      { \ p s  -> LBrack p }
  "]"	      { \ p s  -> RBrack p }
  "("	      { \ p s  -> LPar p }
  ")"	      { \ p s  -> RPar p }
  "if"      { \ p s  -> If p }
  "else"	  { \ p s -> Else p }
  "while"   { \ p s -> While p }
  "System.out.print"  { \ p s  -> Print p }
  "System.out.println" { \ p s -> PrintLn p }
  "="       { \ p s  -> Assign p } 
  "&&"      { \ p s  -> OpAnd p } 
  "=="      { \ p s  -> OpEq p } 
  "<"       { \ p s  -> OpLt p } 
  "+"       { \ p s  -> OpPlus p }
  "-"       { \ p s  -> OpMinus p } 
  "*"       { \ p s  -> OpTimes p }
  "/"       { \ p s  -> OpDivide p }
  "!"			  { \ p s  -> OpNeg p }
  "true"    { \ p s  -> CTrue p }
  "false"   { \ p s  -> CFalse p }
  ","	      { \ p s  -> Comma p }
  ";"	      { \ p s  -> Semicol p }
  "."				{ \ p s  -> Point p }
  $digit+  { \ p s  -> IVal (read s) p }
  $alpha [$alpha $digit \_ \']*  { \ p s  -> Id s p }

{}
