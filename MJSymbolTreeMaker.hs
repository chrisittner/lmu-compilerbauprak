module MJSymbolTreeMaker where
import MJAbsSyn

data SymbolTree = Program [SymbolTree] -- classlist
 | Class Identifier [SymbolTree] [SymbolTree]  -- name varlist methodlist
 | Var Identifier Type -- name typ
 | Method Identifier Type [SymbolTree] [SymbolTree]  -- name typ parameterlist varlist
 | EmptyTree -- the empty symboltree
 deriving (Show, Eq, Ord)

class SymbolTreeMaker x where
 symbolize :: x -> SymbolTree

instance SymbolTreeMaker Prg where
 symbolize (Prg mainclass classdecllist) = Program classes 
     where classes =  (symbolize mainclass) : [symbolize x | x<-classdecllist]

instance SymbolTreeMaker MainClass where
 symbolize (MainClass name args _) = Class name vars methods
     where vars = [] 
           methods = [main] 
           main = Method "main" VoidType [(Var args StringArrayType)] []

instance SymbolTreeMaker ClassDeclaration where
 symbolize (ClassDeclaration name parent varlist methodlist) = Class name vars methods
     where vars = [symbolize x | x<-varlist]
           methods = [symbolize x | x<-methodlist]

instance SymbolTreeMaker VarDeclaration where
 symbolize (VarDeclaration typ name) = Var name typ

instance SymbolTreeMaker MethodDeclaration where
 symbolize (MethodDeclaration typ name parlist varlist _ _) = Method name typ parameter vars
     where parameter = [Var name typ  | (typ, name)<-parlist]
           vars = [symbolize x | x<-varlist]
