module Backend.X86Assem where
import Backend.Names
import Backend.MachineSpecifics
import Text.Printf

eax = mkNamedTemp "%eax"
ebx = mkNamedTemp "%ebx"
ecx = mkNamedTemp "%ecx"
edx = mkNamedTemp "%edx"
ebp = mkNamedTemp "%ebp"
esp = mkNamedTemp "%esp"
esi = mkNamedTemp "%esi"
edi = mkNamedTemp "%edi"

data Operand = Imm Int | Reg Temp | Mem { base::(Maybe Temp), scale::Int, index::(Maybe Temp), offset::(Maybe Int) } deriving (Eq, Ord)
data Cmp = E | NE | L | LE | G | GE | Z deriving (Show, Eq, Ord)
data Oper2 = MOV | ADD | SUB | SHL | SHR | SAL | SAR | AND | OR | XOR | TEST | CMP | LEA  deriving (Show, Eq, Ord)
data Oper1 = PUSH | POP | NEG | NOT | INC | DEC | IMUL | IDIV | ENTER  deriving (Show, Eq, Ord)
data Oper0 = RET | LEAVE | NOP deriving (Show, Eq, Ord)

data X86Assem =
	  OPER2 Oper2 Operand Operand
	| OPER1 Oper1 Operand
	| OPER0 Oper0
	| JMP Label | J Cmp Label | CALL Label
	| LABEL Label deriving (Ord, Eq)

instance Assem X86Assem where
  use (OPER2 MOV (Mem (Just src) _ _ _ ) (Reg src')) = [src, src']
  use (OPER2 MOV _ (Reg src)) = [src]
  use (OPER2 MOV _ (Mem (Just src) _ _ _ )) = [src]
  use (OPER2 MOV (Mem (Just src) _ _ _ ) _) = [src]
  use (OPER2 LEA _ (Reg src)) = [src]
  use (OPER2 LEA _ (Mem (Just src) _ _ _ )) = [src]
  use (OPER2 _ (Reg dest) (Reg src)) = [dest, src]
  use (OPER2 _ (Reg dest) (Mem (Just src) _ _ _ )) = [dest, src]
  use (OPER2 _ (Mem (Just dest) _ _ _ ) (Reg src)) = [dest, src]
  use (OPER2 _ (Mem (Just dest) _ _ _ ) (Mem (Just src) _ _ _ )) = [dest, src]
  use (OPER1 PUSH (Reg src)) = [src] 
  use (OPER1 PUSH (Mem (Just src) _ _ _ )) = [src] 
  use (OPER1 POP _) = []
  use (OPER1 IMUL (Reg src)) = [src, eax]
  use (OPER1 IMUL (Mem (Just src) _ _ _ )) = [src, eax]
  use (OPER1 IDIV (Reg src)) = [src, eax]
  use (OPER1 IDIV (Mem (Just src) _ _ _ )) = [src, eax]
  use (OPER1 ENTER (Reg src)) = [src]
  use (OPER1 ENTER (Mem (Just src) _ _ _ )) = [src]
  use (OPER1 _ (Reg src)) = [src] -- NEG NOT INC DEC
  use (OPER1 _ (Mem (Just src) _ _ _ )) = [src] -- NEG NOT INC DEC
  use (OPER0 RET) = [eax, esi, edi, ebx, ebp, esp] 
  use (CALL _) = []
  use _ = []

  def (OPER2 CMP _ _) = []
  def (OPER2 _ (Reg dest) _) = [dest]
  def (OPER2 _ (Mem (Just dest) _ _ _ ) _) = [dest]
  def (OPER1 PUSH _) = []
  def (OPER1 POP (Reg dest)) = [dest]
  def (OPER1 POP (Mem (Just dest) _ _ _ )) = [dest]
  def (OPER1 IMUL _) = [eax]
  def (OPER1 IDIV _) = [eax, edx]
  def (OPER1 ENTER _) = []
  def (OPER1 _ (Reg dest)) = [dest]
  def (OPER1 _ (Mem (Just dest) _ _ _ )) = [dest]
  def (CALL _) = [eax, ecx, edx]
  def _ = []
  
  jumps (JMP lab) = [lab]
  jumps (J _ lab) = [lab]
  jumps _ = []

  isFallThrough (JMP _) = False
  isFallThrough _ = True

  isMoveBetweenTemps (OPER2 MOV (Reg dest) (Reg src)) = Just (dest,src)
  isMoveBetweenTemps _ = Nothing

  isLabel (LABEL l) = Just l
  isLabel _ = Nothing

  rename (OPER2 o (Reg t1) (Reg t2)) f = OPER2 o (Reg (f t1)) (Reg (f t2))
  rename (OPER2 o (Reg t1) (Mem (Just t2) scale index offset)) f = OPER2 o (Reg (f t1)) (Mem (Just (f t2)) scale index offset)
  rename (OPER2 o (Mem (Just t1) scale index offset) (Reg t2)) f = OPER2 o (Mem (Just (f t1)) scale index offset) (Reg (f t2))
  rename (OPER2 o (Mem (Just t1) scale1 index1 offset1) (Mem (Just t2) scale2 index2 offset2)) f = OPER2 o (Mem (Just (f t1)) scale1 index1 offset1) (Mem (Just (f t2)) scale2 index2 offset2)
  rename (OPER2 o (Reg t1) op2) f = OPER2 o (Reg (f t1))  op2
  rename (OPER2 o (Mem (Just t1) scale index offset) op2) f = OPER2 o (Mem (Just (f t1)) scale index offset) op2
  rename (OPER2 o op1 (Reg t2)) f = OPER2 o op1 (Reg (f t2))
  rename (OPER2 o op1 (Mem (Just t1) scale index offset)) f = OPER2 o op1 (Mem (Just (f t1)) scale index offset)
  rename (OPER1 o (Reg t)) f = OPER1 o (Reg (f t))
  rename (OPER1 o (Mem (Just t) scale index offset )) f = OPER1 o (Mem (Just (f t)) scale index offset )
  rename i _ = i

instance Show Operand where
  show (Imm int) = show int
  show (Reg temp) = show temp
  show (Mem (Just base) scale Nothing (Just n)) = "DWORD PTR [" ++ (show base) ++ (printf "%+d" n) ++ "]" -- (2tes reg nicht verwendet)
  show (Mem (Just base) scale Nothing Nothing) = "DWORD PTR ["++ (show base) ++ "]"
instance Show X86Assem where
  show (OPER2 op arg1 arg2) = "\t" ++ (show op) ++ " " ++ (show arg1) ++ ", " ++ (show arg2)
  show (OPER1 ENTER arg) = "\t" ++ (show ENTER) ++ " " ++ (show arg) ++ ", 0"
  show (OPER1 op arg) = "\t" ++ (show op) ++ " " ++ (show arg)
  show (OPER0 op) = "\t" ++ (show op)
  show (CALL lab) = "\t" ++ "CALL " ++ lab
  show (J cmp lab) = "\t" ++ "J" ++ (show cmp) ++ " " ++ lab
  show (JMP lab) = "\t" ++ "JMP " ++ lab
  show (LABEL lab) = lab ++ ":"
