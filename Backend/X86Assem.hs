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

data Operand = Imm Int | Reg Temp | Mem { base::(Maybe Temp), scale::Int, index::(Maybe Temp), offset::(Maybe Int) } deriving (Eq)
data Cmp = E | NE | L | LE | G | GE | Z deriving (Show, Eq)
data Oper2 = MOV | ADD | SUB | SHL | SHR | SAL | SAR | AND | OR | XOR | TEST | CMP | LEA  deriving (Show, Eq)
data Oper1 = PUSH | POP | NEG | NOT | INC | DEC | IMUL | IDIV | ENTER  deriving (Show, Eq)
data Oper0 = RET | LEAVE | NOP deriving (Show, Eq)

data X86Assem =
	  OPER2 Oper2 Operand Operand
	| OPER1 Oper1 Operand
	| OPER0 Oper0
	| JMP Label | J Cmp Label | CALL Label
	| LABEL Label deriving (Eq)

instance Assem X86Assem where
  use (OPER2 MOV _ (Reg src)) = [src]
  use (OPER2 LEA _ (Reg src)) = [src]
  use (OPER2 _ (Reg dest) (Reg src)) = [dest, src]
  use (OPER1 PUSH (Reg src)) = [src] 
  use (OPER1 POP _) = []
  use (OPER1 IMUL (Reg src)) = [src, mkNamedTemp "%eax"]
  use (OPER1 IDIV (Reg src)) = [src, mkNamedTemp "%eax"]
  use (OPER1 ENTER (Reg src)) = [src]
  use (OPER1 _ (Reg src)) = [src] -- NEG NOT INC DEC
  use (CALL _) = [mkNamedTemp "%eax", mkNamedTemp "%ecx", mkNamedTemp "%edx"]
  use _ = []

  def (OPER2 _ (Reg dest) _) = [dest] -- ToDo: TEST?
  def (OPER1 PUSH _) = []
  def (OPER1 POP (Reg dest)) = [dest]
  def (OPER1 IMUL _) = [mkNamedTemp "%eax"]
  def (OPER1 IDIV _) = [mkNamedTemp "%eax", mkNamedTemp "%edx"]
  def (OPER1 ENTER _) = []
  def (OPER1 _ (Reg dest)) = [dest]
  def (CALL _) = [mkNamedTemp "%eax"]
  def _ = []
  
  jumps (JMP lab) = [lab]
  jumps (J _ lab) = [lab]
  jumps _ = []

  isFallThrough (J _ _) = False
  isFallThrough (JMP _) = False
  isFallThrough _ = True

  isMoveBetweenTemps (OPER2 MOV (Reg dest) (Reg src)) = Just (dest,src)
  isMoveBetweenTemps _ = Nothing

  isLabel (LABEL l) = Just l
  isLabel _ = Nothing

  rename (OPER2 o (Reg t1) (Reg t2)) f = OPER2 o (Reg (f t1)) (Reg (f t2))
  rename (OPER2 o (Reg t1) op2) f = OPER2 o (Reg (f t1))  op2
  rename (OPER2 o op1 (Reg t2)) f = OPER2 o op1 (Reg (f t2))
  rename (OPER1 o (Reg t)) f = OPER1 o (Reg (f t))
  rename i _ = i

instance Show Operand where
  show (Imm int) = show int
  show (Reg temp) = show temp
  show (Mem (Just base) scale Nothing (Just n)) = "DWORD PTR [" ++ (show base) ++ (printf "%+d" n) ++ "]" -- (2tes reg nicht verwendet)
  show (Mem (Just base) scale Nothing Nothing) = "DWORD PTR ["++ (show base) ++ "]"
instance Show X86Assem where
  show (OPER2 op arg1 arg2) = (show op) ++ " " ++ (show arg1) ++ ", " ++ (show arg2)
  show (OPER1 op arg) = (show op) ++ " " ++ (show arg)
  show (OPER0 op) = (show op)
  show (CALL lab) = "CALL " ++ lab
  show (J cmp lab) = "J" ++ (show cmp) ++ " " ++ lab
  show (JMP lab) = "JMP " ++ lab
  show (LABEL lab) = lab ++ ":"
