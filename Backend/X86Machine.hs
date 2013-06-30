module Backend.X86Machine where
import Backend.Names
import Backend.MachineSpecifics

data Operand = Imm Int | Reg Temp | Mem { base::(Maybe Temp), scale::Int, index::(Maybe Temp) } deriving (Show, Eq)
data Cmp = E | NE | L | LE | G | GE | Z deriving (Show, Eq)
data Oper2 = MOV | ADD | SUB | SHL | SHR | SAL | SAR | AND | OR | XOR | TEST | CMP | LEA  deriving (Show, Eq)
data Oper1 = PUSH | POP | NEG | NOT | INC | DEC | IMUL | IDIV | ENTER  deriving (Show, Eq)
data Oper0 = RET | LEAVE | NOP deriving (Show, Eq)

data X86Assem =
	  OPER2 Oper2 Operand Operand
	| OPER1 Oper1 Operand
	| OPER0 Oper0
	| JMP Label | J Cmp Label | CALL Operand [Temp]
	| LABEL Label deriving (Show, Eq)

instance Assem X86Assem where
  use (OPER2 MOV _ (Reg src)) = [src]
  use (OPER2 LEA _ (Reg src)) = [src]
  use (OPER2 _ (Reg dest) (Reg src)) = [dest, src]
  use (OPER1 PUSH (Reg src)) = [src] 
  use (OPER1 POP _) = []
  use (OPER1 IMUL (Reg src)) = [src, mkNamedTemp "eax"]
  use (OPER1 IDIV (Reg src)) = [src, mkNamedTemp "eax"]
  use (OPER1 ENTER (Reg src)) = [src]
  use (OPER1 _ (Reg src)) = [src] -- NEG NOT INC DEC
  use (CALL _ args) = args
  use _ = []

  def (OPER2 _ (Reg dest) _) = [dest] -- ToDo: TEST?
  def (OPER1 PUSH _) = []
  def (OPER1 POP (Reg dest)) = [dest]
  def (OPER1 IMUL _) = [mkNamedTemp "eax"]
  def (OPER1 IDIV _) = [mkNamedTemp "eax"]
  def (OPER1 ENTER _) = []
  def (OPER1 _ (Reg dest)) = [dest]
  def (CALL (Reg dest) _) = [dest, mkNamedTemp "eax", mkNamedTemp "eay", mkNamedTemp "eaz"] -- caller-save regs, return-dest, return-value
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
  rename (CALL (Reg t) args) f = CALL (Reg (f t)) (map f args)
  rename i _ = i


