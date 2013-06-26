module X86Machine where
import Backend.Tree
import Backend.Names
import Backend.MachineSpecifics

data Operand = Imm Int | Reg Temp | Mem { base::(Maybe Temp), scale::Int, index::(Maybe Temp) }
data Cmp = E | NE | L | LE | G | GE | Z
data Oper2 = MOV | ADD | SUB | SHL | SHR | SAL | SAR | AND | OR | XOR | TEST | CMP | LEA 
data Oper1 = PUSH | POP | NEG | NOT | INC | DEC | IMUL | IDIV | ENTER 
data Oper0 = RET | LEAVE | NOP

data X86Assem =
	  OPER2 Oper2 Operand Operand
	| OPER1 Oper1 Operand
	| OPER0 Oper0
	| JMP Label | J Cmp Label | CALL Operand
	| LABEL Label

instance Assem X86Assem where
  use (OPER2 MOV _ src) = [src]
  use (OPER2 LEA _ src) = [src] -- test?
  use (OPER2 _ dest src) = [dest, src]
  use (OPER1 _ src) = [src]
  use _ = []
  def (OPER2 _ dest _) = [dest]
  def (OPER1 _ dest) = [dest]
  def _ = []
  jumps (JMP lab) = [lab]
  jumps (J _ lab) = [lab]
  jumps _ = []
  isFallThrough (J _ _) = False
  isFallThrough (CALL _) = False
  isFallThrough (JMP _) = False
  isFallThrough _ = True -- ?
  isMoveBetweenTemps (OPER2 MOV (Reg _) (Reg _)) = True
  isMoveBetweenTemps _ = False
  isLabel (LABEL l) = Just l
  isLabel _ = Nothing
  rename (OPER2 o (Reg t1) (Reg t2)) f = OPER2 o (Reg (f t1)) (Reg (f t2))
  rename (OPER2 o (Reg t1) op2) f = OPER2 o (Reg (f t1)) (Reg op2)
  rename (OPER2 o op1 (Reg t2)) f = OPER2 o (Reg op1) (Reg (f t2))
  rename (OPER1 o (Reg t)) f = OPER1 o (Reg (f t))
  rename (CALL (Reg t)) f = CALL (Reg (f t))
  rename i = i
