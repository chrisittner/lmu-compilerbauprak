


data Operand = Imm Int | Reg Temp | Mem { base::(Maybe Temp), scale::Int, index::(Maybe Temp) }
data Cmp = E | NE | L | LE | G | GE | Z
data X86Assem = MOV Operand Operand 
	| ADD Operand Operand
	| SUB Operand Operand
	| SHL Operand Operand
	| SHR Operand Operand
	| SAL Operand Operand
	| SAR Operand Operand
	| AND Operand Operand
	| OR Operand Operand
	| XOR Operand Operand
	| TEST Operand Operand
	| CMP Operand Operand
	| LEA  Operand Operand
	| PUSH Operand
	| POP Operand
	| NEG Operand
	| NOT Operand
	| INC Operand
	| DEC Operand
	| IMUL Operand
	| IDIV Operand
	| ENTER Operand
	| RET
	| LEAVE
	| NOP
	| JMP Label
	| CALL Operand
	| J Cmp Label
	| LABEL Label

instance Assem X86Assem where
  use i = error "unreachable" 
  def i = error "unreachable"
  jumps i = error "unreachable"
  isFallThrough i = error "unreachable"
  isMoveBetweenTemps i = error "unreachable"
  isLabel i = error "unreachable"
  rename i = error "unreachable"
