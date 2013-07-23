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
	use (OPER2 MOV (Mem (Just src) _ _ _ ) src') = src : getTemps src'
	use (OPER2 MOV _ src) = getTemps src
	use (OPER2 LEA _ src) = getTemps src
	use (OPER2 _ dest src) = getTemps dest ++ getTemps src
	use (OPER1 PUSH src) = getTemps src
	use (OPER1 POP _) = []
	use (OPER1 IMUL src) = eax : getTemps src
	use (OPER1 IDIV src) = eax : getTemps src
	use (OPER1 ENTER src) = getTemps src
	use (OPER1 _ src) = getTemps src -- NEG NOT INC DEC
	use (OPER0 RET) = [esi, edi, ebx, ebp, eax]
	use _ = []

	def (OPER2 CMP _ _) = []
	def (OPER2 _ dest _) = getTemps dest
	def (OPER1 PUSH _) = []
	def (OPER1 POP dest) = getTemps dest
	def (OPER1 IMUL _) = [eax]
	def (OPER1 IDIV _) = [eax, edx]
	def (OPER1 ENTER _) = []
	def (OPER1 _ dest) = getTemps dest
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

	rename (OPER2 o t t') f = OPER2 o (renameTempInOper t f) (renameTempInOper t' f)
	rename (OPER1 o t) f = OPER1 o (renameTempInOper t f)
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



-- helpers

getTemps :: Operand -> [Temp]
getTemps (Reg t) = [t]
getTemps (Mem (Just t) _ (Just t') _) = [t, t']
getTemps (Mem (Just t) _ _ _) = [t]
getTemps (Mem _ _ (Just t) _) = [t]
getTemps _ = []

renameTempInOper :: Operand -> (Temp -> Temp) -> Operand
renameTempInOper (Reg t) f = Reg $ f t
renameTempInOper (Mem (Just t) scale (Just t') offset) f = Mem (Just $ f t) scale (Just $ f t') offset
renameTempInOper (Mem (Just t) scale index offset) f = Mem (Just $ f t) scale index offset
renameTempInOper (Mem base scale (Just t) offset) f = Mem base scale (Just $ f t) offset
renameTempInOper o _ = o


