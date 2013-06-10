module Backend.Cmm where

import Prelude hiding (EQ,GT,LT)
import Data.List
import qualified Data.Set as Set
import Text.PrettyPrint
import Backend.Names
import Backend.Tree 
import Backend.DummyMachine
import Backend.MachineSpecifics( Frame(..), Fragment(..) )

tempsExp :: Exp -> Set.Set Temp
tempsExp (CONST _) = Set.empty
tempsExp (NAME _) = Set.empty
tempsExp (TEMP t) = Set.singleton t
tempsExp (BINOP _ e1 e2) = (tempsExp e1) `Set.union` (tempsExp e2)
tempsExp (MEM e) = tempsExp e
tempsExp (CALL f as) = (tempsExp f) `Set.union` Set.unions (map tempsExp as)
tempsExp (ESEQ s e) = tempsStm s `Set.union` (tempsExp e)

tempsStm :: Stm -> Set.Set Temp
tempsStm (MOVE d s) = (tempsExp d) `Set.union` (tempsExp s)
tempsStm (EXP e) = tempsExp e
tempsStm (JUMP e _) = tempsExp e
tempsStm (CJUMP _ e1 e2 _ _) = (tempsExp e1) `Set.union` (tempsExp e2)
tempsStm (SEQ s1 s2) = (tempsStm s1) `Set.union` (tempsStm s2)
tempsStm (LABEL _) = Set.empty
tempsStm NOP = Set.empty

cmmBinOp :: BinOp -> String
cmmBinOp PLUS = "+"
cmmBinOp MINUS = "-"
cmmBinOp MUL = "*"
cmmBinOp DIV = "/"
cmmBinOp AND = "&"
cmmBinOp OR = "|"
cmmBinOp LSHIFT = "<<"
cmmBinOp RSHIFT = ">>"
cmmBinOp ARSHIFT = error "ARSHIFT unsupported"
cmmBinOp XOR = "^"

cmmRelOp :: RelOp -> String
cmmRelOp EQ = "=="
cmmRelOp NE = "!="
cmmRelOp LT = "<"
cmmRelOp GT = ">"
cmmRelOp LE = "<="
cmmRelOp GE = ">="
cmmRelOp ULT = error "ULT unsupported"
cmmRelOp ULE = error "ULE unsupported"
cmmRelOp UGT = error "UGT unsupported"
cmmRelOp UGE = error "UGE unsupported"

declVar :: Temp -> Doc -> Doc
declVar t e = text "int32_t" <+> text (show t) <+> equals <+> e <> semi

-- returns (s,e), where 
-- s is a C statement and e is a pure C-expression
cmmExp :: MonadNameGen m => Exp -> m (Doc, Doc)
cmmExp (CONST i) = return (empty, int i)
cmmExp (NAME l) = return (empty, text "(int32_t)" <> text l)
cmmExp (TEMP t) = return (empty, text (show t))
cmmExp (BINOP o e1 e2) = 
  do (s, [ce1, ce2]) <- cmmExpSeq [e1, e2]
     return (s, parens (ce1 <+> text (cmmBinOp o) <+> ce2))
cmmExp (MEM e) = 
  do (s, ce) <- cmmExp e 
     return (s, text "MEM" <> (parens ce))
cmmExp (CALL (NAME l) as) = 
  do (sl, cel) <- cmmExpSeq as
     t <- nextTemp
     return (sl $$ declVar t (text l <> (parens $ hsep $ punctuate comma cel)), 
             text $ show t)
cmmExp (CALL _ _) =  error $ "CALL only implemented for named functions"
cmmExp (ESEQ s e) =
  do cs <- cmmStm s
     (cse, ce) <- cmmExp e
     return (cs $$ cse, ce)

cmmExpSeq :: MonadNameGen m => [Exp] -> m (Doc, [Doc])
cmmExpSeq [] = return (empty, [])
cmmExpSeq (e:es) =
  do (cs, ce) <- cmmExp e
     (css, ces) <- cmmExpSeq es
     if isEmpty css then
       return (cs $$ css, ce:ces)
      else 
       do t <- nextTemp
          return (cs $$ (declVar t ce) $$ css,
                  (text $ show t):ces)

cmmStm :: MonadNameGen m => Stm -> m Doc
cmmStm (MOVE (TEMP t) s) = 
  do (ss, cs) <- cmmExp s
     return $ ss $$ ((text $ show t) <+> equals <+> cs <> semi)
cmmStm (MOVE (MEM d) s) = 
  do (sd, cd) <- cmmExp d
     t <- nextTemp
     (ss, cs) <- cmmExp s
     return $ sd $$ (declVar t cd) $$ ss $$ 
              text "MEM" <> parens (text $ show t) <+> equals <+> cs <> semi
cmmStm (MOVE (ESEQ ds d) s) = cmmStm (SEQ ds (MOVE d s))
cmmStm e@(MOVE _ _) = error $ "Left-hand side of MOVE must be TEMP, MEM or ESEQ: " ++ (show e)
cmmStm (EXP e) =
  do (se, ce) <- cmmExp e
     return $ se $$ (ce <> semi)
cmmStm (JUMP (NAME l) _) = return $ text "goto" <+> (text l) <> semi
cmmStm (JUMP _ _) = error $ "JUMP only implemented for named locations"
cmmStm (CJUMP r e1 e2 lt lf) =
  do (s, [ce1, ce2]) <- cmmExpSeq [e1, e2]
     return $ s $$ 
              (text "if" 
               <+> (parens $ ce1 <+> text (cmmRelOp r) <+> ce2)
               <+> text "goto" <+> text lt <> semi 
               <+> text "else"
               <+> text "goto" <+> text lf <> semi)
cmmStm (SEQ s1 s2) =
  do cs1 <- cmmStm s1
     cs2 <- cmmStm s2
     return $ cs1 $$ cs2
cmmStm (LABEL l) = return $ text l <> colon <+> semi
cmmStm NOP = return empty

cmmFunction :: MonadNameGen m => String -> [Temp] -> Stm -> Exp -> m Doc
cmmFunction name params body e =
  let paramdecls = parens $ hsep $ punctuate comma 
                          $ map (\p -> text "int32_t" <+> (text $ show p)) params
      temps = tempsStm body `Set.union` (tempsExp e)
      locvars = (Set.toList temps) \\ params
  in do bodytext <- cmmStm body
        (se, ce) <- cmmExp e
        return $
          (text "int32_t" <+> (text name) <+> paramdecls <+> lbrace)
          $$ (nest 2 $ 
                (if locvars == [] then empty 
                 else text "int32_t" 
                      <+> (hsep $ punctuate comma (map (text.show) locvars)) <> semi)
                $$ bodytext $$ se
                $$ text "return" <+> ce <> semi)
          $$ rbrace

cmmFragment :: MonadNameGen m => Fragment DummyFrame Stm -> m Doc
cmmFragment frag = 
   cmmFunction (name (frame frag)) (fparams (frame frag))
              (body frag) (TEMP $ returnTemp (frame frag))

cmmDoc :: MonadNameGen m => [Fragment DummyFrame Stm] -> m Doc
cmmDoc frags = 
  do fs <- mapM cmmFragment frags
     return $ text "#include <stdint.h>"
              $$ text "#define MEM(x) *((int32_t*)(x))"
              $$ vcat fs
