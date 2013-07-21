module Main where
import MJLex
import MJParse
import MJSymbolTreeMaker
import MJTypeChecker
import MJTranslate
import Backend.Names
import Backend.Tree
import Backend.MachineSpecifics
import Backend.DummyMachine
import Backend.Cmm
import Backend.Canonicalize
import Backend.X86Assem
import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Liveness
import Backend.RegisterAllocation
import Control.Monad
import Control.Monad.Identity
import System.IO
import System.Environment
import System.Exit
import qualified Debug.Trace as D

main =	do
  input <- getContents

--   args <- getArgs
--   input <- readFile (args !! 0)

  -- 1. Lex:
  let tokens = alexScanTokens input
  -- 2. Parse:
  let abstractSyntaxTree = parse tokens
  -- 3. Create symbol tree:
  let symbolTable = symbolize abstractSyntaxTree
  -- 4. Run type checker:
  let t = typecheck' abstractSyntaxTree symbolTable

  let assemString = runIdentity . withX86Machine $ do
  -- 5. Translate program to intermediate language fragments:
  	ilFragments <- translate' abstractSyntaxTree symbolTable
  -- 6. Canonicalize all fragments (includes blocking and tracing)
  	canFragments <- mapM canonicalize ilFragments
  -- 7. Translate to x86 assembly
  	x86Fragments <- mapM codeGen canFragments
  -- 8. Register Allocation
--  	finalFragments <- mapM regAlloc x86Fragments
--  	let tm (FragmentProc f a) = FragmentProc f (sseq a)
--  	t1<- cmmDoc ilFragments
--  	t2 <- cmmDoc (map tm canFragments)
--  	let a = D.trace (show t1) False
--  	let b = D.trace (show t2) False
--  	return t2
  -- 9. Code emission
--  	printAssembly x86Fragments
  	return x86Fragments





--  putStrLn (show $ map makeInterferenceGraph assemString)
  putStrLn (show $ map (\assems -> (makeLG (makeCFG (enumV assems) (enumV assems)))) (map f assemString)) where
    f :: Fragment X86Frame [X86Assem] -> [X86Assem]
    f (FragmentProc f a) = a


--  putStrLn assemString

