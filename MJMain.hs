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
import Control.Monad
import Control.Monad.Identity
import System.IO
import System.Environment
import System.Exit

main =	do
  input <- getContents

-- um statt dem Dateiinhalt den Dateinamen (mit relativem Pfad) anzugeben: folgende Zeilen einkommentieren:
--   args <- getArgs
--   input <- readFile (args !! 0)

  -- 1. Lex:
  let tokens = alexScanTokens input
  -- 2. Parse:
  let abstractSyntaxTree = parse tokens
  -- 3. Create symbol tree:
  let symbolTable  = symbolize abstractSyntaxTree
  -- 4. Run type checker:
  let t = typecheck' abstractSyntaxTree symbolTable

  let canFragments = runIdentity . withX86Machine $ do
  -- 5. Translate program to intermediate language fragments:
  	ilFragments <- translate' abstractSyntaxTree symbolTable
  -- 6. Canonicalize all fragments (includes blocking and tracing)
  	mapM canonicalize ilFragments

  let assemString = runIdentity . withX86Machine $ do
  -- 7. Translate to x86 assembly
  	x86Fragments <- mapM codeGen canFragments
  -- 8. Register Allocation
  	finalFragments <- regAlloc x86Fragments
  -- 9. Code emission
  	printAssembly finalFragments


-- Todo :
--- IMMER X86Frame statt DummyFram verwenden, sonst muss man irgendwann die params verschieben?



  putStrLn (assemString)
