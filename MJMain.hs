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

tokenize = alexScanTokens

shw :: Fragment DummyFrame [X86Assem] -> String
shw (FragmentProc _ a) = concat (map (++"\n") (map show a))


main =	do
  input <- getContents
  -- um statt dem Dateiinhalt den Dateinamen (mit relativem Pfad) anzugeben: folgende Zeilen einkommentieren
--   args <- getArgs
--   input <- readFile (args !! 0)
--  input <- readFile "../MiniJava-Beispiele/Small/TrivialClass.java"

  let ast = parse . tokenize $ input
  let st  = symbolize ast
  let t   = typecheck' ast st
{-  let c  = runIdentity . withDummyMachine $ translate ast st >>= (\ tr -> canonicalizeStm tr) -}
  let c = runIdentity . withDummyMachine $ do 
  	tr <- translate' ast st
  	can <- mapM canonicalize tr
--  	can <- cmmDoc can
  	return $ can

  let d = runIdentity . withX86Machine $ mapM codeGen c 
--  let e = map makeInterferenceGraph d


--  putStrLn ( show c)
  putStrLn ".intel_syntax\n.global LMain\n\nLMain:\n"
  putStrLn (concat $ map shw d)


  if t then exitSuccess else exitFailure
