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
import Control.Monad
import System.IO
import System.Environment
import System.Exit

tokenize = alexScanTokens

main =	do
  input <- getContents
  -- um statt dem Dateiinhalt den Dateinamen (mit relativem Pfad) anzugeben: folgende Zeilen einkommentieren
  -- args <- getArgs
  -- input <- readFile (args !! 0)

  let ast = parse . tokenize $ input
  let st  = symbolize ast
  let t   = typecheck' ast st
  tr  <- translate' ast st
  c <- cmmDoc tr
--  putStrLn (show ast)
--  putStrLn ('\n': (show st) ++ "\n") 
  putStrLn ( show c)
  
  if t then exitSuccess else exitFailure
