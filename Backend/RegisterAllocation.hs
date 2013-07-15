{-# LANGUAGE FlexibleContexts #-}
module Backend.RegisterAllocation where 

import Backend.MachineSpecifics
import Backend.X86Assem
import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Liveness
import Data.List
import Control.Monad.Trans.State
import Backend.Names
import Data.Maybe
import Debug.Trace

allRegisters' = [eax, ebx, ecx, edx, esi, edi, esp, ebp]
generalPurposeRegisters' =[eax, ebx, ecx, edx, esi, edi]

deg' :: Temp -> [(Temp,Temp)] -> Int
deg' node edges = length.nub $ [ x | (x,y) <- edges, y==node]++[ x | (y,x) <- edges, y==node]

uncolored :: (Temp, Maybe Temp) -> Bool
uncolored (t, Nothing) = True
uncolored _ = False


build :: Graph Temp ->  ([(Temp, Maybe Temp)], [(Temp, Temp)])
build (UGraph temps edges) = (map f temps, edges) where
	f :: Temp -> (Temp, Maybe Temp)
	f t
	 | t `elem` allRegisters' = (t, Just t)
	 | otherwise = (t, Nothing)

	 
simplify :: ([(Temp, Maybe Temp)], [(Temp, Temp)]) -> State [Temp] ([(Temp, Maybe Temp)], [(Temp, Temp)]) 
simplify (nodes, edges) = do
  if lowDegNodes == [] then do return (nodes, edges) else do
    stack <- get
    put $ stack ++ map fst lowDegNodes
    simplify ((nodes \\ lowDegNodes), edges) where
      lowDegNodes = [ x | x <- nodes, deg' (fst x) edges < length generalPurposeRegisters', uncolored x]
	  
	  
selectSpill :: ([(Temp, Maybe Temp)], [(Temp, Temp)]) -> State [Temp] ([(Temp, Maybe Temp)], [(Temp, Temp)])
selectSpill (nodes, edges) = do
  if [ x | x <- nodes, uncolored x] == [] then do return (nodes, edges) else do
    stack <- get
    put $  (fst highestDegNode) : stack
    fp <- simplify (highestDegNode `delete` nodes, edges)
    selectSpill fp where
      highestDegNode = head [x | x <- nodes, uncolored x, deg' (fst x) edges == maxRemainingDeg] -- never empty due to if clause
      maxRemainingDeg = maximum [ deg' (fst x) edges | x <- nodes, uncolored x] -- always exists due to if clause


colorNode :: Temp -> (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) -> (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) 
colorNode node (interferG@(nodes, edges), properSpills)
  | possibleColors == [] = (interferG, node:properSpills) 
  | otherwise            = ((newNode:nodes,edges), properSpills) where
    possibleColors = [x | x <- map Just generalPurposeRegisters', x `notElem` neighborColors]
    neighborColors = [ snd neighbor | neighbor <- nodes, (fst neighbor) `elem` [ x | (x,y) <- edges, y==node]++[ x | (y,x) <- edges, y==node]]
    newNode = (node, head possibleColors)


select :: (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) -> State [Temp] (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp])
select iGAndSpills@(interferG@(nodes, edges), spills) = do
  stack <- get
  if stack == [] then do return iGAndSpills else do
    put $ tail stack
    select $ colorNode (head stack) iGAndSpills



generateSpillList :: Graph Temp -> [Temp]
generateSpillList g | trace ("generateSpillList:\n" ++ show g ++ "") False = undefined {-%%%-}
generateSpillList interferG = snd $ evalState ((simplify (build interferG)) >>= selectSpill >>= \graph -> select (graph, [])) []

coloredNodes :: Graph Temp -> [(Temp, Maybe Temp)]
{- coloredNodes g | trace ("coloredNodes:\n" ++ show g ++ "\n\n") False = undefined {-%%%-} -}
coloredNodes interferG = fst $ fst $ evalState ((simplify (build interferG)) >>= selectSpill >>= \graph -> select (graph, [])) []


regAlloc :: (MachineSpecifics m X86Assem X86Frame) => Fragment X86Frame [X86Assem] -> m (Fragment X86Frame [X86Assem])
regAlloc f | trace ("regAlloc:\n" ++ show f ++ "\n") False = undefined {-%%%-}
regAlloc fragment@(FragmentProc frame instrs) = do  
  if spills' == [] then return $ FragmentProc frame regAllocedAssems' else do
    (frame, assems) <- spill frame instrs spills
    regAlloc (FragmentProc frame assems) where
	  spills = generateSpillList.makeInterferenceGraph $ fragment
	  regAllocedAssems = [foldl (\instr -> \node -> (rename instr (\t -> if t == fst node then fromJust $ snd node else t))) instr (coloredNodes.makeInterferenceGraph $ fragment) | instr <- instrs] -- aufhübschen
	  regAllocedAssems' = trace ("rA:regAllocedAssems: " ++ show regAllocedAssems ++ "\n") regAllocedAssems {-%%%-}
	  spills' = trace ("rA:spills: " ++ show spills) spills {-%%%-}


-- moves zwischen nicht interferierenden temps eliminieren???
	-- falls nicht: zumindest moves von einem register auf sich selbst löschen !
-- interference graph falsch? vgl Tr2.java
-- liveness&registerallocation sind UNLESBAR

















