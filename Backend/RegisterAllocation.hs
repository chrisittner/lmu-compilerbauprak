module RegisterAllocation where 

import qualified Backend.MachineSpecifics
import Backend.X86Assem
import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Liveness
import Data.List
import Control.Monad.Trans.State
import Backend.Names



deg' :: Temp -> [(Temp,Temp)] -> Int
deg' node edges = length ([ x | (x,y) <- edges, y==node]++[ x | (y,x) <- edges, y==node])

updatedDeg :: Temp -> [(Temp,Temp)] -> [(Temp, Maybe Temp)] -> Int
updatedDeg node edges treenodes = length (([ x | (x,y) <- edges, y==node]++[ x | (y,x) <- edges, y==node]) `intersect` map fst treenodes)

uncolored :: (Temp, Maybe Temp) -> Bool
uncolored (t, Nothing) = True
uncolored _ = False

build :: Graph Temp ->  ([(Temp, Maybe Temp)], [(Temp, Temp)])
build (UGraph temps edges) = (map f temps, edges) where
	f :: Temp -> (Temp, Maybe Temp)
	f t
	 | t `elem` allRegisters = (t, Just t)
	 | otherwise = (t, Nothing)

simplify :: ([(Temp, Maybe Temp)], [(Temp, Temp)]) -> State [Temp] ([(Temp, Maybe Temp)], [(Temp, Temp)]) 
simplify (nodes, edges) = do
  if lowDegNodes == [] then do return (nodes, edges) else do
    stack <- get
    put $ stack ++ map fst lowDegNodes
    simplify ((nodes \\ lowDegNodes), edges) where
      lowDegNodes = filter ((\ edges -> \ n@(t, reg) -> if (((updatedDeg t edges nodes) < length allRegisters) && (uncolored n)) then True else False ) edges) nodes


selectSpill :: ([(Temp, Maybe Temp)], [(Temp, Temp)]) -> State [Temp] ([(Temp, Maybe Temp)], [(Temp, Temp)])
selectSpill (nodes, edges) = do
  if [ updatedDeg x edges nodes | x <- (map fst nodes), x `elem` generalPurposeRegisters] == [] then do return (nodes, edges) else do
    stack <- get
    put $  (fst highestDegNode) `delete` stack
    fp <- simplify (highestDegNode `delete` nodes, edges)
    selectSpill fp where
      highestDegNode = head [x | x <- nodes, (updatedDeg (fst x) edges nodes) == maxRemainingDeg] -- never empty due to check before calling selctSpill
      maxRemainingDeg = maximum [ updatedDeg x edges nodes | x <- (map fst nodes), x `elem` generalPurposeRegisters] -- always exists due to check before calling selctSpill

allRegisters = [mkNamedTemp "dummy"] 
generalPurposeRegisters = [mkNamedTemp "dummy"]


colorNode :: Temp -> (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) -> (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) 
colorNode node (interferG@(nodes, edges), properSpills)
  | possibleColors == [] = (interferG, node:properSpills) 
  | otherwise                  = ((newNode:nodes,edges), properSpills) where
    possibleColors = [x | x <- map Just generalPurposeRegisters, x `notElem` neighborColors]
    neighborColors = [ snd neighbor | neighbor <- nodes, (fst neighbor) `elem` [ x | (x,y) <- edges, y==node]++[ x | (y,x) <- edges, y==node], snd neighbor /= Nothing]
    newNode = (node, head possibleColors)
  

select :: (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp]) -> State [Temp] (([(Temp, Maybe Temp)], [(Temp, Temp)]), [Temp])
select iGAndSpills@(interferG@(nodes, edges), spills) = do
  stack <- get
  if stack == [] then do return (interferG, snd (colorNode (head stack) iGAndSpills)) else do
    put $ tail stack
    select (interferG, snd (colorNode (head stack) iGAndSpills))
  


generateSpillList :: Graph Temp -> [Temp]
generateSpillList interferG = snd $ evalState ((simplify (build interferG)) >>= selectSpill >>= \graph -> select (graph, [])) []

  
regAlloc :: (MachineSpecifics m X86Assem f) => Fragment f [X86Assem] -> m Fragment f [X86Assem]
regAlloc fragment@(FragmentProc frame a) = do
  if spills == [] then return fragment else do
    (frame, assems) <- spill frame a spills
    regAlloc (Fragment frame, assems) where
      spills = generateSpillList.makeInterferenceGraph $ fragment
  
















