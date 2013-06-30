module Backend.Liveness where

import Backend.MachineSpecifics
import Backend.X86Machine
import Backend.InstructionSelection
import Data.List

data Graph a = Graph [(Int,a)] [((Int,a), (Int,a))]

addV :: (Eq a) => Graph a -> (Int,a) -> Graph a
addV (Graph v e) node = Graph (node:v) e
addE :: (Eq a) => Graph a -> ((Int,a), (Int,a)) -> Graph a
addE (Graph v e) edge = Graph v (edge:e)
remV :: (Eq a) =>  Graph a ->  (Int,a) -> Graph a
remV (Graph v e) node = Graph (v\\[node]) e
remE :: (Eq a) =>  Graph a -> ((Int,a), (Int,a)) -> Graph a
remE (Graph v e) edge = Graph v (e\\[edge])
succ' :: (Eq a) =>  Graph a ->  (Int,a) -> [(Int,a)]
succ' (Graph v e) node = [ y | (x,y) <- e, x==node ]
pred' :: (Eq a) =>  Graph a -> (Int,a) -> [(Int,a)]
pred' (Graph v e) node = [ x | (x,y) <- e, y==node ]


enumV :: [X86Assem] -> [(Int, X86Assem)]
enumV instrs = [(n,instr) |n <- [1..], instr <- instrs]

makeCFG :: [(Int, X86Assem)] ->[(Int, X86Assem)] -> Graph X86Assem -- CFG = Control flow graph
makeCFG [] _ = Graph [] []
makeCFG (instr:rest) l = (Graph [instr] (makeEdges instr l)) `joinG` (makeCFG rest l) where
 makeEdges:: (Int, X86Assem) -> [(Int, X86Assem)] -> [((Int, X86Assem), (Int, X86Assem))]
 makeEdges (n, instr) list
   | isFallThrough instr = [((n,instr), (x,y)) | (x,y) <- list, x==n+1]
   | otherwise = [((n,instr), (x,y)) | (x,y) <- list, y `elem` (map LABEL (jumps instr))]

joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (v1++v2) (e1++e2)

interferG :: (Eq a) => Graph a -> Graph a
interferG (Graph v e)  


