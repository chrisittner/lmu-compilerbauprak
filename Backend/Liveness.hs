{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts  #-}
module Backend.Liveness where

import Backend.MachineSpecifics
import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Names
import Data.List

data Graph a = Graph [(Int,a)] [((Int,a), (Int,a))] | UGraph [a] [(a, a)] deriving (Show)
{-
addV :: (Eq a) => Graph a -> (Int,a) -> Graph a
addV (Graph v e) node = Graph (node:v) e
addE :: (Eq a) => Graph a -> ((Int,a), (Int,a)) -> Graph a
addE (Graph v e) edge = Graph v (edge:e)
remV :: (Eq a) =>  Graph a ->  (Int,a) -> Graph a
remV (Graph v e) node = Graph (v\\[node]) e
remE :: (Eq a) =>  Graph a -> ((Int,a), (Int,a)) -> Graph a
remE (Graph v e) edge = Graph v (e\\[edge])
-}
succ' :: (Eq a) =>  Graph a ->  (Int,a) -> [(Int,a)]
succ' (Graph v e) node = [ y | (x,y) <- e, x==node ]
pred' :: (Eq a) =>  Graph a -> (Int,a) -> [(Int,a)]
pred' (Graph v e) node = [ x | (x,y) <- e, y==node ]


enumV :: [X86Assem] -> [(Int, X86Assem)]
enumV instrs = zip [1..] instrs

makeCFG :: [(Int, X86Assem)] ->[(Int, X86Assem)] -> Graph X86Assem -- CFG = Control flow graph
makeCFG [] _ = Graph [] []
makeCFG (instr:rest) l = (Graph [instr] (makeEdges instr l)) `joinG` (makeCFG rest l) where
 makeEdges:: (Int, X86Assem) -> [(Int, X86Assem)] -> [((Int, X86Assem), (Int, X86Assem))]
 makeEdges (n, instr) list
   | isFallThrough instr = [((n,instr), (x,y)) | (x,y) <- list, x==n+1]
   | otherwise = [((n,instr), (x,y)) | (x,y) <- list, y `elem` (map LABEL (jumps instr))]

joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (v1++v2) (e1++e2)

makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp])
makeLG cfg@(Graph nodes edges) = makeLiveTemps cfg endlabel where
	endlabel = head [ (n, (instr, [])) | i@(n, instr) <- nodes, succ' cfg i == [] ]

makeLiveTemps :: Graph X86Assem -> (Int, (X86Assem, [Temp])) -> Graph (X86Assem, [Temp])
makeLiveTemps cfg (n, (instr, lives)) = foldl joinG (Graph newnodes []) (map (makeLiveTemps cfg) newnodes)
	where newnodes =  map (\ (m, i) -> (m, (i, (lives ++ (use i)) \\ (def i)))) (pred' cfg (n, instr))

interferG :: Graph (X86Assem, [Temp]) -> Graph Temp
interferG (Graph nodes _) = UGraph (nub (concat (map snd (map snd nodes)))) (foldl interf [] nodes)

interf :: [(Temp, Temp)] -> (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
interf edges (n, (instr, temps)) = nub ([(x,y)| x <- temps, y <- temps, x<y] ++ edges)

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph (Temp)
makeInterferenceGraph (FragmentProc _ assems) = interferG $ makeLG (makeCFG (enumV assems) (enumV assems))

