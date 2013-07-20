{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts  #-}
module Backend.LivenessAnalysis where

import Backend.MachineSpecifics
import Backend.X86Assem
--import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Names
import Data.List
import Debug.Trace

data Graph a = Graph [(Int,a)] [((Int,a), (Int,a))] | UGraph [a] [(a, a)] deriving (Show) -- (the Ints guarantee uniqueness, UGraph = graph with unique nodes)

succ' :: (Eq a) =>  Graph a ->  (Int,a) -> [(Int,a)]
succ' (Graph v e) node = [ y | (x,y) <- e, x==node ]
pred' :: (Eq a) =>  Graph a -> (Int,a) -> [(Int,a)]
pred' (Graph v e) node = [ x | (x,y) <- e, y==node ]
enumV :: [X86Assem] -> [(Int, X86Assem)]
enumV instrs = zip [1..] instrs

makeCFG :: [(Int, X86Assem)] -> [(Int, X86Assem)] -> Graph X86Assem -- CFG = Control flow graph
{- makeCFG instrs _ | trace ("makeCFG: "++show instrs) False = undefined {-%%%-} -}
makeCFG [] _ = Graph [] ([]++trace "foo" []) {-%%%-}
makeCFG (instr:rest) l = (Graph [instr] (makeEdges instr l)) `joinG` (makeCFG rest l) where
 makeEdges:: (Int, X86Assem) -> [(Int, X86Assem)] -> [((Int, X86Assem), (Int, X86Assem))]
 makeEdges (n, instr) list
   | isFallThrough instr = [((n,instr), (x,y)) | (x,y) <- list, x==n+1]
   | otherwise = [((n,instr), (x,y)) | (x,y) <- list, y `elem` (map LABEL (jumps instr))]

joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (v1++v2) (e1++e2)

makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp])
makeLG _ | trace "makeLG" False = undefined {-%%%-}
makeLG cfg@(Graph nodes edges) = makeLiveTemps cfg endlabel where
	endlabel = head $ [ (n, (instr, [])) | i@(n, instr) <- nodes, succ' cfg i == [] ]++trace "foo" []

makeLiveTemps :: Graph X86Assem -> (Int, (X86Assem, [Temp])) -> Graph (X86Assem, [Temp])
makeLiveTemps g i | trace ("makeLiveTemps: "++ show i ) False = undefined {-%%%-}
makeLiveTemps cfg (n, (instr, lives)) = foldl joinG (Graph newnodes []) (map (makeLiveTemps cfg) newnodes')
	where 
		newnodes =  map (\ (m, i) -> (m, (i, (nub (lives ++ (use i))) \\ (def i)))) (pred' cfg (n, instr)) --terminiert nicht
		newnodes' = trace (show newnodes) newnodes {-%%%-}

interferG :: Graph (X86Assem, [Temp]) -> Graph Temp
interferG _ | trace "interferG" False = undefined {-%%%-}
interferG (Graph nodes _) = UGraph (nub (concat (map snd (map snd nodes)))) ((foldl interf [] nodes)++trace "foo" [])

interf :: [(Temp, Temp)] -> (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
interf edges (n, (instr, temps)) = nub ([(x,y)| x <- temps, y <- temps, x<y] ++ edges)

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
makeInterferenceGraph _ | trace "makeInterferenceGraph" False = undefined {-%%%-}
makeInterferenceGraph (FragmentProc _ assems) = interferG $ makeLG (makeCFG (enumV assems) (enumV assems))



















