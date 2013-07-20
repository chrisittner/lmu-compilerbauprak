{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts  #-}
module Backend.Liveness where

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
--enumV a | trace ("enumV:\n" ++ show a ++ "\n") False = undefined {-%%%-}
enumV instrs = zip [1..] instrs

makeCFG :: [(Int, X86Assem)] -> [(Int, X86Assem)] -> Graph X86Assem -- CFG = Control flow graph
--makeCFG g h | trace ("makeCFG:\n" ++ show g ++ "\n") False = undefined {-%%%-}
makeCFG [] _ = Graph [] []
makeCFG (instr:rest) l = (Graph [instr] (makeEdges instr l)) `joinG` (makeCFG rest l) where
 makeEdges:: (Int, X86Assem) -> [(Int, X86Assem)] -> [((Int, X86Assem), (Int, X86Assem))]
 --makeEdges node list | trace ("makeEdges:\n" ++ show node ++ "\n") False = undefined {-%%%-}
 makeEdges (n, instr) list
   | isFallThrough instr = [((n,instr), (x,y)) | (x,y) <- list, x==n+1]
   | otherwise = [((n,instr), (x,y)) | (x,y) <- list, y `elem` (map LABEL (jumps instr))]

joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (v1++v2) (e1++e2)

makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp])
--makeLG g | trace ("makeLG:\n" ++ show g ++ "\n") False = undefined {-%%%-}
makeLG cfg@(Graph nodes edges) = makeright (makeLiveTemps cfg endlabel) where
	endlabel = (fst haad , (fst $ snd haad, snd $ snd haad, []))
	haad = head [ (n, (instr, [])) | i@(n, instr) <- nodes, succ' cfg i == [] ]
	makeright :: (Graph (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])) -> Graph (X86Assem, [Temp])
--	makeright f | trace ("makeright:\n" ++ show f ++ "\n") False = undefined {-%%%-}
	makeright (Graph nodes _ ) = Graph (map (\(n, (instr, lives, olds)) -> (n, (instr, lives))) nodes) []

makeLiveTemps :: Graph X86Assem -> (Int, (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])) -> Graph (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])
--makeLiveTemps cfg node@(n, (instr, lives, oldnodes)) | trace ("makeLiveTemps:\n" ++ show n ++ "\n") False = undefined {-%%%-}
makeLiveTemps cfg node@(n, (instr, lives, oldnodes)) = (foldl joinG (Graph [(node)] []) (map (makeLiveTemps cfg) newnodes)) where
    newnodes = [f x | x <- (pred' cfg (n, instr)), (fst (f x), (fst' $ snd (f x), snd' $ snd (f x))) `notElem` oldnodes]
    f = \ (m, i) -> (m, (i, (nub (lives ++ (use i)) \\ (def i)), nub $ (n, (instr, lives)):oldnodes))
    fst' :: (a, b, c) -> a
    fst' (x, y, z) = x
    snd' :: (a, b, c) -> b
    snd' (x, y, z) = y
	
interferG :: Graph (X86Assem, [Temp]) -> Graph Temp
--interferG g | trace ("interferG:\n" ++ show g ++ "\n") False = undefined {-%%%-}
interferG (Graph nodes _) = UGraph (nub (concat (map snd (map snd nodes)))) (foldl interf [] nodes)

interf :: [(Temp, Temp)] -> (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
interf edges (n, (instr, temps)) = nub ([(x,y)| x <- temps, y <- temps, x<y] ++ edges)

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
--makeInterferenceGraph (FragmentProc f a) | trace ("makeInerferenceGraph:\n" ++ show a ++ "\n") False = undefined {-%%%-}
makeInterferenceGraph (FragmentProc _ assems) = interferG $ makeLG (makeCFG (enumV assems) (enumV assems))



















