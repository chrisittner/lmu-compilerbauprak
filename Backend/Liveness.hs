{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts,FlexibleInstances   #-}
module Backend.Liveness where
import Backend.MachineSpecifics
import Backend.X86Assem
import Backend.InstructionSelection
import Backend.Names
import Data.List
import qualified Data.Set as Set
import Data.Maybe

data Graph a = Graph [(Int,a)] [(Int, Int)] | UGraph [a] [(a, a)] deriving (Show) -- (the Ints guarantee uniqueness, UGraph = graph with unique nodes)
instance Eq (Graph (X86Assem, [Temp], [Temp])) where
  (Graph n e) == (Graph n' e') = (Set.fromList e == Set.fromList e') && foldl compareNodes True (zip n n') where
  	compareNodes :: Bool -> ((Int, (X86Assem, [Temp], [Temp])),(Int, (X86Assem, [Temp], [Temp]))) -> Bool
  	compareNodes b ((m,(i,ins, outs)), (m',(i',ins', outs'))) = b && m==m' && i==i' && (Set.fromList outs == Set.fromList outs') && (Set.fromList ins == Set.fromList ins') 

enumV :: [X86Assem] -> [(Int, X86Assem)]
enumV instrs = zip [1..] instrs
joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (v1++v2) (e1++e2)

makeCFG :: [(Int, X86Assem)] -> Graph X86Assem -- Control flow graph
makeCFG l = foldl (\ (Graph nodes edges) (n, instr) -> Graph nodes $ (if isFallThrough instr then [(n, n+1)| (m, _)<-nodes, m==n+1] else []) ++ [(n, m) | (m,y) <- l, y `elem` (map LABEL (jumps instr))] ++ edges ) (Graph l []) l

makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp]) -- determine the in- and out-sets for each node (slide 251) (first list is ins, second is outs)
makeLG (Graph nodes edges) = dropIns $ makeLG' (Graph nodesEmptyInOut edges) where
	dropIns :: Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp])
	dropIns (Graph nodes edges) = Graph (map (\ (n, (instr, ins, outs)) -> (n, (instr, outs))) nodes) edges
	nodesEmptyInOut = map (\ (n, instr) -> (n, (instr, [], []))) nodes
makeLG' :: Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp], [Temp])
makeLG' lg@(Graph nodes edges) = if lg == newLg then lg else makeLG' newLg where
	newLg = foldr update lg nodes where
		update :: (Int, (X86Assem, [Temp], [Temp])) -> Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp], [Temp])
		update node@(n, (instr, ins, outs)) g@(Graph nodes edges) = Graph nodes' edges where
			nodes' = updatedNode : [node' | node'@(m, _) <- nodes, m/=n]
			updatedNode = (n, (instr, ins', outs'))
			ins'  = nub $ (use instr) ++ ((nub outs) \\ (def instr))
			outs' = nub.concat $ [ins | (_, (_, ins, _)) <- (succs g node)]
			succs :: (Eq a) => Graph a -> (Int, a) -> [(Int, a)] -- returns immidate successors
			succs g@(Graph nodes edges) (n,_) = [node | node@(m,_)<-nodes, (n,m) `elem` edges ]

interferG :: Graph (X86Assem, [Temp]) -> Graph Temp
interferG (Graph nodes _) = UGraph (nub.concat $ [ use instr ++ def instr | instr <- map fst (map snd nodes)]) (removeDuplicates.concat $ map interf nodes) where
	interf :: (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
	interf (n, (instr, temps)) = if isMoveBetweenTemps instr == Nothing then [(x,y)| x <- (def instr), y <- temps] else do
		[(x,y)| x <- (def instr), y <- temps, y /= (snd $ fromJust (isMoveBetweenTemps instr))]
	removeDuplicates ((x,y):t) = (if x /= y then [(x,y)] else []) ++ removeDuplicates (t\\[(x,y),(y,x)])
	removeDuplicates [] = []

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
makeInterferenceGraph (FragmentProc _ assems) = interferG.makeLG.makeCFG.enumV $ assems


