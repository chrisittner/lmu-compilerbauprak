{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts,FlexibleInstances   #-}
module Backend.Liveness where
import Backend.MachineSpecifics
import Backend.X86Assem
import Backend.InstructionSelection
import Backend.Names
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Data.Maybe

data Graph a = Graph [(Int,a)] [(Int, Int)] | UGraph [a] [(a, a)] deriving (Show) -- (the Ints guarantee uniqueness, UGraph = graph with unique nodes)
instance Eq (Graph (X86Assem, [Temp], [Temp])) where
  (Graph n e) == (Graph n' e') = (Set.fromList e == Set.fromList e') && foldl compareNodes True (zip n n') where
  	compareNodes :: Bool -> ((Int, (X86Assem, [Temp], [Temp])),(Int, (X86Assem, [Temp], [Temp]))) -> Bool
  	compareNodes b ((m,(i,ins, outs)), (m',(i',ins', outs'))) = b && m==m' && i==i' && (Set.fromList outs == Set.fromList outs') && (Set.fromList ins == Set.fromList ins') 

{-succ' :: (Eq a) =>  Graph a ->  (Int,a) -> [(Int,a)]
succ' (Graph v e) node = [ y | (x,y) <- e, x==fst node ]
pred' :: (Eq a) =>  Graph a -> (Int,a) -> [(Int,a)]
pred' (Graph v e) node = [ x | (x,y) <- e, y==fst node ]-}
enumV :: [X86Assem] -> [(Int, X86Assem)]
--enumV i |trace ("enumV:"++show i ++"\n\n") False = undefined
enumV instrs = zip [1..] instrs
joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (nub$ v1++v2) (nub$ e1++e2)

makeCFG :: [(Int, X86Assem)] -> [(Int, X86Assem)] -> Graph X86Assem -- Control flow graph
makeCFG [] _ = Graph [] []
makeCFG ((n, instr):rest) l = (Graph [(n, instr)] (fallThroughEdge++jumpEdge)) `joinG` (makeCFG rest l) where
	fallThroughEdge = if isFallThrough instr then [(n, n+1)| (m, _)<-l, m==n+1] else []
	jumpEdge = [(n, m) | (m,y) <- l, y `elem` (map LABEL (jumps instr))]

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
interferG (Graph nodes _) = trace "interferG" $ UGraph (nub.concat $ [ use instr ++ def instr | instr <- map fst (map snd nodes)]) [(x,y) | (x,y) <- (foldl interf [] nodes), (y,x) `notElem` (foldl interf [] nodes)] where
	interf :: [(Temp, Temp)] -> (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
	interf edges (n, (instr, temps)) = if isMoveBetweenTemps instr == Nothing then nub ([(x,y)| x <- (def instr), y <- temps] ++ edges) else do
		nub ([(x,y)| x <- (def instr), y <- temps, y /= (snd $ fromJust (isMoveBetweenTemps instr))] ++ edges)

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
--makeInterferenceGraph (FragmentProc f a) | trace ("makeInerferenceGraph:\n" ++ show a ++ "\n") False = undefined {-%%%-}
makeInterferenceGraph (FragmentProc _ assems) = interferG $ makeLG (makeCFG (trace "enumV" (enumV assems)) (enumV assems))


