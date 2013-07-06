module RegisterAllocation where 

import Backend.MachineSpecifics
import Backend.X86Machine
import Backend.InstructionSelection
import Backend.Names
import Backend.Liveness
import Data.List
import Control.Monad.State



build :: Graph Temp ->  ([(Temp, Maybe String)], [(Temp, Temp)])
build (UGraph temps edges) = (map f temps, edges) where
	f :: Temp -> (Temp, Maybe String)
	f t@(NamedTemp s) = (t, Just s)
	f t = (t, Nothing)

simplify :: ([(Temp, Maybe String)], [(Temp, Temp)]) -> State [Temp] ([(Temp, Maybe String)], [(Temp, Temp)])
simplify (nodes, edges) = do
	let lowDegNodes = filter ((\ edges -> \ n@(t, reg) -> if ((deg' t edges) < 8 + length (([ x | (x,y) <- edges, y==t] ++ [ x | (y,x) <- edges, y==t]) `intersect` map fst nodes) && (uncolored n)) then True else False ) edges) nodes
	(_, stack) <- get
	put $ stack ++ lowDegNodes
	return ((nodes \\ lowDegNodes), edges)
	
	where
		deg' node edges = length [ x | (x,y) <- edges, y==node]
		uncolored (t, Nothing) = True
		uncolored _ = False
























