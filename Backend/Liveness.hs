{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables,FlexibleContexts,FlexibleInstances   #-}
module Backend.Liveness where
import Backend.MachineSpecifics
import Backend.X86Assem
import Backend.InstructionSelection
import Backend.Names
import Data.List
import qualified Data.Set as S
import Data.Maybe

data Graph a = AGraph [(a, Int, S.Set Int)] | UGraph [a] [(a, a)] deriving (Show, Eq) -- (the Ints guarantee uniqueness, UGraph = graph with unique nodes)

makeCFG :: [X86Assem] -> Graph X86Assem -- Control flow graph
makeCFG instrs = AGraph $ map (mkNodes instrs') instrs' where
	instrs' = (zip [1..] instrs)
	mkNodes instrs' (n, instr) = (instr, n , S.fromList $ fallThroughEdges ++ jumpEdges ) where
		fallThroughEdges = if isFallThrough instr && n < length instrs then [n+1] else []
		jumpEdges = [m | (m,y) <- instrs', y `elem` (map LABEL (jumps instr))]


makeLG :: Graph X86Assem -> Graph (X86Assem, S.Set Temp) -- determine the in- and out-sets for each node (slide 251) (first set is ins, second is outs)
makeLG (AGraph nodes) = dropIns $ makeLG' (AGraph nodesEmptyInOut) where
	dropIns (AGraph nodes) = AGraph (map (\ ((instr, ins, outs), n, adj) -> ((instr, outs), n, adj)) nodes)
	nodesEmptyInOut = map (\ (instr, n , adj) -> ((instr, S.empty, S.empty), n, adj)) nodes
makeLG' :: Graph (X86Assem, S.Set Temp, S.Set Temp) -> Graph (X86Assem, S.Set Temp, S.Set Temp)
makeLG' lg@(AGraph nodes) = if lg == newLg then lg else makeLG' newLg where
	newLg = foldr update (AGraph []) nodes
	update :: ((X86Assem, S.Set Temp, S.Set Temp), Int, S.Set Int) -> Graph (X86Assem, S.Set Temp, S.Set Temp) -> Graph (X86Assem, S.Set Temp, S.Set Temp)
	update ((instr, ins, outs), n, adj) (AGraph nodes) = AGraph $ ((instr, ins', outs'), n, adj):nodes where
		ins'  = (S.fromList . use $ instr) `S.union` (outs S.\\ (S.fromList . def $ instr))
		outs' = S.unions . S.toList $ S.map (\n -> S.unions [ins | ((_,ins,_), m,_)<-nodes, n==m] ) adj  -- the in-sets of all successors


interferG :: Graph (X86Assem, S.Set Temp) -> Graph Temp
interferG (AGraph nodes) = UGraph (nub.concat $ [ use instr ++ def instr | instr <- map fst (map fst3 nodes)]) (removeDuplicates.concat $ map interf nodes) where
	interf :: ((X86Assem, S.Set Temp), Int, S.Set Int) -> [(Temp, Temp)]
	interf ((instr, temps), n, _) = if isMoveBetweenTemps instr == Nothing then [(x,y)| x <- (def instr), y <- S.toList temps] else do
		[(x,y)| x <- (def instr), y <- S.toList temps, y /= (snd $ fromJust (isMoveBetweenTemps instr))]
	removeDuplicates ((x,y):t) = (if x /= y then [(x,y)] else []) ++ removeDuplicates (t\\[(x,y),(y,x)])
	removeDuplicates [] = []

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
makeInterferenceGraph (FragmentProc _ assems) = interferG.makeLG.makeCFG $ assems


-- helpers
fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x
