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
enumV instrs = zip [1..] instrs
joinG ::(Eq a) => Graph a -> Graph a -> Graph a
joinG (Graph v1 e1) (Graph v2 e2) = Graph (nub$ v1++v2) (nub$ e1++e2)

makeCFG :: [(Int, X86Assem)] -> [(Int, X86Assem)] -> Graph X86Assem -- Control flow graph
makeCFG [] _ = Graph [] []
makeCFG (instr:rest) l = (Graph [instr] (makeEdges instr l)) `joinG` (makeCFG rest l) where
 makeEdges:: (Int, X86Assem) -> [(Int, X86Assem)] -> [(Int, Int)]
 makeEdges (n, instr) list
   | isFallThrough instr = [(n, n+1)| (m, _)<-list, m==n+1]
   | otherwise = [(n, m) | (m,y) <- list, y `elem` (map LABEL (jumps instr))]


makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp]) -- determine the in- and out-sets for each node (slide 251) (first list is ins, second is outs)
makeLG (Graph nodes edges) = dropIns $ makeLG' (Graph nodesEmptyInOut edges) where
	dropIns :: Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp])
	dropIns (Graph nodes edges) = Graph (map (\ (n, (instr, ins, outs)) -> (n, (instr, outs))) nodes) edges
	nodesEmptyInOut = map (\ (n, instr) -> (n, (instr, [], []))) nodes
makeLG' :: Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp], [Temp])
makeLG' (Graph nodes edges) | trace ("makeLG:\n" ++ show nodes ++ "\n") False = undefined {-%%%-}
makeLG' lg@(Graph nodes edges) = if lg == newLg then lg else makeLG' newLg where
	newLg = foldr update lg nodes where
		update :: (Int, (X86Assem, [Temp], [Temp])) -> Graph (X86Assem, [Temp], [Temp]) -> Graph (X86Assem, [Temp], [Temp])
		update node@(n, (instr, ins, outs)) g@(Graph nodes edges) = Graph nodes' edges where
			nodes' = updatedNode : [node' | node'@(m, _) <- nodes, m/=n]
			updatedNode = (n, (instr, ins'', outs'))
			ins'  = nub $ (use instr) ++ ((nub outs) \\ (def instr))
			ins'' = trace (show n ++ "  " ++ show ins'  ++ show outs' ++ " u:" ++ show (use instr) ++ " d:" ++ show (def instr) ) ins'
			outs' = nub.concat $ [ins | (_, (_, ins, _)) <- (succs g node)]
{-			succs :: (Eq a) => Graph a -> [(Int, a)] -> [(Int, a)] -- closes the list under successors
			succs g@(Graph nodes edges) succlist = if succlist == succlist' then succlist else succs g succlist' where
				succlist' = nub.concat $ map (\ t@(n, _) -> t:[node | node@(m,_)<-nodes, (n,m) `elem` edges ]) succlist -}
			succs :: (Eq a) => Graph a -> (Int, a) -> [(Int, a)]
			succs g@(Graph nodes edges) (n,_) = [node | node@(m,_)<-nodes, (n,m) `elem` edges ]


















{-
makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp])
makeLG cfg@(Graph nodes edges) = Graph [(fst node, (snd node, makeLiveOuts node cfg)) | node <- nodes] []

makeLiveOuts :: (Int, X86Assem) -> Graph X86Assem -> [Temp]
makeLiveOuts node cfg@(Graph nodes edges) = nub [ temp | temp <- temps, node `elem` liveOutNodes temp cfg] where
  temps = nub.concat $ [ use instr | instr <- (map snd nodes)]
  liveOutNodes :: Temp -> Graph X86Assem -> [(Int, X86Assem)]
  --liveOutNodes  temp cfg@(Graph nodes edges) | trace ("liveOutNodes:\n" ++ show temp ++ "\n") False = undefined {-%%%-}
  liveOutNodes temp cfg@(Graph nodes edges) = nub.concat $ map (\n -> mkLiveOutNodes temp n cfg []) [ node | node <- nodes, temp `elem` use (snd node)] where
    mkLiveOutNodes :: Temp -> (Int, X86Assem) -> Graph X86Assem -> [(Int, X86Assem)] -> [(Int, X86Assem)]
    --mkLiveOutNodes temp node cfg alredayVisited | trace ("mkLiveOutNodes:\n" ++ show temp ++ "\n") False = undefined {-%%%-}
    mkLiveOutNodes temp node cfg alreadyVisited = nub $ (filter (isLiveOut temp) (pred' cfg node)) ++ concat (map (\n -> mkLiveOutNodes temp n cfg (node:alreadyVisited)) ((pred' cfg node) \\ alreadyVisited)) where
	  isLiveOut :: Temp -> (Int, X86Assem) -> Bool
	--  isLiveOut t a | trace ("isLiveOut:\n" ++ show t ++ "\n") False = undefined {-%%%-}
	  isLiveOut t (n, instr)
	    | t `elem` def instr = False
	    | otherwise = True
-}


{-
makeLG :: Graph X86Assem -> Graph (X86Assem, [Temp])
--makeLG g | trace ("makeLG:\n" ++ show g ++ "\n") False = undefined {-%%%-}
makeLG cfg@(Graph nodes edges) = makeright (makeLiveTemps cfg endlabel) where
	endlabel = (fst haad , (fst $ snd haad, snd $ snd haad, []))
	haad = head [ (n, (instr, [])) | i@(n, instr) <- nodes, succ' cfg i == [] ]
	makeright :: (Graph (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])) -> Graph (X86Assem, [Temp])
	--makeright f | trace ("makeright:\n" ++ show f ++ "\n") False = undefined {-%%%-}
	makeright (Graph nodes _ ) = Graph (map (\(n, (instr, lives, olds)) -> (n, (instr, lives))) nodes) []

makeLiveTemps :: Graph X86Assem -> (Int, (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])) -> Graph (X86Assem, [Temp], [(Int, (X86Assem, [Temp]))])
--makeLiveTemps cfg node@(n, (instr, lives, oldnodes)) | trace ("makeLiveTemps:\n" ++ show n ++ "\n") False = undefined {-%%%-}
makeLiveTemps cfg node@(n, (instr, lives, oldnodes)) = (foldl joinG (Graph [(node)] []) (map (makeLiveTemps cfg) newnodes)) where
    newnodes = [f x | x <- (pred' cfg (n, instr)), (fst (f x), (fst' $ snd (f x), snd' $ snd (f x))) `notElem` oldnodes]
    f = \ (m, i) -> (m, (i, nub ((use i) ++ (lives \\ def i)), nub $ (n, (instr, lives)):oldnodes))
    fst' :: (a, b, c) -> a
    fst' (x, y, z) = x
    snd' :: (a, b, c) -> b
    snd' (x, y, z) = y
-}













interferG :: Graph (X86Assem, [Temp]) -> Graph Temp
--interferG (Graph nodes edges) | trace ("interferG:\n" ++ show nodes ++ "\n") False = undefined {-%%%-}
interferG (Graph nodes _) = UGraph (nub.concat $ [ use instr ++ def instr | instr <- map fst (map snd nodes)]) [(x,y) | (x,y) <- (foldl interf [] nodes), (y,x) `notElem` (foldl interf [] nodes)]

interf :: [(Temp, Temp)] -> (Int, (X86Assem, [Temp])) -> [(Temp, Temp)]
interf edges (n, (instr, temps)) = if isMoveBetweenTemps instr == Nothing then nub ([(x,y)| x <- (def instr), y <- temps] ++ edges) else do
  nub ([(x,y)| x <- (def instr), y <- temps, y /= (snd $ fromJust (isMoveBetweenTemps instr))] ++ edges)

makeInterferenceGraph :: Fragment f [X86Assem] -> Graph Temp
--makeInterferenceGraph (FragmentProc f a) | trace ("makeInerferenceGraph:\n" ++ show a ++ "\n") False = undefined {-%%%-}
makeInterferenceGraph (FragmentProc _ assems) = interferG $ makeLG (makeCFG (enumV assems) (enumV assems))



















