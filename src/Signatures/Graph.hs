module Signatures.Graph (
        DiGraph(..),
        GraphMatch(..),
        Node,
        diGraphToDot,
        bfs,
        dfs,
        findCircles,
        localComplexity,
        matchGraphs,
        singleEntryNodes,
        singleExitNodes,
        transpose
    ) where

import Control.Monad.State

import qualified Data.IntMap.Lazy as IntMap
import qualified Data.List as List
import qualified Data.IntSet as IntSet
import qualified Signatures.Queue as Queue
import Data.Maybe

type Node = Int

data DiGraph = DiGraph {
    nodes :: IntSet.IntSet,
    edges :: IntMap.IntMap [Node]
} deriving Show

type GraphMatch = (IntMap.IntMap Node)

diGraphToDot :: DiGraph -> String
diGraphToDot (DiGraph n e) = 
    IntSet.foldl
        (\ s n -> s ++ "b_" ++ show n ++ ";\n")
        "digraph {\n"
        n
    ++ List.foldl
        (\ s (a,b) -> s ++ "b_" ++ show a ++ " -> b_" ++ show b ++ ";\n")
        "\n"
        (concatMap (\ (a,b) -> [(a,c) | c <- b]) $ IntMap.assocs e)
    ++ "}"

transpose :: DiGraph -> DiGraph
transpose dg = dg {edges = IntMap.foldrWithKey (\ k ls mp -> foldr (flip (IntMap.insertWith (++)) [k]) mp ls) IntMap.empty $ edges dg}

singleExitNodes :: DiGraph -> IntSet.IntSet
singleExitNodes = IntMap.keysSet . IntMap.filter ((==) 1 . length) . edges

singleEntryNodes :: DiGraph -> IntSet.IntSet
singleEntryNodes = singleExitNodes . transpose

successor :: Node -> DiGraph -> [Node]
successor n = IntMap.findWithDefault [] n . edges

expand :: Functor f =>  a -> f b -> f (a, b)
expand a bs = (\ x -> (a, x)) <$> bs

dfs :: DiGraph -> (Node -> State a ()) -> [Node] -> IntSet.IntSet -> State a ()
dfs gra fun lst vst | null lst = return ()
                    | hd `IntSet.member` vst = dfs gra fun tl vst
                    | otherwise = fun hd >> dfs gra fun newL (IntSet.insert hd vst)
    where (hd, tl) = fromJust $ List.uncons lst
          newL = successor hd gra ++ lst


bfs :: DiGraph -> (Node -> State a ()) -> Queue.Queue Node -> IntSet.IntSet -> State a ()
bfs gra fun queue vst   | null queue = return ()
                        | hd `IntSet.member` vst = bfs gra fun tl vst
                        | otherwise = fun hd >> bfs gra fun newQ (IntSet.insert hd vst)
    where (hd, tl) = Queue.pop queue
          newQ = foldr (Queue.insert) tl $ successor hd gra

-- todo complet rewrite
findCircles' :: DiGraph -> IntSet.IntSet -> [Node] -> (IntSet.IntSet, IntSet.IntSet)
findCircles' gra mark nods | n `IntSet.member` mark = (mark, mempty)
                           | otherwise = foldr (\ nd (m, b) -> let (m', b') = findCircles' gra m (nd:nods) in (m', IntSet.union b b')) (mark', bck) nxt
    where n = head nods
          nxt = successor n gra
          mark' = IntSet.insert n mark
          bck = IntSet.fromList $ filter (`List.elem` nods) nxt

findCircles :: DiGraph -> Node -> IntSet.IntSet
findCircles gra n = snd $ findCircles' gra mempty [n]

-- graph a -> graph b -> matched nodes
matchGraphs :: DiGraph -> DiGraph -> GraphMatch -> Node -> Node -> [GraphMatch]
matchGraphs fs ft ma ns nt | length suc1 /= length suc2 = []
                           | IntSet.isSubsetOf tar (IntSet.fromList suc2) && length suc1New == length suc2New =
                                if null suc1New then
                                    [newMa]
                                else
                                    concatMap
                                        (\ x -> matchGraphs fs ft x ns nt)
                                        . concatMap
                                            (uncurry (matchGraphs fs ft newMa))
                                            $ if null suc1New
                                                then []
                                                else expand (head suc1New) suc2New
                           | otherwise = []
                                    where suc1 = successor ns fs
                                          suc2 = successor nt ft
                                          tar = IntSet.fromList $ mapMaybe (flip IntMap.lookup ma) suc1
                                          suc1New = filter (flip IntMap.notMember ma) suc1
                                          suc2New = IntSet.toList . IntSet.difference (IntSet.fromList suc2) . IntSet.fromList $ IntMap.elems ma
                                          newMa = IntMap.insert ns nt ma

extractSubgraph :: DiGraph -> Node -> Int -> DiGraph
extractSubgraph gr base dps = DiGraph nodes $ IntMap.map (filter (flip IntSet.member nodes)) $ IntMap.restrictKeys (edges gr) nodes
    where nodes = IntSet.unions . take dps . iterate (IntSet.foldr (\ b c -> IntSet.union c . IntSet.fromList $ successor b gr) IntSet.empty) $ IntSet.singleton base

localComplexity :: DiGraph -> Node -> [Int]
localComplexity g1 = List.sort . map (length . flip successor g1) . flip successor g1