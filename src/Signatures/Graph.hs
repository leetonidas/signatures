module Signatures.Graph (
        DiGraph(..),
        GraphMatch(..),
        Node,
        diGraphToDot,
        localComplexity,
        matchGraphs,
    ) where

import qualified Data.IntMap.Lazy as IntMap
import qualified Data.List as List
import qualified Data.IntSet as IntSet
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
        (IntMap.assocs e)
    ++ "}"

successor :: Node -> DiGraph -> [Node]
successor n = IntMap.findWithDefault [] n . edges

expand :: Functor f =>  a -> f b -> f (a, b)
expand a bs = (\ x -> (a, x)) <$> bs
        
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