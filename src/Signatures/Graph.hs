module Signatures.Graph (
        Constrains(..),
        Checker(..),
        DiGraph(..),
        GraphMatch(..),
        Node,
        combTest,
        combinations,
        diGraphToDot,
        localComplexity,
        matchGraphs,
        matchGraphsWithConstrains
    ) where

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe

type Node = Int
type Constrains = IntMap.IntMap (Set.Set Int)
type Checker = Node -> Node -> Constrains -> (Bool, Constrains)
type ExpOrder = Node -> [Node] -> [Node]

data DiGraph = DiGraph {
    nodes :: Set.Set Node,
    edges :: Set.Set (Node, Node)
} deriving Show

data GraphMatch = GraM {
    matches :: Set.Set (Node, Node),
    constrains :: Constrains
} deriving Show

diGraphToDot :: DiGraph -> String
diGraphToDot (DiGraph n e) = 
    Set.foldl
        (\ s n -> s ++ "b_" ++ show n ++ ";\n")
        "digraph {\n"
        n
    ++ Set.foldl
        (\ s (a,b) -> s ++ "b_" ++ show a ++ " -> b_" ++ show b ++ ";\n")
        "\n"
        e
    ++ "}"

successor :: Node -> DiGraph -> Set.Set Node
successor n =
    Set.map snd 
        . Set.filter
            ((==) n . fst)
            . edges


successorNew :: Eq c => ((a, b) -> c) -> [(a,b)] -> [c] -> [c]
successorNew f ls = filter (`notElem` map f ls)

combinations :: [a] -> [b] -> [(a,b)]
combinations x y = concatMap (\ z -> map (\ b -> (z,b)) y) x

combTest :: [a] -> [b] -> [(a,b)]
combTest [] _ = []
combTest (a:_) y = map (\ b -> (a,b)) y

selectiveCombine :: ExpOrder -> [Node] -> [Node] -> [(Node, Node)]
selectiveCombine _ [] = const []
selectiveCombine ord as = combTest as . ord (head as)  

getMatches :: Node -> ((Node, Node) -> Node) -> GraphMatch -> Set.Set (Node, Node)
getMatches x f= Set.filter ((==) x . f) . matches

matchGraphsSkip :: DiGraph -> DiGraph -> GraphMatch -> Checker -> ExpOrder -> Node -> Node -> [GraphMatch]
matchGraphsSkip g1 g2 ma fu ord n1 n2 | length suc1New /= length suc2New = []
                                  | otherwise = if null suc1New then
                                            [ma]
                                        else
                                            concatMap
                                                (\x -> matchGraphsSkip g1 g2 x fu ord n1 n2)
                                                . concatMap
                                                    (uncurry (matchGraphsHelp g1 g2 ma fu ord))
                                                    . (selectiveCombine ord)
                                                        (Set.toList suc1New)
                                                        $ Set.toList suc2New
                                    where suc1 = successor n1 g1
                                          suc2 = successor n2 g2
                                          suc1New = Set.difference suc1 $ Set.map fst (matches ma)
                                          suc2New = Set.difference suc2 $ Set.map snd (matches ma)

-- graph a -> graph b -> matched nodes
matchGraphsHelp :: DiGraph -> DiGraph -> GraphMatch -> Checker -> ExpOrder -> Node -> Node -> [GraphMatch]
matchGraphsHelp g1 g2 ma fu ord n1 n2   | not isMatch || length suc1 /= length suc2 = []
                                    | Set.isSubsetOf tar suc2 && length suc1New == length suc2New =
                                        if null suc1New then
                                            [newMa]
                                        else
                                            concatMap
                                                (\ x -> matchGraphsSkip g1 g2 x fu ord n1 n2)
                                                . concatMap
                                                    (uncurry (matchGraphsHelp g1 g2 newMa fu ord))
                                                    . (selectiveCombine ord) (Set.toList suc1New)
                                                        $ Set.toList suc2New
                                    | otherwise = []
                                        where (isMatch, newCons) = fu n1 n2 $ constrains ma
                                              suc1 = successor n1 g1
                                              suc2 = successor n2 g2
                                              tar = Set.map snd $ getMatches n1 fst ma
                                              suc1New = Set.difference suc1 $ Set.map fst (matches ma)
                                              suc2New = Set.difference suc2 $ Set.map snd (matches ma)
                                              newMa = GraM (Set.insert (n1, n2) $ matches ma) newCons

localComplexity :: DiGraph -> Node -> [Int]
localComplexity g1 = List.sort . map (length . flip successor g1) . Set.toList . flip successor g1

expand :: Functor f =>  a -> f b -> f (a, b)
expand a bs = (\ x -> (a, x)) <$> bs

combHint :: Constrains -> [Node] -> [Node] -> [(Node, Node)]
combHint cs n1s n2s = 
    case List.sortOn
        (length . snd)
        . IntMap.toList $ IntMap.filterWithKey
            (const . flip elem n1s)
            cs of
    [] -> combTest n1s n2s
    (base, cons):xs -> expand base $ Set.toList cons

matchGraphsHint :: DiGraph -> DiGraph -> GraphMatch -> Node -> Node -> [GraphMatch]
matchGraphsHint g1 g2 ma n1 n2   | length suc1 /= length suc2 = []
                                    | Set.isSubsetOf tar suc2 && length suc1New == length suc2New =
                                        if null suc1New then
                                            [newMa]
                                        else
                                            concatMap
                                                (\ x -> matchGraphsHint g1 g2 x n1 n2)
                                                . concatMap
                                                    (uncurry (matchGraphsHint g1 g2 newMa))
                                                    . combHint (constrains ma) (Set.toList suc1New)
                                                        $ Set.toList suc2New
                                    | otherwise = []
                                        where suc1 = successor n1 g1
                                              suc2 = successor n2 g2
                                              tar = Set.map snd $ getMatches n1 fst ma
                                              suc1New = Set.difference suc1 $ Set.map fst (matches ma)
                                              suc2New = Set.difference suc2 $ Set.map snd (matches ma)
                                              newMa = ma {matches = Set.insert (n1, n2) $ matches ma}

matchGraphs :: DiGraph -> Node -> DiGraph -> Node -> Checker -> ExpOrder -> [GraphMatch]
matchGraphs g1 n1 g2 n2 fu ord = matchGraphsHelp g1 g2 (GraM Set.empty IntMap.empty) fu ord n1 n2

matchGraphsWithConstrains :: DiGraph -> Node -> DiGraph -> Node -> Constrains -> [GraphMatch]
matchGraphsWithConstrains g1 n1 g2 n2 cons = matchGraphsHint g1 g2 (GraM Set.empty cons) n1 n2