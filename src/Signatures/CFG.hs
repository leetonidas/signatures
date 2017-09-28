module Signatures.CFG (
    CFG(..),
    DiGraph(..),
    bestFit,
    checkCalls,
    cfgFromFunction,
    matchCFG,
    matchNodes,
    successor,
) where

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe

import Signatures.Function

type Node = Int
type Constrains = IntMap.IntMap (Set.Set Int)
type Checker = BasicBlock -> BasicBlock -> Constrains -> (Bool, Constrains)
type ExpOrder = Node -> [Node] -> [Node]


data CFG = CFG {
entry   :: Node,
mapping :: IntMap.IntMap BasicBlock,
graph   :: DiGraph
} deriving Show

data DiGraph = DiGraph {
nodes :: Set.Set Node,
edges :: IntMap.IntMap [Node]
} deriving Show

data GraphMatch = GraM {
matches :: IntMap.IntMap Node,
constrains :: Constrains
} deriving Show

successor :: Node -> DiGraph -> [Node]
successor n = IntMap.findWithDefault [] n . edges

cfgFromFunction :: Function -> CFG
cfgFromFunction fun = CFG
    (fromJust $ IntMap.lookup (head $ funstart fun) bwdMapping)
    fwdMapping
    $ renameNodes (fromFunction fun) bwdMapping
        where fwdMapping = IntMap.fromList $ zip [1..] $ funblocks fun
              bwdMapping = IntMap.fromList $ zip (map bbstart $ funblocks fun) [1..]

renameNodes :: DiGraph -> IntMap.IntMap Node -> DiGraph
renameNodes (DiGraph n e) mapping = DiGraph (Set.fromList $ IntMap.elems mapping) .
    IntMap.mapKeys update $ IntMap.map (map update) e
        where update = fromJust . flip IntMap.lookup mapping

fromFunction :: Function -> DiGraph
fromFunction (Fun _ _ b) = DiGraph
    (Set.fromList $ map bbstart b)
    . IntMap.fromList $ map (\ bl -> (bbstart bl, bbanc bl)) b

expand :: Functor f =>  a -> f b -> f (a, b)
expand a bs = (\ x -> (a, x)) <$> bs

checkCalls :: Checker
checkCalls b1 b2 cs | length bc1 /= length bc2 = (False, IntMap.empty)
                    | any (maybe 
                            False
                            (null . Set.intersection bc2Set)
                            . flip IntMap.lookup cs)
                        bc1 = (False, IntMap.empty)
                    | otherwise = (True, newCons)
                        where bc1 = bbcall b1
                              bc2 = bbcall b2
                              bc2Set = Set.fromList bc2
                              newCons = foldr (\ cl cns -> IntMap.insertWith Set.intersection cl bc2Set cns) cs bc1

matchNodes :: CFG -> CFG -> Constrains -> Node -> Node -> (Bool, Constrains)
matchNodes fs ft cs ns nt = case IntMap.lookup ns (mapping fs) of
    Nothing -> (False, IntMap.empty)
    Just bs -> case IntMap.lookup nt (mapping ft) of
        Nothing -> (False, IntMap.empty)
        Just bt -> if checkBBSize bs bt then checkCalls bs bt cs else (False, IntMap.empty)

bestFit :: CFG -> CFG -> Node -> [Node] -> [Node]
bestFit fs ft ns nts = List.sortOn (abs . (-) (bbinsCount bs) . bbinsCount . fromJust . flip IntMap.lookup (mapping ft)) eligible
    where bs = fromJust $ IntMap.lookup ns (mapping fs)
          eligible = filter
            ((\ bt -> checkBBSize bs bt &&
                length (bbcall bs) == length (bbcall bt) &&
                length (bbanc bs) == length (bbanc bt))
            . fromJust . flip IntMap.lookup (mapping ft)) nts

matchCFGSkip :: CFG -> CFG -> GraphMatch -> Node -> Node -> [GraphMatch]
matchCFGSkip fs ft ma ns nt  | length suc1New /= length suc2New = []
                             | otherwise = if null suc1New then
                                        [ma]
                                    else
                                        concatMap
                                            (\x -> matchCFGSkip fs ft x ns nt)
                                            . concatMap
                                                (uncurry (matchCFGHelp fs ft ma))
                                                $ if null suc1New
                                                    then []
                                                    else expand (head suc1New) $ bestFit fs ft (head suc1New) suc2New
                                    where suc1 = successor ns $ graph fs
                                          suc2 = successor nt $ graph ft
                                          suc1New = filter (flip IntMap.notMember (matches ma)) suc1
                                          suc2New = Set.toList . Set.difference (Set.fromList suc2) . Set.fromList . IntMap.elems $ matches ma

-- graph a -> graph b -> matched nodes
matchCFGHelp :: CFG -> CFG -> GraphMatch -> Node -> Node -> [GraphMatch]
matchCFGHelp fs ft ma ns nt  | not isMatch || length suc1 /= length suc2 = []
                             | Set.isSubsetOf tar (Set.fromList suc2) && length suc1New == length suc2New =
                                    if null suc1New then
                                        [newMa]
                                    else
                                        concatMap
                                            (\ x -> matchCFGSkip fs ft x ns nt)
                                            . concatMap
                                                (uncurry (matchCFGHelp fs ft newMa))
                                                $ if null suc1New
                                                    then []
                                                    else expand (head suc1New) $ bestFit fs ft (head suc1New) suc2New
                             | otherwise = []
                                    where (isMatch, newCons) = matchNodes fs ft (constrains ma) ns nt
                                          suc1 = successor ns $ graph fs
                                          suc2 = successor nt $ graph ft
                                          tar = Set.fromList $ mapMaybe (flip IntMap.lookup (matches ma)) suc1
                                          suc1New = filter (flip IntMap.notMember (matches ma)) suc1
                                          suc2New = Set.toList . Set.difference (Set.fromList suc2) . Set.fromList . IntMap.elems $ matches ma
                                          newMa = GraM (IntMap.insert ns nt $ matches ma) newCons

matchCFG :: CFG -> CFG -> [GraphMatch]
matchCFG fs ft = matchCFGHelp fs ft (GraM IntMap.empty IntMap.empty) (entry fs) (entry ft)