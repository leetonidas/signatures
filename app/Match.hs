module Match (
        buildStats,
        canMatch,
        checkGraphs,
        checkList,
        intersectsConstrains,
        unionsConstrains,
        constrainsFromMatch,
        matchFunctions
    ) where

import Control.Arrow (second)
import Data.Maybe
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Text.Parsec

import Signatures.Function
import Signatures.Graph

import Utils

type BBChecker = BasicBlock -> BasicBlock -> Constrains -> (Bool, Constrains)

constrainNodeSize :: BBChecker
constrainNodeSize b1 b2 _ = (checkBBSize b1 b2, Map.empty)

checkCalls :: BBChecker
checkCalls b1 b2 cs | length bc1 /= length bc2 = (False, Map.empty)
                    | not . null $ Map.filter Set.null matched = (False, Map.empty)
                    | otherwise = (True, newCons)
                        where bc1 = bbcall b1
                              bc2 = bbcall b2
                              bc2Set = Set.fromList bc2
                              newCons = Map.unionWith Set.intersection (Map.fromList $ map (\a -> (a, bc2Set)) bc1) cs
                              matched = Map.filterWithKey (\ key -> const (key `elem` bc1)) newCons

compareBlocks :: BBChecker
compareBlocks b1 b2 cs | checkBBSize b1 b2 = checkCalls b1 b2 cs
                       | otherwise = (False, Map.empty)

advancedNodeCheck :: BBChecker -> Function -> Function -> Checker
advancedNodeCheck fun f1 f2 n1 n2 cs =case List.find (\ x -> bbstart x == n1) (funblocks f1) of
    Nothing -> (False, Map.empty)
    Just bb1 -> case List.find (\ x -> bbstart x == n2) (funblocks f2) of
        Nothing -> (False, Map.empty)
        Just bb2 -> fun bb1 bb2 cs

checkGraphs :: Function -> Function -> (Bool, Constrains)
checkGraphs f1 f2 = case concatMap
    (\ (n1,n2) -> matchGraphs (fromFunction f1) n1 (fromFunction f2) n2 (advancedNodeCheck compareBlocks f1 f2) (\ a b -> b))
    $ combinations (funstart f1) (funstart f2) of
        [] -> (False, Map.empty)
        xs -> (True, unionsConstrains $ map constrains xs)

checkListHelp :: Function -> [Function] -> [(Function, Constrains)] -> [Function] -> ([(Function, Constrains)], [Function])
checkListHelp _ [] match done = (match, done)
checkListHelp f (x:xs) match done = 
    if 
        hasMacht
    then 
        checkListHelp f xs ((x, matchConstrains):match) done 
    else 
        checkListHelp f xs match (x:done)
            where (hasMacht, matchConstrains) = checkGraphs f x

checkList :: Function -> [Function] -> ([(Function, Constrains)], [Function])
checkList f l = checkListHelp f l [] []

countUniqueCFG :: [Function] -> Int -> Int
countUniqueCFG [] i = i
countUniqueCFG [x] i = i + 1
countUniqueCFG (x:xs) i = countUniqueCFG (snd $ checkList x xs) (i + 1)

unionsConstrains :: [Constrains] -> Constrains
unionsConstrains = Map.unionsWith Set.union

intersectsConstrains :: [Constrains] -> Constrains
intersectsConstrains = Map.unionsWith Set.intersection
    
buildStats :: [Function] -> [(Int, Int, Int)]
buildStats = 
    map
        (\ (x, funs) -> (x, length funs, if x > 2 then countUniqueCFG funs 0 else length funs))
        . groupByBlockCount

matchFunctions :: [Function] -> [Function] -> [(Function, [(Function, Constrains)])]
matchFunctions fsigs ftars = map (\ x -> (x, fst $ checkList x ftars)) fsigs

constrainsFromMatch :: Function -> [Function] -> Constrains
constrainsFromMatch fun matches = Map.singleton (head $ funstart fun) . Set.fromList $ map (head . funstart) matches

constrainCallGraph :: Checker
constrainCallGraph base tar cs = maybe
    (True, cs)
    (\ v -> (tar `Set.member` v, cs))
    $ Map.lookup base cs

canMatch :: DiGraph -> DiGraph -> Function -> Function -> Bool
canMatch g1 g2 f1 f2 = localComplexity g1 (head $ funstart f1) == localComplexity g2 (head $ funstart f2)
