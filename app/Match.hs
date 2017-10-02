module Match (
        buildStats,
        canMatch,
        checkAll,
        checkGraphs,
--        checkList,
--        intersectsConstrains,
--        unionsConstrains,
--        constrainsFromMatch,
        matchFunctions,
        matchFunctions2,
        matchBatchCFG,
        funFromCFGmatch
    ) where

import Control.Arrow (second)
import Data.Maybe
import qualified Data.IntMap.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Text.Parsec

import Signatures.Function
import Signatures.CFG
import Signatures.Graph

import Utils

checkGraphs :: Function -> Function -> Bool
checkGraphs f1 f2 = not . null . matchCFG (cfgFromFunction f1) $ cfgFromFunction f2

checkAll :: Function -> [Function] -> ([Function], [Function])
checkAll fun = List.partition (checkGraphs fun)

{-
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
-}

countUniqueCFG :: [Function] -> Int -> Int
countUniqueCFG [] i = i
countUniqueCFG [x] i = i + 1
countUniqueCFG (x:xs) i = countUniqueCFG (snd $ checkAll x xs) (i + 1)

{-
unionsConstrains :: [Constrains] -> Constrains
unionsConstrains = Map.unionsWith Set.union

intersectsConstrains :: [Constrains] -> Constrains
intersectsConstrains = Map.unionsWith Set.intersection
-}

buildStats :: [Function] -> [(Int, Int, Int)]
buildStats = 
    map
        (\ (x, funs) -> (x, length funs, if x > 2 then countUniqueCFG funs 0 else length funs))
        . groupByBlockCount

{-
matchFunctions :: [Function] -> [Function] -> [(Function, [(Function, Constrains)])]
matchFunctions fsigs ftars = map (\ x -> (x, fst $ checkList x ftars)) fsigs
-}

matchFunctions :: [Function] -> [Function] -> [(Function, [Function])]
matchFunctions fsigs ftars = map (\ x -> (x, fst $ checkAll x ftars)) fsigs

matchFunctions2 :: [Function] -> [Function] -> Bool -> [(Function, [Function])]
matchFunctions2 fsigs ftars nrm = map (\ (a,b) -> (a, map fst $ filter (not . null . matchCFG b . snd) cfgTars)) cfgSgis
    where cfgSgis = map (\ a -> (a, (if nrm then combineNodes . extractLeafs else id) $ cfgFromFunction a)) fsigs
          cfgTars = map (\ a -> (a, (if nrm then combineNodes . extractLeafs else id) $ cfgFromFunction a)) ftars

matchBatchCFG :: [(Function, CFG)] -> [(Function, CFG)] -> [(Function, [Function])]
matchBatchCFG fsigs ftars = map (\ s -> let sCfg = snd s in (fst s, map fst $ filter (not . null . matchCFG sCfg . snd) ftars)) fsigs

funFromCFGmatch :: CFG -> Map.IntMap Function -> Function
funFromCFGmatch cfg funMap = fromJust $ Map.lookup (bbstart . fromJust . Map.lookup (entry cfg) $ mapping cfg) funMap

{-
constrainsFromMatch :: Function -> [Function] -> Constrains
constrainsFromMatch fun matches = Map.singleton (head $ funstart fun) . Set.fromList $ map (head . funstart) matches
-}

{-
constrainCallGraph :: Checker
constrainCallGraph base tar cs = maybe
    (True, cs)
    (\ v -> (tar `Set.member` v, cs))
    $ Map.lookup base cs
-}

canMatch :: DiGraph -> DiGraph -> Function -> Function -> Bool
canMatch g1 g2 f1 f2 = localComplexity g1 (head $ funstart f1) == localComplexity g2 (head $ funstart f2)
