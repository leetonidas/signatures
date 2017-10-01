module Signatures.CFG (
    CFG(..),
    Constrains(..),
    DiGraph(..),
    anotatePaths,
    bestFit,
    checkCalls,
    cfgFromFunction,
    extractLeafs,
    matchCFG,
    matchNodes,
    successor,
    updateCFG
) where

import Debug.Trace

import Control.Arrow
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.List.Extra
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Data.Maybe

import Signatures.Function
import Signatures.Graph
import qualified Signatures.Queue as Queue

type Constrains = IntMap.IntMap IntSet.IntSet
type Checker = BasicBlock -> BasicBlock -> Constrains -> (Bool, Constrains)
type PathID = Int

data CFG = CFG {
entry   :: Node,
mapping :: IntMap.IntMap BasicBlock,
graph   :: DiGraph
} deriving Show

data CFGMatch = CFGMatch {
matches :: IntMap.IntMap Node,
constrains :: Constrains
} deriving Show

successor :: Node -> DiGraph -> [Node]
successor n = IntMap.findWithDefault [] n . edges

sanitize :: DiGraph -> DiGraph
sanitize dg = dg {edges = IntMap.map (filter (flip IntSet.member (nodes dg))) $ edges dg}

myFromJust :: String -> Maybe a -> a
myFromJust str dat = case dat of
    Nothing -> error str
    Just di -> di

cfgFromFunction :: Function -> CFG
cfgFromFunction fun = CFG
    (myFromJust "cfgFromFunction" $ IntMap.lookup (head $ funstart fun) bwdMapping)
    fwdMapping
    $ renameNodes (fromFunction fun) bwdMapping
        where fwdMapping = IntMap.fromList $ zip [1..] $ funblocks fun
              bwdMapping = IntMap.fromList $ zip (map bbstart $ funblocks fun) [1..]

buildPaths :: IntMap.IntMap [Node] -> IntMap.IntMap [PathID]
buildPaths = IntMap.foldrWithKey (\ pid nodes ma -> foldr (\ node -> IntMap.insertWith (++) node [pid]) ma nodes) IntMap.empty

collectTransitive :: [PathID] -> [PathID] -> IntMap.IntMap [PathID] -> [PathID]
collectTransitive [] col _ = col
collectTransitive base col chld = collectTransitive (concat $ mapMaybe (`IntMap.lookup` chld) base) (base ++ col) chld

anotatePaths' :: DiGraph -> Queue.Queue (PathID, Node) -> PathID -> IntMap.IntMap [PathID] -> IntMap.IntMap [PathID] -> IntMap.IntMap [Node] -> IntMap.IntMap [(Node, [PathID])] -> IntSet.IntSet -> (IntMap.IntMap [PathID], IntSet.IntSet)
anotatePaths' cfg queue nxtID anc children paths spl mark  | null queue = (buildPaths paths, mark)
                                                            | or loopTar = trace ("mark " ++ show n) $ anotatePaths' cfg pts nxtID anc children paths spl $ IntSet.insert n mark
                                                            | all (not . null) cutPaths = trace ("combine to " ++ show parents ++ " from " ++ show (pt:siblings) ++ " purging " ++ show toPurge) $
                                                                anotatePaths'
                                                                    cfg
                                                                    (Queue.insertFront (parents, n) $ Queue.filter (not . flip elem (siblings ++ toPurge) . fst) pts)
                                                                    nxtID
                                                                    (IntMap.withoutKeys anc toPurgeSet)
                                                                    (IntMap.delete parents . IntMap.withoutKeys children . IntSet.union toPurgeSet $ IntSet.fromList siblings)
                                                                    (IntMap.union (IntMap.fromList $ zip siblings newPaths) $ IntMap.withoutKeys paths toPurgeSet)
                                                                    newSplit mark
                                                            | otherwise = case sucs of
                                                                [] -> trace (show pt ++ " hit an end")
                                                                    $ anotatePaths' cfg pts nxtID anc children updatePaths spl mark
                                                                (s:[]) -> trace (show pt ++ " advances")
                                                                    $ anotatePaths' cfg (Queue.insert (pt, s) pts) nxtID anc children updatePaths spl mark
                                                                ss -> trace (show pt ++ " splits into " ++ show newBorn)
                                                                    $ anotatePaths'
                                                                        cfg
                                                                        (foldr (Queue.insert) pts $ zip newBorn ss)
                                                                        (nxtID + length sucs)
                                                                        (foldr (\ i -> IntMap.insert i family) anc newBorn)
                                                                        (IntMap.insert pt newBorn children)
                                                                        updatePaths
                                                                        (IntMap.insertWith (++) pt [(n, newBorn)] spl)
                                                                        mark
            where ((pt, n), pts) = {-trace (unlines [show queue, show anc, show paths, show spl]) $-} Queue.pop queue
                  myAnc = IntMap.findWithDefault [] pt anc
                  family = pt:myAnc
                  parents = head myAnc
                  loopTar = map (List.elem n . flip (IntMap.findWithDefault []) paths) family
                  siblings = filter (/= pt) . fromJust $ IntMap.lookup parents children
                  cutPaths = map (List.dropWhile (/= n) . flip (IntMap.findWithDefault []) paths) siblings
                  newPaths = map tail cutPaths
                  purgeBase = map (\ (s,p) -> (s, List.span (flip List.notElem p . fst) $ IntMap.findWithDefault [] s spl)) $ zip siblings newPaths
                  toPurge = collectTransitive (concatMap (concatMap snd . fst . snd) purgeBase) [] children
                  toPurgeSet = IntSet.fromList toPurge
                  newSplit = IntMap.union (IntMap.fromList $ map (second snd) purgeBase) $ IntMap.withoutKeys spl toPurgeSet
                  updatePaths = IntMap.insertWith (++) pt [n] paths
                  sucs = successor n cfg
                  newBorn = take (length sucs) [nxtID..]

anotatePaths :: CFG -> (IntMap.IntMap [PathID], IntSet.IntSet) 
anotatePaths cfg =
    anotatePaths'
        (graph cfg)
        (Queue.Queue [(1, entry cfg)] [])
        3
        (IntMap.singleton 1 [0])
        (IntMap.singleton 0 [1,2])
        IntMap.empty
        IntMap.empty
        IntSet.empty

updateCFG :: CFG -> IntMap.IntMap BasicBlock -> CFG
updateCFG (CFG ent ma (DiGraph no ed)) update =
    CFG 
        ent
        (IntMap.union update ma)
        . DiGraph
-- union prefers the first IntMap so order does matter here
            (IntSet.union no $ IntMap.keysSet update)
            $ IntMap.union (IntMap.map bbanc update) ed

deleteFromCFG :: CFG -> IntSet.IntSet -> CFG
deleteFromCFG (CFG ent ma (DiGraph no ed)) del =
    CFG
        ent
        (IntMap.withoutKeys ma del)
        . DiGraph
            (IntSet.difference no del)
                $ IntMap.withoutKeys ed del
              

getLeafs :: CFG -> IntSet.IntSet
getLeafs = IntMap.keysSet . IntMap.filter (null . bbanc) . mapping

nodesToInline :: IntSet.IntSet -> CFG -> IntSet.IntSet
nodesToInline leafs = IntMap.keysSet . IntMap.filter (any (`IntSet.member` leafs) . bbanc) . mapping

extractLeafs' :: CFG -> [Node] -> IntSet.IntSet -> Int -> [(Int, BasicBlock)] -> CFG
extractLeafs' cfg [] leafs _ upd = newCFG {mapping = IntMap.withoutKeys (mapping newCFG) leafs}
    where newCFG = flip deleteFromCFG leafs . updateCFG cfg $ IntMap.fromList upd
extractLeafs' cfg (n:ns) leafs maxIndexUsed upd =
    extractLeafs'
        cfg
        ns
        leafs
        (maxIndexUsed + length newBlocks)
        ((n,updatedBlock):newBlocks ++ upd)
    where orig = fromJust . IntMap.lookup n $ mapping cfg
          (toUpd, retain) = List.partition (`IntSet.member` leafs) $ bbanc orig
          newBlocks = zip [maxIndexUsed + 1..] $ mapMaybe (`IntMap.lookup` mapping cfg) toUpd
          updatedBlock = orig {bbanc = retain ++ map fst newBlocks}

extractLeafs :: CFG -> CFG
extractLeafs cfg = extractLeafs' cfg (IntSet.toList $ nodesToInline leafs cfg) leafs (fst . IntMap.findMax $ mapping cfg) []
    where leafs = getLeafs cfg

renameNodes :: DiGraph -> IntMap.IntMap Node -> DiGraph
renameNodes (DiGraph n e) mapping = DiGraph (IntSet.fromList $ IntMap.elems mapping) .
    IntMap.mapKeys update1 $ IntMap.map (map update) e
        where update1 = myFromJust "update1" . flip IntMap.lookup mapping
              update = (\ x -> myFromJust ("elem " ++ show x ++ "not found in " ++ show mapping) $ IntMap.lookup x mapping)

fromFunction :: Function -> DiGraph
fromFunction (Fun _ _ b) = sanitize . DiGraph
    (IntSet.fromList $ map bbstart b)
    . IntMap.fromList $ map (\ bl -> (bbstart bl, bbanc bl)) b

expand :: Functor f =>  a -> f b -> f (a, b)
expand a bs = (\ x -> (a, x)) <$> bs

checkCalls :: Checker
checkCalls b1 b2 cs | length bc1 /= length bc2 = (False, IntMap.empty)
                    | any (maybe 
                            False
                            (IntSet.null . IntSet.intersection bc2Set)
                            . flip IntMap.lookup cs)
                        bc1 = (False, IntMap.empty)
                    | otherwise = (True, newCons)
                        where bc1 = bbcall b1
                              bc2 = bbcall b2
                              bc2Set = IntSet.fromList bc2
                              newCons = foldr (\ cl cns -> IntMap.insertWith IntSet.intersection cl bc2Set cns) cs bc1

matchNodes :: CFG -> CFG -> Constrains -> Node -> Node -> (Bool, Constrains)
matchNodes fs ft cs ns nt = case IntMap.lookup ns (mapping fs) of
    Nothing -> (False, IntMap.empty)
    Just bs -> case IntMap.lookup nt (mapping ft) of
        Nothing -> (False, IntMap.empty)
        Just bt -> if checkBBSize bs bt then checkCalls bs bt cs else (False, IntMap.empty)

bestFit :: CFG -> CFG -> Node -> [Node] -> [Node]
bestFit fs ft ns nts = List.map fst $ List.sortOn (abs . (-) (bbinsCount bs) . bbinsCount . snd) eligible
    where bs = myFromJust "bf2" $ IntMap.lookup ns (mapping fs)
          comb = map (\ x -> (x, fromJust . IntMap.lookup x $ mapping ft)) nts
          eligible = filter
            (\ (_,bt) -> checkBBSize bs bt &&
                length (bbcall bs) == length (bbcall bt) &&
                length (bbanc bs) == length (bbanc bt))
            comb

matchCFGSkip :: CFG -> CFG -> CFGMatch -> Node -> Node -> [CFGMatch]
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
                                          suc2New = IntSet.toList . IntSet.difference (IntSet.fromList suc2) . IntSet.fromList . IntMap.elems $ matches ma

-- graph a -> graph b -> matched nodes
matchCFGHelp :: CFG -> CFG -> CFGMatch -> Node -> Node -> [CFGMatch]
matchCFGHelp fs ft ma ns nt  | not isMatch || length suc1 /= length suc2 = []
                             | IntSet.isSubsetOf tar (IntSet.fromList suc2) && length suc1New == length suc2New =
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
                                          tar = IntSet.fromList $ mapMaybe (flip IntMap.lookup (matches ma)) suc1
                                          suc1New = filter (flip IntMap.notMember (matches ma)) suc1
                                          suc2New = IntSet.toList . IntSet.difference (IntSet.fromList suc2) . IntSet.fromList . IntMap.elems $ matches ma
                                          newMa = CFGMatch (IntMap.insert ns nt $ matches ma) newCons

matchCFG :: CFG -> CFG -> [CFGMatch]
matchCFG fs ft = matchCFGHelp fs ft (CFGMatch IntMap.empty IntMap.empty) (entry fs) (entry ft)