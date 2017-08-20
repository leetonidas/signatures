module Signatures.Function (
        BasicBlock(..),
        Function(..),
        buildFun,
        collectCalls,
        getFunName,
        inlineLeaves,
        mergeCandidats,
        mergeNodes,
        nEntryBlocks,
        normalize,
        removeBlocksFrom,
        singleExitBlocks,
        toDot
    ) where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Maybe
import Numeric (showHex)

import Signatures.Nuc

data BasicBlock = BB {
    bbstart :: Int,
    bbinsCount :: Int,
    bbanc :: [Int],
    bbcall :: [Int]
} deriving Show

data Function = Fun {
    funstart :: [Int],
    funname :: String,
    funblocks :: [BasicBlock]
} deriving Show

getFunName :: Function -> String
getFunName (Fun s n _) = if not $ null n then n else "fun_" ++ showHex (minimum s) ""

toDot :: Function -> String
toDot (Fun s n b) = unlines $ ["digraph " ++ getFunName (Fun s n b) ++ "{"]
    ++ map (\ x -> "b_" ++ show (bbstart x) ++ ";") b 
    ++ concatMap (\ x -> map (\y -> "b_" ++ show (bbstart x) ++ " -> b_" ++ show y ++ ";") $ bbanc x) b
    ++ ["}"]

sanitize :: Function -> Function
sanitize fun = fun {
    funblocks = map 
        (\ x -> let anc = bbanc x in
            x {bbanc = List.nub $ filter (`elem` nodeaddr) anc
            , bbcall = List.nub $ filter (`notElem` nodeaddr) anc})
        $ funblocks fun}
                where nodeaddr = map bbstart $ funblocks fun

nEntryBlocks :: Int -> [Int] -> [BasicBlock] -> [BasicBlock]
nEntryBlocks 0 et bs = let tars = List.nub $ et ++ concatMap bbanc bs in filter (flip notElem tars . bbstart) bs
nEntryBlocks n et bs = filter (flip elem (concat
        . filter
            ((==) n . length)
            . List.group
                . List.sort
                    $ et ++ concatMap bbanc bs)
    . bbstart) bs

singleExitBlocks :: [BasicBlock] -> [BasicBlock]
singleExitBlocks = filter ((==) 1 . length . bbanc)

removeUnreachableBlocks :: [Int] -> [BasicBlock] -> [BasicBlock]
removeUnreachableBlocks et bs = nEntryBlocks 0 et bs `removeBlocksFrom` bs

inlineLeaves :: [Int] -> [BasicBlock] -> [BasicBlock]
inlineLeaves et bs = removeUnreachableBlocks et (map (uncurry mergeNode) pairs ++ removeBlocksFrom (map fst pairs) bs)
    where pairs = inlineLeavePairs bs

inlineLeavePairs :: [BasicBlock] -> [(BasicBlock, BasicBlock)]
inlineLeavePairs bs =
    mapMaybe 
        (\x -> maybe
            Nothing
            (curry Just x)
            $ List.find
                ((==) (head $ bbanc x) . bbstart)
                leaves)
        $ singleExitBlocks bs
            where leaves = filter (null . bbanc) bs

mergeCandidats :: [Int] -> [BasicBlock] -> [(BasicBlock, BasicBlock)]
mergeCandidats et bs = mapMaybe
    (\ x -> maybe
        Nothing
        (\ y -> Just (x, y))
        $ List.find ((==) (head $ bbanc x) . bbstart) singleEntry)
    singleExit
            where singleEntry = nEntryBlocks 1 et bs
                  singleExit = singleExitBlocks bs

removeBlocksFrom :: [BasicBlock] -> [BasicBlock] -> [BasicBlock]
removeBlocksFrom x = 
    removeSortedBlocksFromSorted []
        (List.sortOn bbstart x)
        . List.sortOn bbstart

removeSortedBlocksFromSorted :: [BasicBlock] -> [BasicBlock] -> [BasicBlock] -> [BasicBlock]
removeSortedBlocksFromSorted ok [] b = ok ++ b
removeSortedBlocksFromSorted ok _ [] = ok
removeSortedBlocksFromSorted ok (x:xs) (b:bs) = if bbstart x == bbstart b
    then removeSortedBlocksFromSorted ok xs bs
    else removeSortedBlocksFromSorted (b:ok) (x:xs) bs

mergeNodes :: [Int] -> [BasicBlock] -> [BasicBlock]
mergeNodes entryNodes blocks =
    let toMerge = mergeCandidats entryNodes blocks in
        case filter (flip notElem (map (bbstart . snd) toMerge) . bbstart . fst) toMerge of 
            [] -> blocks
            mc -> mergeNodes entryNodes (map (uncurry mergeNode) mc
                ++ removeBlocksFrom (concatMap (\ (a,b) -> [a,b]) mc) blocks)

mergeNode :: BasicBlock -> BasicBlock -> BasicBlock
mergeNode (BB s i _ c) (BB _ i2 a c2) = BB s (i + i2) a (c ++ c2)

normalize :: Function -> Function
normalize fun = fun {funblocks = mergeNodes (funstart fun) . inlineLeaves (funstart fun) $ funblocks fun}

buildBBHelp :: BasicBlock -> [BBprop] -> BasicBlock
buildBBHelp b [] = b
buildBBHelp (BB _ n a c) (BBst s : ps) = buildBBHelp (BB s n a c) ps
buildBBHelp (BB s _ a c) (BBcnt n : ps) = buildBBHelp (BB s n a c) ps
buildBBHelp (BB s n _ c) (BBanc a : ps) = buildBBHelp (BB s n (List.nub a) c) ps
buildBBHelp (BB s n a _) (BBcall c : ps) = buildBBHelp (BB s n a c) ps

buildBB :: [BBprop] -> BasicBlock
buildBB = buildBBHelp (BB 0 0 [] [])

buildFunHelp :: Function -> [Fprop] -> Function
buildFunHelp f [] = f
buildFunHelp (Fun x _ b) (Fnm n : ps) = buildFunHelp (Fun x n b) ps
buildFunHelp (Fun _ n b) (Fst x : ps) = buildFunHelp (Fun x n b) ps
buildFunHelp (Fun x n _) (Fblk b : ps) = buildFunHelp (Fun x n $ map buildBB b) ps

buildFun :: [Fprop] -> Function
buildFun = buildFunHelp (Fun [] "" [])

collectCalls :: Function -> Set.Set Int
collectCalls = foldr (Set.union . Set.fromList . bbcall) Set.empty . funblocks
