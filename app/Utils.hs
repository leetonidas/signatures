module Utils (
        buildCallGraph,
        fromFunction,
        getFromNuc,
        groupByBlockCount,
        normalizeCalls,
        printDotFiles
    ) where

import Control.Monad
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IntMap
import Text.Parsec
import Control.Parallel
import Control.Parallel.Strategies

import Signatures.Function
import Signatures.Graph
import Signatures.Nuc

printDotFiles :: String -> [Function] -> IO ()
printDotFiles pre = mapM_ (\ x -> writeFile (pre ++ "/" ++ getFunName x) $ toDot x)

getFromNuc :: FilePath -> IO [Function]
getFromNuc path = 
    either
        (const [])
        (map buildFun)
        . parse
            parseNuc
            path
            <$> readFile path

fromFunction :: Function -> DiGraph
fromFunction (Fun _ _ b) = DiGraph
    (Set.fromList $ map bbstart b)
    . Set.fromList $ concatMap (\ x -> map (\y -> (bbstart x, y)) $ bbanc x) b

groupByBlockCount :: [Function] -> [(Int, [Function])]
groupByBlockCount = 
    map
        (\ x -> (length . funblocks $ head x, x))
        . List.groupBy
            (\(Fun _ _ b1) (Fun _ _ b2) -> length b1 == length b2)
            . List.sortOn
                (length . funblocks)

buildEntryMap :: [Function] -> IntMap.IntMap Int
buildEntryMap = IntMap.fromList
    . concatMap
        (\ x -> let entry = head $ funstart x in
            map (\ y -> (y, entry))
                $ funstart x)

normalizeFunCalls :: Function -> IntMap.IntMap Int -> Function
normalizeFunCalls fun mapping = fun {
    funblocks = map
        (\ block -> block {
            bbcall = mapMaybe
                (`IntMap.lookup` mapping)
                $ bbcall block})
        $ funblocks fun}

normalizeCalls :: [Function] -> [Function]
normalizeCalls funs = map (`normalizeFunCalls` buildEntryMap funs) funs `using` parListChunk 50 rseq

buildCallGraph :: [Function] -> DiGraph
buildCallGraph funs =
    DiGraph
        (Set.fromList 
            $ map (head . funstart) funs)
        (Set.unions . withStrategy (parListChunk 50 rseq) $ map
            (\x -> Set.mapMonotonic
                (\y -> (head $ funstart x, y))
                $ collectCalls x)
            funs)

sodoku :: [(Function, [Functino])] -> [(Function, [Function])]
