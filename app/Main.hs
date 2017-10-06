module Main where

import Control.Arrow
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import System.Directory
import System.Environment

import Signatures.Function
import Signatures.Graph
import Signatures.Nuc
import Signatures.CFG

import Utils
import Match
import Options

runSignatures :: Maybe Options -> IO ()
runSignatures opt | isNothing opt || not (checkOpt optJ) = getUsageInfo <$> getProgName >>= putStrLn
                  | optMode optJ == MDot = runDot optJ
                  | optMode optJ == MInfo = runInfo optJ
                  | optMode optJ == MLoose = runLoose optJ
                  | otherwise = runStrict optJ
                    where optJ = fromJust opt

checkCallGraph :: DiGraph -> DiGraph -> Function -> [Function] -> (Function, [Function])
checkCallGraph cg1 cg2 sig tars = (sig, filter (canMatch cg1 cg2 sig) tars) 

matchGroup2 :: DiGraph -> DiGraph -> IM.IntMap Function -> IM.IntMap Function -> (Int, [(Function, CFG)], [(Function, CFG)]) -> (Int, [(Function, [Function])])
matchGroup2 callGra1 callGra2 sigMap tarMap (a, b, c) = (a, filter
    (not . null . snd)
        . map
        (uncurry $ checkCallGraph callGra1 callGra2)
        . using (matchBatchCFG b c) $ parList rseq) 

matchGroup :: DiGraph -> DiGraph -> (Int, [Function], [Function]) -> (Int, [(Function, [Function])])
matchGroup callGra1 callGra2 (a, b, c) = (a, filter
    (not . null . snd)
        . map
        (uncurry $ checkCallGraph callGra1 callGra2)
        . using (matchFunctions b c) $ parList rseq)

statsForGroup :: [(Function, [Function])] -> (Int, Int, Int, Int)
statsForGroup matches = (countUniqueCorrect, countCorrect, countFalsePositive, count)
    where (correct, wrong) = List.partition (uncurry List.elem) matches
          countUniqueCorrect = length $ filter ((==) 1 . length . snd) correct
          countFalsePositive = length $ filter (not . null . snd) wrong
          countCorrect = length correct
          count = length matches

collectNames :: [Function] -> [String]
collectNames funs = filter (not . null) $ map funname funs

runDot, runStrict, runLoose, runInfo :: Options -> IO ()
runDot opts = printDotFiles outdir
    =<< map normalize <$> getFromNuc (optSigNuc opts)
        <* createDirectoryIfMissing False outdir
             where outdir = optOutDir opts

runInfo opts = do 
    when (optVerbose opts) (putStrLn $ "Info mode: " ++ optSigNuc opts)
    funs <- getFromNuc (optSigNuc opts)
    (mapM_
        (\ (a,b) -> putStrLn $ show a ++ ": " ++ show b)
        $ buildStatsCFG funs (optNormalize opts))
    let cfgs = map ((if (optNormalize opts) then combineNodes . extractLeafs else id) . cfgFromFunction) funs
    putStrLn $ "widest switch: " ++ show (maximum $ map (IM.foldr (\ a -> max (length a)) 0 . edges . graph) cfgs)

runLoose opt = do
    when (optVerbose opt) (putStrLn $ "Matching \"" ++ optSigNuc opt ++ "\" against \"" ++ optTarNuc opt ++ "\"")
    f1 <- (IM.fromList . map (\ x -> (head $ funstart x, x))) <$> getFromNuc (optSigNuc opt) :: IO (IM.IntMap Function)
    f2 <- f1 `par` (IM.fromList . map (\ x -> (head $ funstart x, x))) <$> getFromNuc (optTarNuc opt) :: IO (IM.IntMap Function)
    f2 `seq` when (optVerbose opt) (putStrLn $ "loaded " ++ show (length f1) ++ " functions from " ++ optSigNuc opt) 
    when (optVerbose opt) (putStrLn $ "loaded " ++ show (length f2) ++ " functions from " ++ optTarNuc opt)
--    putStrLn $ "named f1: " ++ show (length $ filter (not . null . funname) f1)
--    putStrLn $ "not unique f1: " ++ unwords (map head . filter ((<) 1 . length) . List.group . List.sort . filter (not . null) $ map funname f1)
--    putStrLn $ "named f2: " ++ show (length $ filter (not . null . funname) f2)
    
    let sigCFGs = IM.foldr (\ f -> (:) (f, (if optNormalize opt then combineNodes . extractLeafs else id) $ cfgFromFunction f)) [] f1
    let tarCFGs = IM.foldr (\ f -> (:) (f, (if optNormalize opt then combineNodes . extractLeafs else id) $ cfgFromFunction f)) [] f2
    let callGra1 = buildCallGraph $ IM.elems f1
    let callGra2 = callGra1 `par` buildCallGraph (IM.elems f2)
    let groups =
            reverse . dropWhile ((>) (optMin opt) . (\ (x,_,_) -> x))
                . mapMaybe
                    (\ (bc, funs) -> maybe Nothing (\x -> Just (bc, funs, x)) . lookup bc $ groupByCFGNodeCount tarCFGs)
                    $ groupByCFGNodeCount sigCFGs

    let f2Names = foldr (\ (_,_,funs) nm -> foldr (\ a b -> let name = funname $ fst a in if null name then b else Set.insert name b) nm funs) Set.empty groups
    let f1Names = foldr (\ (_,funs,_) nm -> foldr (\ a b -> let name = funname $ fst a in if null name then b else name:b) nm funs) [] groups
    
--    putStrLn $ "unique names f1: " ++ show (length f1Names)
--    putStrLn $ "unique names f2: " ++ show (length f2Names)
    let allMatch = IM.filter (\ a -> let name = funname a in not (null name) && Set.member name (Set.fromList . map funname $ IM.elems f2)) f1
    let possMatch = filter (`Set.member` f2Names) f1Names

    let matched = parMap rseq (matchGroup2 callGra1 callGra2 f1 f2) groups

    let named_match = matched `seq` parMap rseq (second $ filter (not . null . funname . fst)) matched

    let stats = map (second statsForGroup) named_match
    
    _ <- when (optVerbose opt) $ mapM_ (\ (a,b) -> putStrLn $ "nodes: " ++ show a ++ ": " ++ show b) stats
    _ <- putStrLn . ((++) "Total: " . show) . (\ (a,b,c) -> (a,b,c, length possMatch, length allMatch)) $ foldr (\ (_,(a1,b1,c1,_)) (a2,b2,c2) -> (a1 + a2, b1 + b2, c1 + c2)) (0,0,0) stats

    when (not $ optSkipPrint opt) . putStrLn . unlines . withStrategy (parListChunk 50 rseq) . map (\ x -> getFunName (fst x) ++ " -> " ++ unwords (map getFunName $ snd x)) 
        $ concatMap snd matched
runStrict _ = putStrLn "Strict Mode"

-- options

main :: IO ()
main = (sigOpts <$> getArgs) >>= runSignatures
 