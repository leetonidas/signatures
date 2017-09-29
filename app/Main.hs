module Main where

import Control.Arrow
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import System.Directory
import System.Environment

import Signatures.Function
import Signatures.Graph
import Signatures.Nuc

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

runInfo opts = when (optVerbose opts) (putStrLn $ "Info mode: " ++ optSigNuc opts) *>
    (mapM_
        (\ (a,b,c) -> putStrLn $ show a ++ ": " ++ show b ++ " (" ++ show c ++ ")")
        . buildStats
            =<< getFromNuc (optSigNuc opts))
                    where outdir = optOutDir opts

runLoose opt = do
    _ <- when (optVerbose opt) (putStrLn $ "Matching \"" ++ optSigNuc opt ++ "\" against \"" ++ optTarNuc opt ++ "\"")
    f1 <- (\x -> if optNormalize opt then map normalize x else x) <$> getFromNuc (optSigNuc opt)
    f2 <- f1 `par` (\x -> if optNormalize opt then map normalize x else x) <$> getFromNuc (optTarNuc opt)
    _ <- f2 `seq` when (optVerbose opt) (putStrLn $ "loaded " ++ show (length f1) ++ " functions from " ++ optSigNuc opt) 
    _ <- when (optVerbose opt) (putStrLn $ "loaded " ++ show (length f2) ++ " functions from " ++ optTarNuc opt)
--    putStrLn $ "named f1: " ++ show (length $ filter (not . null . funname) f1)
--    putStrLn $ "not unique f1: " ++ unwords (map head . filter ((<) 1 . length) . List.group . List.sort . filter (not . null) $ map funname f1)
--    putStrLn $ "named f2: " ++ show (length $ filter (not . null . funname) f2)
    let callGra1 = buildCallGraph f1
    let callGra2 = callGra1 `par` buildCallGraph f2
    let f2Names = Set.fromList $ collectNames f2
    let f1Named = map (\ a -> (length $ funblocks a, funname a)) $ filter (not . null . funname) f1
    let allMatch = filter (flip Set.member f2Names . snd) f1Named
--    putStrLn $ "unique names f1: " ++ show (length f1Names)
--    putStrLn $ "unique names f2: " ++ show (length f2Names)
    let possMatch = filter ((<=) (optMin opt) . fst) allMatch

    let groups =
            reverse . dropWhile ((>) (optMin opt) . (\ (x,_,_) -> x))
                . mapMaybe
                    (\ (bc, funs) -> maybe Nothing (\x -> Just (bc, funs, x)) . lookup bc $ groupByBlockCount f2)
                    $ groupByBlockCount f1
    
    let matched = parMap rseq (matchGroup callGra1 callGra2) groups

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
 