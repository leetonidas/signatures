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
statsForGroup matches = (countUniqueCorrect, countCorrect, count - countCorrect, count)
    where correct = filter (uncurry List.elem) matches
          countUniqueCorrect = length $ filter ((==) 1 . length . snd) correct
          countCorrect = length correct
          count = length matches

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
    let callGra1 = buildCallGraph f1
    let callGra2 = callGra1 `par` buildCallGraph f2
    let f2Names = Set.fromList
            . map funname
                $ filter
                    (\x -> (optMin opt <= length (funblocks x))
                        && (not . null $ funname x))
                    f2
    let possMatch = Set.fromList
            . map funname
                $ filter
                    (\x -> (optMin opt <= length (funblocks x))
                        && (not . null $ funname x)
                        && (funname x `Set.member` f2Names))
                    f1
    let groups =
            reverse . dropWhile ((>) (optMin opt) . (\ (x,_,_) -> x))
                . mapMaybe
                    (\ (bc, funs) -> maybe Nothing (\x -> Just (bc, funs, x)) . lookup bc $ groupByBlockCount f2)
                    $ groupByBlockCount f1
    
    let matched = parMap rseq (matchGroup callGra1 callGra2) groups

    let named_match = matched `seq` parMap rseq (second $ filter (not . null . funname . fst)) matched

    let stats = map (second statsForGroup) named_match
    
    _ <- when (optVerbose opt) $ mapM_ (\ (a,b) -> putStrLn $ "nodes: " ++ show a ++ ": " ++ show b) stats
    _ <- putStrLn . ((++) "Total: " . show) . (\ (a,b,c) -> (a,b,c, length possMatch)) $ foldr (\ (_,(a1,b1,c1,_)) (a2,b2,c2) -> (a1 + a2, b1 + b2, c1 + c2)) (0,0,0) stats

    when (not $ optSkipPrint opt) . putStrLn . unlines . withStrategy (parListChunk 50 rseq) . map (\ x -> getFunName (fst x) ++ " -> " ++ unwords (map getFunName $ snd x)) 
        $ concatMap snd matched
runStrict _ = putStrLn "Strict Mode"

-- options

main :: IO ()
main = (sigOpts <$> getArgs) >>= runSignatures
 