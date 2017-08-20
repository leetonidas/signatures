module Main where

import Control.Arrow
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
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
    let callGra2 = callGra1 `par` buildCallGraph f2
    let groups =
            reverse . dropWhile ((>) (optMin opt) . (\ (x,_,_) -> x))
                . mapMaybe
                    (\ (bc, funs) -> maybe Nothing (\x -> Just (bc, funs, x)) . lookup bc $ groupByBlockCount f2)
                    $ groupByBlockCount f1
    let matched = par callGra2 . seq groups $ parMap rseq
            (\ (a,b,c) -> (a, filter
                (not . null . snd)
                . map
                    (\ (a,bs) -> (a, filter (canMatch callGra1 callGra2 a) $ map fst bs))
                    $ matchFunctions b c))
                groups

    let named_match = matched `seq` parMap rseq (second $ filter (not . null . funname . fst)) matched

    let stats = if not $ optVerbose opt then [] else parMap rseq (\ (a,x) -> 
            let unique = filter ((==) 1 . length . snd)
                uniquMatched = unique x in "matching: " ++ show a ++ " ( "
                        ++ show (length $ filter (\ (f,ms) -> funname f == funname (head ms)) uniquMatched)
                        ++ " | " ++ show (length uniquMatched)
                        ++ " | " ++ show
                            (length . filter
                                (\x -> (not . null $ funname x)
                                    && (funname x `Set.member` possMatch))
                                . (\ (_,x,_) -> x)
                                    . fromJust
                                        $ List.find ((==) a . (\ (x,_,_) -> x)) groups)
                        ++ ")")
                named_match
    
    _ <- stats `seq` when (optVerbose opt) $ mapM_ putStrLn stats 
    _ <- putStr "Matches: "
    _ <- putStr . (\ (a,b) -> "(" ++ show a ++ " | " ++ show b ++ " | ")
            $ foldl
                (\ (a,b) mt -> let named = snd mt in
                    let unique = filter ((==) 1 . length . snd) named in
                        let correct = filter (\ (a,b) -> funname a == funname (head b)) unique in
                            (a + length correct, b + length unique))
                (0, 0)
                named_match
    _ <- putStrLn $ show (length possMatch) ++ ")"
    putStrLn . unlines . withStrategy (parListChunk 50 rseq) . map (\ x -> getFunName (fst x) ++ " -> " ++ unwords (map getFunName $ snd x)) 
        $ concatMap snd matched
runStrict _ = putStrLn "Strict Mode"

-- options

main :: IO ()
main = (sigOpts <$> getArgs) >>= runSignatures
 