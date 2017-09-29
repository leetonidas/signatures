module Options (
        Options(..),
        Mode(..),
        checkOpt,
        defaultOps,
        getUsageInfo,
        sigOpts
    ) where

import Data.Maybe
import System.Console.GetOpt

data Mode = MDot | MInfo | MLoose | MStrict deriving (Eq,Ord,Show,Enum,Bounded)

data Options = Options {
    optVerbose :: Bool,
    optSigNuc :: String,
    optTarNuc :: String,
    optOutDir :: String,
    optMin :: Int,
    optMode :: Mode,
    optNormalize :: Bool,
    optSkipPrint :: Bool
} deriving Show

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["verbose"] (NoArg (\ x -> x {optVerbose = True})) "prints additional information"
    , Option ['s'] ["sig-nuc"] (ReqArg (\ s x -> x {optSigNuc = s}) "FILE") "the nucleus file for the signature binary"
    , Option ['t'] ["tar-nuc"] (ReqArg (\ s x -> x {optTarNuc = s}) "FILE") "the nucleus file for the target binary"
    , Option ['d'] ["dir"]     (ReqArg (\ s x -> x {optOutDir = s}) "DIR")  "the directory for the output file(s)"
    , Option ['l'] ["level"]   (ReqArg (\ s x -> x {optMode = getMode s}) "level")   "the level used ( dot | info | loose | strict )"
    , Option ['m'] ["min"]     (ReqArg (\ s x -> x {optMin = read s}) "num") "the minimum number of blocks to be considered for matching"
    , Option [] ["no-normalize"] (NoArg (\ x -> x {optNormalize = False})) "does not normalize functions before matching"
    , Option ['n'] ["no-print"] (NoArg (\ x -> x {optSkipPrint = True})) "prevents the program to print the list of matched functions"]

getMode :: String -> Mode
getMode opt = case opt of
    "dot" -> MDot
    "strict" -> MStrict
    "loose" -> MLoose
    "info" -> MInfo

defaultOps :: Options
defaultOps = Options False "" "" "out" 13 MInfo True False

sigOpts :: [String] -> Maybe Options
sigOpts argv =
    case getOpt RequireOrder options argv of
        (o,[],[]) -> Just $ foldl (flip id) defaultOps o
        _ -> Nothing 

header :: String -> String
header s = "Usage: " ++ s ++ " -l level [OPTIONS]"  

getUsageInfo :: String -> String
getUsageInfo str = usageInfo (header str) options

checkOpt :: Options -> Bool
checkOpt opt | optMode opt `elem` [MDot, MInfo] = not . null $ optSigNuc opt
             | optMode opt == MLoose = not $ any (null . flip ($) opt) [optSigNuc, optTarNuc]
             | optMode opt == MStrict = not $ any (null . flip ($) opt) [optSigNuc, optTarNuc]
