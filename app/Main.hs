{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- The final CSV header is:
--
-- > macro1Var, macro2Var, ..., macroNVar, header!!0, header!!1, ...
--
-- where the 'macroXVar' are the lexiographically sorted keys to the
-- 'macros' mapping.
import Control.Applicative
import Control.Monad
import Data.List
import System.FilePath
import BenchUtils
import System.IO.Temp
import System.IO
import Data.Maybe (fromMaybe, maybeToList)
import Data.Char (toLower)
import SimpleGetOpt
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)

data Result = Result { params                               :: [(Text,Text)]
                     , memoryUsage                          :: [Double]
                     , values                               :: [Text]
                     } deriving (Eq, Ord, Show)

data Settings = Settings
    { macros   :: Map String [String]
    , runs     :: Int
    , inputs   :: [FilePath]
    , bin      :: FilePath
    , binFlags :: [String]
    , tee      :: Bool
    , logging  :: Logging
    , indicies :: [Int]
    , header   :: [Text]
    } deriving (Show)

data Logging = NoLog | LogToFile FilePath | LogTo Handle deriving (Show,Eq)

argCfg :: OptSpec Settings
argCfg = OptSpec
    { progDefaults = Settings { macros = Map.empty
                                 , runs     = 1
                                 , inputs   = []
                                 , bin      = "echo"
                                 , binFlags = []
                                 , tee      = False
                                 , logging  = NoLog
                                 , indicies = []
                                 , header   = []
                                 }
    , progOptions =
        [ Option ['r'] ["runs"] "Number of runs (which are then averaged - XXX flag currently IGNORED)"
            $ ReqArg "NUM" $ \a s ->
            case readMaybe a of
                Just n | n > 0 -> Right s { runs  = n }
                _              -> Left "Invalid number of runs."
        , Option ['t'] ["tee"] "Tee the output to file _and_ stdout (default: False)."
            $ OptArg "BOOL" $ \m s -> Right s { tee = maybe True ((`elem` ["true","t","yes","y"]) . map toLower) m }
        , Option ['l'] ["log"] "Record log info to a named file"
            $ ReqArg "FILE" $ \f s -> Right s { logging = LogToFile f }
        , Option ['b','e'] ["bin","exe"] "The binary to execute over the processed template."
            $ ReqArg "FILE" $ \f s -> Right s { bin = f }
        , Option ['f'] ["flags"] "The flags and other arguments to pass to the binary."
            $ ReqArg "Bin-ARGS" $ \f s -> Right s { binFlags = f : binFlags s }
        , Option ['i'] ["indicies"] "Zero-based index positions of interest in the list of words from stdout."
            $ ReqArg "COMMA-NUMS" $ \str s -> Right s { indicies = concatMap maybeToList $ map readMaybe $ parseCommaSep (dropParen str) }
        , Option ['h'] ["header"] "Header labels for each selected index into the stdout stream."
            $ ReqArg "COMMA-STRINGS" $ \hs s -> Right s { header = map T.pack $ parseCommaSep hs }
        , Option ['m'] ["macro"] "Add a rewrite macro: -m varName=val | -m var=(low,low+step,high) | -m var=[val1,val2,valN] (in the first case 'val' must not start with '(' or '[')"
            $ ReqArg "REWRITE" $ \a s ->
                let (name,drop 1 -> match) = span (/= '=') a
                    vals = case readMatch match of
                            Tuple a b c  -> map show [a,b..c]
                            Set   xs     -> xs
                            Other str    -> [str]
                in Right s { macros = Map.insertWith (++) name vals (macros s) }
        ]
    , progParamDocs = [ ("Template", "The template for benchmarking.") ]
    , progParams = \p s -> Right s { inputs = ordNub (p : inputs s) }
    }

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

data MatchValue = Tuple Integer Integer Integer | Set [String] | Other String
readMatch :: String -> MatchValue
readMatch str@('(':_) =
        case parseCommaSep (safeinit (drop 1 str)) of
            [a,b,c] -> case (readMaybe a, readMaybe b, readMaybe c) of
                           (Just x,Just y,Just z) -> Tuple x y z
                           _                      -> error "Ranged variables must be integers."
            _       -> error "Ranged variables require a 3-tuple (low,low+step,high)"
readMatch str@('[':_) = Set $ parseCommaSep (safeinit (drop 1 str))
readMatch str         = Other str

safeinit xs = take (length xs - 1) xs
dropParen ('(':rest) = safeinit rest
dropParen ('[':rest) = safeinit rest
dropParen xs         = xs

-- | parseCommaSep "a,b,c" == ["a","b","c"]
parseCommaSep = unfoldr (\x -> if x == [] then Nothing else Just $ span (/=',') (drop 1 x)) . (' ' :)

main :: IO ()
main =
 do cfg <- openLog =<< getOpts argCfg
    forM_ (inputs cfg) $ \templateFile ->
     do tests   <- buildTestsIO templateFile (Map.toList $ macros cfg)
        results <- runTests cfg tests
        closeLogging (logging cfg)
        processResults (tee cfg) (templateFile <.> "csv") (header cfg) results

processResults :: Bool -> FilePath -> [Text] -> [Result] -> IO ()
processResults b fp hdr rs =
  do when b (T.putStrLn doc)
     T.writeFile fp doc
 where doc       = T.unlines (renderHeader hdr macroVars : map (renderResult macroVars) rs)
       macroVars :: [Text]
       macroVars = ordNub $ map fst $ concatMap params rs

-- Renders a result by ordering the  params in the same order as 'vars'
-- The placing 'values' at the end.
renderResult :: [Text] -> Result -> Text
renderResult vars res =
    T.concat (intersperse ", " fields)
 where
  fields :: [Text]
  fields  = tshow (maximum (0:memoryUsage res)) : varVals ++ values res
  varVals = map (\v -> fromMaybe "" $ Map.lookup v mp) vars
  mp      = Map.fromList $ params res

renderHeader :: [Text] -> [Text] -> Text
renderHeader hdrs macroVars = T.concat (intersperse ", " fields)
 where
  fields    = "Memory Usage (Bytes)" : macroVars ++ hdrs

-- Read in the template, apply the macros, and output a list of test files.
buildTestsIO :: FilePath -> [(String, [String])] -> IO [([(Text,Text)],Text)]
buildTestsIO templateFile macros =
 do template <- T.readFile templateFile
    let replacements [] = [[]]
        replacements ((name,vals):xs) = do
            v           <- vals
            rest        <- replacements xs
            return $ (T.pack name,T.pack v) : rest
    return $ map (\m -> (m,applyTemplate template m)) (replacements macros)

applyTemplate :: Text -> [(Text,Text)] -> Text
applyTemplate t [] = t
applyTemplate t (p:rs) = applyTemplate (app t p) rs
 where app t (a,b) = T.replace a b t

runTests :: Settings -> [([(Text,Text)], Text)] -> IO [Result]
runTests cfg = mapM (runTest cfg)

runTest :: Settings -> ([(Text,Text)], Text) -> IO Result
runTest (Settings {..}) (benchSettings,benchContents) =
    withTempFile "." ("tempForBinBench-" ++ sanitize bin <.> "m") $ \file hdl -> do
        T.hPutStr hdl benchContents
        hClose hdl
        threadDelay 250000 -- XXX ugly race it seems with hClose and file access on some platforms.
        (raw,mem) <- readProcessMemory bin (binFlags ++ [file]) ""
        logRawData logging benchSettings raw mem
        let ws = words raw ++ repeat "error"
            rd = T.pack . (ws !!)
        return $ Result benchSettings mem (map rd indicies)
 where
     sanitize = takeFileName

logRawData (LogTo hdl) st raw mem =
    T.hPutStr hdl $ T.concat [ "\n***\nsettings: "
                             , rset
                             , "\n---\nmax memory: "
                             , rmem
                             , "\n---\n"
                             , T.pack raw
                             ]
 where
    rmem = tshow (maximum (0:mem))
    rset = T.concat $ intersperse "," $ map renderSet st
    renderSet (nm,val) = nm `T.append` ": " `T.append` val

logRawData _ _ _ _     = return ()

closeLogging (LogTo hdl) = hClose hdl
closeLogging _           = return ()

openLog :: Settings -> IO Settings
openLog s =
        case logging s of
            LogToFile f ->
                do h <- openFile f WriteMode
                   return s { logging = LogTo h }
            _ -> return s { logging = NoLog }

tshow = T.pack . show
