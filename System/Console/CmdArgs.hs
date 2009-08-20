{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    Simple command line argument handling
-}

module System.Console.CmdArgs(
    Data, Typeable,
    Default(..),
    cmdArgs, cmdModes,
    isQuiet, isNormal, isLoud,
    modeValue,
    module System.Console.CmdArgs.UI
    ) where

import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.IORef
import System.Environment
import System.Exit
import System.FilePath
import Data.Char
import Control.Monad.State
import Data.Function

import System.Console.CmdArgs.Type
import System.Console.CmdArgs.UI
import System.Console.CmdArgs.Expand


---------------------------------------------------------------------
-- DEFAULTS

class Default a where
    def :: a

instance Default Bool where def = False
instance Default [a] where def = []

---------------------------------------------------------------------
-- VERBOSITY CONTROL

{-# NOINLINE verbosity #-}
verbosity :: IORef Int -- 0 = quiet, 1 = normal, 2 = verbose
verbosity = unsafePerformIO $ newIORef 1

isQuiet, isNormal, isLoud :: IO Bool
isQuiet = return True
isNormal = fmap (>=1) $ readIORef verbosity
isLoud = fmap (>=2) $ readIORef verbosity


---------------------------------------------------------------------
-- MAIN DRIVERS

cmdArgs :: Data a => String -> Mode a -> IO a
cmdArgs short mode = cmdModes short [mode]


cmdModes :: Data a => String -> [Mode a] -> IO a
cmdModes short modes = do
    modes <- return $ expand modes
    args <- parseArgs modes `fmap` getArgs
    (args,mode) <- return $ modeArgs modes args
    when (hasArg args "!help") $ do
        case mode of
            Right mode -> showHelp short [mode]
            Left err -> showHelp short modes
        exitSuccess
    when (hasArg args "!version") $ do
        putStrLn short
        exitSuccess
    mode <- case mode of
        Right x -> return x
        Left x -> putStrLn x >> exitFailure
    args <- return $ fileArgs mode args
    sequence_ [putStrLn x >> exitFailure | Err x <- args]
    when (hasArg args "!verbose") $ writeIORef verbosity 2
    when (hasArg args "!quiet") $ writeIORef verbosity 0
    return $ applyArgs args $ modeValue mode


modeValue :: Mode a -> a
modeValue (Mode x _ _) = x


---------------------------------------------------------------------
-- UTILITIES

setField :: Data a => a -> String -> (Dynamic -> Dynamic) -> a
setField x name v = flip evalState (constrFields $ toConstr x) $ flip gmapM x $ \i -> do
    n:ns <- get
    put ns
    return $ if n == name then fromDyn (v $ toDyn i) i else i


---------------------------------------------------------------------
-- HELP INFORMATION

showHelp :: String -> [Mode a] -> IO ()
showHelp short xs = do
    prog <- fmap (map toLower . takeBaseName) getProgName
    let one = length xs == 1
    let info = [([prog ++ (if one then "" else " " ++ modeName top) ++ " [FLAG]" ++ showArgs flags
                 ,"  " ++ flagText top]
                ,concatMap showFlag flags)
               | Mode _ top flags <- xs]
    let dupes = if one then [] else foldr1 intersect (map snd info)
    showBlock $
        Left short :
        concat [Left "" : map Left mode ++ Left "" : map Right (args \\ dupes) | (mode,args) <- info] ++
        (if null dupes then [] else Left "":Left "Common flags:":map Right dupes) ++
        concat [map Left $ "":suf | Mode _ top _ <- xs, HelpSuffix suf <- top]


showBlock :: [Either String (String,String,String)] -> IO ()
showBlock xs = putStr $ unlines $ map f xs
    where f (Left x) = x
          f (Right (a,b,c)) = "  " ++ pad an a ++ pad bn b ++ " " ++ c

          (as,bs,_) = unzip3 [x | Right x <- xs]
          an = maximum $ map length as
          bn = maximum $ map length bs
          pad n x = x ++ replicate (n - length x + 1) ' '


showArgs :: [Flag] -> String
showArgs = concatMap ((' ':) . snd) . sort . concatMap f
    where
        f xs = case (isFlagArgs xs, [x | FldArgPos x <- xs], head $ [x | FldTyp x <- xs] ++ ["FILE"]) of
            (True,_,x) -> [(maxBound :: Int,"[" ++ x ++ "]")]
            (_,[i],x) -> [(i,x)]
            _ -> []


showFlag :: Flag -> [(String,String,String)]
showFlag xs =
    [(unwords (map ("-"++) short)
     ,unwords (map ("--"++) long) ++ val
     ,flagText xs ++ maybe "" (\x -> " (default=" ++ x ++ ")") (defaultArg xs))
    | isFlagFlag xs]
    where
        (short,long) = partition ((==) 1 . length) [x | Flag x <- xs]
        val = if isFlagBool xs then ""
              else ['['|opt] ++ "=" ++ typ ++ [']'|opt]
        typ = head $ [x | FldTyp x <- xs] ++ ["VALUE"]
        opt = isFlagOpt xs


defaultArg :: Flag -> Maybe String
defaultArg xs = listToMaybe $ [x | FldEmpty x <- xs] ++ case head [x | FldValue x <- xs] of
    x | Just v <- fromDynamic x, v /= "" -> [v]
    x | Just v <- fromDynamic x, v /= (0::Int) -> [show v]
    x | Just v <- fromDynamic x, v /= (0::Integer) -> [show v]
    x | Just v <- fromDynamic x, v /= (0::Float) -> [show v]
    x | Just v <- fromDynamic x, v /= (0::Double) -> [show v]
    _ -> []


---------------------------------------------------------------------
-- PROCESS FLAGS

data Arg = Field String (Dynamic -> Dynamic)
         | Err String
         | Arg String


modesFlags :: [Mode a] -> [Flag]
modesFlags xs = nubBy ((==) `on` flagName) $ concat [x | Mode _ _ x <- xs]

parseArgs :: [Mode a] -> [String] -> [Arg]
parseArgs modes [] = []

parseArgs modes (('-':x:xs):ys) | xs /= "" && x `elem` expand = parseArgs modes (['-',x]:('-':xs):ys)
    where expand = [x | flag <- modesFlags modes, isFlagBool flag, Flag [x] <- flag]

parseArgs modes (('-':x:xs):ys) | x /= '-' = parseArgs modes (x2:ys)
    where x2 = if null xs then '-':'-':x:[] else '-':'-':x:'=':xs

parseArgs modes (('-':'-':x):xs)
    | Left msg <- flg = err msg : parseArgs modes xs
    | Right flag <- flg = let name = head [x | FldName x <- flag] in
    case flagType flag of
        FlagBool -> 
            [err "does not take an argument" | b /= ""] ++
            [Field name (const $ toDyn True)] ++ parseArgs modes xs
        FlagItem r ->
            if not (isFlagOpt flag) && null b && (null xs || "-" `isPrefixOf` head xs)
            then err "needs an argument" : parseArgs modes xs
            else let (text,rest) = case flagOpt flag of
                        Just v | null b -> (v, xs)
                        _ | null b -> (head xs, tail xs)
                        _ -> (drop 1 b, xs)
                 in (case r text of
                        Nothing -> err "couldn't parse argument" 
                        Just v -> Field name v)
                    : parseArgs modes rest
    where
        flg = pickFlag (modesFlags modes) a
        (a,b) = break (== '=') x
        err msg = Err $ "Error on flag " ++ show x ++ ", flag " ++ msg

parseArgs flags (x:xs) = Arg x : parseArgs flags xs



pickFlag :: [Flag] -> String -> Either String Flag
pickFlag flags name = case (match,prefix) of
        ([x],_) -> Right x
        ([],[x]) -> Right x
        ([],[]) -> Left "unknown"
        _ -> Left "ambiguous"
    where
        match = [flag | flag <- flags, or [x == name | Flag x <- flag]]
        prefix = [flag | flag <- flags, or [name `isPrefixOf` x | Flag x <- flag]]


hasArg :: [Arg] -> String -> Bool
hasArg xs name = or [x == name | Field x _ <- xs]


-- expand out the Arg
fileArgs :: Mode a -> [Arg] -> [Arg]
fileArgs (Mode _ _ flags) = f (flgPos ++ flgArg ++ flgErr)
    where
        flgPos = map snd $ sortBy (compare `on` fst) [(i,\x -> Field (flagName flag) (const $ toDyn x)) | flag <- flags, FldArgPos i <- flag]
        flgArg = concat [repeat $ \x -> Field (flagName flag) (\v -> toDyn $ fromDyn v [""] ++ [x]) | flag <- flags, isFlagArgs flag]
        flgErr = repeat $ \x -> Err $ "Can't deal with further file arguments: " ++ show x

        f (t:ts) (Arg x:xs) = t x : f ts xs
        f ts (x:xs) = x : f ts xs
        f ts [] = []


applyArgs :: Data a => [Arg] -> a -> a
applyArgs (Field name upd:args) x = applyArgs args (setField x name upd)
applyArgs [] x = x


modeArgs :: [Mode a] -> [Arg] -> ([Arg], Either String (Mode a))
modeArgs [mode] xs = (xs, Right mode)
modeArgs modes (Arg x:xs) = (,) xs $ case [mode | mode@(Mode _ top _) <- modes, x `isPrefixOf` modeName top] of
    [] -> Left $ "Unknown mode: " ++ x
    [x@(Mode _ _ flags)] -> case [n | Field n _ <- xs, n `notElem` map flagName flags] of
        [] -> Right x
        bad:_ -> Left $ "Flag " ++ show bad ++ " not permitted in this mode"
    xs -> Left $ "Ambiguous mode, could be one of: " ++ unwords (map modeName [x | Mode _ x _ <- xs])
modeArgs modes xs = (xs, Left "No mode given")
