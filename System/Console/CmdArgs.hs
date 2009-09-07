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
import System.Console.CmdArgs.Flag


---------------------------------------------------------------------
-- DEFAULTS

class Default a where
    def :: a

instance Default Bool where def = False
instance Default [a] where def = []
instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0


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

modeValue :: Mode a -> a
modeValue = modeVal


cmdArgs :: Data a => String -> Mode a -> IO a
cmdArgs short mode = cmdModes short [mode]


cmdModes :: Data a => String -> [Mode a] -> IO a
cmdModes short modes = do
    modes <- return $ expand modes
    args <- parseArgs modes `fmap` getArgs
    (args,mode) <- return $ modeArgs modes args
    when (hasArg args "!help") $ do
        case mode of
            Right (True,mode) -> showHelp short [mode]
            _ -> showHelp short modes
        exitSuccess
    when (hasArg args "!version") $ do
        putStrLn short
        exitSuccess
    mode <- case mode of
        Right (_,x) -> return x
        Left x -> putStrLn x >> exitFailure
    args <- return $ fileArgs mode args
    sequence_ [putStrLn x >> exitFailure | Err x <- args]
    when (hasArg args "!verbose") $ writeIORef verbosity 2
    when (hasArg args "!quiet") $ writeIORef verbosity 0
    return $ applyArgs args $ modeValue mode


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
    prog <- return $ head $ mapMaybe modeProg xs ++ [prog]
    let one = length xs == 1
    let info = [([unwords $ prog : [name | not one] ++ "[FLAG]" : args
                 ,"  " ++ text]
                ,concatMap helpFlag flags)
               | Mode{modeName=name,modeFlags=flags,modeText=text} <- xs
               , let args = map snd $ sortBy (compare `on` fst) $ concatMap helpFlagArgs flags]
    let dupes = if one then [] else foldr1 intersect (map snd info)
    showBlock $
        Left short :
        concat [ Left "" : map Left mode ++ [Left "" | flags /= []] ++ map Right flags
               | (mode,args) <- info, let flags = args \\ dupes] ++
        (if null dupes then [] else Left "":Left "Common flags:":map Right dupes) ++
        concat [map Left $ "":suf | suf@(_:_) <- map modeHelpSuffix xs]


showBlock :: [Either String (String,String,String)] -> IO ()
showBlock xs = putStr $ unlines $ map f xs
    where f (Left x) = x
          f (Right (a,b,c)) = "  " ++ pad an a ++ pad bn b ++ " " ++ c

          (as,bs,_) = unzip3 [x | Right x <- xs]
          an = maximum $ map length as
          bn = maximum $ map length bs
          pad n x = x ++ replicate (n - length x + 1) ' '


---------------------------------------------------------------------
-- PROCESS FLAGS

data Arg = Field String (Dynamic -> Dynamic)
         | Err String
         | Arg String


modesFlags :: [Mode a] -> [Flag]
modesFlags xs = nubBy ((==) `on` flagKey) $ concatMap modeFlags xs

parseArgs :: [Mode a] -> [String] -> [Arg]
parseArgs modes [] = []

parseArgs modes (('-':x:xs):ys) | xs /= "" && x `elem` expand = parseArgs modes (['-',x]:('-':xs):ys)
    where expand = [x | flag <- modesFlags modes, isFlagBool flag, [x] <- flagFlag flag]

parseArgs modes (('-':x:xs):ys) | x /= '-' = parseArgs modes (x2:ys)
    where x2 = '-':'-':x:['='| xs /= [] && head xs /= '=']++xs

parseArgs modes (('-':'-':x):xs)
    | Left msg <- flg = err msg : parseArgs modes xs
    | Right flag <- flg = let name = flagName flag in
    case flagType flag of
        FlagBool r -> 
            [err "does not take an argument" | b /= ""] ++
            [Field name (const r)] ++ parseArgs modes xs
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
        match = [flag | flag <- flags, any (name ==) (flagFlag flag)]
        prefix = [flag | flag <- flags, any (name `isPrefixOf`) (flagFlag flag)]


hasArg :: [Arg] -> String -> Bool
hasArg xs name = or [x == name | Field x _ <- xs]


-- expand out the Arg
fileArgs :: Mode a -> [Arg] -> [Arg]
fileArgs Mode{modeFlags=flags} = f (flgPos ++ flgArg ++ flgErr)
    where
        flgPos = map snd $ sortBy (compare `on` fst) [(i,\x -> Field name (const $ toDyn x)) | Flag{flagName=name,flagArgs=Just (Just i)} <- flags]
        flgArg = concat [repeat $ \x -> Field name (\v -> toDyn $ fromDyn v [""] ++ [x]) | Flag{flagName=name, flagArgs=Just Nothing} <- flags]
        flgErr = repeat $ \x -> Err $ "Can't deal with further file arguments: " ++ show x

        f (t:ts) (Arg x:xs) = t x : f ts xs
        f ts (x:xs) = x : f ts xs
        f ts [] = []


applyArgs :: Data a => [Arg] -> a -> a
applyArgs (Field name upd:args) x = applyArgs args (setField x name upd)
applyArgs [] x = x


-- True = explicitly given, False = used default
modeArgs :: [Mode a] -> [Arg] -> ([Arg], Either String (Bool, Mode a))
modeArgs [mode] xs = (xs, Right (False,mode))
modeArgs modes o@(Arg x:xs) = case [mode | mode <- modes, x `isPrefixOf` modeName mode] of
    [] -> (,) o $ modeArgsDefault modes "Unknown mode: "
    [mode] -> case [n | Field n _ <- xs, n `notElem` map flagName (modeFlags mode)] of
        [] -> (,) xs $ Right (True,mode)
        bad:_ -> (,) o $ Left $ "Flag " ++ show bad ++ " not permitted in this mode"
    xs -> (,) o $ Left $ "Ambiguous mode, could be one of: " ++ unwords (map modeName xs)
modeArgs modes xs = (xs, modeArgsDefault modes "No mode given")

modeArgsDefault modes err = case filter modeDef modes of
    [] -> Left err
    x:_ -> Right (False,x)
