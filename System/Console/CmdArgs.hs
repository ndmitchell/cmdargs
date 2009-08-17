{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    Simple command line argument handling
-}

module System.Console.CmdArgs(
    Data, Typeable,
    Default(..),
    cmdArgs, (&),
    isQuiet, isNormal, isLoud,
    text, args, typ, typFile, typDir, helpSuffix, empty, flag
    ) where

import Prelude hiding (catch)
import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.IORef
import System.Environment
import Control.Monad
import Control.Exception
import System.Exit
import System.FilePath
import Data.Char
import Control.Monad.State


---------------------------------------------------------------------
-- DEFAULTS

class Default a where
    def :: a

instance Default Bool where def = False
instance Default [a] where def = []

---------------------------------------------------------------------
-- STATE MANAGEMENT

{-# NOINLINE verbosity #-}
verbosity :: IORef Int -- 0 = quiet, 1 = normal, 2 = verbose
verbosity = unsafePerformIO $ newIORef 1

isQuiet, isNormal, isLoud :: IO Bool
isQuiet = return True
isNormal = fmap (>=1) $ readIORef verbosity
isLoud = fmap (>=2) $ readIORef verbosity


---------------------------------------------------------------------
-- STATE MANAGEMENT

{-# NOINLINE info #-}
info :: IORef [Info]
info = unsafePerformIO $ newIORef []

(&) :: forall a . a -> Info -> a
(&) x i = unsafePerformIO $ do
    modifyIORef info (i:)
    return x

collect :: IO [Info]
collect = do
    x <- readIORef info
    writeIORef info []
    return x

data CmdMode = CmdMode 

---------------------------------------------------------------------
-- USER INTERFACE

type Flag = [Info]
type Mode = [Info]

data Info
    = FldName String -- the record name
    | FldType TypeRep -- the field type
    | FldValue Dynamic
    | FldEmpty String
    | FldTyp String
    | FldText String
    | FldFlag String
    | FldArgs
    | FldArg Int
    | FldExplicit
    | HelpSuffix [String]
      deriving Show

-- | A default argument if none is specified
empty :: (Show a, Typeable a) => a -> Info
empty x = case cast x of
    Just y -> FldEmpty y
    _ -> FldEmpty $ show x

-- | The type of the argument
typ :: String -> Info
typ = FldTyp

-- | Descriptive text for the option
text :: String -> Info
text = FldText

-- | Flags which work
flag :: String -> Info
flag = FldFlag

-- | Where to put the non-flag arguments
args :: Info
args = FldArgs


argPos :: Int -> Info
argPos = FldArg


typDir, typFile :: Info
typFile = typ "FILE"
typDir = typ "DIR"


helpSuffix :: [String] -> Info
helpSuffix = HelpSuffix


---------------------------------------------------------------------
-- PRESUPPLIED ARGS

autoArgs :: [Flag]
autoArgs = map (def++) $
    [[FldName "!help", flag "help", flag "?", text "Show usage information"] -- , empty "", typ "FORMAT"]
    ,[FldName "!version", flag "version", flag "V", text "Show version information"]
    ,[FldName "!verbose", flag "verbose", flag "v", text "Higher verrbosity"]
    ,[FldName "!quiet", flag "quiet", flag "q", text "Lower verbosity"]
    ]
    where def = [FldType (typeOf True), FldValue (toDyn False), FldExplicit]


---------------------------------------------------------------------
-- STRUCTURED FLAGS

-- Simple stuff
isFlagArgs xs = [] /= [() | FldArgs{} <- xs]
isFlagExplicit xs = [] /= [() | FldExplicit{} <- xs]
isFlagOpt = isJust . flagOpt
flagOpt xs = listToMaybe [x | FldEmpty x <- xs]
flagText xs = unwords [x | FldText x <- xs]
isFlagBool xs = case flagType xs of FlagBool -> True; _ -> False
hasFlagType = isJust . toFlagType
flagType = fromJust . toFlagType


-- Flag types
data FlagType
    = FlagBool
    | FlagItem (String -> Maybe (Dynamic -> Dynamic))

toFlagType :: Flag -> Maybe FlagType
toFlagType xs
    | typ == typeOf True = Just FlagBool
    | Just r <- toFlagTypeRead False typ = Just $ FlagItem r
    | a == typeRepTyCon (typeOf ""), Just r <- toFlagTypeRead True (head b) = Just $ FlagItem r
    where typ = head [x | FldType x <- xs]
          (a,b) = splitTyConApp typ

toFlagTypeRead :: Bool -> TypeRep -> Maybe (String -> Maybe (Dynamic -> Dynamic))
toFlagTypeRead list x
    | x == typeOf "" = with (\x -> [(x,"")])
    | x == typeOf (0 :: Int) = with (reads :: ReadS Int)
    | x == typeOf (0 :: Integer) = with (reads :: ReadS Integer)
    | x == typeOf (0 :: Float) = with (reads :: ReadS Float)
    | x == typeOf (0 :: Double) = with (reads :: ReadS Double)
    | otherwise = Nothing
    where
        with :: forall a . Typeable a => ReadS a -> Maybe (String -> Maybe (Dynamic -> Dynamic))
        with r = Just $ \x -> case r x of
            [(v,"")] -> Just $ \old -> if list then toDyn $ fromJust (fromDynamic old) ++ [v] else toDyn v
            _ -> Nothing


---------------------------------------------------------------------
-- MAIN DRIVERS

cmdArgs :: Data a => String -> a -> IO a
cmdArgs short val = do
    evaluate val
    mode <- collect
    ref <- newIORef (constrFields $ toConstr val, [])
    val <- flip gmapM val $ \i -> do
        res <- evaluate i
        info <- collect
        let typ = typeOf i
        modifyIORef ref $ \(fld:flds, xs) ->
            if hasFlagType [FldType typ]
            then (flds, (FldName fld:FldType typ:FldValue (toDyn i):info):xs)
            else error $ "Can't handle a type of " ++ fld
        return res
    flags <- fmap (flagsExpand . reverse . snd) $ readIORef ref

    args <- parseArgs flags `fmap` getArgs
    when (hasArg args "!help") $ do
        showHelp short mode flags
        exitSuccess
    args <- return $ expandArgs flags args
    case [x | Err x <- args] of
        x:_ -> putStrLn x >> exitFailure
        [] -> return ()
    when (hasArg args "!version") $ do
        putStrLn short
        exitSuccess
    when (hasArg args "!verbose") $ writeIORef verbosity 2
    when (hasArg args "!quiet") $ writeIORef verbosity 0
    return $ applyArgs args val


cmdArgsMode :: Data a => String -> [a] -> IO a
cmdArgsMode short xs = cmdArgs short (head xs)


---------------------------------------------------------------------
-- FLAG EXPANSION

flagsExpand :: [Flag] -> [Flag]
flagsExpand = assignShort . assignLong . (autoArgs++)


assignLong :: [Flag] -> [Flag]
assignLong = map f
    where f xs = [FldFlag $ flagName x | FldName x <- xs, not $ isFlagArgs xs, not $ isFlagExplicit xs] ++ xs


assignShort :: [Flag] -> [Flag]
assignShort xs = zipWith (++) flags xs
    where
        seen = [y | x <- xs, FldFlag [y] <- x]
        guesses = map guess xs
        dupes = nub $ concat guesses \\ nub (concat guesses)
        flags = [[FldFlag [i] | i <- g, i `notElem` (seen++dupes)] | g <- guesses]

        -- guess at a possible short flag
        guess ys = if [() | FldFlag [_] <- ys] /= [] then [] else take 1 [x | FldFlag (x:_) <- ys]


flagName :: String -> String
flagName xs | "_" `isSuffixOf` xs = flagName $ init xs
flagName xs = [if x == '_' then '-' else x | x <- xs]


---------------------------------------------------------------------
-- UTILITIES

errorIO :: String -> IO a
errorIO x = putStrLn x >> exitFailure


setField :: Data a => a -> String -> (Dynamic -> Dynamic) -> a
setField x name v = flip evalState (constrFields $ toConstr x) $ flip gmapM x $ \i -> do
    n:ns <- get
    put ns
    return $ if n == name then fromDyn (v $ toDyn i) i else i


---------------------------------------------------------------------
-- HELP INFORMATION

showHelp :: String -> Mode -> [Flag] -> IO ()
showHelp short mode flags = do
    x <- getProgName
    let ty = head $ [y | x <- flags, isFlagArgs x, FldTyp y <- x] ++ ["FILE"]
    showBlock $
        Left short :
        Left "" :
        Left (map toLower (takeBaseName x) ++ " [FLAG] ["++ty++"]") :
        Left "" :
        concatMap (map Right . showArg) flags ++
        concat [map Left $ "":xs | HelpSuffix xs <- mode]


showBlock :: [Either String (String,String,String)] -> IO ()
showBlock xs = putStr $ unlines $ map f xs
    where f (Left x) = x
          f (Right (a,b,c)) = "  " ++ pad an a ++ pad bn b ++ " " ++ c

          (as,bs,_) = unzip3 [x | Right x <- xs]
          an = maximum $ map length as
          bn = maximum $ map length bs
          pad n x = x ++ replicate (n - length x + 1) ' '


showArg :: Flag -> [(String,String,String)]
showArg xs =
    [(unwords (map ("-"++) short)
     ,unwords (map ("--"++) long) ++ val
     ,flagText xs ++ maybe "" (\x -> " (default=" ++ x ++ ")") (defaultArg xs))
    | not $ isFlagArgs xs]
    where
        (short,long) = partition ((==) 1 . length) [x | FldFlag x <- xs]
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


parseArgs :: [Flag] -> [String] -> [Arg]
parseArgs flags [] = []

parseArgs flags (('-':x:xs):ys) | xs /= "" && x `elem` expand = parseArgs flags (['-',x]:('-':xs):ys)
    where expand = [x | flag <- flags, isFlagBool flag, FldFlag [x] <- flag]

parseArgs flags (('-':x:xs):ys) | x /= '-' = parseArgs flags (x2:ys)
    where x2 = if null xs then '-':'-':x:[] else '-':'-':x:'=':xs

parseArgs flags (('-':'-':x):xs)
    | Left msg <- flg = err msg : parseArgs flags xs
    | Right flag <- flg = let name = head [x | FldName x <- flag] in
    case flagType flag of
        FlagBool -> 
            [err "does not take an argument" | b /= ""] ++
            [Field name (const $ toDyn True)] ++ parseArgs flags xs
        FlagItem r ->
            if not (isFlagOpt flag) && null b && (null xs || "-" `isPrefixOf` head xs)
            then err "needs an argument" : parseArgs flags xs
            else let (text,rest) = case flagOpt flag of
                        Just v | null b -> (v, xs)
                        _ | null b -> (head xs, tail xs)
                        _ -> (drop 1 b, xs)
                 in (case r text of
                        Nothing -> err "couldn't parse argument" 
                        Just v -> Field name v)
                    : parseArgs flags rest
    where
        flg = pickFlag flags a
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
        match = [flag | flag <- flags, or [x == name | FldFlag x <- flag]]
        prefix = [flag | flag <- flags, or [name `isPrefixOf` x | FldFlag x <- flag]]


hasArg :: [Arg] -> String -> Bool
hasArg xs name = or [x == name | Field x _ <- xs]


-- expand out the Arg
expandArgs :: [Flag] -> [Arg] -> [Arg]
expandArgs flags (Arg x:xs) = case filter isFlagArgs flags of
    flag:_ -> Field (head [x | FldName x <- flag]) (\v -> toDyn $ fromDyn v [""] ++ [x]) : expandArgs flags xs
    [] -> Err ("Can't deal with file arguments: " ++ show x) : expandArgs flags xs
expandArgs flags (x:xs) = x : expandArgs flags xs
expandArgs flags [] = []


applyArgs :: Data a => [Arg] -> a -> a
applyArgs (Field name upd:args) x = applyArgs args (setField x name upd)
applyArgs [] x = x
