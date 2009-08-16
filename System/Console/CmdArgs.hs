{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    Simple command line argument handling
-}

module System.Console.CmdArgs(module System.Console.CmdArgs, Data, Typeable) where

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
-- STRUCTURED FLAGS

-- Simple stuff
isFlagArgs xs = [] /= [() | FldArgs{} <- xs]
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
cmdArgs short x = do
    evaluate x
    mode <- collect
    ref <- newIORef (constrFields $ toConstr x, [])
    x <- flip gmapM x $ \i -> do
        res <- evaluate i
        info <- collect
        let typ = typeOf i
        modifyIORef ref $ \(fld:flds, xs) ->
            if hasFlagType [FldType typ]
            then (flds, (FldName fld:FldType typ:FldValue (toDyn i):info):xs)
            else error $ "Can't handle a type of " ++ fld
        return res
    flags <- fmap (reverse . snd) $ readIORef ref
    flags <- return $ assignLong flags
    flags <- return $ assignShort flags

    args <- expandShort flags `fmap` getArgs
    when (any (`elem` args) ["-?","--help"]) $ do
        showHelp short mode flags
        exitSuccess
    when (any (`elem` args) ["-V","--version"]) $ do
        putStrLn short
        exitSuccess
    process flags args x


cmdArgsMode :: Data a => String -> [a] -> IO a
cmdArgsMode short xs = cmdArgs short (head xs)


---------------------------------------------------------------------
-- UTILITIES

reservedShort = "?Vvq"


expandShort :: [Flag] -> [String] -> [String]
expandShort flags = concatMap f
    where
        expand = reservedShort ++ [x | flag <- flags, isFlagBool flag, FldFlag [x] <- flag]
        f ('-':x:xs) | x `elem` expand && xs /= "" = ['-',x] : f ('-':xs)
        f x = [x]


assignLong :: [Flag] -> [Flag]
assignLong = map f
    where f xs = [FldFlag $ flagName x | FldName x <- xs, not $ isFlagArgs xs] ++ xs


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
        Left ("  " ++ map toLower (takeBaseName x) ++ " [FLAG] ["++ty++"]") :
        Left "" :
        Right ("-?","--help","Show usage information") :
        Right ("-V","--version","Show version information") :
        Right ("-v","--verbose","Higher verbosity") :
        Right ("-q","--quiet","Lower verbosity") :
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

process :: Data a => [Flag] -> [String] -> a -> IO a
process flags [] a = return a
process flags (x:xs) a
    | x `elem` ["-v","--verbose"] = writeIORef verbosity 2 >> process flags xs a
    | x `elem` ["-q","--quiet"  ] = writeIORef verbosity 0 >> process flags xs a

process flags xs a = f flags
    where
        f [] = errorIO $ "Unrecognised flag: " ++ head xs
        f (y:ys) = case processOne y xs a of
            Nothing -> f ys
            Just v -> do (a,xs) <- v; process flags xs a


processOne :: Data a => Flag -> [String] -> a -> Maybe (IO (a, [String]))
processOne flag (('-':x:xs):ys) val | x /= '-' = processOne flag (x2:ys) val
    where x2 = if null xs then '-':'-':x:[] else '-':'-':x:'=':xs

processOne flag (('-':'-':x):xs) val | or [n == a | FldFlag n <- flag] = Just $
    case flagType flag of
        FlagBool -> do
            when (b /= "") $ err "does not take an argument"
            return (setValue $ const $ toDyn True, xs)
        FlagItem r -> do
            (text,rest) <- case flagOpt flag of
                Nothing | null b && (null xs || "-" `isPrefixOf` head xs) -> err "needs an argument"
                Just v | null b -> return (v, xs)
                _ | null b -> return (head xs, tail xs)
                _ -> return (drop 1 b, xs)
            upd <- case r text of
                Nothing -> err "couldn't parse argument"
                Just v -> return v
            return (setValue upd, rest)
    where
        (a,b) = break (== '=') x
        setValue = setField val (head [x | FldName x <- flag])
        err msg = errorIO $ "Error on flag " ++ show x ++ ", flag " ++ msg



processOne flag o@(x:xs) val | not ("-" `isPrefixOf` x) && isFlagArgs flag = Just $ return (val2,xs)
    where val2 = setField val (head [x | FldName x <- flag]) (\v -> toDyn $ fromDyn v [""] ++ [x])

processOne _ _ _ = Nothing

