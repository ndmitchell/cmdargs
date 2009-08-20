{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    Simple command line argument handling
-}

module System.Console.CmdArgs(
    Data, Typeable,
    Default(..),
    cmdArgs, cmdModes,
    mode, modeValue, Mode, (&),
    isQuiet, isNormal, isLoud,
    text, args, argPos, typ, typFile, typDir, helpSuffix, empty, flag
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
import Data.Function


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

type Flag = [Info]

data Mode a = Mode a [Info] [Flag]

modeValue :: Mode a -> a
modeValue (Mode x _ _) = x

mode :: Data a => a -> Mode a
mode val = unsafePerformIO $ do
    evaluate val
    let con = toConstr val
    top <- fmap (ModName (map toLower $ showConstr con):) collect
    ref <- newIORef (constrFields con, [])
    val <- flip gmapM val $ \i -> do
        res <- evaluate i
        info <- collect
        let typ = typeOf i
        modifyIORef ref $ \(fld:flds, xs) ->
            if hasFlagType [FldType typ]
            then (flds, (FldName fld:FldType typ:FldValue (toDyn i):info):xs)
            else error $ "Can't handle a type of " ++ fld
        return res
    flags <- fmap (reverse . snd) $ readIORef ref
    return $ Mode val top flags


---------------------------------------------------------------------
-- USER INTERFACE

data Info
    = FldName String -- the record name
    | FldType TypeRep -- the field type
    | FldValue Dynamic
    | FldEmpty String
    | FldArgs
    | FldArgPos Int
    | FldTyp String
    | ModName String -- the constructor name
    | Text String
    | Flag String
    | Explicit
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
text = Text

-- | Flags which work
flag :: String -> Info
flag = Flag

-- | Where to put the non-flag arguments
args :: Info
args = FldArgs

-- | 0-based argument position
argPos :: Int -> Info
argPos = FldArgPos


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
    where def = [FldType (typeOf True), FldValue (toDyn False), Explicit]


---------------------------------------------------------------------
-- STRUCTURED FLAGS

-- Simple stuff
isFlagArgs xs = [] /= [() | FldArgs{} <- xs]
isFlagArgPos xs = [] /= [() | FldArgPos{} <- xs]
isFlagFlag xs = not $ isFlagArgs xs || isFlagArgPos xs
flagName xs = head [x | FldName x <- xs]
isFldFlag Flag{} = True; isFldFlag _ = False
fldFlags xs = [x | Flag x <- xs]
isExplicit xs = [] /= [() | Explicit{} <- xs]
isFlagOpt = isJust . flagOpt
flagOpt xs = listToMaybe [x | FldEmpty x <- xs]
flagText xs = unwords [x | Text x <- xs]
isFlagBool xs = case flagType xs of FlagBool -> True; _ -> False
hasFlagType = isJust . toFlagType
flagType = fromJust . toFlagType
modeName xs = head [x | ModName x <- xs]


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


---------------------------------------------------------------------
-- FLAG EXPANSION

-- (fldname,([flags],explicit))
type FlagNames = [(String,([String],Bool))]

-- Error if:
--   Two things with the same FldName have different FldFlag or Explicit
--   Two fields without the same FldName have different FldFlag
expand :: [Mode a] -> [Mode a]
expand xs | not $ checkFlags ys = error "Flag's don't meet their condition"
          | otherwise = xs3
    where
        xs3 = [Mode a b [if isFlagFlag c then filter (not . isFldFlag) c ++ map Flag (fst $ fromJust $ lookup (flagName c) ys2) else c | c <- cs] | Mode a b cs <- xs2]
        ys2 = assignShort $ assignLong ys
        ys = sort $ nub [(flagName x, (fldFlags x, isExplicit x)) | Mode _ _ x <- xs2, x <- x, isFlagFlag x]
        xs2 = [Mode a b (autoArgs++c) | Mode a b c <- xs]


checkFlags :: FlagNames -> Bool
checkFlags xs | any ((/=) 1 . length) $ groupBy ((==) `on` fst) xs = error "Two record names have different flags"
              | nub names /= names = error "One flag has been assigned twice"
              | otherwise = True
    where names = concatMap (fst . snd) xs


assignLong :: FlagNames -> FlagNames
assignLong xs = map f xs
    where
        seen = concatMap (fst . snd) xs
        f (name,(already,False)) | name `notElem` seen = (name,(g name:already,False))
        f x = x
        g xs | "_" `isSuffixOf` xs = g $ init xs
        g xs = [if x == '_' then '-' else x | x <- xs]


assignShort :: FlagNames -> FlagNames
assignShort xs = zipWith (\x (a,(b,c)) -> (a,(maybe [] (return . return) x ++ b,c))) good xs
    where
        seen = concat $ filter ((==) 1 . length) $ concatMap (fst . snd) xs
        guesses = map guess xs :: [Maybe Char]
        dupes = let gs = catMaybes guesses in nub $ gs \\ nub gs
        good = [if maybe True (`elem` (dupes++seen)) g then Nothing else g | g <- guesses] :: [Maybe Char]

        -- guess at a possible short flag
        guess (name,(already,False)) | all ((/=) 1 . length) already = Just $ head $ head already
        guess _ = Nothing


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
