{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module System.Console.CmdArgs.UI(
    mode, Mode, (&=), (&),
    text, args, argPos, typ, typFile, typDir, helpSuffix, empty, flag, prog,
    unknownFlags, defMode, enum
    ) where

import System.Console.CmdArgs.Type
import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.IORef
import Control.Exception
import Data.Char
import Control.Monad.State
import Data.Function


infix 1 &=
infixl 2 &

---------------------------------------------------------------------
-- STATE MANAGEMENT

{-# NOINLINE info #-}
info :: IORef [Info]
info = unsafePerformIO $ newIORef []

(&=) :: forall a . a -> [Info] -> a
(&=) x is = unsafePerformIO $ do
    writeIORef info is
    return x

(&) :: [Info] -> [Info] -> [Info]
(&) = (++)

collect :: a -> IO [Info]
collect x = do
    evaluate x
    x <- readIORef info
    writeIORef info [] -- don't leak the info's
    return x

mode :: Data a => a -> Mode a
mode val = unsafePerformIO $ do
    info <- collect val
    let con = toConstr val
        name = map toLower $ showConstr con
    ref <- newIORef $ constrFields con
    flags <- liftM concat $ sequence $ flip gmapQ val $ \i -> do
        info <- collect i
        n:ns <- readIORef ref
        writeIORef ref ns
        case toFlagType $ typeOf i of
            _ | [FldEnum xs] <- info -> return [x{flagName=n} | x <- xs]
            Nothing -> error $ "Can't handle a type of " ++ show (typeOf i)
            Just x -> return [flagInfo flagDefault{flagName=n,flagKey=n,flagVal=toDyn i,flagType=x} info]
    return $ modeInfo modeDefault{modeVal=val,modeName=name,modeFlags=flags} info


---------------------------------------------------------------------
-- INFO ITEMS

data Info
    = FldEmpty String
    | FldArgs
    | FldArgPos Int
    | FldTyp String
    | Text String
    | FldFlag String
    | Explicit
    | HelpSuffix [String]
    | FldUnknown
    | FldEnum [Flag]
    | ModDefault
    | ModProg String
      deriving Show


modeInfo :: Mode a -> [Info] -> Mode a
modeInfo = foldl $ \m x -> case x of
    Text x -> m{modeText=x}
    HelpSuffix x -> m{modeHelpSuffix=x}
    ModDefault -> m{modeDef=True}
    ModProg x -> m{modeProg=Just x}
    x -> error $ "Invalid info at mode level: " ++ show x


flagInfo :: Flag -> [Info] -> Flag
flagInfo = foldl $ \m x -> case x of
    Text x -> m{flagText=x}
    Explicit -> m{flagExplicit=True}
    FldTyp x -> m{flagTyp=x}
    FldEmpty x -> m{flagOpt=Just x}
    FldFlag x -> m{flagFlag=x:flagFlag m}
    FldArgs -> m{flagArgs=Just Nothing}
    FldArgPos i -> m{flagArgs=Just (Just i)}
    FldUnknown -> m{flagUnknown=True}
    x -> error $ "Invalid info at argument level: " ++ show x


---------------------------------------------------------------------
-- USER INTERFACE

-- | A default argument if none is specified
empty :: (Show a, Typeable a) => a -> [Info]
empty x = return $ case cast x of
    Just y -> FldEmpty y
    _ -> FldEmpty $ show x

-- | The type of the argument
typ :: String -> [Info]
typ = return . FldTyp

-- | Descriptive text for the option
text :: String -> [Info]
text = return . Text

-- | Flags which work
flag :: String -> [Info]
flag = return . FldFlag

-- | Where to put the non-flag arguments
args :: [Info]
args = [FldArgs]

-- | 0-based argument position
argPos :: Int -> [Info]
argPos = return . FldArgPos


typDir, typFile :: [Info]
typFile = typ "FILE"
typDir = typ "DIR"


helpSuffix :: [String] -> [Info]
helpSuffix = return . HelpSuffix


unknownFlags :: [Info]
unknownFlags = [FldUnknown]

defMode :: [Info]
defMode = [ModDefault]

prog :: String -> [Info]
prog = return . ModProg

enum :: (Typeable a, Eq a, Show a) => a -> [a] -> a
enum def xs = unsafePerformIO $ do
    ys <- forM xs $ \x -> do
        y <- collect x
        return $ flagInfo flagDefault{flagKey=map toLower (show x), flagType=FlagBool (toDyn x), flagVal = toDyn False} y
    return $ def &= [FldEnum ys]
