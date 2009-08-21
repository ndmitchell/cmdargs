{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.CmdArgs.UI(
    mode, Mode, (&),
    text, args, argPos, typ, typFile, typDir, helpSuffix, empty, flag
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


---------------------------------------------------------------------
-- STATE MANAGEMENT

{-# NOINLINE info #-}
info :: IORef [Info]
info = unsafePerformIO $ newIORef []

(&) :: forall a . a -> Info -> a
(&) x i = unsafePerformIO $ do
    modifyIORef info (i:)
    return x

collect :: a -> IO [Info]
collect x = do
    writeIORef info []
    evaluate x
    x <- readIORef info
    return x

mode :: Data a => a -> Mode a
mode val = unsafePerformIO $ do
    info <- collect val
    let con = toConstr val
        name = map toLower $ showConstr con
    flags <- sequence $ flip gmapQ val $ \i -> do
        info <- collect i
        case toFlagType $ typeOf i of
            Nothing -> error $ "Can't handle a type of " ++ show (typeOf i)
            Just x -> return $ flagInfo flagDefault{flagVal=toDyn i,flagType=x} info
    flags <- return $ zipWith (\flag name -> flag{flagName=name}) flags (constrFields con)
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
      deriving Show


modeInfo :: Mode a -> [Info] -> Mode a
modeInfo = foldl $ \m x -> case x of
    Text x -> m{modeText=x}
    HelpSuffix x -> m{modeHelpSuffix=x}
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
    x -> error $ "Invalid info at argument level: " ++ show x


---------------------------------------------------------------------
-- USER INTERFACE

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
flag = FldFlag

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
