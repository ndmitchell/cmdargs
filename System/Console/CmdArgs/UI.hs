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
import System.Environment
import Control.Monad
import Control.Exception
import System.Exit
import System.FilePath
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

collect :: IO [Info]
collect = do
    x <- readIORef info
    writeIORef info []
    return x

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
