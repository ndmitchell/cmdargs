
-- |  A module to deal with verbosity, how \'chatty\' a program should be.
--    This module defines the 'Verbosity' data type, along with functions
--    for manipulating a global verbosity value.
module System.Console.CmdArgs.Verbosity(
    Verbosity(..), setVerbosity, getVerbosity,
    isNormal, isLoud,
    whenNormal, whenLoud
    ) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe


-- | The verbosity data type
data Verbosity
    = Quiet  -- ^ Only output essential messages (typically errors)
    | Normal -- ^ Output normal messages (typically errors and warnings)
    | Loud   -- ^ Output lots of messages (typically errors, warnings and status updates)
      deriving (Eq,Ord,Bounded,Enum,Show,Read)


{-# NOINLINE ref #-}
ref :: IORef Verbosity
ref = unsafePerformIO $ newIORef Normal


-- | Set the global verbosity.
setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef ref

-- | Get the global verbosity. Initially @Normal@ before any calls to 'setVerbosity'.
getVerbosity :: IO Verbosity
getVerbosity = readIORef ref

-- | Used to test if warnings should be output to the user.
--   @True@ if the verbosity is set to 'Normal' or 'Loud' (when @--quiet@ is /not/ specified).
isNormal :: IO Bool
isNormal = fmap (>=Normal) getVerbosity

-- | Used to test if status updates should be output to the user.
--   @True@ if the verbosity is set to 'Loud' (when @--verbose@ is specified).
isLoud :: IO Bool
isLoud = fmap (>=Loud) getVerbosity


-- | An action to perform if the verbosity is normal or higher, based on 'isNormal'.
whenNormal :: IO () -> IO ()
whenNormal act = do
    b <- isNormal
    when b act

-- | An action to perform if the verbosity is loud, based on 'isLoud'.
whenLoud :: IO () -> IO ()
whenLoud act = do
    b <- isLoud
    when b act
