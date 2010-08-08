{-# LANGUAGE RecordWildCards, PatternGuards #-}
{-|
    This module provides simple command line argument processing.
    The main function of interest is 'cmdArgs'.
    A simple example is:

    @data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)@

    @sample = Sample{hello = 'def' '&=' 'help' \"World argument\" '&=' 'opt' \"world\"}@
    @         '&=' summary \"Sample v1\"@

    @main = print =<< 'cmdArgs' sample@


    Attributes are used to control a number of behaviours:
    
    * The help message: 'help', 'typ', 'details', 'summary', 'program'
    
    * Flag behaviour: 'opt', 'enum', 'verbosity'
    
    * Flag name assignment: 'name', 'explicit'
    
    * Controlling non-flag arguments: 'args', 'argPos'

    * multi-mode programs: 'modes', 'auto'

    Each field in the record must be one of the supported atomic types
    (@String@, @Int@, @Integer@, @Float@, @Double@, @Bool@) or a list (@[]@)
    or @Maybe@ wrapping at atomic type.

    /Warning:/ Values created with annotations are not pure - the first
    time they are used they will include the annotations, but subsequently
    they will not. To capture the annotations, so they can be used multiple times,
    use 'cmdArgsMode'.
-}

module System.Console.CmdArgs.Implicit(
    -- * Running command lines
    cmdArgs, cmdArgsMode, cmdArgsRun, CmdArgs(..),
    -- * Constructing command lines
    -- | Attributes can work on a flag (inside a field), on a mode (outside the record),
    --   or on all modes (outside the 'modes' call).
    (&=), modes, enum,
    module System.Console.CmdArgs.Implicit.UI,
    -- * Re-exported for convenience
    -- | Provides a few opaque types (for writing type signatures),
    --   verbosity control, default values with 'def' and the
    --   @Data@/@Typeable@ type classes.
    module System.Console.CmdArgs.Verbosity,
    module System.Console.CmdArgs.Default,
    Ann, Mode,
    Data, Typeable
    ) where

import Data.Data
import System.Environment
import System.Exit
import System.Console.CmdArgs.Explicit(Mode,process)
import System.Console.CmdArgs.Implicit.Ann
import System.Console.CmdArgs.Implicit.Capture
import System.Console.CmdArgs.Implicit.Step1
import System.Console.CmdArgs.Implicit.Step2
import System.Console.CmdArgs.Implicit.Step3
import System.Console.CmdArgs.Implicit.UI
import System.Console.CmdArgs.Verbosity
import System.Console.CmdArgs.Default


-- | Take annotated records and run the corresponding command line.
--   Shortcut for @'cmdArgsRun' . 'cmdArgsMode'@.
cmdArgs :: Data a => a -> IO a
cmdArgs = cmdArgsRun . cmdArgsMode


-- | Take annotated records and turn them in to a 'Mode' value, that can
--   make use of the "System.Console.CmdArgs.Explicit" functions (i.e. 'process').
--
--   Annotated records are impure, and will only contain annotations on
--   their first use. The result of this function is pure, and can be reused.
cmdArgsMode :: Data a => a -> Mode (CmdArgs a)
cmdArgsMode = step3 . step2 . step1 . capture


-- | Run a Mode structure. This function reads the command line arguments
--   and then performs as follows:
--
--   * If invalid arguments are given, it will display the error message
--     and exit.
--
--   * If @--help@ is given, it will display the help message and exit.
--
--   * If @--version@ is given, it will display the version and exit.
--
--   * In all other circumstances the program will return a value.
--
--   * Additionally, if either @--quiet@ or @--verbose@ is given (see 'verbosity')
--     it will set the verbosity (see 'setVerbosity').
cmdArgsRun :: Mode (CmdArgs a) -> IO a
cmdArgsRun m = do
    args <- getArgs
    case process m args of
        Left x -> do putStrLn x; exitFailure
        Right CmdArgs{..} 
            | Just x <- cmdArgsHelp -> do putStrLn x; exitSuccess
            | Just x <- cmdArgsVersion -> do putStrLn x; exitSuccess
            | otherwise -> do
                maybe (return ()) setVerbosity cmdArgsVerbosity
                return cmdArgsValue


-- | Modes: \"I want a program with multiple modes, like darcs or cabal.\"
--
--   Takes a list of modes, and creates a mode which includes them all.
--   If you want one of the modes to be chosen by default, see 'auto'.
--
-- > data Modes = Mode1 | Mode2 | Mode3 deriving Data
-- > cmdArgs $ modes [Mode1,Mode2,Mode3]
modes :: Data a => [a] -> a
modes = many

-- | Flag: \"I want several different flags to set this one field to different values.\"
--
--   This annotation takes a type which is an enumeration, and provides multiple
--   separate flags to set the field to each value.
--
-- > data State = On | Off deriving Data
-- > data Mode = Mode {state :: State}
-- > cmdArgs $ Mode {state = enum [On &= help "Turn on",Off &= help "Turn off"]}
-- >   --on   Turn on
-- >   --off  Turn off
enum :: Data a => [a] -> a
enum = many
