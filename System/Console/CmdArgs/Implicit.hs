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
    
    * The help message: 'help', 'typ', 'details', 'summary', 'program', 'groupname'
    
    * Flag behaviour: 'opt', 'enum', 'verbosity', 'ignore'
    
    * Flag name assignment: 'name', 'explicit'
    
    * Controlling non-flag arguments: 'args', 'argPos'

    * multi-mode programs: 'modes', 'auto'

    /Supported Types/: Each field in the record must be one of the supported
    atomic types (@String@, @Int@, @Integer@, @Float@, @Double@, @Bool@, an
    enumeration, a tuple of atomic types) or a list (@[]@) or @Maybe@ wrapping
    at atomic type.

    /Missing Fields/: If a field is shared by multiple modes, it may be omitted
    in subsequent modes, and will default to the previous value.

    /Purity/: Values created with annotations are not pure - the first
    time they are computed they will include the annotations, but subsequently
    they will not. If you wish to run the above example in a more robust way:

    @sample = 'cmdArgsMode' $ Sample{hello = ... -- as before@

    @main = print =<< 'cmdArgsRun' sample@

    Even using this scheme, sometimes GHC's optimisations may share values who
    have the same annotation. To disable sharing you may need to specify
    @\{\-\# OPTIONS_GHC -fno-cse \#\-\}@ in the module you define the flags.
-}
module System.Console.CmdArgs.Implicit(
    -- * Running command lines
    cmdArgs, cmdArgsMode, cmdArgsRun, cmdArgs_, cmdArgsMode_, cmdArgsApply, CmdArgs(..),
    -- * Constructing command lines
    -- | Attributes can work on a flag (inside a field), on a mode (outside the record),
    --   or on all modes (outside the 'modes' call).
    module System.Console.CmdArgs.Implicit.UI,
    -- ** Impure
    (&=), modes, enum,
    -- ** Pure
    (+=), record, atom, Annotate((:=)), enum_, modes_,
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
import Data.Generics.Any
import System.Exit
import System.Console.CmdArgs.Explicit(Mode,processArgs,remap)
import System.Console.CmdArgs.Implicit.Ann
import System.Console.CmdArgs.Annotate hiding ((&=))
import qualified System.Console.CmdArgs.Annotate as A((&=))
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Implicit.Local
import System.Console.CmdArgs.Implicit.Global
import System.Console.CmdArgs.Implicit.UI
import System.Console.CmdArgs.Verbosity
import System.Console.CmdArgs.Default


-- | Take impurely annotated records and run the corresponding command line.
--   Shortcut for @'cmdArgsRun' . 'cmdArgsMode'@.
cmdArgs :: Data a => a -> IO a
cmdArgs = cmdArgsRun . cmdArgsMode


-- | Take purely annotated records and run the corresponding command line.
--   Shortcut for @'cmdArgsRun' . 'cmdArgsMode_'@.
cmdArgs_ :: Data a => Annotate Ann -> IO a
cmdArgs_ = cmdArgsRun . cmdArgsMode_


cmdArgsCapture :: Data a => Capture Ann -> Mode (CmdArgs a)
cmdArgsCapture = remap embed proj . global . local
    where embed = fmap fromAny
          proj x = (fmap Any x, embed)


-- | Take impurely annotated records and turn them in to a 'Mode' value, that can
--   make use of the "System.Console.CmdArgs.Explicit" functions (i.e. 'process').
--
--   Annotated records are impure, and will only contain annotations on
--   their first use. The result of this function is pure, and can be reused.
cmdArgsMode :: Data a => a -> Mode (CmdArgs a)
cmdArgsMode = cmdArgsCapture . capture


-- | Take purely annotated records and turn them in to a 'Mode' value, that can
--   make use of the "System.Console.CmdArgs.Explicit" functions (i.e. 'process').
cmdArgsMode_ :: Data a => Annotate Ann -> Mode (CmdArgs a)
cmdArgsMode_ = cmdArgsCapture . capture_


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
cmdArgsRun m = cmdArgsApply =<< processArgs m


-- | Perform the necessary actions dictated by a 'CmdArgs' structure.
--
--   * If 'cmdArgsHelp' is @Just@, it will display the help message and exit.
--
--   * If 'cmdArgsVersion' is @Just@, it will display the version and exit.
--
--   * In all other circumstances it will return a value.
--
--   * Additionally, if 'cmdArgsVerbosity' is @Just@ (see 'verbosity')
--     it will set the verbosity (see 'setVerbosity').
cmdArgsApply :: CmdArgs a -> IO a
cmdArgsApply CmdArgs{..}
    | Just x <- cmdArgsHelp = do putStr x; exitSuccess
    | Just x <- cmdArgsVersion = do putStr x; exitSuccess
    | otherwise = do
        maybe (return ()) setVerbosity cmdArgsVerbosity
        return cmdArgsValue


-- | Modes: \"I want a program with multiple modes, like darcs or cabal.\"
--
--   Takes a list of modes, and creates a mode which includes them all.
--   If you want one of the modes to be chosen by default, see 'auto'.
--
-- > data Modes = Mode1 | Mode2 | Mode3 deriving Data
-- > cmdArgs $ modes [Mode1,Mode2,Mode3]
modes :: Data val => [val] -> val
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
enum :: Data val => [val] -> val
enum = many


-- | Add an annotation to a value. Note that if the value is evaluated
--   more than once the annotation will only be available the first time.
{-# INLINE (&=) #-}
(&=) :: Data val => val -> Ann -> val
(&=) = (A.&=)


enum_ :: (Data c, Data f) => (c -> f) -> [Annotate Ann] -> Annotate Ann
enum_ = (:=+)

modes_ :: [Annotate Ann] -> Annotate Ann
modes_ = many_


