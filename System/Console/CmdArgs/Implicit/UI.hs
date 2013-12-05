{-|
    This module describes the attributes that can be specified on flags and modes.

    Many attributes have examples specified on the following data type:

    > data Sample = Sample {hello :: String}
-}
module System.Console.CmdArgs.Implicit.UI where

import System.Console.CmdArgs.Implicit.Ann
import Data.Typeable


-- | Flag: \"I want users to be able to omit the value associated with this flag.\"
--
--   Make the value of a flag optional. If @--flag@ is given, it will
--   be treated as @--flag=/this_argument/@.
--
-- > {hello = def &= opt "foo"}
-- >   -h --hello[=VALUE]    (default=foo)
--
--   Note that all flags in CmdArgs are optional, and if omitted will use their default value.
--   Those annotated with @opt@ also allow the flag to be present without an associated value.
--   As an example:
--
-- > {hello = "DEFAULT" &= opt "OPTIONAL"}
--
-- > $ main
-- > {hello = "DEFAULT"}
-- > $ main --hello
-- > {hello = "OPTIONAL"}
-- > $ main --hello=VALUE
-- > {hello = "VALUE"}
opt :: (Show a, Typeable a) => a -> Ann
opt x = FlagOptional $ case cast x of
    Just y -> y
    _ -> show x

-- | Flag: \"For this flag, users need to give something of type ...\"
--
--   The the type of a flag's value, usually upper case. Only used
--   for the help message. Commonly the type will be @FILE@ ('typFile')
--   or @DIR@ ('typDir').
--
-- > {hello = def &= typ "MESSAGE"}
-- >   -h --hello=MESSAGE
typ :: String -> Ann
typ = FlagType

-- | Flag: \"Users must give a file for this flag's value.\"
--
--   Alias for @'typ' "FILE"@.
typFile :: Ann
typFile = typ "FILE"

-- | Flag: \"Users must give a directory for this flag's value.\"
--
--   Alias for @'typ' "DIR"@.
typDir :: Ann
typDir = typ "DIR"


-- | Flag/Mode: \"The help message is ...\"
--
--   Descriptive text used in the help output.
--
-- > {hello = def &= help "Help message"}
-- >   -h --hello=VALUE      Help message
help :: String -> Ann
help = Help

{-
-- | Flag: Specify group membership for this flag
--
-- > {str = def &= group "Repository Management"
-- >   ---- Repository Management ----
-- >   -s --str=VALUE
group :: String -> Ann
group = FldGroup
-}

-- | Flag: \"Use this flag name for this field.\"
--
--   Add flags which trigger this option.
--
-- > {hello = def &= name "foo"}
-- >   -h --hello --foo=VALUE
name :: String -> Ann
name = Name

-- | Flag: \"Put non-flag arguments here.\"
--
--   All argument flags not captured by 'argPos' are returned by 'args'.
--
-- > {hello = def &= args}
args :: Ann
args = FlagArgs

-- | Flag: \"Put the nth non-flag argument here.\"
--
--   This field should be used to store a particular argument position
--   (0-based).
--
-- > {hello = def &= argPos 0}
argPos :: Int -> Ann
argPos = FlagArgPos


-- | Flag\/Mode: \"Give these flags/modes a group name in the help output.\"
--
--   This mode will be used for all following modes/flags, until the
--   next @groupname@.
--
-- > {hello = def &= groupname "Welcomes"}
-- > Welcomes
-- >   -h --hello=VALUE
groupname :: String -> Ann
groupname = GroupName


-- | Mode: \"A longer description of this mode is ...\"
--
--   Suffix to be added to the help message.
--
-- > Sample{..} &= details ["More details on the website www.example.org"]
details :: [String] -> Ann
details = ModeHelpSuffix

-- | Modes: \"My program name\/version\/copyright is ...\"
--
--   One line summary of the entire program, the first line of
--   @--help@ and the only line of @--version@.
--
-- > Sample{..} &= summary "CmdArgs v0.0, (C) Neil Mitchell 1981"
summary :: String -> Ann
summary = ProgSummary


-- | Mode: \"If the user doesn't give a mode, use this one.\"
--
--   This mode is the default. If no mode is specified and a mode has this
--   attribute then that mode is selected, otherwise an error is raised.
--
-- > modes [Mode1{..}, Mode2{..} &= auto, Mode3{..}]
auto :: Ann
auto = ModeDefault


-- | Modes: \"My program executable is named ...\"
--
--   This is the name of the program executable. Only used in the help message.
--   Defaults to the type of the mode.
--
-- > Sample{..} &= program "sample"
program :: String -> Ann
program = ProgProgram


-- | Flag: \"Don't guess any names for this field.\"
--
--   A field should not have any flag names guessed for it.
--   All flag names must be specified by 'flag'.
--
-- > {hello = def &= explicit &= name "foo"}
-- >   --foo=VALUE
explicit :: Ann
explicit = Explicit


-- | Flag/Mode: \"Ignore this field, don't let the user set it.\"
--
--   A mode or field is not dealt with by CmdArgs.
--
-- > {hello = def, extra = def &= ignore}
-- >   --hello=VALUE
ignore :: Ann
ignore = Ignore


-- | Modes: \"My program needs verbosity flags.\"
--
--   Add @--verbose@ and @--quiet@ flags.
verbosity :: Ann
verbosity = ProgVerbosity


-- | Modes: \"Customise the help argument.\"
--
--   Add extra options to a help argument, such as 'help', 'name', 'ignore' or 'explicit'.
--
-- > Sample{..} &= helpArg [explicit, name "h"]
helpArg :: [Ann] -> Ann
helpArg = ProgHelpArg


-- | Modes: \"Customise the version argument.\"
--
--   Add extra options to a version argument, such as 'help', 'name', 'ignore', 'summary' or 'explicit'.
--
-- > Sample{..} &= versionArg [ignore]
versionArg :: [Ann] -> Ann
versionArg = ProgVersionArg

-- | Modes: \"Customise the verbosity arguments.\"
--
--   Add extra options to a verbosity arguments (@--verbose@ and @--quiet@),
--   such as 'help', 'name', 'ignore' or 'explicit'. The verbose options come
--   first, followed by the quiet options.
--
-- > Sample{..} &= verbosityArgs [ignore] [name "silent", explicit]
verbosityArgs :: [Ann] -> [Ann] -> Ann
verbosityArgs = ProgVerbosityArgs


-- | Program: \"Turn off \@ expansion.\"
--
--   Usually arguments starting with \@ are treated as a file containing
--   a set of arguments. This annotation turns off that behaviour.
--
-- > Sample{..} &= noAtExpand
noAtExpand :: Ann
noAtExpand = ProgNoAtExpand
