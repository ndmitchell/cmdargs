
-- | This module constructs command lines. You may either use the helper functions
--   ('flagNone', 'flagOpt', 'mode' etc.) or construct the type directly. These
--   types are intended to give all the necessary power to the person constructing
--   a command line parser.
--
--   For people constructing simpler command line parsers, the module
--   "System.Console.CmdArgs.Implicit" may be more appropriate.
module System.Console.CmdArgs.Explicit(
    -- * Running command lines
    module System.Console.CmdArgs.Explicit.Process,
    -- * Constructing command lines
    module System.Console.CmdArgs.Explicit.Type,
    module System.Console.CmdArgs.Explicit,
    -- * Displaying help
    module System.Console.CmdArgs.Explicit.Help
    ) where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Explicit.Process
import System.Console.CmdArgs.Explicit.Help
import System.Console.CmdArgs.Default
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Verbosity

import Data.Char


-- | Create a help flag triggered by @-?@/@--help@.
flagHelpSimple :: (a -> a) -> Flag a
flagHelpSimple f = flagNone ["help","?"] f "Display help message"


-- | Create a help flag triggered by @-?@/@--help@. The user
--   may optionally modify help by specifying the format, such as:
--
-- > --help=all          - help for all modes
-- > --help=html         - help in HTML format
-- > --help=100          - wrap the text at 100 characters
-- > --help=100,one      - full text wrapped at 100 characters
flagHelpFormat :: (HelpFormat -> TextFormat -> a -> a) -> Flag a
flagHelpFormat f = (flagOpt "" ["help","?"] upd "" "Display help message"){flagInfo = FlagOptRare ""}
    where
        upd s v = case format s of
            Left e -> Left e
            Right (a,b) -> Right $ f a b v

        format :: String -> Either String (HelpFormat,TextFormat)
        format xs = foldl (\acc x -> either Left (f x) acc) (Right def) (sep xs)
            where
                sep = words . map (\x -> if x `elem` ":," then ' ' else toLower x)
                f x (a,b) = case x of
                    "all" -> Right (HelpFormatAll,b)
                    "one" -> Right (HelpFormatOne,b)
                    "def" -> Right (HelpFormatDefault,b)
                    "html" -> Right (a,HTML)
                    "text" -> Right (a,defaultWrap)
                    _ | all isDigit x -> Right (a,Wrap $ read x)
                    _ -> Left "unrecognised help format, expected one of: all one def html text <NUMBER>"


-- | Create a version flag triggered by @-V@/@--version@.
flagVersion :: (a -> a) -> Flag a
flagVersion f = flagNone ["version","V"] f "Print version information"


-- | Create verbosity flags triggered by @-v@/@--verbose@ and
--   @-q@/@--quiet@
flagsVerbosity :: (Verbosity -> a -> a) -> [Flag a]
flagsVerbosity f =
    [flagNone ["verbose","v"] (f Loud) "Loud verbosity"
    ,flagNone ["quiet","q"] (f Quiet) "Quiet verbosity"]
