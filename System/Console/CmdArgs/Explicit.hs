{-# LANGUAGE ScopedTypeVariables #-}
{-|
    This module constructs command lines. You may either use the helper functions
    ('flagNone', 'flagOpt', 'mode' etc.) or construct the type directly. These
    types are intended to give all the necessary power to the person constructing
    a command line parser.

    For people constructing simpler command line parsers, the module
    "System.Console.CmdArgs.Implicit" may be more appropriate.

    As an example of a parser:

  > arguments :: Mode [(String,String)]
  > arguments = mode "explicit" [] "Explicit sample program" (flagArg (upd "file") "FILE")
  >     [flagOpt "world" ["hello","h"] (upd "world") "WHO" "World argument"
  >     ,flagReq ["greeting","g"] (upd "greeting") "MSG" "Greeting to give"
  >     ,flagHelpSimple (("help",""):)]
  >     where upd msg x v = Right $ (msg,x):v

    And this can be invoked by:

  > main = do
  >     xs <- processArgs arguments
  >     if ("help","") `elem` xs then
  >         print $ helpText HelpFormatDefault arguments
  >      else
  >         print xs

    /Groups/: The 'Group' structure allows flags/modes to be grouped for the purpose of
    displaying help. When processing command lines, the group structure is ignored.

    /Modes/: The Explicit module allows multiple mode programs by placing additional modes
    in 'modeGroupModes'. Every mode is allowed sub-modes, and thus multiple levels of mode
    may be created. Given a mode @x@ with sub-modes @xs@, if the first argument corresponds
    to the name of a sub-mode, then that sub-mode will be applied. If not, then the arguments
    will be processed by mode @x@. Consequently, if you wish to force the user to explicitly
    enter a mode, simply give sub-modes, and leave 'modeArgs' as @Nothing@. Alternatively, if
    you want one sub-mode to be selected by default, place all it's flags both in the sub-mode
    and the outer mode.

    /Parsing rules/: Command lines are parsed as per most GNU programs. Short arguments single
    letter flags start with @-@, longer flags start with @--@, and everything else is considered
    an argument. Anything after @--@ alone is considered to be an argument. For example:

  > -f --flag argument1 -- --argument2

    This command line passes one single letter flag (@f@), one longer flag (@flag@) and two arguments
    (@argument1@ and @--argument2@).
-}
module System.Console.CmdArgs.Explicit(
    -- * Running command lines
    process, processArgs, processValue,
    -- * Constructing command lines
    module System.Console.CmdArgs.Explicit.Type,
    flagHelpSimple, flagHelpFormat, flagVersion, flagsVerbosity,
    -- * Displaying help
    module System.Console.CmdArgs.Explicit.Help,
    -- * Utilities for working with command line
    module System.Console.CmdArgs.Explicit.SplitJoin,
    Complete(..), complete
    ) where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Explicit.Process
import System.Console.CmdArgs.Explicit.Help
import System.Console.CmdArgs.Explicit.SplitJoin
import System.Console.CmdArgs.Explicit.Complete
import System.Console.CmdArgs.Default
import System.Console.CmdArgs.Helper
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Verbosity

import Control.Monad
import Data.Char
import Data.Maybe
import System.Environment
import System.Exit
import System.IO


-- | Process the flags obtained by @'getArgs'@ with a mode. Displays
--   an error and exits with failure if the command line fails to parse, or returns
--   the associated value. Implemented in terms of 'process'. This function makes
--   use of the following environment variables:
--
-- * @$CMDARGS_COMPLETE@ - causes the program to produce completions using 'complete', then exit.
--   Completions are based on the result of 'getArgs', the index of the current argument is taken
--   from @$CMDARGS_COMPLETE@ (set it to @-@ to complete the last argument), and the index within
--   that argument is taken from @$CMDARGS_COMPLETE_POS@ (if set).
--
-- * @$CMDARGS_HELPER@\/@$CMDARGS_HELPER_/PROG/@ - uses the helper mechanism for entering command
--   line programs as described in "System.Console.CmdArgs.Helper".
processArgs :: Mode a -> IO a
processArgs m = do
    env <- getEnvironment
    case lookup "CMDARGS_COMPLETE" env of
        Just x -> do
            args <- getArgs
            let argInd = fromMaybe (length args - 1) $ readMay x
                argPos = fromMaybe (if argInd >= 0 && argInd < length args then length (args !! argInd) else 0) $
                         readMay =<< lookup "CMDARGS_COMPLETE_POS" env
            print $ complete m (concatMap words args) (argInd,argPos)
            exitWith ExitSuccess
        Nothing -> do
            nam <- getProgName
            let var = mplus (lookup ("CMDARGS_HELPER_" ++ show (map toUpper $ head $ modeNames m ++ [nam])) env)
                            (lookup "CMDARGS_HELPER" env)
            case var of
                Nothing -> run =<< getArgs
                Just cmd -> do
                    res <- execute cmd m []
                    case res of
                        Left err -> do
                            hPutStrLn stderr $ "Error when running helper " ++ cmd
                            hPutStrLn stderr err
                            exitFailure               
                        Right args -> run args
    where
        run args = case process m args of
            Left x -> do hPutStrLn stderr x; exitFailure
            Right x -> return x


readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing


-- | Process a list of flags (usually obtained from @getArgs@) with a mode. Displays
--   an error and exits with failure if the command line fails to parse, or returns
--   the associated value. Implemeneted in terms of 'process'. This function
--   does not take account of any environment variables that may be set
--   (see 'processArgs').
processValue :: Mode a -> [String] -> a
processValue m xs = case process m xs of
    Left x -> error x
    Right x -> x


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
                    "bash" -> Right (HelpFormatBash,Wrap 1000000)
                    "zsh"  -> Right (HelpFormatZsh ,Wrap 1000000)
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
