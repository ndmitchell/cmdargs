{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    This module provides simple command line argument processing.
    The main function of interest is 'cmdArgs'.
    A simple example is:

    @data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)@

    @sample = 'mode' $ Sample{hello = def '&=' 'text' \"World argument\" '&' 'empty' \"world\"}@

    @main = print =<< 'cmdArgs' \"Sample v1, (C) Neil Mitchell 2009\" [sample]@


    Attributes are used to control a number of behaviours:
    
    * The help message: 'text', 'typ', 'helpSuffix', 'prog'
    
    * Default behaviour: 'empty', 'defMode'
    
    * Flag name assignment: 'flag', 'explicit', 'enum'
    
    * Controlling non-flag arguments: 'args', 'argPos', 'unknownFlags'
-}

module System.Console.CmdArgs(
    -- * Running command lines
    cmdArgs, cmdArgsParse, modeValue,
    -- * Attributes
    module System.Console.CmdArgs.UI,
    -- * Verbosity control
    isQuiet, isNormal, isLoud,
    -- * Display help information
    HelpFormat(..), cmdArgsHelp, getCmdArgsHelp,
    -- * Default values
    Default(..),
    -- * Re-exported for convenience
    Data, Typeable
    ) where

import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.Either
import Data.IORef
import System.Environment
import System.Exit
import System.FilePath
import Data.Char
import Control.Monad.State
import Data.Function

import System.Console.CmdArgs.Type
import System.Console.CmdArgs.UI
import System.Console.CmdArgs.Expand
import System.Console.CmdArgs.Flag
import System.Console.CmdArgs.Help


---------------------------------------------------------------------
-- DEFAULTS

-- | Class for default values
class Default a where
    -- | Provide a default value
    def :: a

instance Default Bool where def = False
instance Default [a] where def = []
instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0


---------------------------------------------------------------------
-- VERBOSITY CONTROL

{-# NOINLINE verbosity #-}
verbosity :: IORef Int -- 0 = quiet, 1 = normal, 2 = verbose
verbosity = unsafePerformIO $ newIORef 1

-- | Used to test if essential messages should be output to the user.
--   Always true (since even @--quiet@ wants essential messages output).
--   Must be called after 'cmdArgs'.
isQuiet :: IO Bool
isQuiet = return True

-- | Used to test if normal messages should be output to the user.
--   True unless @--quiet@ is specified.
--   Must be called after 'cmdArgs'.
isNormal :: IO Bool
isNormal = fmap (>=1) $ readIORef verbosity

-- | Used to test if helpful debug messages should be output to the user.
--   False unless @--verbose@ is specified.
--   Must be called after 'cmdArgs'.
isLoud :: IO Bool
isLoud = fmap (>=2) $ readIORef verbosity


---------------------------------------------------------------------
-- MAIN DRIVERS

-- | Extract the default value from inside a Mode.
modeValue :: Mode a -> a
modeValue = modeVal


-- | Pure function for parsing arguments.  Given explicit command-line
--   words and the operation modes this will return either an error
--   (or version) string generator or else it will return the selected
--   mode's structure.
--
--   Alternatively: use 'cmdArgs' for a version that will
--   automatically read the current command line and throw IO
--   exceptions on failures.
cmdArgsParse :: Data a
    => String -- ^ Information about the program, something like: @\"ProgramName v1.0, Copyright PersonName 2000\"@.
    -> [Mode a] -- ^ The modes of operation, constructed by 'mode'. For single mode programs it is a singleton list.
    -> Maybe Int -- ^ The terminal width for displaying any help text
                 -- (if known); Nothing = use default of 80 chars.
    -> [String] -- ^ The command-line arguments to be parsed
    -> Either (Bool,String -> String) a  -- ^ Left is (is success?, string_func), where
                                         -- string_func is called with the program name and will
                                         -- return the error or version string to be
                                         -- displayed.  Right is the selected mode structure.
cmdArgsParse short modes width inputargs =
    let emodes = expand modes
        (mode,args) = parseModes emodes inputargs
        wanthelp = hasAction args "!help"
        wantver = hasAction args "!version"
        argerrors = [x | Error x <- args]
        argerror = listToMaybe argerrors
        parsed = Right . applyActions args . modeValue
        failmsg m = Left (False, const m)
        helpmode = case mode of
                     Right (True,mode) -> Just mode
                     _ -> Nothing
        helpfmt = fromDyn (op undefined) ""
        Update _ op = fromJust $ getAction args "!help"
    in case (wanthelp, wantver) of
         (True,_) -> Left (True, getCmdArgsHelp short emodes helpmode helpfmt width)
         (False,True) -> Left (True, const short)
         _ -> flip (either failmsg) mode $ 
              \msv -> maybe (parsed $ snd msv) failmsg argerror


-- | The main entry point for programs using CmdArgs.  This is the IO
--   version that will automatically read the current command-line
--   arguments and will generate program exit calls if appropriate
--   (e.g. on invalid arg input).
--
--   Approximately:
--
--   @cmdArgs s ms = cmdArgsParse s ms `fmap` getArgs
--
--   For an example see "System.Console.CmdArgs".
cmdArgs :: Data a
    => String -- ^ Information about the program, something like: @\"ProgramName v1.0, Copyright PersonName 2000\"@.
    -> [Mode a] -- ^ The modes of operation, constructed by 'mode'. For single mode programs it is a singleton list.
    -> IO a
cmdArgs short modes = do
    termwidth <- catch (fmap (Just . read) $ getEnv "COLUMNS") $ return . (const Nothing)
    parsed <- cmdArgsParse short modes termwidth `fmap` getArgs
    case parsed of
      Left (s,e) -> getProgName >>= putStrLn . e
                    >> if s then exitSuccess else exitFailure
      Right p -> return p


---------------------------------------------------------------------
-- HELP INFORMATION

-- | Format to display help in.
data HelpFormat
    = Text -- ^ As output on the console.
    | HTML -- ^ Suitable for inclusion in web pages (uses a table rather than explicit wrapping).
    deriving (Eq,Ord,Show,Read,Enum,Bounded)

-- | Fetch the help message as multi-line String specifying help
--   output as it would appear with @--help@.  The first argument
--   should match the first argument to 'cmdArgs'.
--
--   Alternatively, use 'cmdArgsHelp' to display the help message to stdout.
getCmdArgsHelp :: String -- ^ Information about the program (see 'cmdArgs' above)
               -> [Mode a] -- ^ All specified modes
               -> Maybe (Mode a) -- ^ Help is returned for all modes or just the mode specified here
               -> String -- ^ Format to return help in (e.g. "text" or "html")
               -> Maybe Int -- ^ Width of the terminal for formatting the help text (if known).
               -> String -- ^ Raw program name
               -> String -- ^ Returns formatted help information
getCmdArgsHelp i ms cm hf tw rpn = let hi = (helpInfo rpn i ms (maybe ms (flip (:) []) cm))
                                   in showHelp hi hf $ maybe 80 id tw


-- | Display the help message, as it would appear with @--help@.
--   The first argument should match the first argument to 'cmdArgs'.
cmdArgsHelp :: String -> [Mode a] -> HelpFormat -> IO String
cmdArgsHelp short xs format = do
    rpn <- getProgName
    termwidth <- catch (fmap read $ getEnv "COLUMNS") $ return . const 80
    return $ showHelp (helpInfo rpn short modes modes) (show format) termwidth
    where modes = expand xs


helpInfo :: String -> String -> [Mode a] -> [Mode a] -> [Help]
helpInfo rpn short tot now =
    let prog = head $ mapMaybe modeProg tot ++ [map toLower $ takeBaseName rpn]
        justlist = length now /= 1 && length tot > 2
        info = [(if justlist
                   then [Deuce (name ++ " --", head [head hl | length hl > 0, length (head hl) > 0])]
                   else [Norm $ unwords $ prog : [['['|def] ++ name ++ [']'|def] | length tot /= 1] ++ trail] ++
                            [Para ["  ", text] | text /= ""]
                ,concatMap helpFlag flags)
               | Mode{modeName=name,modeFlags=flags,modeText=text,modeDef=def} <- now
               , let args = map snd $ sortBy (compare `on` fst) $ concatMap helpFlagArgs flags
                     trail =  "[FLAG]" : args
                     hl = lines text
               ]
        dupes = if length now == 1 then [] else foldr1 intersect (map snd info)
        flggroupinfo fs = let fgs = groupBy ((==) `on` snd) $ sortBy (compare `on` snd) fs
                              gid g = if null g then Nothing else snd $ head g
                              ginfo g | isNothing (gid g) = map (Trip . fst) g
                                      | otherwise = Norm ("  ____" ++ (fromJust $ gid g) ++ " Flags____") : 
                                                    map (Trip . fst) g
                          in concatMap ginfo fgs
    in
        Para [short] :
        concat [ Norm "" : mode ++ [Norm "" | flags /= []] ++ flggroupinfo flags
               | (mode,args) <- info, let flags = if justlist then [] else args \\ dupes] ++
        (if null dupes then [] else Norm "":Norm "Common flags:":flggroupinfo dupes) ++
        concat [ map Norm $ "":suf | suf@(_:_) <- map modeHelpSuffix tot]


---------------------------------------------------------------------
-- PROCESS FLAGS

parseModes :: [Mode a] -> [String] -> (Either String (Bool, Mode a), [Action])
parseModes modes args
    | [mode] <- modes = (Right (False,mode), parseFlags (modeFlags mode) args)
    | [] <- poss, Just mode <- def = (Right (False,mode), parseFlags (modeFlags mode) args)
    | [mode] <- poss = (Right (True, mode), parseFlags (modeFlags mode) $ tail args)
    | otherwise = (Left err, parseFlags autoFlags args)
    where
        err = if null poss
              then "No mode given, expected one of: " ++ unwords (map modeName modes)
              else "Multiple modes given, could be any of: " ++ unwords (map modeName poss)

        def = listToMaybe $ filter modeDef modes
        poss = let f eq = [m | a <- take 1 args, m <- modes, a `eq` modeName m]
                   (exact,prefix) = (f (==), f isPrefixOf)
               in if null exact then prefix else exact


---------------------------------------------------------------------
-- APPLICATION

setField :: Data a => a -> String -> (Dynamic -> Dynamic) -> a
setField x name v = flip evalState (constrFields $ toConstr x) $ flip gmapM x $ \i -> do
    n:ns <- get
    put ns
    return $ if n == name then fromDyn (v $ toDyn i) i else i


applyActions :: Data a => [Action] -> a -> a
applyActions (Update name op:as) x | not $ "!" `isPrefixOf` name = applyActions as $ setField x name op
applyActions (_:as) x = applyActions as x
applyActions [] x = x
