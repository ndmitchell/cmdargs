{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, PatternGuards #-}
{-|
    Simple command line argument handling
-}

module System.Console.CmdArgs(
    Data, Typeable,
    Default(..),
    cmdArgs, cmdModes,
    HelpFormat(..), cmdArgsHelp, cmdModesHelp,
    isQuiet, isNormal, isLoud,
    modeValue,
    module System.Console.CmdArgs.UI
    ) where

import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
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

class Default a where
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

isQuiet, isNormal, isLoud :: IO Bool
isQuiet = return True
isNormal = fmap (>=1) $ readIORef verbosity
isLoud = fmap (>=2) $ readIORef verbosity


---------------------------------------------------------------------
-- MAIN DRIVERS

modeValue :: Mode a -> a
modeValue = modeVal


cmdArgs :: Data a => String -> Mode a -> IO a
cmdArgs short mode = cmdModes short [mode]


cmdModes :: Data a => String -> [Mode a] -> IO a
cmdModes short modes = do
    modes <- return $ expand modes
    (mode,args) <- parseModes modes `fmap` getArgs
    when (hasAction args "!help") $ do
        hlp <- case mode of
            Right (True,mode) -> helpInfo short modes [mode]
            _ -> helpInfo short modes modes
        let Update _ op = fromJust $ getAction args "!help"
        putStr $ showHelp hlp (fromDyn (op undefined) "")
        exitSuccess
    when (hasAction args "!version") $ do
        putStrLn short
        exitSuccess
    mode <- case mode of
        Right (_,x) -> return x
        Left x -> putStrLn x >> exitFailure
    sequence_ [putStrLn x >> exitFailure | Error x <- args]
    when (hasAction args "!verbose") $ writeIORef verbosity 2
    when (hasAction args "!quiet") $ writeIORef verbosity 0
    return $ applyActions args $ modeValue mode


---------------------------------------------------------------------
-- HELP INFORMATION

data HelpFormat = Text | HTML deriving (Eq,Ord,Show,Read,Enum,Bounded)

cmdArgsHelp :: String -> Mode a -> HelpFormat -> IO String
cmdArgsHelp short x = cmdModesHelp short [x]

cmdModesHelp :: String -> [Mode a] -> HelpFormat -> IO String
cmdModesHelp short xs format = fmap (`showHelp` (show format)) $ helpInfo short modes modes
    where modes = expand xs


helpInfo :: String -> [Mode a] -> [Mode a] -> IO [Help]
helpInfo short tot now = do
    prog <- fmap (map toLower . takeBaseName) getProgName
    prog <- return $ head $ mapMaybe modeProg tot ++ [prog]
    let info = [([Code $ unwords $ prog : [['['|def] ++ name ++ [']'|def] | length tot /= 1] ++ "[FLAG]" : args] ++
                 [Indent $ text | text /= ""]
                ,concatMap helpFlag flags)
               | Mode{modeName=name,modeFlags=flags,modeText=text,modeDef=def} <- now
               , let args = map snd $ sortBy (compare `on` fst) $ concatMap helpFlagArgs flags]
    let dupes = if length now == 1 then [] else foldr1 intersect (map snd info)
    return $
        Norm short :
        concat [ Norm "" : mode ++ [Norm "" | flags /= []] ++ map Trip flags
               | (mode,args) <- info, let flags = args \\ dupes] ++
        (if null dupes then [] else Norm "":Norm "Common flags:":map Trip dupes) ++
        concat [ Norm "" : [if "  " `isPrefixOf` s then Indent $ drop 2 s else Norm s | s <- suf]
               | suf@(_:_) <- map modeHelpSuffix tot]


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
applyActions (a:as) x = applyActions as x
applyActions [] x = x
