{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

-- | Module for implementing CmdArgs helpers. A CmdArgs helper is an external program,
--   that helps a user construct the command line arguments. To use a helper set the
--   environment variable @$CMDARGS_HELPER@ (or @$CMDARGS_HELPER_/YOURPROGRAM/@) to
--   one of:
--
-- * @echo /foo/@ will cause @/foo/@ to be used as the command arguments.
--
-- * @cmdargs-browser@ will cause a web browser to appear to help entering the arguments.
--   For this command to work, you will need to install the @cmdargs-browser@ package:
--   <http://hackage.haskell.org/package/cmdargs-browser>
module System.Console.CmdArgs.Helper(Check, execute, receive, reply, comment) where
-- Should really be under Explicit, but want to export it top-level as Helper

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Explicit.Process
import System.Console.CmdArgs.Explicit.SplitJoin
import System.Process
import Control.Arrow
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Unsafe


-- | Run a remote command line entry
execute
    :: String -- ^ Name of the command to run, e.g. @echo argument@, @cmdargs-browser@
    -> Mode a -- ^ Mode to run remotely
    -> [String] -- ^ Initial set of command line flags (not supported by everything)
    -> IO (Either String [String]) -- return either an error, or a list of flags to use
execute cmd mode args
    | "echo" == takeWhile (not . isSpace) cmd = return $ Right $ splitArgs $ drop 4 cmd
    | otherwise = withBuffering stdout NoBuffering $ do
        (Just hin, Just hout, _, _) <- createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe}
        hSetBuffering hin LineBuffering
        hSetBuffering hout LineBuffering
        hPutStrLn hin $ writeMode mode
        loop hin hout
    where
        loop hin hout = do
            x <- hGetLine hout
            if "Result " `isPrefixOf` x then
                return $ read $ drop 7 x
             else if "Check " `isPrefixOf` x then do
                hPutStrLn hin $ show $ uncurry (check mode) $ read $ drop 6 x
                loop hin hout
             else if "#" `isPrefixOf` x then do
                putStrLn x
                loop hin hout
             else
                return $ Left $ "Unexpected message from program: " ++ show x

withBuffering hndl mode act = bracket
    (do old <- hGetBuffering hndl; hSetBuffering hndl mode; return old)
    (hSetBuffering hndl)
    (const act)


-- | Type of function to check arguments, given a list of arguments, a number a positional arguments to skip,
--   returns either Nothing (success) or Just an error message (failure).
type Check = Int -> [String] -> Maybe String


check :: Mode a -> Check
check mode skip args = either Just (const Nothing) $ process (dropArgs skip mode) args
    where
        dropArgs i m = m{modeGroupModes = fmap (dropArgs i) $ modeGroupModes m
                        ,modeArgs = (drop i *** id) $ modeArgs m}


-- | Receive information, include a mode (where all functions are undefined),
--   along with a way to check command line arguments.
receive :: IO (Mode (), Check)
receive = do
    mode <- fmap readMode getLine
    return (mode, check)
    where
        check a b = unsafePerformIO $ do
            putStrLn $ "Check " ++ show (a,b)
            hFlush stdout
            fmap read getLine


-- | Send a reply with either an error, or which messages to use. This function exits the program.
reply :: Either String [String] -> IO ()
reply x = do
    putStrLn $ "Result " ++ show x
    hFlush stdout
    exitWith ExitSuccess


-- | Send a comment to the person who invoked you, useful for debugging.
comment :: String -> IO ()
comment x = putStrLn $ "# " ++ x


---------------------------------------------------------------------
-- SERIALISE A MODE

writeMode :: Mode a -> String
writeMode = show . pack


readMode :: String -> Mode ()
readMode = unpack . read


data Pack = Ctor String [(String, Pack)]
          | List [Pack]
          | Char Char
          | String String
          | None -- ^ Never generated, only used for reading in bad cases
            deriving (Show,Read)

class Packer a where
    pack :: a -> Pack
    unpack :: Pack -> a

a *= b = (a, pack b)
ctor x (Ctor y xs) | x == y = xs
ctor _ _ = []
a =* b = unpack $ fromMaybe None $ lookup a b
none x = error $ "CmdArgs.Helper, field not populated: " ++ x

instance Packer a => Packer [a] where
    pack xs = if length ys == length zs && not (null ys) then String zs else List ys
        where ys = map pack xs
              zs = [x | Char x <- ys]

    unpack (String xs) = unpack $ List $ map Char xs
    unpack (List xs) = map unpack xs
    unpack _ = []

instance Packer Char where
    pack = Char
    unpack (Char x) = x
    unpack _ = ' '

instance (Packer a, Packer b) => Packer (a,b) where
    pack (a,b) = Ctor "(,)" ["fst" *= a, "snd" *= b]
    unpack x = ("fst" =* y, "snd" =* y)
        where y = ctor "(,)" x

instance Packer a => Packer (Maybe a) where
    pack Nothing = Ctor "Nothing" []
    pack (Just x) = Ctor "Just" ["fromJust" *= x]
    unpack x@(Ctor "Just" _) = Just $ "fromJust" =* ctor "Just" x
    unpack _ = Nothing

instance Packer Bool where
    pack False = Ctor "False" []
    pack True = Ctor "True" []
    unpack (Ctor "True" _) = True
    unpack _ = False

instance Packer a => Packer (Group a) where
    pack Group{..} = Ctor "Group"
        ["groupUnnamed" *= groupUnnamed
        ,"groupHidden" *= groupHidden
        ,"groupNamed" *= groupNamed]
    unpack x = let y = ctor "Group" x in Group
        {groupUnnamed = "groupUnnamed" =* y
        ,groupHidden = "groupHidden" =* y
        ,groupNamed = "groupNamed" =* y}       

instance Packer (Mode a) where
    pack Mode{..} = Ctor "Mode"
        ["modeGroupModes" *= modeGroupModes
        ,"modeNames" *= modeNames
        ,"modeHelp" *= modeHelp
        ,"modeHelpSuffix" *= modeHelpSuffix
        ,"modeArgs" *= modeArgs
        ,"modeGroupFlags" *= modeGroupFlags]
    unpack x = let y = ctor "Mode" x in Mode
        {modeGroupModes = "modeGroupModes" =* y
        ,modeNames = "modeNames" =* y
        ,modeHelp = "modeHelp" =* y
        ,modeHelpSuffix = "modeHelpSuffix" =* y
        ,modeArgs = "modeArgs" =* y
        ,modeGroupFlags = "modeGroupFlags" =* y
        ,modeValue = none "modeValue"
        ,modeCheck = none "modeCheck"
        ,modeReform = none "modeReform"
        }

instance Packer (Flag a) where
    pack Flag{..} = Ctor "Flag"
        ["flagNames" *= flagNames
        ,"flagInfo" *= flagInfo
        ,"flagType" *= flagType
        ,"flagHelp" *= flagHelp]
    unpack x = let y = ctor "Flag" x in Flag
        {flagNames = "flagNames" =* y
        ,flagInfo = "flagInfo" =* y
        ,flagType = "flagType" =* y
        ,flagHelp = "flagHelp" =* y
        ,flagValue = none "flagValue"}

instance Packer (Arg a) where
    pack Arg{..} = Ctor "Arg"
        ["argType" *= argType
        ,"argRequire" *= argRequire]
    unpack x = let y = ctor "Arg" x in Arg
        {argType = "argType" =* y
        ,argRequire = "argRequire" =* y
        ,argValue = none "argValue"}

instance Packer FlagInfo where
    pack FlagReq = Ctor "FlagReq" []
    pack (FlagOpt x) = Ctor "FlagOpt" ["fromFlagOpt" *= x]
    pack (FlagOptRare x) = Ctor "FlagOptRare" ["fromFlagOpt" *= x]
    pack FlagNone = Ctor "FlagNone" []
    unpack x@(Ctor name _) = case name of
        "FlagReq" -> FlagReq
        "FlagOpt" -> FlagOpt $ "fromFlagOpt" =* ctor name x
        "FlagOptRare" -> FlagOpt $ "fromFlagOpt" =* ctor name x
    unpack _ = FlagNone
