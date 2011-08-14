
-- | Module for implementing CmdArgs helpers, sending messages for remote argument entry.
--   Most users should not need to use this module.
module System.Console.CmdArgs.Helper(Check, execute, receive, reply, comment) where

import System.Console.CmdArgs.Explicit
import System.Process
import Data.Char
import Data.List
import System.Exit
import System.IO


-- | Run a remote command line entry
execute
    :: String -- ^ Name of the command to run, e.g. @echo argument@, @cmdargs-browser@
    -> Mode a -- ^ Mode to run remotely
    -> [String] -- ^ Initial set of command line flags (not supported by everything)
    -> IO (Either String [String]) -- return either an error, or a list of flags to use
execute cmd mode args
    | "echo" == takeWhile (not . isSpace) cmd = return $ Right $ splitArgs $ drop 4 cmd
    | otherwise = do
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
                hPutStrLn hout $ show $ uncurry (check mode) $ read $ drop 6 x
                loop hin hout
             else if "#" `isPrefixOf` x then do
                putStrLn x
                loop hin hout
             else
                return $ Left $ "Unexpected message from program: " ++ show x


-- | Type of function to check arguments, given a list of arguments, a number a positional arguments to skip,
--   returns either Nothing (success) or Just an error message (failure).
type Check = [String] -> Int -> IO (Maybe String)


check :: Mode a -> [String] -> Int -> Maybe String
check mode args skip = Nothing


-- | Receive information, include a mode (where all functions are undefined),
--   along with a way to check command line arguments.
receive :: IO (Mode (), Check)
receive = do
    mode <- fmap readMode getLine
    return (mode, check)
    where
        check a b = do
            putStrLn $ "Check " ++ show (a,b)
            fmap read getLine


-- | Send a reply with either an error, or which messages to use. This function exits the program.
reply :: Either String [String] -> IO ()
reply x = do
    putStrLn $ "Result " ++ show x
    exitWith ExitSuccess


-- | Send a comment to the person who invoked you, useful for debugging.
comment :: String -> IO ()
comment x = putStrLn $ "# " ++ x


---------------------------------------------------------------------
-- SERIALISE A MODE

writeMode :: Mode a -> String
writeMode _ = "Mode"


readMode :: String -> Mode ()
readMode x = modeEmpty ()
