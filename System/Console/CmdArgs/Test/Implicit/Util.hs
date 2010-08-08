
module System.Console.CmdArgs.Test.Implicit.Util where

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import Data.Maybe

data Tester a = Tester
    {(===) :: [String] -> a -> IO ()
    ,fails :: [String] -> IO ()
    ,isHelp :: [String] -> IO ()
    ,isVersion :: [String] -> IO ()
    ,isVerbosity :: [String] -> Verbosity -> IO ()
    }


tester :: (Show a, Eq a) => String -> Mode (CmdArgs a) -> Tester a
tester name m = Tester (===) fails isHelp isVersion isVerbosity
    where
        failed msg args xs = failure msg $ ("Name","Implicit "++name):("Args",show args):xs

        (===) args v = case process m args of
            Left x -> failed "Failed when should have succeeded" args [("Error",x)]
            Right x | cmdArgsValue x /= v -> failed "Wrong parse" args [("Expected",show v),("Got",show x)]
                    | otherwise -> success

        fails args = case process m args of
            Left x -> success
            Right x -> failed "Succeeded when should have failed" args [("Result",show x)]

        isHelp args = case process m args of
            Right x | isJust $ cmdArgsHelp x -> success
            _ -> failed "Failed on isHelp" args []

        isVersion args = case process m args of
            Right x | isJust $ cmdArgsVersion x -> success
            _ -> failed "Failed on isVersion" args []

        isVerbosity args v = case process m args of
            Right x | fromMaybe Normal (cmdArgsVerbosity x) == v -> success
            _ -> failed "Failed on isVerbosity" args []
