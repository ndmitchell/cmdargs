{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Test.Implicit.Util where

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import Data.Char
import Data.List
import Data.Maybe

toDemo :: (Typeable a, Show a) => Mode (CmdArgs a) -> Mode Demo
toDemo = newDemo $ \x -> cmdArgsApply x >>= print


data Tester a = Tester
    {(===) :: [String] -> a -> IO ()
    ,fails :: [String] -> IO ()
    ,isHelp :: [String] -> [String] -> IO ()
    ,isVersion :: [String] -> String -> IO ()
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

        isHelp args want = case process m args of
            Right x | Just got <- cmdArgsHelp x, match want (lines got) -> success
            _ -> failed "Failed on isHelp" args []

        isVersion args want = case process m args of
            Right x | Just got <- cmdArgsVersion x, match [want] [got] -> success
            _ -> failed "Failed on isVersion" args []

        isVerbosity args v = case process m args of
            Right x | fromMaybe Normal (cmdArgsVerbosity x) == v -> success
            _ -> failed "Failed on isVerbosity" args []


match :: [String] -> [String] -> Bool
match want got = any f $ tails got
    where f xs = length xs >= length want && and (zipWith matchLine want xs)


matchLine :: String -> String -> Bool
matchLine (' ':' ':x) (' ':' ':y) = matchLine (dropWhile isSpace x) (dropWhile isSpace y)
matchLine (x:xs) (y:ys) | x == y = matchLine xs ys
matchLine [] [] = True
matchLine _ _ = False

