{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Test.Implicit.Util where

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe

toDemo :: (Typeable a, Show a) => Mode (CmdArgs a) -> Mode Demo
toDemo = newDemo $ \x -> cmdArgsApply x >>= print


invalid :: Data a => (() -> a) -> IO ()
invalid a = do
    res <- try $ evaluate $ length $ show $ cmdArgsMode $ a ()
    case res of
        Left (ErrorCall _) -> success
        Right _ -> failure "Expected exception" []


data Tester a = Tester
    {(===) :: [String] -> a -> IO ()
    ,fails :: [String] -> IO ()
    ,isHelp :: [String] -> [String] -> IO ()
    ,isHelpNot :: [String] -> [String] -> IO ()
    ,isVersion :: [String] -> String -> IO ()
    ,isVerbosity :: [String] -> Verbosity -> IO ()
    }

testers :: (Show a, Eq a) => String -> [Mode (CmdArgs a)] -> Tester a
testers name = foldr1 f . map (tester name)
    where
        f (Tester x1 x2 x3 x4 x5 x6) (Tester y1 y2 y3 y4 y5 y6) =
            Tester (f2 x1 y1) (f1 x2 y2) (f2 x3 y3) (f2 x4 y4) (f2 x5 y5) (f2 x6 y6)
        f1 x y a = x a >> y a
        f2 x y a b = x a b >> y a b


tester :: (Show a, Eq a) => String -> Mode (CmdArgs a) -> Tester a
tester name m = Tester (===) fails isHelp isHelpNot isVersion isVerbosity
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

        isHelpNot args want = case process m args of
            Right x | Just got <- cmdArgsHelp x, not $ match want (lines got) -> success
            _ -> failed "Failed on isHelpNot" args []

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

