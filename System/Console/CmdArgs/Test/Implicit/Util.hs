{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Test.Implicit.Util(
    module System.Console.CmdArgs.Test.Implicit.Util,
    Complete(..)
    ) where

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
    ,completion :: [String] -> (Int,Int) -> [Complete] -> IO ()
    }

testers :: (Show a, Eq a) => String -> [Mode (CmdArgs a)] -> Tester a
testers name = foldr1 f . map (tester name)
    where
        f (Tester x1 x2 x3 x4 x5 x6 x7) (Tester y1 y2 y3 y4 y5 y6 y7) =
            Tester (f2 x1 y1) (f1 x2 y2) (f2 x3 y3) (f2 x4 y4) (f2 x5 y5) (f2 x6 y6) (f3 x7 y7)
        f1 x y a = x a >> y a
        f2 x y a b = x a b >> y a b
        f3 x y a b c = x a b c >> y a b c


tester :: (Show a, Eq a) => String -> Mode (CmdArgs a) -> Tester a
tester name m = Tester (===) fails isHelp isHelpNot isVersion isVerbosity completion
    where
        failed msg args xs = failure msg $ ("Name","Implicit "++name):("Args",show args):xs

        f args cont = case process m args of
            Left x -> cont $ Left x
            Right x -> cont $ Right x
{-
            o@(Right x)
                | x2 == Right x -> cont $ Right x
                | otherwise -> do
                    failed "Reform failed" args [("Reformed",show args2),("Expected",show o),("Got",show x2)]
                    error "failure!"
                    cont $ Right x
                where args2 = cmdArgsReform m x
                      x2 = process m args2
-}

        (===) args v = f args $ \x -> case x of
            Left x -> failed "Failed when should have succeeded" args [("Error",x)]
            Right x | cmdArgsValue x /= v -> failed "Wrong parse" args [("Expected",show v),("Got",show x)]
                    | otherwise -> success

        fails args = f args $ \x -> case x of
            Left x -> success
            Right x -> failed "Succeeded 52 should have failed" args [("Result",show x)]

        isHelp args want = f args $ \x -> case x of
            Right x | Just got <- cmdArgsHelp x, match want (lines got) -> success
            _ -> failed "Failed on isHelp" args [("Want",show want)]

        isHelpNot args want = f args $ \x -> case x of
            Right x | Just got <- cmdArgsHelp x, not $ match want (lines got) -> success
            _ -> failed "Failed on isHelpNot" args []

        isVersion args want = f args $ \x -> case x of
            Right x | Just got <- cmdArgsVersion x, (want ++ "\n") == got -> success
            _ -> failed "Failed on isVersion" args $
                ("Want",want) : [("Got",got) | Right x <- [x], Just got <- [cmdArgsVersion x]]

        isVerbosity args v = f args $ \x -> case x of
            Right x | fromMaybe Normal (cmdArgsVerbosity x) == v -> success
            _ -> failed "Failed on isVerbosity" args []

        completion args pos res
            | res == ans = success
            | otherwise = failed "Failed on completion" args [("Position",show pos),("Want",shw res),("Got",shw ans)]
            where ans = complete m args pos
                  shw = intercalate ", " . lines . show


match :: [String] -> [String] -> Bool
match want got = any f $ tails got
    where f xs = length xs >= length want && and (zipWith matchLine want xs)


matchLine :: String -> String -> Bool
matchLine (' ':' ':x) (' ':' ':y) = matchLine (dropWhile isSpace x) (dropWhile isSpace y)
matchLine (x:xs) (y:ys) | x == y = matchLine xs ys
matchLine [] [] = True
matchLine _ _ = False

