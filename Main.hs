{-# LANGUAGE PatternGuards #-}

module Main where

import System.Console.CmdArgs
import qualified System.Console.CmdArgs.Test.All as Test
import System.IO
import System.Environment
import Data.List

import qualified System.Console.CmdArgs.Test.Implicit.HLint as H
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as D
import qualified System.Console.CmdArgs.Test.Implicit.Maker as M


main = do
    args <- getArgs
    case args of
        "test":_ -> Test.test
        "generate":_ -> generateManual
        x:xs | Just y <- lookup x Test.demo -> y xs
        _ -> error "CmdArgs test program, expected one of: test hlint diffy maker"


---------------------------------------------------------------------
-- GENERATE MANUAL

generateManual :: IO ()
generateManual = do
    src <- readFile "cmdargs.htm"
    () <- length src `seq` return ()
    res <- fmap unlines $ f $ lines src
    () <- length res `seq` return ()
    h <- openBinaryFile "cmdargs.htm" WriteMode
    hPutStr h res
    hClose h
    where
        f (x:xs) | "<!-- BEGIN " `isPrefixOf` x = do
            ys <- generateChunk $ init $ drop 2 $ words x
            zs <- f $ tail $ dropWhile (not . isPrefixOf "<!-- END") xs
            return $ x : ys ++ ["<!-- END -->"] ++ zs
        f [] = return []
        f (x:xs) = fmap (x:) $ f xs

generateChunk :: [String] -> IO [String]
generateChunk ["help",x] = do
    src <- readFile $ x ++ ".hs"
    let str = head [takeWhile (/= '\"') $ drop 1 $ dropWhile (/= '\"') x | x <- lines src, "main" `isPrefixOf` x]
    () <- length src `seq` return ()
    fmap lines $ case x of
        "hlint" -> cmdArgsHelp str H.modes HTML
        "diffy" -> cmdArgsHelp str D.modes HTML
        "maker" -> cmdArgsHelp str M.modes HTML

generateChunk ["code",x] = do
    src <- readFile $ x ++ ".hs"
    return $ ["<pre>"] ++ lines src ++ ["</pre>"]
