{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.CmdArgs
import Control.Monad
import System.Environment
import Control.Exception
import Data.List

import qualified HLint as H
import qualified Diffy as D
import qualified Maker as M


main = do
    args <- getArgs
    case args of
        "hlint":xs -> withArgs xs H.main
        "diffy":xs -> withArgs xs D.main
        "maker":xs -> withArgs xs M.main
        "test":_ -> testHLint >> testDiffy >> testMaker >> putStrLn "Test successful"
        "generate":_ -> generateManual
        _ -> error "CmdArgs test program, expected one of: test hlint diffy maker"


test x = (map modeValue x, (===), fails)
    where
        (===) args v = do
            res <- withArgs args $ cmdArgs "" x
            when (res /= v) $
                error $ "Mismatch on flags " ++ show args

        fails args = do
            res <- try $ withArgs args $ cmdArgs "" x
            case res of
                Left (e :: SomeException) -> return ()
                Right _ -> error $ "Expected failure " ++ show args


---------------------------------------------------------------------
-- TESTS

testHLint = do
    let ([v],(===),fails) = test H.modes
    [] === v
    fails ["-ch"]
    ["--colo"] === v{H.color=True}
    ["-ct"] === v{H.color=True,H.test=True}
    ["--colour","--test"] === v{H.color=True,H.test=True}
    ["-thfoo"] === v{H.test=True,H.hint=["foo"]}
    ["-cr"] === v{H.color=True,H.report=["report.html"]}
    ["--cpp-define=val","x"] === v{H.cpp_define=["val"],H.files=["x"]}
    fails ["--cpp-define"]
    ["--cpp-define","val","x","y"] === v{H.cpp_define=["val"],H.files=["x","y"]}


testDiffy = do
    let ([create,diff],(===),fails) = test D.modes
    fails []
    ["create"] === create
    fails ["create","file1"]
    ["create","--src","x"] === create{D.src="x"}
    ["create","--src","x","--src","y"] === create{D.src="y"}
    fails ["diff","--src","x"]
    fails ["create","foo"]
    ["diff","foo1","foo2"] === diff{D.old="foo1",D.new="foo2"}
    fails ["diff","foo1"]
    fails ["diff","foo1","foo2","foo3"]


testMaker = do
    let ([build,wipe,tst],(===),fails) = test M.modes
    [] === build
    ["build","foo","--profile"] === build{M.files=["foo"],M.method=M.Profile}
    ["foo","--profile"] === build{M.files=["foo"],M.method=M.Profile}
    ["foo","--profile","--release"] === build{M.files=["foo"],M.method=M.Release}
    ["-d"] === build{M.method=M.Debug}
    ["build","-j3"] === build{M.threads=3}
    ["build","-j=3"] === build{M.threads=3}
    fails ["build","-jN"]
    -- FIXME: should fail, but -t gets intepreted as --t, which matches --threaded
    -- fails ["build","-t1"]
    ["wipe"] === wipe
    ["test"] === tst
    ["test","foo"] === tst{M.extra=["foo"]}
    ["test","foo","-baz","-j3","--what=1"] === tst{M.extra=["foo","-baz","--what=1"],M.threads=3}


---------------------------------------------------------------------
-- GENERATE MANUAL

generateManual :: IO ()
generateManual = do
    src <- readFile "cmdargs.htm"
    () <- length src `seq` return ()
    res <- fmap unlines $ f $ lines src
    () <- length res `seq` return ()
    writeFile "cmdargs.htm" res
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
