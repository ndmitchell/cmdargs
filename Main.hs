{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.CmdArgs
import Control.Monad
import System.Environment
import Control.Exception

import qualified HLint as H
import qualified DirDiff as D


main = do
    x:xs <- getArgs
    case x of
        "hlint" -> withArgs xs H.main
        "dirdiff" -> withArgs xs D.main
        "test" -> testHLint >> testDirDiff >> putStrLn "Test successful"


test x = (map modeValue x, (===), fails)
    where
        (===) args v = do
            res <- withArgs args $ cmdModes "" x
            when (res /= v) $
                error $ "Mismatch on flags " ++ show args

        fails args = do
            res <- try $ withArgs args $ cmdModes "" x
            case res of
                Left (e :: SomeException) -> return ()
                Right _ -> error $ "Expected failure " ++ show args


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


testDirDiff = do
    let ([create,diff],(===),fails) = test D.modes
    fails []

