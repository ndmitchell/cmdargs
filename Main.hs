{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.CmdArgs
import Control.Monad
import System.Environment
import Control.Exception

import HLint hiding (main)
import qualified HLint


main = do
    x:xs <- getArgs
    case x of
        "hlint" -> withArgs xs HLint.main
        "test" -> testHLint


testHLint = do
    let v = modeValue hlint
    [] === v
    fails ["-ch"]
    ["--colo"] === v{color=True}
    ["-ct"] === v{color=True,test=True}
    ["--colour","--test"] === v{color=True,test=True}
    ["-thfoo"] === v{test=True,hint=["foo"]}
    ["-cr"] === v{color=True,report=["report.html"]}
    ["--cpp-define=val","x"] === v{cpp_define=["val"],files=["x"]}
    fails ["--cpp-define"]
    ["--cpp-define","val","x","y"] === v{cpp_define=["val"],files=["x","y"]}
    putStrLn "Tests successful"


(===) args v = do
    res <- withArgs args $ cmdArgs "" hlint
    when (res /= v) $
        error $ "Mismatch on flags " ++ show args

fails args = do
    res <- try $ withArgs args $ cmdArgs "" hlint
    case res of
        Left (e :: SomeException) -> return ()
        Right _ -> error $ "Expected failure " ++ show args
