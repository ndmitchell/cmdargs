{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

import System.Console.CmdArgs

-- for the tests only
import Control.Monad
import System.Environment
import Control.Exception


data HLint = HLint
    {report :: [FilePath]
    ,hint :: [FilePath]
    ,color :: Bool
    ,ignore :: [String]
    ,show_ :: Bool
    ,test :: Bool
    ,cpp_define :: [String]
    ,cpp_include :: [String]
    ,files :: [String]
    }
    deriving (Data,Typeable,Show,Eq)

hlint = mode $ HLint
    {report = def & empty "report.html" & typFile & text "Generate a report in HTML"
    ,hint = def & typFile & text "Hint/ignore file to use"
    ,color = def & flag "c" & flag "colour" & text "Color the output (requires ANSI terminal)"
    ,ignore = def & typ "MESSAGE" & text "Ignore a particular hint"
    ,show_ = def & text "Show all ignored ideas"
    ,test = def & text "Run in test mode"
    ,cpp_define = def & typ "NAME[=VALUE]" & text "CPP #define"
    ,cpp_include = def & typDir & text "CPP include path"
    ,files = def & args & typ "FILE/DIR"
    } &
    helpSuffix ["HLint gives hints on how to improve Haskell code.",""
               ,"To check all Haskell files in 'src' and generate a report type:","  hlint src --report"]


main = print =<< cmdMode "HLint v1.6.5, (C) Neil Mitchell 2006-2009" hlint


smoke = do
    let v = modeValue hlint
    [] === v
    fails ["-ch"]
    ["--colo"] === v{color=True}
    ["-ct"] === v{color=True,test=True}
    ["-thfoo"] === v{test=True,hint=["foo"]}
    ["-cr"] === v{color=True,report=["report.html"]}
    ["--cpp-define=val","x"] === v{cpp_define=["val"],files=["x"]}
    fails ["--cpp-define"]
    ["--cpp-define","val","x","y"] === v{cpp_define=["val"],files=["x","y"]}
    putStrLn "Tests successful"
    where

(===) args v = do
    res <- withArgs args $ cmdMode "" hlint
    when (res /= v) $
        error $ "Mismatch on flags " ++ show args

fails args = do
    res <- try $ withArgs args $ cmdMode "" hlint
    case res of
        Left (e :: SomeException) -> return ()
        Right _ -> error $ "Expected failure " ++ show args
