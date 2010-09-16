{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse -fno-warn-unused-binds #-}
module System.Console.CmdArgs.Test.Implicit.Maker where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util

data Method = Debug | Release | Profile
              deriving (Data,Typeable,Show,Eq)

data Maker
    = Wipe
    | Test {threads :: Int, extra :: [String]}
    | Build {threads :: Int, method :: Method, files :: [FilePath]}
      deriving (Data,Typeable,Show,Eq)

threadsMsg x = x &= help "Number of threads to use" &= name "j" &= typ "NUM"

wipe = Wipe &= help "Clean all build objects"

test_ = Test
    {threads = threadsMsg def
    ,extra = def &= typ "ANY" &= args
    } &= help "Run the test suite"

build = Build
    {threads = threadsMsg def
    ,method = enum
        [Release &= help "Release build"
        ,Debug &= help "Debug build"
        ,Profile &= help "Profile build"]
    ,files = def &= args
    } &= help "Build the project" &= auto

mode = cmdArgsMode $ modes [build,wipe,test_] &= help "Build helper program" &= program "maker" &= summary "Maker v1.0"

mode_ = cmdArgsMode_ $ modes_ [build,wipe,test_] += help "Build helper program" += program "maker" += summary "Maker v1.0"
  where
    threads_ = threads := def += help "Number of threads to use" += name "j" += typ "NUM"

    wipe = record Wipe{} [] += help "Clean all build objects"

    test_ = record Test{}
        [threads_
        ,extra := def += typ "ANY" += args
        ] += help "Run the test suite"

    build = record Build{}
        [threads_ 
        ,enum_ method
            [atom Release += help "Release build"
            ,atom Debug += help "Debug build"
            ,atom Profile += help "Profile build"]
        ,files := def += args
        ] += help "Build the project" += auto


test = do
    let Tester{..} = testers "Maker" [mode,mode_]
    [] === build
    isHelp ["-?=one"] ["Common flags:"]
    isHelpNot ["-?=one"] ["  -d --debug  Debug build"]
    isHelp ["-?=all"] ["maker [build] [OPTIONS] [ITEM]"]
    isHelp ["build","-?=one"] ["maker [build] [OPTIONS] [ITEM]"]
    isHelp ["-?=one"] ["  Build helper program"]
    ["build","foo","--profile"] === build{files=["foo"],method=Profile}
    ["foo","--profile"] === build{files=["foo"],method=Profile}
    ["foo","--profile","--release"] === build{files=["foo"],method=Release}
    ["-d"] === build{method=Debug}
    ["build","-j3"] === build{threads=3}
    ["build","-j=3"] === build{threads=3}
    fails ["build","-jN"]
    fails ["build","-t1"]
    ["wipe"] === wipe
    ["test"] === test_
    ["test","foo"] === test_{extra=["foo"]}
    ["test","foo","-j3"] === test_{extra=["foo"],threads=3}
    fails ["test","foo","-baz","-j3","--what=1"]
    ["test","foo","--","-baz","-j3","--what=1"] === test_{extra=["foo","-baz","-j3","--what=1"]}
    ["test","--","foo","-baz","-j3","--what=1"] === test_{extra=["foo","-baz","-j3","--what=1"]}
    ["--"] === build
    ["test","--"] === test_
    ["test","-j3","--","foo","-baz","-j3","--what=1"] === test_{extra=["foo","-baz","-j3","--what=1"],threads=3}
    ["test","-"] === test_{extra=["-"]}
    ["build","-"] === build{files=["-"]}
