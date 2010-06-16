{-# LANGUAGE DeriveDataTypeable #-}
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

threadsMsg = text "Number of threads to use" & flag "j" & typ "NUM"

wipe = mode $ Wipe &= prog "maker" & text "Clean all build objects"

test_ = mode $ Test
    {threads = def &= threadsMsg
    ,extra = def &= typ "ANY" & args & unknownFlags
    } &= text "Run the test suite"

build = mode $ Build
    {threads = def &= threadsMsg
    ,method = enum Release
        [Debug &= text "Debug build"
        ,Release &= text "Release build"
        ,Profile &= text "Profile build"]
    ,files = def &= args
    } &= text "Build the project" & defMode

modes = [build,wipe,test_]

demo = cmdArgs "Maker v1.0" modes


test = do
    let ([build,wipe,tst],(===),fails) = tester modes
    [] === build
    ["build","foo","--profile"] === build{files=["foo"],method=Profile}
    ["foo","--profile"] === build{files=["foo"],method=Profile}
    ["foo","--profile","--release"] === build{files=["foo"],method=Release}
    ["-d"] === build{method=Debug}
    ["build","-j3"] === build{threads=3}
    ["build","-j=3"] === build{threads=3}
    fails ["build","-jN"]
    -- FIXME: should fail, but -t gets intepreted as --t, which matches --threaded
    -- fails ["build","-t1"]
    ["wipe"] === wipe
    ["test"] === tst
    ["test","foo"] === tst{extra=["foo"]}
    ["test","foo","-baz","-j3","--what=1"] === tst{extra=["foo","-baz","--what=1"],threads=3}

