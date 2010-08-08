{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
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

mode = cmdArgsMode $ modes [build,wipe,test_] &= program "maker" &= summary "Maker v1.0"

test = do
    let Tester{(===),fails} = tester "Maker" mode
    [] === build
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

