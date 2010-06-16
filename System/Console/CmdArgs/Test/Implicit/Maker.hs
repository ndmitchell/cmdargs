{-# LANGUAGE DeriveDataTypeable #-}
module System.Console.CmdArgs.Test.Implicit.Maker where
import System.Console.CmdArgs

data Method = Debug | Release | Profile
              deriving (Data,Typeable,Show,Eq)

data Maker
    = Wipe
    | Test {threads :: Int, extra :: [String]}
    | Build {threads :: Int, method :: Method, files :: [FilePath]}
      deriving (Data,Typeable,Show,Eq)

threadsMsg = text "Number of threads to use" & flag "j" & typ "NUM"

wipe = mode $ Wipe &= prog "maker" & text "Clean all build objects"

test = mode $ Test
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

modes = [build,wipe,test]

main = print =<< cmdArgs "Maker v1.0" modes
