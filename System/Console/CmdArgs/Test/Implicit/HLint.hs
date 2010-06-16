{-# LANGUAGE DeriveDataTypeable #-}
module System.Console.CmdArgs.Test.Implicit.HLint where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util

data HLint = HLint
    {report :: [FilePath]
    ,hint :: [FilePath]
    ,color :: Bool
    ,ignore :: [String]
    ,show_ :: Bool
    ,test_ :: Bool
    ,cpp_define :: [String]
    ,cpp_include :: [String]
    ,files :: [String]
    }
    deriving (Data,Typeable,Show,Eq)

hlint = mode $ HLint
    {report = def &= empty "report.html" & typFile & text "Generate a report in HTML"
    ,hint = def &= typFile & text "Hint/ignore file to use"
    ,color = def &= flag "c" & flag "colour" & text "Color the output (requires ANSI terminal)"
    ,ignore = def &= typ "MESSAGE" & text "Ignore a particular hint"
    ,show_ = def &= text "Show all ignored ideas"
    ,test_ = def &= text "Run in test mode"
    ,cpp_define = def &= typ "NAME[=VALUE]" & text "CPP #define"
    ,cpp_include = def &= typDir & text "CPP include path"
    ,files = def &= args & typ "FILE/DIR"
    } &=
    prog "hlint" &
    text "Suggest improvements to Haskell source code" &
    helpSuffix ["To check all Haskell files in 'src' and generate a report type:","  hlint src --report"]

modes = [hlint]

demo = cmdArgs "HLint v1.6.5, (C) Neil Mitchell 2006-2009" modes


test = do
    let ([v],(===),fails) = tester modes
    [] === v
    fails ["-ch"]
    ["--colo"] === v{color=True}
    ["-ct"] === v{color=True,test_=True}
    ["--colour","--test"] === v{color=True,test_=True}
    ["-thfoo"] === v{test_=True,hint=["foo"]}
    ["-cr"] === v{color=True,report=["report.html"]}
    ["--cpp-define=val","x"] === v{cpp_define=["val"],files=["x"]}
    fails ["--cpp-define"]
    ["--cpp-define","val","x","y"] === v{cpp_define=["val"],files=["x","y"]}


