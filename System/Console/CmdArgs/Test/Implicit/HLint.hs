{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module System.Console.CmdArgs.Test.Implicit.HLint where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util


data HLint = HLint
    {report :: [FilePath]
    ,hint :: [FilePath]
    ,color :: Bool
    ,ignore :: [String]
    ,show_ :: Bool
    ,extension :: [String]
    ,language :: [String]
    ,utf8 :: Bool
    ,encoding :: String
    ,find :: [FilePath]
    ,test_ :: Bool
    ,datadir :: [FilePath]
    ,cpp_define :: [String]
    ,cpp_include :: [FilePath]
    ,files :: [FilePath]
    }
    deriving (Data,Typeable,Show,Eq)

hlint = HLint
    {report = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
    ,hint = def &= typFile &= help "Hint/ignore file to use"
    ,color = def &= name "c" &= name "colour" &= help "Color the output (requires ANSI terminal)"
    ,ignore = def &= typ "MESSAGE" &= help "Ignore a particular hint"
    ,show_ = def &= help "Show all ignored ideas"
    ,extension = def &= typ "EXT" &= help "File extensions to search (defaults to hs and lhs)"
    ,language = def &= name "X" &= typ "LANG" &= help "Language extension (Arrows, NoCPP)"
    ,utf8 = def &= help "Use UTF-8 text encoding"
    ,encoding = def &= typ "ENC" &= help "Choose the text encoding"
    ,find = def &= typFile &= help "Find hints in a Haskell file"
    ,test_ = def &= help "Run in test mode"
    ,datadir = def &= typDir &= help "Override the data directory"
    ,cpp_define = def &= typ "NAME[=VALUE]" &= help "CPP #define"
    ,cpp_include = def &= typDir &= help "CPP include path"
    ,files = def &= args &= typ "FILES/DIRS"
    } &=
    verbosity &=
    help "Suggest improvements to Haskell source code" &=
    summary "HLint v0.0.0, (C) Neil Mitchell" &=
    details ["Hlint gives hints on how to improve Haskell code",""
            ,"To check all Haskell files in 'src' and generate a report type:","  hlint src --report"]

mode = cmdArgsMode hlint

test = do
    let Tester{..} = tester "HLint" mode
    [] === hlint
    fails ["-ch"]
    isVerbosity ["--color","--quiet"] Quiet
    isVerbosity ["--color","--verbose"] Loud
    isVerbosity ["--color","--quiet","--verbose"] Loud
    isVerbosity [] Normal
    isHelp ["-?"] ["HLint v0.0.0, (C) Neil Mitchell"]
    isHelp ["--help"] ["  hlint src --report"]
    ["--colo"] === hlint{color=True}
    ["--colour","--colour=false"] === hlint
    ["--colour=true"] === hlint{color=True}
    ["-c=off"] === hlint
    ["-ct"] === hlint{color=True,test_=True}
    ["--colour","--test"] === hlint{color=True,test_=True}
    ["-thfoo"] === hlint{test_=True,hint=["foo"]}
    ["-cr"] === hlint{color=True,report=["report.html"]}
    ["--cpp-define=val","x"] === hlint{cpp_define=["val"],files=["x"]}
    fails ["--cpp-define"]
    ["--cpp-define","val","x","y"] === hlint{cpp_define=["val"],files=["x","y"]}


