{-# LANGUAGE DeriveDataTypeable #-}

module HLint where

import System.Console.CmdArgs


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
    {report = def &= empty "report.html" & typFile & text "Generate a report in HTML"
    ,hint = def &= typFile & text "Hint/ignore file to use"
    ,color = def &= flag "c" & flag "colour" & text "Color the output (requires ANSI terminal)"
    ,ignore = def &= typ "MESSAGE" & text "Ignore a particular hint"
    ,show_ = def &= text "Show all ignored ideas"
    ,test = def &= text "Run in test mode"
    ,cpp_define = def &= typ "NAME[=VALUE]" & text "CPP #define"
    ,cpp_include = def &= typDir & text "CPP include path"
    ,files = def &= args & typ "FILE/DIR"
    } &=
    prog "hlint" &
    text "Suggest improvements to Haskell source code" &
    helpSuffix ["HLint gives hints on how to improve Haskell code.",""
               ,"To check all Haskell files in 'src' and generate a report type:","  hlint src --report"]

modes = [hlint]

main = print =<< cmdArgs "HLint v1.6.5, (C) Neil Mitchell 2006-2009" hlint
