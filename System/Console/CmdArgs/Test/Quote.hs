{-# LANGUAGE QuasiQuotes #-}

module System.Console.CmdArgs.Test.Quote(HLint(..)) where

import System.Console.CmdArgs
import System.Console.CmdArgs.Quote

[defaultRecord|
    data HLint = HLint
        {report :: [FilePath] = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
        ,hint :: [FilePath] = def &= typFile &= help "Hint/ignore file to use"
        ,color :: Bool = def &= name "c" &= name "colour" &= help "Color the output (requires ANSI terminal)"
        }
|]
