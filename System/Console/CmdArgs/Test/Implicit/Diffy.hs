{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module System.Console.CmdArgs.Test.Implicit.Diffy where
import System.Console.CmdArgs
import System.Console.CmdArgs.Quote
import System.Console.CmdArgs.Test.Implicit.Util

data Diffy = Create {src :: Maybe FilePath, out :: FilePath}
           | Diff {old :: FilePath, new :: FilePath, out :: FilePath}
             deriving (Data,Typeable,Show,Eq)

outFlags x = x &= help "Output file" &= typFile

create = Create
    {src = def &= help "Source directory" &= typDir
    ,out = outFlags "ls.txt"
    } &= help "Create a fingerprint"

diff = Diff
    {old = def &= typ "OLDFILE" &= argPos 0
    ,new = def &= typ "NEWFILE" &= argPos 1
    ,out = outFlags "diff.txt"
    } &= help "Perform a diff"

mode = cmdArgsMode $ modes [create,diff] &= help "Create and compare differences" &= program "diffy" &= summary "Diffy v1.0"


$(cmdArgsQuote
    [d|
        outFlags_ x = x &=# help "Output file" &=# typFile

        create_ = Create
            {src = Nothing &=# help "Source directory" &=# typDir
            ,out = outFlags_ "ls.txt"
            } &=# help "Create a fingerprint"

        diff_ = Diff
            {old = "" &=# typ "OLDFILE" &=# argPos 0
            ,new = "" &=# typ "NEWFILE" &=# argPos 1
            ,out = outFlags_ "diff.txt"
            } &=# help "Perform a diff"

        mode_ = cmdArgsMode# $ modes# [create_,diff_] &=# help "Create and compare differences" &=# program "diffy" &=# summary "Diffy v1.0"
    |])


-- STOP MANUAL

test = do
    let Tester{..} = testers "Diffy" [mode,mode_]
    fails []
    isHelp ["--help"] ["diffy [COMMAND] ... [OPTIONS]"] -- FIXME: Should know that root is not valid, thus no brackets on [COMMAND]
    isHelp ["create","--help"] []
    isHelp ["diff","--help"] []
    isHelpNot ["--help"] ["diffy"]
    isVersion ["--version"] "Diffy v1.0"
    isVersion ["--numeric-version"] "1.0"
    ["create"] === create
    fails ["create","file1"]
    fails ["create","--quiet"]
    fails ["create","--verbose"]
    isVerbosity ["create"] Normal
    ["create","--src","x"] === create{src=Just "x"}
    ["create","--src","x","--src","y"] === create{src=Just "y"}
    fails ["diff","--src","x"]
    fails ["create","foo"]
    ["diff","foo1","foo2"] === diff{old="foo1",new="foo2"}
    fails ["diff","foo1"]
    fails ["diff","foo1","foo2","foo3"]
    completion [] (0,0) [CompleteValue "create",CompleteValue "diff",CompleteValue "--out",CompleteValue "--help",CompleteValue "--version",CompleteValue "--numeric-version"]
    completion ["d"] (0,1) [CompleteValue "diff"]
    completion ["dd"] (0,2) []
