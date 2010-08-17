{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module System.Console.CmdArgs.Test.Implicit.Diffy where
import System.Console.CmdArgs
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

test = do
    let Tester{..} = tester "Diffy" mode
    fails []
    isHelp ["--help"] []
    isHelp ["create","--help"] []
    isHelp ["diff","--help"] []
    isVersion ["--version"] "Diffy v1.0"
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

