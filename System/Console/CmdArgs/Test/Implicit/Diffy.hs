{-# LANGUAGE DeriveDataTypeable #-}
module System.Console.CmdArgs.Test.Implicit.Diffy where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util

data Diffy = Create {src :: FilePath, out :: FilePath}
           | Diff {old :: FilePath, new :: FilePath, out :: FilePath}
             deriving (Data,Typeable,Show,Eq)

outFlags = text "Output file" & typFile

create = mode $ Create
    {src = "." &= text "Source directory" & typDir
    ,out = "ls.txt" &= outFlags
    } &= prog "diffy" & text "Create a fingerprint"

diff = mode $ Diff
    {old = def &= typ "OLDFILE" & argPos 0
    ,new = def &= typ "NEWFILE" & argPos 1
    ,out = "diff.txt" &= outFlags
    } &= text "Perform a diff"

modes = [create,diff]

demo = cmdArgs "Diffy v1.0" modes


test = do
    let ([create,diff],(===),fails) = tester modes
    fails []
    ["create"] === create
    fails ["create","file1"]
    ["create","--src","x"] === create{src="x"}
    ["create","--src","x","--src","y"] === create{src="y"}
    fails ["diff","--src","x"]
    fails ["create","foo"]
    ["diff","foo1","foo2"] === diff{old="foo1",new="foo2"}
    fails ["diff","foo1"]
    fails ["diff","foo1","foo2","foo3"]

