{-# LANGUAGE DeriveDataTypeable #-}
module System.Console.CmdArgs.Test.Implicit.Diffy where
import System.Console.CmdArgs

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

main = print =<< cmdArgs "Diffy v1.0" modes
