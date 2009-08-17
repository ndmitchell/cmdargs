{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

data DirDiff = Create {src :: FilePath, out :: FilePath}
             | Diff {old :: FilePath, new :: FilePath, out :: FilePath}
               deriving (Data,Typeable,Show)


outFlags x = x & text "Output file" & typFile

create = mode $ Create
    {src = "." & text "Source directory" & typDir
    ,out = outFlags "ls.txt"
    } & text "Create a fingerprint"

diff = mode $ Diff
    {old = def & typ "OLDFILE" & argPos 0
    ,new = def & typ "NEWFILE" & argPos 1
    ,out = outFlags "diff.txt"
    } & text "Perform a diff"


main = print =<< cmdModes "DirDiff v1.0" [create,diff]
