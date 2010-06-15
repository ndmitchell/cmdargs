
module System.Console.CmdArgs.Test.Util where

failure x ys = error $ unlines $ "" : x : [a ++ ": " ++ b | (a,b) <- ys]

success = putChar '.'

-- seq used to obtain better program coverage
hpc = seq
