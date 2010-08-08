
module System.Console.CmdArgs.Test.Util where


failure :: String -> [(String,String)] -> IO ()
failure x ys = putStr $ unlines $ "" : "" : "FAILURE" : x : [a ++ ": " ++ b | (a,b) <- ys]


success :: IO ()
success = putChar '.'


-- seq used to obtain better program coverage
hpc = seq
