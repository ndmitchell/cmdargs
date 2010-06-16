
module System.Console.CmdArgs.Test.All(test,demo) where

import qualified System.Console.CmdArgs.Test.Explicit as Explicit
import qualified System.Console.CmdArgs.Test.Implicit as Implicit
import qualified System.Console.CmdArgs.Test.GetOpt as GetOpt

test :: IO ()
test = do
    Explicit.test
    GetOpt.test
    putStrLn "" -- FIXME: Once implicit doesn't write to the console
    Implicit.test
    putStrLn "Test successful"

demo :: [(String, [String] -> IO ())]
demo = GetOpt.demo ++ Explicit.demo ++ Implicit.demo
