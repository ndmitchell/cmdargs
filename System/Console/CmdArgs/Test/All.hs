
module System.Console.CmdArgs.Test.All(test,demo) where

import qualified System.Console.CmdArgs.Test.Explicit as Explicit
import qualified System.Console.CmdArgs.Test.Implicit as Implicit
import qualified System.Console.CmdArgs.Test.GetOpt as GetOpt

test :: IO ()
test = do
    Explicit.test
    GetOpt.test
    Implicit.test
    putStrLn "\nTest completed"

demo :: [(String, [String] -> IO ())]
demo = GetOpt.demo ++ Explicit.demo ++ Implicit.demo
