
module System.Console.CmdArgs.Test.All(test,demo,Demo,runDemo) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import qualified System.Console.CmdArgs.Test.Explicit as Explicit
import qualified System.Console.CmdArgs.Test.Implicit as Implicit
import qualified System.Console.CmdArgs.Test.GetOpt as GetOpt
import qualified System.Console.CmdArgs.Test.SplitJoin as SplitJoin

test :: IO ()
test = do
    Explicit.test
    GetOpt.test
    Implicit.test
    SplitJoin.test
    putStrLn "\nTest completed"

demo :: [Mode Demo]
demo = GetOpt.demo ++ Explicit.demo ++ Implicit.demo
