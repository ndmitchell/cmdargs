
module System.Console.CmdArgs.Test.All(test) where

import qualified System.Console.CmdArgs.Test.Explicit as Explicit
import qualified System.Console.CmdArgs.Test.GetOpt as GetOpt

test :: IO ()
test = do
    Explicit.test
    GetOpt.test
    putStrLn ""
