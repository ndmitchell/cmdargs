
module System.Console.CmdArgs.Test.All(test) where

import qualified System.Console.CmdArgs.Test.Explicit as Explicit

test :: IO ()
test = do
    Explicit.test
    putStrLn ""
