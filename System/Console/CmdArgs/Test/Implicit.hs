
module System.Console.CmdArgs.Test.Implicit(test, demo) where

import System.Console.CmdArgs.Implicit
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as Diffy
import qualified System.Console.CmdArgs.Test.Implicit.HLint as HLint
import qualified System.Console.CmdArgs.Test.Implicit.Maker as Maker
import System.Environment

test = Diffy.test >> HLint.test >> Maker.test

demo = [f "diffy" Diffy.mode, f "hlint" HLint.mode, f "maker" Maker.mode]
    where f s x = (s, \a -> withArgs a $ cmdArgsRun x >>= print)
