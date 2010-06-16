
module System.Console.CmdArgs.Test.Implicit(test, demo) where

import qualified System.Console.CmdArgs.Test.Implicit.Diffy as Diffy
import qualified System.Console.CmdArgs.Test.Implicit.HLint as HLint
import qualified System.Console.CmdArgs.Test.Implicit.Maker as Maker
import System.Environment


test = Diffy.test >> HLint.test >> Maker.test

demo = [f "diffy" Diffy.demo, f "hlint" HLint.demo, f "maker" Maker.demo]
    where f s x = (s, \a -> withArgs a $ x >>= print)
