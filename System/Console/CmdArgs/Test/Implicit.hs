
module System.Console.CmdArgs.Test.Implicit(test, demo) where

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Test.Util
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as Diffy
import qualified System.Console.CmdArgs.Test.Implicit.HLint as HLint
import qualified System.Console.CmdArgs.Test.Implicit.Maker as Maker

test = Diffy.test >> HLint.test >> Maker.test

demo = [newDemo f Diffy.mode, newDemo f HLint.mode, newDemo f Maker.mode]
    where f x = cmdArgsApply x >>= print
