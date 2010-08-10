
module System.Console.CmdArgs.Test.Implicit(test, demo) where

import System.Console.CmdArgs.Implicit
import System.Console.CmdArgs.Test.Util
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as Diffy
import qualified System.Console.CmdArgs.Test.Implicit.HLint as HLint
import qualified System.Console.CmdArgs.Test.Implicit.Maker as Maker
import qualified System.Console.CmdArgs.Test.Implicit.Tests as Tests

test = Diffy.test >> HLint.test >> Maker.test >> Tests.test1 >> Tests.test2

demo = [newDemo f Diffy.mode, newDemo f HLint.mode, newDemo f Maker.mode, newDemo f Tests.mode1, newDemo f Tests.mode2]
    where f x = cmdArgsApply x >>= print
