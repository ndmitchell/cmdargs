
module System.Console.CmdArgs.Test.Implicit(test, demo) where

import System.Console.CmdArgs.Test.Implicit.Util
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as Diffy
import qualified System.Console.CmdArgs.Test.Implicit.HLint as HLint
import qualified System.Console.CmdArgs.Test.Implicit.Maker as Maker
import qualified System.Console.CmdArgs.Test.Implicit.Tests as Tests

test = Diffy.test >> HLint.test >> Maker.test >> Tests.test

demo = toDemo Diffy.mode : toDemo HLint.mode : toDemo Maker.mode : Tests.demos
