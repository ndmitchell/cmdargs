
module System.Console.CmdArgs.Explicit.Help where

import System.Console.CmdArgs.Explicit.Type

instance Show (Mode a) where
    show a = "Mode"

instance Show (Flag a) where
    show a = "Flag"
