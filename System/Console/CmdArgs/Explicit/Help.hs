
module System.Console.CmdArgs.Explicit.Help where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Text

instance Show (Mode a) where
    show = show . helpText


helpText :: Mode a -> [Text]
helpText _ = [Line "Help output"]
