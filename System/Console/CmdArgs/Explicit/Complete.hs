
-- | This module does command line completion
module System.Console.CmdArgs.Explicit.Complete(complete) where

import System.Console.CmdArgs.Explicit.Type


-- | Given a mode, a set of arguments that have already been entered, and the argument you are currently editing,
--   return either a prefix of the flag and the filepath you are typing, or a list of commands you could type now.
complete :: Mode a -> [String] -> Int -> Either (String, FilePath) [String]
complete mode args i = Right []
