
-- | This module does command line completion
module System.Console.CmdArgs.Explicit.Complete(Complete(..), complete) where

import System.Console.CmdArgs.Explicit.Type


-- | How to complete a command line option.
data Complete
    = CompleteFile String FilePath -- ^ Complete to a prefix, and a file
    | CompleteDir String FilePath -- ^ Complete to a prefix, and a directory
    | CompleteValue String -- ^ Complete to a particular value
      deriving (Eq,Ord,Show)

-- | Given a mode, a set of arguments that have already been entered, and the argument you are currently editing,
--   return a set of the commands you could type now, in preference order.
complete :: Mode a -> [String] -> Int -> [Complete]
complete mode args i = [CompleteValue next]
    where
        (seen,nxt) = splitAt i args
        next = head $ nxt ++ [""]
