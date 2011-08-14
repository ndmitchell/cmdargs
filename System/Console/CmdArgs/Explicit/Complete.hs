
-- Useful references:
-- http://www.debian-administration.org/article/317/An_introduction_to_bash_completion_part_2

module System.Console.CmdArgs.Explicit.Complete(completeBash) where

import System.Console.CmdArgs.Explicit.Type


-- | Generate bash completion information. To run, do:
--   @source `program --help=bash`@
completeBash :: Mode a -> String
completeBash m = unlines
    ["_" ++ prog ++ "()"
    ,"{"
    ,"   COMPREPLY=(todo)"
    ,"}"
    ,"complete -F _" ++ prog ++ " " ++ prog
    ]
    where prog = head $ modeNames m ++ ["unknown"]
