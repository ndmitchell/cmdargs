
-- Suggested by reddit user eegreg
--
-- This feature will not be ready until GHC 6.14, which includes declaration-level
-- quasi-quotes.

module System.Console.CmdArgs.Quote where

{-
Implement a quasi-quoter for default records:

[defaultRecord|
data HLint = HLint
    {report :: [FilePath] = def &= opt "report.html" &= typFile &= help "Generate a report in HTML"
    ,hint :: [FilePath] = def &= typFile &= help "Hint/ignore file to use"
    ,color :: Bool = def &= name "c" &= name "colour" &= help "Color the output (requires ANSI terminal)"
    }
]

This produces the standard record, ignoring everything before the = sign. It also produces
a Default instance which returns the record.
-}
