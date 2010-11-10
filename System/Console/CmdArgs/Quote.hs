
-- Suggested by reddit user eegreg
--
-- This feature will not be ready until GHC 7, which includes declaration-level
-- quasi-quotes.

module System.Console.CmdArgs.Quote(defaultRecord) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote


defaultRecord :: QuasiQuoter
defaultRecord = QuasiQuoter{quoteDec = quote}


quote :: String -> Q [Dec]
quote x = error $ "To quote: " ++ show x

{-
This produces the standard record, ignoring everything before the = sign. It also produces
a Default instance which returns the record.
-}
