{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.CmdArgs.Implicit.Ann where

import Data.Data

-- | The general type of annotations that can be associated with a value.
data Ann
    = Help String
    | Name String
    | Explicit
    | Ignore
    | GroupName String

    | FlagOptional String
    | FlagArgs
    | FlagArgPos Int
    | FlagType String

    | ModeDefault
    | ModeHelpSuffix [String]

    | ProgSummary String
    | ProgProgram String
    | ProgVerbosity
      deriving (Eq,Ord,Show,Data,Typeable)
