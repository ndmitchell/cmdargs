{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.CmdArgs.Implicit.Ann where

import Data.Data

-- | The general type of annotations that can be associated with a value.
data Ann
    = Help String
    | Name String
    | Explicit
    | GroupName String

    | FlagOptional String
    | FlagArgs
    | FlagArgPos Int
    | FlagType String
    | FlagEnum -- private, specified by 'one'
    | FlagInherit -- private, specified by omitting it (which throws RecConError)

    | ModeDefault
    | ModeHelpSuffix [String]

    | ProgSummary String
    | ProgProgram String
    | ProgVerbosity
      deriving (Eq,Ord,Show,Data,Typeable)
