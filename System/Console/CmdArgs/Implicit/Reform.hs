{-# LANGUAGE PatternGuards, RecordWildCards #-}

module System.Console.CmdArgs.Implicit.Reform(reform) where

import System.Console.CmdArgs.Implicit.Local
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Verbosity

import Data.Generics.Any
import Data.List
import Data.Maybe


reform :: Prog_ -> CmdArgs Any -> Maybe [String]
reform Prog_{..} CmdArgs{..} = Just $
    f "help" progHelpArg (isJust cmdArgsHelp) ++
    f "version" progVersionArg (isJust cmdArgsVersion) ++
    f "verbose" (fst progVerbosityArgs) (cmdArgsVerbosity == Just Loud) ++
    f "quiet" (snd progVerbosityArgs) (cmdArgsVerbosity == Just Quiet)
    where
        f ex (Just x) True = pickArg $ builtinNames x ++ [ex]
        f _ _ _ = []


pickArg :: [String] -> [String]
pickArg xs = case partition ((==) 1 . length) xs of
    (_, x:_) -> ["--" ++ x]
    (x:_, _) -> ["-" ++ x]
    _ -> []

{-

data Prog_ = Prog_
    {progModes :: [Mode_]
    ,progSummary :: Maybe [String]
    ,progProgram :: String
    ,progHelp :: String -- only for multiple mode programs
    ,progVerbosityArgs :: (Maybe Builtin_, Maybe Builtin_)
    ,progHelpArg :: Maybe Builtin_
    ,progVersionArg :: Maybe Builtin_
    } deriving Show
-}
