{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Flag where

import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Data.Function

import System.Console.CmdArgs.Type


type Priority = Int -- higher is more priority

data Update = Update String (Dynamic -> Dynamic)
            | Special String String
            | Error String


---------------------------------------------------------------------
-- HELP INFORMATION FOR A FLAG

helpFlag :: Flag -> [(String,String,String)]
helpFlag xs =
    [(unwords (map ("-"++) short)
     ,unwords (map ("--"++) long) ++ val
     ,flagText xs ++ maybe "" (\x -> " (default=" ++ x ++ ")") (defaultFlag xs))
    | isFlagFlag xs]
    where
        (short,long) = partition ((==) 1 . length) $ flagFlag xs
        val = if isFlagBool xs then ""
              else ['['|opt] ++ "=" ++ flagTypDef "VALUE" xs ++ [']'|opt]
        opt = isFlagOpt xs


-- Given a flag, see what argument positions it should have
-- with the Int being a sort order
helpFlagArgs :: Flag -> [(Int,String)]
helpFlagArgs xs = case (flagArgs xs, flagTypDef "FILE" xs) of
    (Just Nothing,x) -> [(maxBound :: Int,"[" ++ x ++ "]")]
    (Just (Just i),x) -> [(i,x)]
    _ -> []


defaultFlag :: Flag -> Maybe String
defaultFlag x = flagOpt x `mplus` case flagVal x of
    x | Just v <- fromDynamic x, v /= "" -> Just v
      | Just v <- fromDynamic x, v /= (0::Int) -> Just $ show v
      | Just v <- fromDynamic x, v /= (0::Integer) -> Just $ show v
      | Just v <- fromDynamic x, v /= (0::Float) -> Just $ show v
      | Just v <- fromDynamic x, v /= (0::Double) -> Just $ show v
    _ -> Nothing


---------------------------------------------------------------------
-- PROCESS A FLAG

-- Priorities: 2 = flag, 1 = file, 0 = unknown
processFlag :: Flag -> [String] -> Maybe (Priority, Update, [String])
processFlag _ = undefined
