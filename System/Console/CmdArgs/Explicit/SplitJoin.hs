{-# LANGUAGE RecordWildCards #-}
module System.Console.CmdArgs.Explicit.SplitJoin(splitArgs, joinArgs) where

import Data.Char
import Data.List
import Data.Maybe


-- | Given a sequence of arguments, join them together in a manner that could be used on
--   the command line, giving preference to the Windows @cmd@ shell quoting conventions.
--   @\\@.
--
--   For a version more intended for actual running the result, see "System.Process.showCommandForUser"
joinArgs :: [String] -> String
joinArgs = unwords . map f
    where
        f x = q ++ x ++ q
            where q = ['\"' | not $ null $ " \"" `intersect` x]



-- | Given a string, split into the available arguments. The inverse of 'joinArgs'.
splitArgs :: String -> [String]
splitArgs = join . initial
    where
        -- Nothing is start a new string
        -- Just x is accumulate onto the existing string
        join :: [Maybe Char] -> [String]
        join [] = []
        join xs = map fromJust a : join (drop 1 b)
            where (a,b) = break isNothing xs

        initial (x:xs) | isSpace x = initial xs
                       | otherwise = Just x : map Just xs
        initial [] = []
