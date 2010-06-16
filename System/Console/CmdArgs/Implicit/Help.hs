{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Implicit.Help(Text(..), showHelp) where

import Data.Char
import Data.List
import System.Console.CmdArgs.Text


showHelp :: [Text] -> String -> Int -> String
showHelp help format defwidth = case readTextFormat $ map toLower format of
    Just (Wrap Nothing) -> showText (Wrap $ Just defwidth) help
    Just y -> showText y help
    Nothing -> "Unknown help mode " ++ show format ++ ", expected one of: text text:N html\n\n" ++
               showText (Wrap $ Just defwidth) help


readTextFormat :: String -> Maybe TextFormat
readTextFormat x = case x of
    "html" -> Just HTML
    _ | "text:" `isPrefixOf` x -> Just $ Wrap $ Just $ read $ drop 5 x
      | x `elem` ["text",""] -> Just $ Wrap Nothing
      | (y,"") <- span isDigit x -> Just $ Wrap $ Just $ read y
    _ -> Nothing
