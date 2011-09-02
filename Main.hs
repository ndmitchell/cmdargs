
module Main where

import System.Console.CmdArgs.Test.All
import qualified System.Console.CmdArgs.Test.Implicit.Diffy as D
import qualified System.Console.CmdArgs.Test.Implicit.HLint as H
import qualified System.Console.CmdArgs.Test.Implicit.Maker as M
import System.Console.CmdArgs.Implicit(CmdArgs(..))
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default

import Control.Monad
import Data.List
import Data.Maybe
import System.IO


data Args = Test
          | Generate
          | Help HelpFormat TextFormat
          | Version
          | Demo Demo

args = (modes "cmdargs" (Help def def) "CmdArgs demo program" ms){modeGroupFlags = toGroup flags}
    where
        flags = [flagHelpFormat $ \a b _ -> Help a b
                ,flagVersion $ const Version
                ,flagNone ["test","t"] (const Test) "Run the tests"
                ,flagNone ["generate","g"] (const Generate) "Generate the manual"]

        ms = map (remap Demo (\(Demo x) -> (x,Demo))) demo


main = do
    x <- processArgs args
    let ver = "CmdArgs demo program, (C) Neil Mitchell"
    case x of
        Version -> putStrLn ver
        Help hlp txt -> do
            let xs = showText txt $ helpText [ver] hlp args
            putStrLn xs
            when (hlp == HelpFormatBash) $ do
                writeFileBinary "cmdargs.bash_comp" xs
                putStrLn "# Output written to cmdargs.bash_comp"
        Test -> test
        Generate -> generateManual
        Demo x -> runDemo x


writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = do
    h <- openBinaryFile file WriteMode
    hPutStr h x
    hClose h


---------------------------------------------------------------------
-- GENERATE MANUAL

generateManual :: IO ()
generateManual = do
    src <- readFile "cmdargs.htm"
    () <- length src `seq` return ()
    res <- fmap unlines $ f $ lines src
    () <- length res `seq` return ()
    h <- openBinaryFile "cmdargs.htm" WriteMode
    hPutStr h res
    hClose h
    where
        f (x:xs) | "<!-- BEGIN " `isPrefixOf` x = do
            ys <- generateChunk $ init $ drop 2 $ words x
            zs <- f $ tail $ dropWhile (not . isPrefixOf "<!-- END") xs
            return $ x : ys ++ ["<!-- END -->"] ++ zs
        f [] = return []
        f (x:xs) = fmap (x:) $ f xs

generateChunk :: [String] -> IO [String]
generateChunk ["help",x] = return $ case x of
    "hlint" -> f H.mode
    "diffy" -> f D.mode
    "maker" -> f M.mode
    where f = lines . fromJust . cmdArgsHelp . flip processValue ["--help=html"]

generateChunk ["code",x] = do
    src <- readFile $ "System/Console/CmdArgs/Test/Implicit/" ++ x ++ ".hs"
    return $ ["<pre>"] ++ recode (lines src) ++ ["</pre>"]


recode :: [String] -> [String]
recode = concatMap f . blanks . takeWhile (/= "-- STOP MANUAL")
    where
        blanks ("":"":xs) = blanks ("":xs)
        blanks [""] = []
        blanks [] = []
        blanks (x:xs) = x : blanks xs

        f x | x == "import System.Console.CmdArgs.Test.Implicit.Util" = []
            | "{-# OPTIONS_GHC " `isPrefixOf` x = []
            | "{-# LANGUAGE " `isPrefixOf` x = ["{-# LANGUAGE DeriveDataTypeable #-}"]
            | "module System.Console.CmdArgs.Test.Implicit." `isPrefixOf` x = ["module " ++ drop 44 x]
        f x = [x]
