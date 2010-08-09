{-# LANGUAGE PatternGuards #-}

module Main where

import System.Console.CmdArgs.Test.All
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default


data Args = Test
          | Generate
          | Help HelpFormat TextFormat
          | Version
          | Demo Demo

args = (modes "cmdargs" (Help def def) "CmdArgs demo program" ms){modeGroupFlags = toGroup flags}
    where
        flags = [flagHelpFormat $ \a b _ -> Help a b
                ,flagVersion $ const Version
                ,flagNone ["t","test"] (const Test) "Run the tests"
                ,flagNone ["g","generate"] (const Generate) "Generate the manual"]

        ms = map (remap Demo (\(Demo x) -> (x,Demo))) demo


main = do
    x <- processArgs args
    let ver = "CmdArgs demo program, (C) Neil Mitchell"
    case x of
        Version -> putStrLn ver
        Help hlp txt -> putStrLn $ showText txt $ Line ver : Line "" : helpText hlp args
        Test -> test
        Generate -> generateManual
        Demo x -> runDemo x


---------------------------------------------------------------------
-- GENERATE MANUAL

generateManual :: IO ()
generateManual = do
    error "todo"
{-
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
generateChunk ["help",x] = do
    src <- readFile $ x ++ ".hs"
    let str = head [takeWhile (/= '\"') $ drop 1 $ dropWhile (/= '\"') x | x <- lines src, "main" `isPrefixOf` x]
    () <- length src `seq` return ()
    fmap lines $ case x of
        "hlint" -> cmdArgsHelp str H.modes HTML
        "diffy" -> cmdArgsHelp str D.modes HTML
        "maker" -> cmdArgsHelp str M.modes HTML

generateChunk ["code",x] = do
    src <- readFile $ x ++ ".hs"
    return $ ["<pre>"] ++ lines src ++ ["</pre>"]
-}
