
module System.Console.CmdArgs.Help(Help, showHelp) where

import Data.Char


type Help = [Either String (String,String,String)]


showHelp :: Help -> String -> IO ()
showHelp help format = putStr $ case map toLower format of
    "html" -> showHTML help
    "confluence" -> showConfluence help
    x | x `elem` ["console",""] -> showConsole help
    _ -> "Unknown help mode " ++ show format ++ ", expected one of: console html confluence\n\n" ++
         showConsole help



showHTML = showConsole
showConfluence = showConsole


showConsole :: Help -> String
showConsole xs = unlines $ map f xs
    where f (Left x) = x
          f (Right (a,b,c)) = "  " ++ pad an a ++ pad bn b ++ " " ++ c

          (as,bs,_) = unzip3 [x | Right x <- xs]
          an = maximum $ map length as
          bn = maximum $ map length bs
          pad n x = x ++ replicate (n - length x + 1) ' '
