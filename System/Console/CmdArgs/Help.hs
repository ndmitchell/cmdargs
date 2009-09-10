
module System.Console.CmdArgs.Help(Help(..), showHelp) where

import Data.Char


data Help = Norm String
          | Trip (String,String,String)


showHelp :: [Help] -> String -> String
showHelp help format = case map toLower format of
    "html" -> showHTML help
    x | x `elem` ["text",""] -> showText help
    _ -> "Unknown help mode " ++ show format ++ ", expected one of: text html\n\n" ++
         showText help


showText :: [Help] -> String
showText xs = unlines $ map f xs
    where
        f (Norm x) = x
        f (Trip (a,b,c)) = "  " ++ pad an a ++ pad bn b ++ " " ++ c

        (as,bs,_) = unzip3 [x | Trip x <- xs]
        an = maximum $ map length as
        bn = maximum $ map length bs
        pad n x = x ++ replicate (n - length x + 1) ' '


showHTML :: [Help] -> String
showHTML xs = unlines $
    ["<table class='cmdargs'>"] ++
    map f xs ++
    ["</table>"]
    where
        f (Norm x) = "<tr><td colspan='3'" ++ (if null a then "" else " class='indent'") ++ ">" ++
                     (if null b then "&nbsp;" else escape b) ++ "</td></tr>"
            where (a,b) = span isSpace x
        f (Trip (a,b,c)) = "<tr><td class='indent'>" ++ escape a ++ "</td>" ++
                           "<td>" ++ escape b ++ "</td>" ++
                           "<td>" ++ escape c ++ "</td></tr>"


escape :: String -> String
escape = concatMap f
    where f '&' = "&amp;"
          f '>' = "&gt;"
          f '<' = "&lt;"
          f x = [x]
