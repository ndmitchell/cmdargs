
module System.Console.CmdArgs.Help(Help(..), showHelp) where

import Data.Char


data Help = Norm String
          | Indent String
          | Code String
          | Trip (String,String,String) -- code,code,norm


showHelp :: [Help] -> String -> String
showHelp help format = case map toLower format of
    "html" -> showHTML help
    x | x `elem` ["text",""] -> showText help
    _ -> "Unknown help mode " ++ show format ++ ", expected one of: text html\n\n" ++
         showText help


showText :: [Help] -> String
showText xs = unlines $ map f xs
    where f (Norm x) = x
          f (Indent x) = "  " ++ x
          f (Code x) = x
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
    where f (Norm x) = "<tr><td colspan='3'>" ++ (if null x then "&nbsp;" else escape x) ++ "</td></tr>"
          f (Indent x) = "<tr><td class='indent' colspan='3'>" ++ escape x ++ "</td></tr>"
          f (Code x) = "<tr><td colspan='3'><tt>" ++ escape x ++ "</tt></td></tr>"
          f (Trip (a,b,c)) = "<tr><td class='indent'><tt>" ++ escape a ++ "</tt></td>" ++
                             "<td><tt>" ++ escape b ++ "</tt></td>" ++
                             "<td>" ++ escape c ++ "</td></tr>"


escape :: String -> String
escape = concatMap f
    where f '&' = "&amp;"
          f '>' = "&gt;"
          f '<' = "&lt;"
          f x = [x]
