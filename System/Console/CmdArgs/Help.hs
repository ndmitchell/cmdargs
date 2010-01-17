
module System.Console.CmdArgs.Help(Help(..), showHelp) where

import Data.Char
import Data.Maybe
import Data.List
import Text.Format.Para


data Help = Norm String
          | Deuce (String,String)
          | Trip (String,String,String)
          | Para [String]


showHelp :: [Help] -> String -> Int -> String
showHelp help format defwidth = case map toLower format of
    "html" -> showHTML help
    "simple" -> showSimple help
    x | take 5 x == "text:" -> showText (read $ drop 5 x) help
    x | x `elem` ["text",""] -> showText defwidth help
    _ -> "Unknown help mode " ++ show format ++
        ", expected one of: text text:N html simple\n\n" ++ showText 80 help


showText :: Int -> [Help] -> String
showText width xs = unlines $ map f xs
    where
        f (Norm x) = x
        f (Deuce (a,b)) = cfmt width ("  " ++ a ++ sep aw a) b
        f (Trip (a,b,c)) = cfmt width ("  " ++ pad an a ++ pad bn b) c
        f (Para x) = intercalate "\n" $ formatParas width Nothing x

        cfmt w i s = intercalate "\n" $ formatParas w (Just i) [s]

        (ae,_) = unzip [x | Deuce x <- xs]
        aw = maximum $ map length ae
        sep n s = replicate (n - length s) ' '

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
        f (Deuce (a,b)) = "<tr><td class='indent'>" ++ escape a ++ "</td><td colspan='2'>" ++
                          (if null b then "&nbsp;" else escape b) ++ "</td></tr>"
        f (Trip (a,b,c)) = "<tr><td class='indent'>" ++ escape a ++ "</td>" ++
                           "<td>" ++ escape b ++ "</td>" ++
                           "<td>" ++ escape c ++ "</td></tr>"
        f (Para xs) = "<tr>" ++
                      "<td colspan='3'>" ++
                      "<p>" ++ concatMap htmlpara xs ++ "</p>" ++
                      "</td></tr>"
            where htmlpara = id

        escape :: String -> String
        escape = concatMap f'
            where f' '&' = "&amp;"
                  f' '>' = "&gt;"
                  f' '<' = "&lt;"
                  f' x = [x]

showSimple :: [Help] -> String
showSimple xs = unlines $ catMaybes $ map f $ tail xs
    where
        f (Norm x)
            | null x = Nothing
            | head x == ' ' = Nothing
            | head x == 'C' = Nothing
            | otherwise = Just $ "C:" ++ x
        f (Trip (a, b, c)) = Just $ "O:" ++ a ++ ":" ++ b ++ ":" ++ c
        f (Deuce (a, b)) = Just $ "L:" ++ a ++ ":" ++ b
        f (Para []) = Nothing
        f (Para xs) = Just $ "P:" ++ (intercalate "\nP:" $ lines $ concat xs)

