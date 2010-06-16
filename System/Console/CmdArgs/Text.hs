{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Text(TextFormat(..), Text(..), showText) where

import Data.Char
import Data.Maybe
import Data.List
import Text.Format.Para


defaultTextWidth = 70

data TextFormat = HTML
                | Simple
                | Text (Maybe Int) -- ^ With width

instance Show TextFormat where
    show HTML = "html"
    show Simple = "simple"
    show (Text x) = "text" ++ maybe "" (\y -> ":" ++ show y) x

instance Read TextFormat where
    readsPrec _ "html" = [(HTML,"")]
    readsPrec _ "simple" = [(Simple,"")]
    readsPrec _ "text" = [(Text Nothing,"")]
    readsPrec _ x | "text:" `isPrefixOf` x = [(Text $ Just x,y) | (x,y) <- reads $ drop 5 x]
    readsPrec _ _ = []


data Text = Line String -- a single line
          | Cols (String,String,String) -- a single line with three columns (always indented by 2 spaces)

instance Show Text where
    showList = showString . showText defaultTextWidth
    show x = showText defaultTextWidth [x]


showHelp :: TextFormat -> [Text] -> String
showHelp HTML = showHTML
showHelp Simple = showSimple
showHelp (Text x) = showText (fromMaybe defaultTextWidth x)


showText :: Int -> [Text] -> String
showText width xs = unlines $ concatMap f xs
    where
        (as,bs,cs) = unzip3 [x | Cols x <- xs]
        an = maximum (map length as) + 1
        bn = maximum (map length bs) + 1
        abn = 2 + an + bn
        pad n x = x ++ replicate (n - length x) ' '

        f (Line x) = map (a++) $ wrap (width - length a) x
            where (a,b) = span isSpace x
        f (Cols (a,b,c)) = ("  " ++ pad an a ++ pad bn b ++ c1) : map (replicate abn ' ' ++) cs 
            where c1:cs = if null c then [""] else wrap (width - abn) c


-- | Split the text into strips of no-more than the given width
wrap :: Int -> String -> [String]
wrap width = combine . split
    where
        split :: String -> [(String,Int)] -- string, amount of space after
        split "" = []
        split x = (a,length c) : split d
            where (a,b) = break isSpace x
                  (c,d) = span isSpace b

        -- combine two adjacent chunks while they are less than width
        combine :: [(String,Int)] -> [String]
        combine ((a,b):(c,d):xs) | length a + b + length c < width = combine $ (a ++ replicate b ' ' ++ c,d):xs
        combine (x:xs) = fst x : combine xs
        combine [] = []


showHTML :: [Text] -> String
showHTML xs = unlines $
    ["<table class='cmdargs'>"] ++
    map f xs ++
    ["</table>"]
    where
        f (Line x) = "<tr><td colspan='3'" ++ (if null a then "" else " class='indent'") ++ ">" ++ escape b ++ "</td></tr>"
            where (a,b) = span isSpace x
        f (Cols (a,b,c)) = "<tr><td class='indent'>" ++ intercalate "</td><td>" (map escape [a,b,c]) ++ "</td></tr>"

        escape :: String -> String
        escape "" = "&nbsp;"
        escape xs = concatMap g xs
            where g '&' = "&amp;"
                  g '>' = "&gt;"
                  g '<' = "&lt;"
                  g x = [x]


showSimple :: [Text] -> String
showSimple xs = "I have no idea what simple format is, but I'm guess it should go direct from Mode..."
