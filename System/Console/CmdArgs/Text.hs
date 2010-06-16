{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Text(TextFormat(..), Text(..), showText) where

import Control.Arrow
import Data.Char
import Data.Function
import Data.List
import Data.Maybe


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
          | Cols [String] -- a single line with columns (always indented by 2 spaces)

instance Show Text where
    showList = showString . showText defaultTextWidth
    show x = showText defaultTextWidth [x]


showHelp :: TextFormat -> [Text] -> String
showHelp HTML = showHTML
showHelp Simple = showSimple
showHelp (Text x) = showText (fromMaybe defaultTextWidth x)


---------------------------------------------------------------------
-- TEXT OUTPUT

showText :: Int -> [Text] -> String
showText width xs = unlines $ concatMap f xs
    where
        cs :: [(Int,[Int])]
        cs = map (\x -> (fst $ head x, map maximum $ transpose $ map snd x)) $
                groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
                [(length x, map length $ init x) | Cols x <- xs]
        pad n x = x ++ replicate (n - length x) ' '

        f (Line x) = map (a++) $ wrap (width - length a) x
            where (a,b) = span isSpace x

        f (Cols xs) = (concatMap ("  "++) $ zipWith pad ys xs ++ [z1]) : zs
            where ys = fromJust $ lookup (length xs) cs
                  z1:zs = if null (last xs) then [""] else wrap (width - sum ys - (2 * length xs)) (last xs)


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


---------------------------------------------------------------------
-- HTML OUTPUT

showHTML :: [Text] -> String
showHTML xs = unlines $
    ["<table class='cmdargs'>"] ++
    map f xs ++
    ["</table>"]
    where
        cols = maximum [length x | Cols x <- xs]

        f (Line x) = tr $ td cols (null a) b
            where (a,b) = span isSpace x
        f (Cols xs) = tr $ concatMap (td 1 True) (init xs) ++ td (cols + 1 - length xs) True (last xs)

        tr x = "<tr>" ++ x ++ "</tr>"
        td cols indent x = "<td" ++ (if cols == 1 then "" else " colspan='" ++ show cols ++ "'")
                                 ++ (if indent then " class='indent'" else "") ++ ">" ++
                           text x ++ "</td>"
        text "" = "&nbsp;"
        text xs = concatMap g xs
            where g '&' = "&amp;"
                  g '>' = "&gt;"
                  g '<' = "&lt;"
                  g x = [x]


---------------------------------------------------------------------
-- SIMPLE OUTPUT

showSimple :: [Text] -> String
showSimple xs = "I have no idea what simple format is, but I'm guess it should go direct from Mode..."
