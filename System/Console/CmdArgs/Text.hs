{-# LANGUAGE PatternGuards #-}

-- | A module to represent text with very basic formatting. Values are of
--   type ['Text'] and shown with 'showText'.
--
--   As an example of the formatting:
--
-- > [Line "Cooking for hungry people."
-- > ,Line "Welcome to my cookery recipe program, I sure hope you enjoy using it!"
-- > ,Line ""
-- > ,Cols ["Omlette","  A tasty eggy treat."]
-- > ,Cols ["  -m"," --mushrooms","  Some mushrooms, or in fact any other ingredients you have in the cupboards"]
-- > ,Cols ["  -e"," --eggs", "  But always you need eggs"]
-- > ,Line ""
-- > ,Cols ["Spagetti Bolognaise", "  An Italian delight."]
-- > ,Cols ["  -s"," --spagetti","  The first word in the name"]
-- > ,Cols ["  -b"," --bolognaise","  The second word in the name"]
-- > ,Cols ["  -d"," --dolmio","  The magic ingredient!"]
-- > ,Line ""
-- > ,Line "    The author of this program explicitly disclaims any liability for poisoning people who get their recipes off the internet."]
--
--   With @putStrLn ('showText' ('Wrap' 50) demo)@ gives:
--
-- > Cooking for hungry people.
-- > Welcome to my cookery recipe program, I sure hope
-- > you enjoy using it!
-- >
-- > Omlette              A tasty eggy treat.
-- >   -m --mushrooms   Some mushrooms, or in fact
-- >                    any other ingredients you have
-- >                    in the cupboards
-- >   -e --eggs        But always you need eggs
-- >
-- > Spagetti Bolognaise  An Italian delight.
-- >   -s --spagetti    The first word in the name
-- >   -b --bolognaise  The second word in the name
-- >   -d --dolmio      The magic ingredient!
-- >
-- >     The author of this program explicitly
-- >     disclaims any liability for poisoning people
-- >     who get their recipes off the internet.
module System.Console.CmdArgs.Text(TextFormat(..), defaultWrap, Text(..), showText) where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Console.CmdArgs.Default


-- | Wrap with the default width of 80 characters.
defaultWrap :: TextFormat
defaultWrap = Wrap 80

-- | How to output the text.
data TextFormat = HTML -- ^ Display as HTML.
                | Wrap Int -- ^ Display as text wrapped at a certain width (see 'defaultWrap').
                  deriving (Read,Show,Eq,Ord)

instance Default TextFormat where def = defaultWrap

-- | The data type representing some text, typically used as @[Text]@. The formatting
--   is described by:
--
--   * 'Line' values represent a paragraph of text, and may be wrapped depending on the 'TextFormat'.
--     If a 'Line' value is wrapped then all leading space will be treated as an indent.
--
--   * 'Cols' values represent columns of text. Within any @[Text]@ all columns of the same length
--     are grouped in tabs, with the final column being wrapped if necessary. All columns are placed
--     adjacent with no space between them - for this reason most columns will start with a space.
data Text = Line String -- a single line
          | Cols [String] -- a single line with columns (always indented by 2 spaces)

instance Show Text where
    showList = showString . showText defaultWrap
    show x = showText defaultWrap [x]


-- | Show some text using the given formatting.
showText :: TextFormat -> [Text] -> String
showText HTML = showHTML
showText (Wrap x) = showWrap x


---------------------------------------------------------------------
-- TEXT OUTPUT

showWrap :: Int -> [Text] -> String
showWrap width xs = unlines $ concatMap f xs
    where
        cs :: [(Int,[Int])]
        cs = map (\x -> (fst $ head x, map maximum $ transpose $ map snd x)) $
                groupBy ((==) `on` fst) $ sortBy (compare `on` fst)
                [(length x, map length $ init x) | Cols x <- xs]
        pad n x = x ++ replicate (n - length x) ' '

        f (Line x) = map (a++) $ wrap1 (width - length a) b
            where (a,b) = span isSpace x

        f (Cols xs) = (concat $ zipWith pad ys xs ++ [z1]) : map (replicate n ' '++) zs
            where ys = fromJust $ lookup (length xs) cs
                  n = sum ys + length (takeWhile isSpace $ last xs)
                  z1:zs = wrap1 (width - n) (last xs)


wrap1 width x = ["" | null res] ++ res
    where res = wrap width x

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

        f (Line x) = tr $ td cols x
        f (Cols xs) = tr $ concatMap (td 1) (init xs) ++ td (cols + 1 - length xs) (last xs)

        tr x = "<tr>" ++ x ++ "</tr>"
        td cols x = "<td" ++ (if cols == 1 then "" else " colspan='" ++ show cols ++ "'")
                          ++ (if a /= "" then " style='padding-left:" ++ show (length a) ++ "ex;'" else "") ++
                     ">" ++ if null b then "&nbsp;" else concatMap esc b ++ "</td>"
            where (a,b) = span isSpace x

        esc '&' = "&amp;"
        esc '>' = "&gt;"
        esc '<' = "&lt;"
        esc x = [x]
