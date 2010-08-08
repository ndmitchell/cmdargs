
module System.Console.CmdArgs.Explicit.Help(HelpFormat(..), helpText) where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default
import Data.List
import Data.Maybe


-- | Specify the format to output the help.
data HelpFormat
    = HelpFormatDefault -- ^ Equivalent to 'HelpFormatAll' if there is not too much text, otherwise 'HelpFormatOne'.
    | HelpFormatOne -- ^ Display only the first mode.
    | HelpFormatAll -- ^ Display all modes.
      deriving (Read,Show,Enum,Bounded,Eq,Ord)

instance Default HelpFormat where def = HelpFormatDefault


instance Show (Mode a) where
    show = show . helpTextDefault []


-- | Generate a help message from a mode.
helpText :: HelpFormat -> Mode a -> [Text]
helpText HelpFormatDefault = helpTextDefault []
helpText HelpFormatOne = helpTextOne []
helpText HelpFormatAll = helpTextAll []


helpTextDefault path x = if length all > 40 then one else all
    where all = helpTextAll path x
          one = helpTextOne path x


-- | Help text for all modes
--
-- > <program> [OPTIONS] <file_args>
-- > <options>
-- > <program> MODE [FLAGS]
helpTextAll :: [Name] -> Mode a -> [Text]
helpTextAll = helpTextAny (\x y -> Line "" : helpTextAll x y)


-- | Help text for only this mode
--
-- > <program> [OPTIONS] <file_args>
-- > <options>
-- > <program> MODE [FLAGS]
-- > <options>
helpTextOne :: [Name] -> Mode a -> [Text]
helpTextOne = helpTextAny f
    where f _ x = [cols [head $ modeNames x ++ [""], modeHelp x]]


helpTextAny :: ([Name] -> Mode a -> [Text]) -> [Name] -> Mode a -> [Text]
helpTextAny f path x =
    [Line $ unwords $ path2 ++ ["[OPTIONS]" | not $ null $ modeFlags x] ++ map argType (maybeToList $ modeArgs x)] ++
    [Line $ " " ++ modeHelp x | not $ null $ modeHelp x] ++
    [Line "" | not $ null $ modeFlags x] ++
    helpGroup helpFlag (modeGroupFlags x) ++
    helpGroup (f path2) (modeGroupModes x) ++
    [Line "" | not $ null $ modeHelpSuffix x] ++
    map (\x -> Line $ " " ++ x) (modeHelpSuffix x)
    where path2 = path ++ take 1 (modeNames x)


helpGroup :: (a -> [Text]) -> Group a -> [Text]
helpGroup f xs = concatMap f (groupUnnamed xs) ++ concatMap g (groupNamed xs)
    where g (a,b) = Line a : concatMap f b


helpFlag :: Flag a -> [Text]
helpFlag x = [cols [unwords $ map ("-"++) a2, unwords $ map ("--"++) b2, flagHelp x]]
        where
            (a,b) = partition ((==) 1 . length) $ flagNames x
            (a2,b2) = if null b then (add a opt, b) else (a, add b opt)
            add x y = if null x then x else (head x ++ y) : tail x
            hlp = if null (flagType x) then "ITEM" else flagType x
            opt = case flagInfo x of
                FlagReq -> '=' : hlp
                FlagOpt x -> "[=" ++ hlp ++ "]"
                _ -> ""

cols (x:xs) = Cols $ ("  "++x) : map (' ':) xs
