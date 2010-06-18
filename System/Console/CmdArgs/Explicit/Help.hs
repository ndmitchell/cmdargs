
module System.Console.CmdArgs.Explicit.Help where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Text
import Data.List

instance Show (Mode a) where
    show = show . helpText


helpText :: Mode a -> [Text]
helpText m = helpGroup helpMode (modeGroupList m) ++
            [Line "" | null $ modeList m] ++
            helpGroup helpFlag (modeGroupFlags m)


helpGroup :: (a -> [Text]) -> Group a -> [Text]
helpGroup f xs = concat [[Line x | x /= ""] ++ concatMap f y | (x,y) <- xs, x /= "_"]


helpMode :: ([Name],Mode a) -> [Text]
helpMode (xs,y) = [Cols [commas xs, modeHelp y]]


helpFlag :: Flag a -> [Text]
helpFlag x = case flagInfo x of
    FlagUnnamed -> []
    FlagNamed typ names -> [Cols [commas $ map ("-"++) a2, commas $ map ("--"++) b2, flagHelp x]]
        where
            (a,b) = partition ((==) 1 . length) names
            (a2,b2) = if null b then (add a opt, b) else (a, add b opt)
            add x y = if null x then x else (head x ++ y) : tail x
            hlp = if null (flagType x) then "ITEM" else flagType x
            opt = case typ of
                ArgReq -> '=' : hlp
                ArgOpt x -> "[=" ++ hlp ++ "]"
                _ -> ""


commas = intercalate ", "
