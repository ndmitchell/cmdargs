{-# OPTIONS_GHC -fno-warn-orphans #-} -- Not good reasons, but shouldn't be too fatal
{-
Sample renderings:

-- ONE MODE
Program description

programname [OPTIONS] FILE1 FILE2 [FILES]
  Program to perform some action

  -f --flag     description
Flag grouping:
  -a --another  description


-- MANY MODES WITH ONE SHOWN
Program description

programname [COMMAND] [OPTIONS] ...
  Program to perform some action

Commands:
  [build]  Build action here
  test     Test action here

Flags:
  -s --special  Special for the root only
Common flags:
  -? --help     Build action here


-- MANY MODES WITH ALL SHOWN
Program description

programname [COMMAND] [OPTIONS] ...
  Program to perform some action

  -s --special  Special for the root only
Common flags:
  -? --help     Build action here

programname [build] [OPTIONS] [FILES}
  Action to perform here
-}

module System.Console.CmdArgs.Explicit.Help(HelpFormat(..), helpText) where

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Explicit.Complete
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default
import Data.List
import Data.Maybe


-- | Specify the format to output the help.
data HelpFormat
    = HelpFormatDefault -- ^ Equivalent to 'HelpFormatAll' if there is not too much text, otherwise 'HelpFormatOne'.
    | HelpFormatOne -- ^ Display only the first mode.
    | HelpFormatAll -- ^ Display all modes.
    | HelpFormatBash -- ^ Bash completion information
    | HelpFormatZsh -- ^ Z shell completion information
      deriving (Read,Show,Enum,Bounded,Eq,Ord)

instance Default HelpFormat where def = HelpFormatDefault


instance Show (Mode a) where
    show = show . helpTextDefault

instance Show (Flag a) where
    show = show . helpFlag

instance Show (Arg a) where
    show = show . argType

-- | Generate a help message from a mode.  The first argument is a prefix,
--   which is prepended when not using 'HelpFormatBash' or 'HelpFormatZsh'.
helpText :: [String] -> HelpFormat -> Mode a -> [Text]
helpText pre HelpFormatDefault x = helpPrefix pre ++ helpTextDefault x
helpText pre HelpFormatOne x = helpPrefix pre ++ helpTextOne x
helpText pre HelpFormatAll x = helpPrefix pre ++ helpTextAll x
helpText pre HelpFormatBash x = map Line $ completeBash $ head $ modeNames x ++ ["unknown"]
helpText pre HelpFormatZsh x = map Line $ completeZsh $ head $ modeNames x ++ ["unknown"]


helpPrefix :: [String] -> [Text]
helpPrefix xs = map Line xs ++ [Line "" | not $ null xs]


helpTextDefault x = if length all > 40 then one else all
    where all = helpTextAll x
          one = helpTextOne x


-- | Help text for all modes
--
-- > <program> [OPTIONS] <file_args>
-- > <options>
-- > <program> MODE [SUBMODE] [OPTIONS] [FLAG]
helpTextAll :: Mode a -> [Text]
helpTextAll = disp . push ""
    where
        disp m = uncurry (++) (helpTextMode m) ++ concatMap (\x -> Line "" : disp x) (modeModes m)
        push s m = m{modeNames = map (s++) $ modeNames m
                    ,modeGroupModes = fmap (push s2) $ modeGroupModes m}
            where s2 = s ++ concat (take 1 $ modeNames m) ++ " "


-- | Help text for only this mode
--
-- > <program> [OPTIONS] <file_args>
-- > <options>
-- > <program> MODE [FLAGS]
-- > <options>
helpTextOne :: Mode a -> [Text]
helpTextOne m = pre ++ ms ++ suf
    where
        (pre,suf) = helpTextMode m
        ms = space $ [Line "Commands:" | not $ null $ groupUnnamed $ modeGroupModes m] ++ helpGroup f (modeGroupModes m)
        f m = return $ cols [concat $ take 1 $ modeNames m, ' ' : modeHelp m]


helpTextMode :: Mode a -> ([Text], [Text])
helpTextMode x@Mode{modeGroupFlags=flags,modeGroupModes=modes} = (pre,suf)
    where
        pre = [Line $ unwords $ take 1 (modeNames x) ++
                  ["[COMMAND] ..." | notNullGroup modes] ++
                  ["[OPTIONS]" | not $ null $ fromGroup flags] ++
                  helpArgs (modeArgs x)] ++
              [Line $ "  " ++ modeHelp x | not $ null $ modeHelp x]
        suf = space
                  ([Line "Flags:" | mixedGroup flags] ++
                   helpGroup helpFlag (modeGroupFlags x)) ++
              space (map Line $ modeHelpSuffix x)


helpGroup :: (a -> [Text]) -> Group a -> [Text]
helpGroup f xs = concatMap f (groupUnnamed xs) ++ concatMap g (groupNamed xs)
    where g (a,b) = Line (a ++ ":") : concatMap f b


helpArgs :: ([Arg a], Maybe (Arg a)) -> [String]
helpArgs (ys,y) = [['['|o] ++ argType x ++ [']'|o] | (i,x) <- zip [0..] xs, let o = False && req <= i]
    where xs = ys ++ maybeToList y
          req = maximum $ 0 : [i | (i,x) <- zip [1..] xs, argRequire x]


helpFlag :: Flag a -> [Text]
helpFlag x = [cols [unwords $ map ("-"++) a2, unwords $ map ("--"++) b2, ' ' : flagHelp x]]
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
space xs = [Line "" | not $ null xs] ++ xs


nullGroup x = null (groupUnnamed x) && null (groupNamed x)
notNullGroup = not . nullGroup
mixedGroup x = not $ null (groupUnnamed x) || null (groupNamed x) -- has both unnamed and named
