{-# LANGUAGE PatternGuards #-}

-- | This module does command line completion
module System.Console.CmdArgs.Explicit.Complete(
    Complete(..), complete,
    completeBash, completeZsh
    ) where

import System.Console.CmdArgs.Explicit.Type
import Control.Monad
import Data.List
import Data.Maybe


-- | How to complete a command line option.
--   The 'Show' instance is suitable for parsing from shell scripts.
data Complete
    = CompleteValue String -- ^ Complete to a particular value
    | CompleteFile String FilePath -- ^ Complete to a prefix, and a file
    | CompleteDir String FilePath -- ^ Complete to a prefix, and a directory
      deriving (Eq,Ord)

instance Show Complete where
    show (CompleteValue a) = "VALUE " ++ a
    show (CompleteFile a b) = "FILE " ++ a ++ " " ++ b
    show (CompleteDir a b) = "DIR " ++ a ++ " " ++ b

    showList xs = showString $ unlines (map show xs)


prepend :: String -> Complete -> Complete
prepend a (CompleteFile b c) = CompleteFile (a++b) c
prepend a (CompleteDir b c) = CompleteDir (a++b) c
prepend a (CompleteValue b) = CompleteValue (a++b)


-- | Given a current state, return the set of commands you could type now, in preference order.
complete
    :: Mode a -- ^ Mode specifying which arguments are allowed
    -> [String] -- ^ Arguments the user has already typed
    -> (Int,Int) -- ^ 0-based index of the argument they are currently on, and the position in that argument
    -> [Complete]
-- Roll forward looking at modes, and if you match a mode, enter it
-- If the person just before is a flag without arg, look at how you can complete that arg
-- If your prefix is a complete flag look how you can complete that flag
-- If your prefix looks like a flag, look for legitimate flags
-- Otherwise give a file/dir if they are arguments to this mode, and all flags
-- If you haven't seen any args/flags then also autocomplete to any child modes
complete mode_ args_ (i,_) = nub $ followArgs mode args now
    where
        (seen,next) = splitAt i args_
        now = head $ next ++ [""]
        (mode,args) = followModes mode_ seen


-- | Given a mode and some arguments, try and drill down into the mode
followModes :: Mode a -> [String] -> (Mode a, [String])
followModes m (x:xs) | Just m2 <- pickBy modeNames x $ modeModes m = followModes m2 xs
followModes m xs = (m,xs)


pickBy :: (a -> [String]) -> String -> [a] -> Maybe a
pickBy f name xs = find (\x -> name `elem` f x) xs `mplus`
                   find (\x -> any (name `isPrefixOf`) (f x)) xs


-- | Follow args deals with all seen arguments, then calls on to deal with the next one
followArgs :: Mode a -> [String] -> (String -> [Complete])
followArgs m = first
    where
        first [] = expectArgFlagMode (modeModes m) (argsPick 0) (modeFlags m)
        first xs = norm 0 xs

        -- i is the number of arguments that have gone past
        norm i [] = expectArgFlag (argsPick i) (modeFlags m)
        norm i ("--":xs) = expectArg $ argsPick (i + length xs)
        norm i (('-':'-':x):xs) | null b, flagInfo flg == FlagReq = val i flg xs
                                | otherwise = norm i xs
            where (a,b) = break (== '=') x
                  flg = getFlag a
        norm i (('-':x:y):xs) = case flagInfo flg of
            FlagReq | null y -> val i flg xs
                    | otherwise -> norm i xs
            FlagOpt{} -> norm i xs
            _ | "=" `isPrefixOf` y -> norm i xs
              | null y -> norm i xs
              | otherwise -> norm i (('-':y):xs)
            where flg = getFlag [x]
        norm i (x:xs) = norm (i+1) xs

        val i flg [] = expectVal flg
        val i flg (x:xs) = norm i xs

        argsPick i = let (lst,end) = modeArgs m in if i < length lst then Just $ lst !! i else end

        -- if you can't find the flag, pick one that is FlagNone (has all the right fallback)
        getFlag x = fromMaybe (flagNone [] id "") $ pickBy flagNames x $ modeFlags m


expectArgFlagMode :: [Mode a] -> Maybe (Arg a) -> [Flag a] -> String -> [Complete]
expectArgFlagMode mode arg flag x
    | "-" `isPrefixOf` x = expectFlag flag x ++ [CompleteValue "-" | x == "-", isJust arg]
    | otherwise = expectMode mode x ++ expectArg arg x ++ expectFlag flag x

expectArgFlag :: Maybe (Arg a) -> [Flag a] -> String -> [Complete]
expectArgFlag arg flag x
    | "-" `isPrefixOf` x = expectFlag flag x ++ [CompleteValue "-" | x == "-", isJust arg]
    | otherwise = expectArg arg x ++ expectFlag flag x

expectMode :: [Mode a] -> String -> [Complete]
expectMode mode = expectStrings (map modeNames mode)

expectArg :: Maybe (Arg a) -> String -> [Complete]
expectArg Nothing x = []
expectArg (Just arg) x = expectFlagHelp (argType arg) x

expectFlag :: [Flag a] -> String -> [Complete]
expectFlag flag x
    | (a,_:b) <- break (== '=') x = case pickBy (map f . flagNames) a flag of
        Nothing -> []
        Just flg -> map (prepend (a ++ "=")) $ expectVal flg b
    | otherwise = expectStrings (map (map f . flagNames) flag) x
    where f x = "-" ++ ['-' | length x > 1] ++ x

expectVal :: Flag a -> String -> [Complete]
expectVal flg = expectFlagHelp (flagType flg)

expectStrings :: [[String]] -> String -> [Complete]
expectStrings xs x = map CompleteValue $ concatMap (take 1 . filter (x `isPrefixOf`)) xs

expectFlagHelp :: FlagHelp -> String -> [Complete]
expectFlagHelp typ x = case typ of
    "FILE" -> [CompleteFile "" x]
    "DIR" -> [CompleteDir "" x]
    "FILE/DIR" -> [CompleteFile "" x, CompleteDir "" x]
    "DIR/FILE" -> [CompleteDir "" x, CompleteFile "" x]
    '[':s | "]" `isSuffixOf` s -> expectFlagHelp (init s) x
    _ -> []


---------------------------------------------------------------------
-- BASH SCRIPT

completeBash :: String -> [String]
completeBash prog =
    ["# Completion for " ++ prog
    ,"# Generated by CmdArgs: http://community.haskell.org/~ndm/cmdargs/"
    ,"_" ++ prog ++ "()"
    ,"{"
    ,"    # local CMDARGS_DEBUG=1 # uncomment to debug this script"
    ,""
    ,"    COMPREPLY=()"
    ,"    function add { COMPREPLY[((${#COMPREPLY[@]} + 1))]=$1 ; }"
    ,"    IFS=$'\\n\\r'"
    ,""
    ,"    export CMDARGS_COMPLETE=$((${COMP_CWORD} - 1))"
    ,"    result=`" ++ prog ++ " ${COMP_WORDS[@]:1}`"
    ,""
    ,"    if [ -n $CMDARGS_DEBUG ]; then"
    ,"        echo Call \\(${COMP_WORDS[@]:1}, $CMDARGS_COMPLETE\\) > cmdargs.tmp"
    ,"        echo $result >> cmdargs.tmp"
    ,"    fi"
    ,"    unset CMDARGS_COMPLETE"
    ,"    unset CMDARGS_COMPLETE_POS"
    ,""
    ,"    for x in $result ; do"
    ,"        case $x in"
    ,"            VALUE\\ *)"
    ,"                add ${x:6}"
    ,"                ;;"
    ,"            FILE\\ *)"
    ,"                local prefix=`expr match \"${x:5}\" '\\([^ ]*\\)'`"
    ,"                local match=`expr match \"${x:5}\" '[^ ]* \\(.*\\)'`"
    ,"                for x in `compgen -f -- \"$match\"`; do"
    ,"                    add $prefix$x"
    ,"                done"
    ,"                ;;"
    ,"            DIR\\ *)"
    ,"                local prefix=`expr match \"${x:4}\" '\\([^ ]*\\)'`"
    ,"                local match=`expr match \"${x:4}\" '[^ ]* \\(.*\\)'`"
    ,"                for x in `compgen -d -- \"$match\"`; do"
    ,"                    add $prefix$x"
    ,"                done"
    ,"                ;;"
    ,"        esac"
    ,"    done"
    ,"    unset IFS"
    ,""
    ,"    if [ -n $CMDARGS_DEBUG ]; then"
    ,"        echo echo COMPREPLY: ${#COMPREPLY[@]} = ${COMPREPLY[@]} >> cmdargs.tmp"
    ,"    fi"
    ,"}"
    ,"complete -o bashdefault -F _" ++ prog ++ " " ++ prog
    ]


---------------------------------------------------------------------
-- ZSH SCRIPT

completeZsh :: String -> [String]
completeZsh _ = ["echo TODO: help add Zsh completions to cmdargs programs"]
