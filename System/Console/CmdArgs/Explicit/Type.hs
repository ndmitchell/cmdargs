
module System.Console.CmdArgs.Explicit.Type where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe


boolTrue = ["true","yes","on","enabled","1"]
boolFalse = ["false","no","off","disabled","0"]


type Name = String
type Help = String
type FlagHelp = String


-- | Any flags in a "_" group are hidden
type Group a = [(Help,[a])]

fromGroup :: Group a -> [a]
fromGroup = concatMap snd

toGroup :: [a] -> Group a
toGroup x = [("",x)]


data Mode a = Mode
    {modeGroupList :: Group ([Name], Mode a) -- ^ The available sub-modes
    ,modeValue :: a -- ^ Value to start with
    ,modeHelp :: Help -- ^ Help text
    ,modeGroupFlags :: Group (Flag a) -- ^ Groups of flags, [("",xs)] for all in same group
    }

modeList = fromGroup . modeGroupList
modeFlags = fromGroup . modeGroupFlags

{-|
The 'FlagArg' type has the following meaning:

             ArgReq      ArgOpt       ArgOptRare/ArgNone
-xfoo        -x=foo      -x=foo       -x= -foo
-x foo       -x=foo      -x foo       -x= foo
-x=foo       -x=foo      -x=foo       -x=foo
--xx foo     --xx=foo    --xx foo      --xx foo
--xx=foo     --xx=foo    --xx=foo      --xx=foo
-}
data FlagArg
    = ArgReq             -- ^ Required argument
    | ArgOpt String      -- ^ Optional argument
    | ArgOptRare String  -- ^ Optional argument that requires an = before the value
    | ArgNone            -- ^ No argument
      deriving (Eq,Ord)

fromArgOpt (ArgOpt x) = x
fromArgOpt (ArgOptRare x) = x

data FlagInfo
    = FlagNamed
        {flagNamedArg :: FlagArg
        ,flagNamedNames :: [Name]}
    | FlagUnnamed
      deriving (Eq,Ord)


type Update a = String -> a -> Either String a

data Flag a = Flag
    {flagInfo :: FlagInfo
    ,flagValue :: Update a
    ,flagType :: FlagHelp -- the type of data for the user, i.e. FILE/DIR/EXT
    ,flagHelp :: Help
    }


---------------------------------------------------------------------
-- FLAG CREATORS

flagNone :: [Name] -> (a -> a) -> Help -> Flag a
flagNone names f help = Flag (FlagNamed ArgNone names) upd "" help
    where upd _ x = Right $ f x

flagOpt :: String -> [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagOpt def names upd typ help = Flag (FlagNamed (ArgOpt def) names) upd typ help

flagReq :: [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagReq names upd typ help = Flag (FlagNamed ArgReq names) upd typ help

flagArg :: Update a -> FlagHelp -> Flag a
flagArg upd typ = Flag FlagUnnamed upd typ ""

flagBool :: [Name] -> (Bool -> a -> a) -> Help -> Flag a
flagBool names f help = Flag (FlagNamed (ArgOptRare "") names) upd "" help
    where upd s x = if s == "" || ls `elem` boolTrue then Right $ f True x
                    else if ls `elem` boolFalse then Right $ f False x
                    else Left "expected boolean value (true/false)"
                where ls = map toLower s


---------------------------------------------------------------------
-- MODE/MODES CREATORS

mode :: a -> Help -> [Flag a] -> Mode a
mode value help flags = Mode [] value help $ toGroup flags

modes :: a -> Help -> [(Name,Mode a)] -> Mode a
modes value help xs = Mode (toGroup (map (first return) xs)) value help []


---------------------------------------------------------------------
-- CHECK FLAGS

-- | The 'modeNames' values are distinct between different modes.
--   The names/positions/arbitrary are distinct within one mode
checkMode :: Mode a -> Maybe String
checkMode x =
    (checkNames "modes" $ concatMap fst $ modeList x) `mplus`
    msum (map (checkMode . snd) $ modeList x) `mplus`
    (checkGroup $ modeGroupList x) `mplus`
    (checkGroup $ modeGroupFlags x) `mplus`
    (checkNames "flag names" $ concat [y | FlagNamed _ y <- xs]) `mplus`
    (check "Duplicate unnamed flags" $ unnamed > 1)
    where xs = map flagInfo $ modeFlags x
          unnamed = length [() | FlagUnnamed <- xs]


checkGroup :: Group a -> Maybe String
checkGroup x =
    (check "Empty group name" $ all (\x -> fst x /= "") $ drop 1 x) `mplus`
    (check "Empty group contents" $ case x of [("",[])] -> True; _ -> all (not . null . snd) x)


checkNames :: String -> [Name] -> Maybe String
checkNames msg xs = check "Empty names" (all (not . null) xs) `mplus` do
    bad <- listToMaybe $ xs \\ nub xs
    let dupe = filter (== bad) xs
    return $ "Sanity check failed, multiple " ++ msg ++ ": " ++ unwords (map show dupe)


check :: String -> Bool -> Maybe String
check msg True = Nothing
check msg False = Just msg
