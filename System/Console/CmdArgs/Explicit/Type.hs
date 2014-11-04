{-# LANGUAGE CPP #-}

module System.Console.CmdArgs.Explicit.Type where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid
#endif


-- | A name, either the name of a flag (@--/foo/@) or the name of a mode.
type Name = String

-- | A help message that goes with either a flag or a mode.
type Help = String

-- | The type of a flag, i.e. @--foo=/TYPE/@.
type FlagHelp = String


---------------------------------------------------------------------
-- UTILITY

-- | Parse a boolean, accepts as True: true yes on enabled 1. 
parseBool :: String -> Maybe Bool
parseBool s | ls `elem` true  = Just True
            | ls `elem` false = Just False
            | otherwise = Nothing
    where
        ls = map toLower s
        true = ["true","yes","on","enabled","1"]
        false = ["false","no","off","disabled","0"]


---------------------------------------------------------------------
-- GROUPS

-- | A group of items (modes or flags). The items are treated as a list, but the
--   group structure is used when displaying the help message.
data Group a = Group
    {groupUnnamed :: [a] -- ^ Normal items.
    ,groupHidden :: [a] -- ^ Items that are hidden (not displayed in the help message).
    ,groupNamed :: [(Help, [a])] -- ^ Items that have been grouped, along with a description of each group.
    } deriving Show 

instance Functor Group where
    fmap f (Group a b c) = Group (map f a) (map f b) (map (second $ map f) c)

instance Monoid (Group a) where
    mempty = Group [] [] []
    mappend (Group x1 x2 x3) (Group y1 y2 y3) = Group (x1++y1) (x2++y2) (x3++y3)

-- | Convert a group into a list.
fromGroup :: Group a -> [a]
fromGroup (Group x y z) = x ++ y ++ concatMap snd z

-- | Convert a list into a group, placing all fields in 'groupUnnamed'.
toGroup :: [a] -> Group a
toGroup x = Group x [] []


---------------------------------------------------------------------
-- TYPES

-- | A mode. Do not use the 'Mode' constructor directly, instead
--   use 'mode' to construct the 'Mode' and then record updates.
--   Each mode has three main features:
--
--   * A list of submodes ('modeGroupModes')
--
--   * A list of flags ('modeGroupFlags')
--
--   * Optionally an unnamed argument ('modeArgs')
--
--  To produce the help information for a mode, either use 'helpText' or 'show'.
data Mode a = Mode
    {modeGroupModes :: Group (Mode a) -- ^ The available sub-modes
    ,modeNames :: [Name] -- ^ The names assigned to this mode (for the root mode, this name is used as the program name)
    ,modeValue :: a -- ^ Value to start with
    ,modeCheck :: a -> Either String a -- ^ Check the value reprsented by a mode is correct, after applying all flags
    ,modeReform :: a -> Maybe [String] -- ^ Given a value, try to generate the input arguments.
    ,modeExpandAt :: Bool -- ^ Expand @\@@ arguments with 'expandArgsAt', defaults to 'True', only applied if using an 'IO' processing function.
                          --   Only the root 'Mode's value will be used.
    ,modeHelp :: Help -- ^ Help text
    ,modeHelpSuffix :: [String] -- ^ A longer help suffix displayed after a mode
    ,modeArgs :: ([Arg a], Maybe (Arg a)) -- ^ The unnamed arguments, a series of arguments, followed optionally by one for all remaining slots
    ,modeGroupFlags :: Group (Flag a) -- ^ Groups of flags
    }

-- | Extract the modes from a 'Mode'
modeModes :: Mode a -> [Mode a]
modeModes = fromGroup . modeGroupModes

-- | Extract the flags from a 'Mode'
modeFlags :: Mode a -> [Flag a]
modeFlags = fromGroup . modeGroupFlags

-- | The 'FlagInfo' type has the following meaning:
--
--
-- >              FlagReq     FlagOpt      FlagOptRare/FlagNone
-- > -xfoo        -x=foo      -x=foo       -x -foo
-- > -x foo       -x=foo      -x foo       -x foo
-- > -x=foo       -x=foo      -x=foo       -x=foo
-- > --xx foo     --xx=foo    --xx foo     --xx foo
-- > --xx=foo     --xx=foo    --xx=foo     --xx=foo
data FlagInfo
    = FlagReq             -- ^ Required argument
    | FlagOpt String      -- ^ Optional argument
    | FlagOptRare String  -- ^ Optional argument that requires an = before the value
    | FlagNone            -- ^ No argument
      deriving (Eq,Ord,Show)

-- | Extract the value from inside a 'FlagOpt' or 'FlagOptRare', or raises an error.
fromFlagOpt :: FlagInfo -> String
fromFlagOpt (FlagOpt x) = x
fromFlagOpt (FlagOptRare x) = x

-- | A function to take a string, and a value, and either produce an error message
--   (@Left@), or a modified value (@Right@).
type Update a = String -> a -> Either String a

-- | A flag, consisting of a list of flag names and other information.
data Flag a = Flag
    {flagNames :: [Name] -- ^ The names for the flag.
    ,flagInfo :: FlagInfo -- ^ Information about a flag's arguments.
    ,flagValue :: Update a -- ^ The way of processing a flag.
    ,flagType :: FlagHelp -- ^ The type of data for the flag argument, i.e. FILE\/DIR\/EXT
    ,flagHelp :: Help -- ^ The help message associated with this flag.
    }


-- | An unnamed argument. Anything not starting with @-@ is considered an argument,
--   apart from @\"-\"@ which is considered to be the argument @\"-\"@, and any arguments
--   following @\"--\"@. For example:
--
-- > programname arg1 -j - --foo arg3 -- -arg4 --arg5=1 arg6
--
--   Would have the arguments:
--
-- > ["arg1","-","arg3","-arg4","--arg5=1","arg6"]
data Arg a = Arg
    {argValue :: Update a -- ^ A way of processing the argument.
    ,argType :: FlagHelp -- ^ The type of data for the argument, i.e. FILE\/DIR\/EXT
    ,argRequire :: Bool -- ^ Is at least one of these arguments required, the command line will fail if none are set
    }


---------------------------------------------------------------------
-- CHECK FLAGS

-- | Check that a mode is well formed.
checkMode :: Mode a -> Maybe String
checkMode x = msum
    [checkNames "modes" $ concatMap modeNames $ modeModes x
    ,msum $ map checkMode $ modeModes x
    ,checkGroup $ modeGroupModes x
    ,checkGroup $ modeGroupFlags x
    ,checkNames "flag names" $ concatMap flagNames $ modeFlags x]
    where
        checkGroup :: Group a -> Maybe String
        checkGroup x = msum
            [check "Empty group name" $ all (not . null . fst) $ groupNamed x
            ,check "Empty group contents" $ all (not . null . snd) $ groupNamed x]

        checkNames :: String -> [Name] -> Maybe String
        checkNames msg xs = check "Empty names" (all (not . null) xs) `mplus` do
            bad <- listToMaybe $ xs \\ nub xs
            let dupe = filter (== bad) xs
            return $ "Sanity check failed, multiple " ++ msg ++ ": " ++ unwords (map show dupe)

        check :: String -> Bool -> Maybe String
        check msg True = Nothing
        check msg False = Just msg


---------------------------------------------------------------------
-- REMAP

class Remap m where
    remap :: (a -> b) -- ^ Embed a value
          -> (b -> (a, a -> b)) -- ^ Extract the mode and give a way of re-embedding
          -> m a -> m b

remap2 :: Remap m => (a -> b) -> (b -> a) -> m a -> m b
remap2 f g = remap f (\x -> (g x, f))

instance Remap Mode where
    remap f g x = x
        {modeGroupModes = fmap (remap f g) $ modeGroupModes x
        ,modeValue = f $ modeValue x
        ,modeCheck = \v -> let (a,b) = g v in fmap b $ modeCheck x a
        ,modeReform = modeReform x . fst . g
        ,modeArgs = (fmap (remap f g) *** fmap (remap f g)) $ modeArgs x
        ,modeGroupFlags = fmap (remap f g) $ modeGroupFlags x}

instance Remap Flag where
    remap f g x = x{flagValue = remapUpdate f g $ flagValue x}

instance Remap Arg where
    remap f g x = x{argValue = remapUpdate f g $ argValue x}

remapUpdate f g upd = \s v -> let (a,b) = g v in fmap b $ upd s a


---------------------------------------------------------------------
-- MODE/MODES CREATORS

-- | Create an empty mode specifying only 'modeValue'. All other fields will usually be populated
--   using record updates.
modeEmpty :: a -> Mode a
modeEmpty x = Mode mempty [] x Right (const Nothing) True "" [] ([],Nothing) mempty

-- | Create a mode with a name, an initial value, some help text, a way of processing arguments
--   and a list of flags.
mode :: Name -> a -> Help -> Arg a -> [Flag a] -> Mode a
mode name value help arg flags = (modeEmpty value){modeNames=[name], modeHelp=help, modeArgs=([],Just arg), modeGroupFlags=toGroup flags}

-- | Create a list of modes, with a program name, an initial value, some help text and the child modes.
modes :: String -> a -> Help -> [Mode a] -> Mode a
modes name value help xs = (modeEmpty value){modeNames=[name], modeHelp=help, modeGroupModes=toGroup xs}


---------------------------------------------------------------------
-- FLAG CREATORS

-- | Create a flag taking no argument value, with a list of flag names, an update function
--   and some help text.
flagNone :: [Name] -> (a -> a) -> Help -> Flag a
flagNone names f help = Flag names FlagNone upd "" help
    where upd _ x = Right $ f x

-- | Create a flag taking an optional argument value, with an optional value, a list of flag names,
--   an update function, the type of the argument and some help text.
flagOpt :: String -> [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagOpt def names upd typ help = Flag names (FlagOpt def) upd typ help

-- | Create a flag taking a required argument value, with a list of flag names,
--   an update function, the type of the argument and some help text.
flagReq :: [Name] -> Update a -> FlagHelp -> Help -> Flag a
flagReq names upd typ help = Flag names FlagReq upd typ help

-- | Create an argument flag, with an update function and the type of the argument.
flagArg :: Update a -> FlagHelp -> Arg a
flagArg upd typ = Arg upd typ False

-- | Create a boolean flag, with a list of flag names, an update function and some help text.
flagBool :: [Name] -> (Bool -> a -> a) -> Help -> Flag a
flagBool names f help = Flag names (FlagOptRare "") upd "" help
    where
        upd s x = case if s == "" then Just True else parseBool s of
            Just b -> Right $ f b x
            Nothing -> Left "expected boolean value (true/false)"
