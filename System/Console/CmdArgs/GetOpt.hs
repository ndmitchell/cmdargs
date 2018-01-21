{-# LANGUAGE DeriveFunctor #-}

-- | This provides a compatiblity wrapper to the @System.Console.GetOpt@ module in @base@.
--   That module is essentially a Haskell port of the GNU @getopt@ library.
--
--   /Changes:/ The changes from @GetOpt@ are listed in the documentation for each function.
module System.Console.CmdArgs.GetOpt(
    convert, getOpt, getOpt', usageInfo,
    ArgOrder(..), OptDescr(..), ArgDescr(..)
    ) where

import System.Console.CmdArgs.Explicit


-- | What to do with options following non-options.
--
--   /Changes:/ Only 'Permute' is allowed, both @RequireOrder@ and @ReturnInOrder@
--   have been removed.
data ArgOrder a = Permute


-- | Each 'OptDescr' describes a single option/flag.
--
--   The arguments to 'Option' are:
--
--   * list of short option characters
--
--   * list of long option strings (without @\"--\"@, may not be 1 character long)
--
--   * argument descriptor
--
--   * explanation of option for userdata
data OptDescr a = Option
    [Char]
    [String]
    (ArgDescr a)
    String
    deriving (Functor)


-- | Describes whether an option takes an argument or not, and if so
--   how the argument is injected into a value of type @a@.
data ArgDescr a
   = NoArg                   a         -- ^ no argument expected
   | ReqArg (String       -> a) String -- ^ option requires argument
   | OptArg (Maybe String -> a) String -- ^ optional argument
    deriving (Functor)


-- | Return a string describing the usage of a command, derived from
--   the header (first argument) and the options described by the 
--   second argument.
usageInfo :: String -> [OptDescr a] -> String
usageInfo desc flags = unlines $ desc : drop 2 (lines $ show $ convert "" flags)


-- | Process the command-line, and return the list of values that matched
--   (and those that didn\'t). The arguments are:
--
--   * The order requirements (see 'ArgOrder')
--
--   * The option descriptions (see 'OptDescr')
--
--   * The actual command line arguments (presumably got from 
--     'System.Environment.getArgs').
--
--   'getOpt' returns a triple consisting of the option arguments, a list
--   of non-options, and a list of error messages.
--
--   /Changes:/ The list of errors will contain at most one entry, and if an
--   error is present then the other two lists will be empty.
getOpt  :: ArgOrder  a -> [OptDescr  a] -> [String] -> ([a], [String], [String])
getOpt _ flags args =
    case process (convert "" flags) args of
        Left x -> ([],[],[x])
        Right (a,b) -> (a,b,[])


-- | /Changes:/ This is exactly the same as 'getOpt', but the 3rd element of the
--   tuple (second last) will be an empty list.
getOpt' :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String], [String])
getOpt' x y z = (a,b,[],c)
    where (a,b,c) = getOpt x y z


-- | Given a help text and a list of option descriptions, generate a 'Mode'.
convert :: String -> [OptDescr a] -> Mode ([a],[String])
convert help flags = mode "program" ([],[]) help args (map f flags)
    where
        args = flagArg (\x (a,b) -> Right (a,b++[x])) "ARG"

        f (Option short long x help) = case x of
            NoArg x -> flagNone names (\(a,b) -> (a++[x],b)) help
            ReqArg op x -> flagReq names (\x (a,b) -> Right (a++[op x],b)) x help
            OptArg op x -> flagOpt "" names (\x (a,b) -> Right (a++[op $ if null x then Nothing else Just x],b)) x help
            where names = map return short ++ long
