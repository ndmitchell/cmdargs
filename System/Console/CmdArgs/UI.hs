{-# LANGUAGE PatternGuards #-}
{-|
    This module describes the attributes that can be specified on flags and modes.

    Many attributes have examples specified on the following data type:

    > data Sample = Sample
    >    {str :: String
    >    ,strs :: [String]}
-}

module System.Console.CmdArgs.UI(
    -- ** Attribute mechanism
    mode, Mode, (&=), (&), Info,
    -- ** Flag attributes
    text, args, argPos, typ, typFile, typDir, empty, flag, unknownFlags, enum, explicit,
    -- ** Mode attributes
    prog, helpSuffix, defMode
    ) where

import System.Console.CmdArgs.Type
import System.IO.Unsafe
import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.IORef
import Control.Exception
import Data.Char
import Control.Monad.State
import Data.Function


infix 1 &=
infixl 2 &

---------------------------------------------------------------------
-- STATE MANAGEMENT

{-# NOINLINE info #-}
info :: IORef [Info]
info = unsafePerformIO $ newIORef []

-- | Add attributes to a value.
--
-- > value &= attrib1 & attrib2
(&=) :: a -> [Info] -> a
(&=) x is = unsafePerformIO $ do
    writeIORef info is
    return x

-- | Add two pieces of information together
(&) :: [Info] -> [Info] -> [Info]
(&) = (++)

collect :: a -> IO [Info]
collect x = do
    evaluate x
    x <- readIORef info
    writeIORef info [] -- don't leak the info's
    return x

-- | Safely encapsulate a value with attributes.
mode :: Data a => a -> Mode a
mode val = unsafePerformIO $ do
    info <- collect val
    let con = toConstr val
        name = map toLower $ showConstr con
    ref <- newIORef $ constrFields con
    flags <- liftM concat $ sequence $ flip gmapQ val $ \i -> do
        info <- collect i
        n:ns <- readIORef ref
        writeIORef ref ns
        case toFlagType $ typeOf i of
            _ | [FldEnum xs] <- info -> return [x{flagName=n} | x <- xs]
            Nothing -> error $ "Can't handle a type of " ++ show (typeOf i)
            Just x -> return [flagInfo flagDefault{flagName=n,flagKey=n,flagVal=toDyn i,flagType=x} info]
    return $ modeInfo modeDefault{modeVal=val,modeName=name,modeFlags=flags} info


---------------------------------------------------------------------
-- INFO ITEMS

data Info
    = FldEmpty String
    | FldArgs
    | FldArgPos Int
    | FldTyp String
    | Text String
    | FldFlag String
    | FldExplicit
    | HelpSuffix [String]
    | FldUnknown
    | FldEnum [Flag]
    | ModDefault
    | ModProg String
      deriving Show


modeInfo :: Mode a -> [Info] -> Mode a
modeInfo = foldl $ \m x -> case x of
    Text x -> m{modeText=x}
    HelpSuffix x -> m{modeHelpSuffix=x}
    ModDefault -> m{modeDef=True}
    ModProg x -> m{modeProg=Just x}
    x -> error $ "Invalid info at mode level: " ++ show x


flagInfo :: Flag -> [Info] -> Flag
flagInfo = foldl $ \m x -> case x of
    Text x -> m{flagText=x}
    FldExplicit -> m{flagExplicit=True}
    FldTyp x -> m{flagTyp=x}
    FldEmpty x -> m{flagOpt=Just x}
    FldFlag x -> m{flagFlag=x:flagFlag m}
    FldArgs -> m{flagArgs=Just Nothing}
    FldArgPos i -> m{flagArgs=Just (Just i)}
    FldUnknown -> m{flagUnknown=True}
    x -> error $ "Invalid info at argument level: " ++ show x


---------------------------------------------------------------------
-- USER INTERFACE

-- | Flag: Make the value of a flag optional, using the supplied
--   value if none is given.
--
-- > {str = def &= empty "foo"}
-- >   -s --str[=VALUE]    (default=foo)
empty :: (Show a, Typeable a) => a -> [Info]
empty x = return $ case cast x of
    Just y -> FldEmpty y
    _ -> FldEmpty $ show x

-- | Flag: The the type of a flag's value, usually upper case. Only
--   used for the help message.
--
-- > {str = def &= typ "FOO"}
-- >   -s --str=FOO
typ :: String -> [Info]
typ = return . FldTyp

-- | Flag/Mode: Descriptive text used in the help output.
--
-- > {str = def &= text "Help message"}
-- >   -s --str=VALUE      Help message
text :: String -> [Info]
text = return . Text

-- | Flag: Add flags which trigger this option.
--
-- > {str = def &= flag "foo"}
-- >   -s --str --foo=VALUE
flag :: String -> [Info]
flag = return . FldFlag

-- | Flag: This field should be used to store the non-flag arguments. Can
--   only be applied to fields of type @[String]@.
--
-- > {strs = def &= args}
args :: [Info]
args = [FldArgs]

-- | Flag: This field should be used to store a particular argument position
--   (0-based).
--
-- > {str = def &= argPos 0}
argPos :: Int -> [Info]
argPos = return . FldArgPos


-- | Flag: Alias for @typ \"FILE\"@.
typFile :: [Info]
typFile = typ "FILE"

-- | Flag: Alias for @typ \"DIR\"@.
typDir :: [Info]
typDir = typ "DIR"


-- | Mode: Suffix to be added to the help message.
helpSuffix :: [String] -> [Info]
helpSuffix = return . HelpSuffix

-- | Flag: This field should be used to store all unknown flag arguments.
--   If no @unknownFlags@ field is set, unknown flags raise errors.
--   Can only be applied to fields of tyep @[String]@.
--
-- > {strs = def &= unknownFlags}
unknownFlags :: [Info]
unknownFlags = [FldUnknown]

-- | Mode: This mode is the default mode, if no mode is specified then
--   this mode is active. If there is no default mode and no mode is given
--   then an error is raised.
defMode :: [Info]
defMode = [ModDefault]

-- | Mode: This is the name of the program running, used to override the result
--   from @getProgName@.
prog :: String -> [Info]
prog = return . ModProg

-- | Flag: A field is an enumeration of possible values.
--
-- > data Choice = Yes | No deriving (Data,Typeable,Show,Eq)
-- > data Sample = Sample {choice :: Choice}
-- > {choice = Yes & enum [Yes &= "say yes", No &= "say no"]}
--
-- >   -y --yes    say yes (default)
-- >   -n --no     say nos
enum :: (Typeable a, Eq a, Show a) => a -> [a] -> a
enum def xs = unsafePerformIO $ do
    ys <- forM xs $ \x -> do
        y <- collect x
        return $ flagInfo flagDefault{flagKey=map toLower (show x), flagType=FlagBool (toDyn x), flagVal = toDyn False} y
    return $ def &= [FldEnum ys]

-- | Flag: A field should not have any flag names guessed for it.
--   All flags must be specified by 'flag'.
explicit :: [Info]
explicit = [FldExplicit]
