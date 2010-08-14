{-# LANGUAGE RecordWildCards, ViewPatterns, PatternGuards, DeriveDataTypeable #-}

-- | This module takes the result of Structure, and traslates it to
--   the CmdArgs.Explicit format.
module System.Console.CmdArgs.Implicit.Step3(
    step3,
    -- cmdArgs_privateArgsSeen is exported, otherwise Haddock
    -- gets confused when using RecordWildCards
    CmdArgs(..), cmdArgsHasValue
    ) where

import System.Console.CmdArgs.Implicit.Step2
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.Console.CmdArgs.Text

import Control.Arrow
import Data.Data
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Function


-- | A structure to store the additional data relating to @--help@,
--   @--version@, @--quiet@ and @--verbose@.
data CmdArgs a = CmdArgs
    {cmdArgsValue :: a -- ^ The underlying value being wrapped.
    ,cmdArgsHelp :: Maybe String -- ^ @Just@ if @--help@ is given, then gives the help message for display.
    ,cmdArgsVersion :: Maybe String -- ^ @Just@ if @--verion@ is given, then gives the version message for display.
    ,cmdArgsVerbosity :: Maybe Verbosity -- ^ @Just@ if @--quiet@ or @--verbose@ is given, then gives the verbosity to use.
    ,cmdArgsPrivate :: CmdArgsPrivate -- ^ Private: Only exported due to Haddock limitations.
    }
    deriving (Show,Data,Typeable)

instance Functor CmdArgs where
    fmap f x = x{cmdArgsValue = f $ cmdArgsValue x}

cmdArgsHasValue :: CmdArgs a -> Bool
cmdArgsHasValue x = isNothing (cmdArgsHelp x) && isNothing (cmdArgsVersion x)

data CmdArgsPrivate = CmdArgsPrivate
    Int -- ^ The number of arguments that have been seen
    deriving (Data,Typeable)

instance Show CmdArgsPrivate where show _ = "CmdArgsPrivate"

incArgsSeen x = x{cmdArgsPrivate = CmdArgsPrivate $ getArgsSeen x + 1}
getArgsSeen CmdArgs{cmdArgsPrivate = CmdArgsPrivate i} = i


step3 :: Prog2 a -> Mode (CmdArgs a)
step3 p = common p $ transProg $ liftProg p


---------------------------------------------------------------------
-- COMMON
-- Add common flags (--help/--version etc)

common :: Prog2 a -> Mode (CmdArgs a) -> Mode (CmdArgs a)
common p m
    | null $ modeModes m = addNormal m $ commonFlags p $ addNormal m
    | otherwise = addCommon m2 $ commonFlags p $ addCommon m2
    where
        add m xs = m{modeGroupFlags = xs `mappend` modeGroupFlags m}
        addNormal m xs = add m $ toGroup xs
        addCommon m xs = add m $ Group [] [] [("Common flags",xs)]
        addHidden m xs = add m $ Group [] xs []

        m2 = m{modeGroupModes = fmap f $ modeGroupModes m}
        f m = addHidden m $ commonFlags p $ addCommon $ m{modeNames = map ((prog2Name p ++ " ") ++) $ modeNames m}


-- add common flags to a mode
commonFlags :: Prog2 a -> ([Flag (CmdArgs a)] -> Mode (CmdArgs a)) -> [Flag (CmdArgs a)]
commonFlags Prog2{..} add = flags
    where
        help hlp txt = showText txt $ Line prog2Summary : Line "" : helpText hlp (add flags)
        flags = 
            [flagHelpFormat $ \hlp txt x -> x{cmdArgsHelp = Just $ help hlp txt}
            ,flagVersion $ \x -> x{cmdArgsVersion = Just prog2Summary}] ++
            if not prog2Verbosity then [] else flagsVerbosity $ \v x -> x{cmdArgsVerbosity=Just v}


---------------------------------------------------------------------
-- TRANSLATE
-- Translate in to the CmdArgs.Explicit domain

transProg :: Prog2 (CmdArgs a) -> Mode (CmdArgs a)
transProg p = res{modeNames = [prog2Name p]}
    where
        res = if length ys == 1 then snd $ head ys else defMode{modeGroupModes = toGroups ys, modeHelp = prog2Help p}
        defMode = maybe zeroMode (silentMode . snd . (ys!!)) $ prog2ModeDefault p
        silentMode m = m{modeGroupFlags=Group [] (modeFlags m) [], modeArgs=fmap (\x -> x{argType=""}) (modeArgs m)}
        ys = zip (map mode2Group $ prog2Modes p) $
                 zipWith transMode (map ((==) (prog2ModeDefault p) . Just) [0..]) $ prog2Modes p

        zeroMode = Mode (toGroup []) [] (embed $ error msg) chk "" [] Nothing $ toGroup []
            where msg = "System.Console.CmdArgs.Implicit: No default mode given (see cmdArgsHelp/cmdArgsVersion)"
                  chk x = if cmdArgsHasValue x then Left "No mode given and no default mode" else Right x


transMode :: Bool -> Mode2 (CmdArgs a) -> Mode (CmdArgs a)
transMode auto Mode2{..} = transArgs mode2Args $ Mode
    (toGroup [])
    (["[" ++ head mode2Names ++ "]" | auto] ++ mode2Names)
    mode2Value
    Right
    mode2Help
    mode2Suffix
    Nothing
    (toGroups $ map (flag2Group &&& transFlag) mode2Flags)


toGroups :: [(String,a)] -> Group a
toGroups xs = Group (f "") [] (map (id &&& f) names)
    where names = filter (not . null) $ nub $ map fst xs
          f x = map snd $ filter ((==) x . fst) xs


transFlag :: Flag2 (CmdArgs a) -> Flag (CmdArgs a)
transFlag Flag2{..} = case flag2Upd of
    Flag2String upd -> (maybe flagReq flagOpt flag2Opt) flag2Names upd flag2FlagHelp flag2Help
    Flag2Bool upd -> flagBool flag2Names upd flag2Help
    Flag2Value upd -> flagNone flag2Names upd flag2Help


transArgs :: [Arg2 (CmdArgs a)] -> Mode (CmdArgs a) -> Mode (CmdArgs a)
transArgs [] x = x
transArgs xs x = x{modeCheck=chk, modeArgs = Just $ flagArg upd hlp}
    where
        (ord,rep) = orderArgs xs
        mn = length $ dropWhile (isJust . arg2Opt) $ reverse ord

        chk v | not $ cmdArgsHasValue v = Right v
              | n < mn = Left $ "Requires at least " ++ show mn ++ " arguments, got " ++ show n
              | otherwise = foldl f (addOptArgs n v) (drop n ord)
            where n = getArgsSeen v
                  f (Right v) arg = arg2Upd arg (fromJust $ arg2Opt arg) v
                  f x _ = x

        -- if we have repeating args which is also opt, translate that here
        addOptArgs n v
            | Just x <- rep, Just o <- arg2Opt x, Just n <= findIndex (isNothing . arg2Pos) (ord ++ [x]) = arg2Upd x o v
            | otherwise = Right v

        hlp = unwords $ a ++ map (\x -> "["++x++"]") b
            where (a,b) = splitAt mn $ map arg2FlagHelp $ ord ++ maybeToList rep

        upd s v | n < length ord = arg2Upd (ord !! n) s v2
                | Just x <- rep = arg2Upd x s v2
                | otherwise = Left $ "expected at most " ++ show (length ord)
            where n = getArgsSeen v
                  v2 = incArgsSeen v


-- return the arguments in order, plus those at the end
orderArgs :: [Arg2 a] -> ([Arg2 a], Maybe (Arg2 a))
orderArgs args = (f 0 ord, listToMaybe rep)
    where
        (rep,ord) = span (isNothing . arg2Pos) $ sortBy (compare `on` arg2Pos) args
        f i [] = []
        f i (x:xs) = case fromJust (arg2Pos x) `compare` i of
            LT -> f i xs
            EQ -> x : f (i+1) xs
            GT -> take 1 rep ++ f (i+1) (x:xs)


---------------------------------------------------------------------
-- LIFT
-- Add the CmdArgs structure

embed x = CmdArgs x Nothing Nothing Nothing $ CmdArgsPrivate 0
proj x = (cmdArgsValue x, \y -> x{cmdArgsValue=y})

liftProg :: Prog2 a -> Prog2 (CmdArgs a)
liftProg x = x{prog2Modes = map liftMode $ prog2Modes x}

liftMode :: Mode2 a -> Mode2 (CmdArgs a)
liftMode x = x
    {mode2Value = embed $ mode2Value x
    ,mode2Flags = map liftFlag $ mode2Flags x
    ,mode2Args = map liftArg $ mode2Args x}

liftFlag x = x{flag2Upd = liftType $ flag2Upd x}
liftArg x = x{arg2Upd = fromFlag2String $ liftType $ Flag2String $ arg2Upd x}
liftType (Flag2String upd) = Flag2String $ \s v -> let (a,b) = proj v in fmap b $ upd s a
liftType (Flag2Bool upd) = Flag2Bool $ \s v -> let (a,b) = proj v in b $ upd s a
liftType (Flag2Value upd) = Flag2Value $ \v -> let (a,b) = proj v in b $ upd a
