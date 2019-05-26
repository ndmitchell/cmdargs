{-# LANGUAGE PatternGuards, RecordWildCards #-}

module System.Console.CmdArgs.Implicit.Global(global) where

import System.Console.CmdArgs.Implicit.Local
import System.Console.CmdArgs.Implicit.Reform
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.Generics.Any
import Data.List
import Data.Maybe


global :: Prog_ -> Mode (CmdArgs Any)
global x = setReform (reform y) $ setHelp y $ setProgOpts x $ collapse $ assignGroups y
    where y = assignNames $ extraFlags x


setProgOpts :: Prog_ -> Mode a -> Mode a
setProgOpts p m = m{modeExpandAt = not $ progNoAtExpand p
                   ,modeGroupModes = fmap (setProgOpts p) $ modeGroupModes m}


---------------------------------------------------------------------
-- COLLAPSE THE FLAGS/MODES UPWARDS

collapse :: Prog_ -> Mode (CmdArgs Any)
collapse x | length ms == 1 = (snd $ head ms){modeNames=[progProgram x]}
           | length auto > 1 = err "prog" "Multiple automatic modes"
           | otherwise = (head $ map zeroMode auto ++ map (emptyMode . snd) ms)
                {modeNames=[progProgram x], modeGroupModes=grouped, modeHelp=progHelp x}
    where
        grouped = Group (pick Nothing) [] [(g, pick $ Just g) | g <- nub $ mapMaybe (modeGroup . fst) ms]
        pick x = [m | (m_,m) <- ms, modeGroup m_ == x]

        ms = map (id &&& collapseMode) $ progModes x
        auto = [m | (m_,m) <- ms, modeDefault m_]


-- | A mode devoid of all it's contents
emptyMode :: Mode (CmdArgs Any) -> Mode (CmdArgs Any)
emptyMode x = x
    {modeCheck = \x -> if cmdArgsHasValue x then Left "No mode given and no default mode" else Right x
    ,modeGroupFlags = groupUncommonDelete $ modeGroupFlags x
    ,modeArgs=([],Nothing), modeHelpSuffix=[]}

-- | A mode whose help hides all it's contents
zeroMode :: Mode (CmdArgs Any) -> Mode (CmdArgs Any)
zeroMode x = x
    {modeGroupFlags = groupUncommonHide $ modeGroupFlags x
    ,modeArgs = let zeroArg x = x{argType=""} in map zeroArg *** fmap zeroArg $ modeArgs x
    ,modeHelpSuffix=[]}


collapseMode :: Mode_ -> Mode (CmdArgs Any)
collapseMode x =
    applyFixups (map flagFixup $ modeFlags_ x) $
    collapseArgs [x | x@Arg_{} <- modeFlags_ x] $
    collapseFlags [x | x@Flag_{} <- modeFlags_ x] $
    modeMode x


applyFixups :: [Fixup] -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
applyFixups xs m = m{modeCheck = either Left (Right . fmap fix) . modeCheck m}
    where fix a = foldr ($) a [x | Fixup x <- xs]


collapseFlags :: [Flag_] -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
collapseFlags xs x = x{modeGroupFlags = Group (pick Nothing) [] [(g, pick $ Just g) | g <- groups]}
    where
        pick x = map flagFlag $ filter ((==) x . flagGroup) xs
        groups = nub $ mapMaybe flagGroup xs


collapseArgs :: [Flag_] -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
collapseArgs [] x = x
collapseArgs xs x = x{modeCheck=chk, modeArgs = ([], Just $ flagArg upd hlp)}
    where
        argUpd = argValue . flagArg_

        (ord,rep) = orderArgs xs
        mn = length $ dropWhile (isJust . flagArgOpt) $ reverse ord

        chk v | not $ cmdArgsHasValue v = Right v
              | n < mn = Left $ "Requires at least " ++ show mn ++ " arguments, got " ++ show n
              | otherwise = foldl f (addOptArgs n v) (drop n ord)
            where n = getArgsSeen v
                  f (Right v) arg = argUpd arg (fromJust $ flagArgOpt arg) v
                  f x _ = x

        -- if we have repeating args which is also opt, translate that here
        addOptArgs n v
            | Just x <- rep, Just o <- flagArgOpt x, Just n <= findIndex (isNothing . flagArgPos) (ord ++ [x]) = argUpd x o v
            | otherwise = Right v

        hlp = unwords $ a ++ map (\x -> "["++x++"]") b
            where (a,b) = splitAt mn $ map (argType . flagArg_) $ ord ++ maybeToList rep

        upd s v | n < length ord = argUpd (ord !! n) s v2
                | Just x <- rep = argUpd x s v2
                | otherwise = Left $ "expected at most " ++ show (length ord)
            where n = getArgsSeen v
                  v2 = incArgsSeen v


-- return the arguments in order, plus those at the end
orderArgs :: [Flag_] -> ([Flag_], Maybe Flag_)
orderArgs args = (f 0 ord, listToMaybe rep)
    where
        (rep,ord) = span (isNothing . flagArgPos) $ sortBy (compare `on` flagArgPos) args
        f i [] = []
        f i (x:xs) = case fromJust (flagArgPos x) `compare` i of
            LT -> f i xs
            EQ -> x : f (i+1) xs
            GT -> take 1 rep ++ f (i+1) (x:xs)


---------------------------------------------------------------------
-- DEAL WITH GROUPS

assignGroups :: Prog_ -> Prog_
assignGroups p = assignCommon $ p{progModes = map (\m -> m{modeFlags_ = f Nothing $ modeFlags_ m}) $ progModes p}
    where
        f grp [] = []
        f grp (x@Flag_{}:xs) = x{flagGroup=grp2} : f grp2 xs
            where grp2 = flagGroup x `mplus` grp
        f grp (x:xs) = x : f grp xs


assignCommon :: Prog_ -> Prog_
assignCommon p =
    p{progModes = [m{modeFlags_ =
        [if isFlag_ f && show (flagFlag f) `elem` com then f{flagGroup = Just commonGroup} else f | f <- modeFlags_ m]}
    | m <- progModes p]}
    where
        com = map head $ filter ((== length (progModes p)) . length) $ group $ sort
              [show $ flagFlag f | m <- progModes p, f@Flag_{flagGroup=Nothing} <- modeFlags_ m]


commonGroup = "Common flags"

groupSplitCommon :: Group a -> ([a], Group a)
groupSplitCommon (Group unnamed hidden named) = (concatMap snd com, Group unnamed hidden uni)
    where (com,uni) = partition ((==) commonGroup . fst) named

groupCommonHide x = let (a,b) = groupSplitCommon x in b{groupHidden = groupHidden b ++ a}
groupUncommonHide x = let (a,b) = groupSplitCommon x in Group [] (fromGroup b) [(commonGroup,a) | not $ null a]
groupUncommonDelete x = let a = fst $ groupSplitCommon x in Group [] [] [(commonGroup,a) | not $ null a]


---------------------------------------------------------------------
-- ADD EXTRA PIECES

extraFlags :: Prog_ -> Prog_
extraFlags p = p{progModes = map f $ progModes p}
    where f m = m{modeFlags_ = modeFlags_ m ++ flags}
          grp = if length (progModes p) > 1 then Just commonGroup else Nothing
          wrap x = def{flagFlag=x, flagExplicit=True, flagGroup=grp}
          flags = changeBuiltin_ (progHelpArg p) (wrap $ flagHelpFormat $ error "flagHelpFormat undefined") ++
                  changeBuiltin_ (progVersionArg p) (wrap $ flagVersion vers) ++
                  [wrap $ flagNumericVersion $ \x -> x{cmdArgsVersion = Just $ unlines v}
                        | Just v <- [progNumericVersionOutput p]] ++
                  changeBuiltin_ (fst $ progVerbosityArgs p) (wrap loud) ++
                  changeBuiltin_ (snd $ progVerbosityArgs p) (wrap quiet)
          [loud,quiet] = flagsVerbosity verb
          vers x = x{cmdArgsVersion = Just $ unlines $ progVersionOutput p}
          verb v x = x{cmdArgsVerbosity = Just v}


changeBuiltin :: Maybe Builtin_ -> Flag a -> [Flag a]
changeBuiltin Nothing _ = []
changeBuiltin (Just Builtin_{..}) x = [x
    {flagNames = builtinNames ++ if builtinExplicit then [] else flagNames x
    ,flagHelp = fromMaybe (flagHelp x) builtinHelp}]

changeBuiltin_ :: Maybe Builtin_ -> Flag_ -> [Flag_]
changeBuiltin_ Nothing _ = []
changeBuiltin_ (Just b) x = [x{flagFlag=y, flagGroup = builtinGroup b `mplus` flagGroup x}
    | y <- changeBuiltin (Just b) $ flagFlag x]


setHelp :: Prog_ -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
setHelp p = mapModes0 add ""
    where
        mapModes0 f pre m = f pre $ mapModes1 f pre m
        mapModes1 f pre m = m{modeGroupModes = fmap (mapModes0 f (pre ++ head (modeNames m) ++ " ")) $ modeGroupModes m}

        add pre m = changeHelp p m $ \hlp txt x -> x{cmdArgsHelp=Just $ showText txt $ msg hlp}
            where msg hlp = helpText (progHelpOutput p) hlp (prepare m{modeNames = map (pre++) $ modeNames m})

        prepare = mapModes1 (\_ m -> m{modeGroupFlags = groupCommonHide $ modeGroupFlags m}) ""


changeHelp :: Prog_ -> Mode a -> (HelpFormat -> TextFormat -> a -> a) -> Mode a
changeHelp p m upd = m{modeGroupFlags = fmap f $ modeGroupFlags m}
    where hlp = changeBuiltin (progHelpArg p) $ flagHelpFormat upd
          f flg = if concatMap flagNames hlp == flagNames flg then head hlp else flg


setReform :: (a -> Maybe [String]) -> Mode a -> Mode a
setReform f m = m{modeReform = f, modeGroupModes = fmap (setReform f) $ modeGroupModes m}


---------------------------------------------------------------------
-- ASSIGN NAMES

assignNames :: Prog_ -> Prog_
assignNames x = x{progModes = map f $ namesOn fromMode toMode $ progModes x}
    where
        fromMode x = Names (modeNames $ modeMode x) [asName $ ctor $ cmdArgsValue $ modeValue $ modeMode x | not $ modeExplicit x]
        toMode xs x = x{modeMode = (modeMode x){modeNames=["["++head xs++"]" | modeDefault x] ++ xs}}

        fromFlagLong x = Names (flagNames $ flagFlag x) [asName $ fromMaybe (flagField x) (flagEnum x) | not $ flagExplicit x]
        fromFlagShort x = Names ns $ nub [take 1 s | not $ flagExplicit x, all ((/=) 1 . length) ns, s <- ns]
            where ns = flagNames $ flagFlag x
        toFlag xs x = x{flagFlag = (flagFlag x){flagNames=xs}}

        f x = x{modeFlags_ = rest ++ namesOn fromFlagShort toFlag (namesOn fromFlagLong toFlag flgs)}
            where (flgs,rest) = partition isFlag_ $ modeFlags_ x

        isFlag_ Flag_{} = True
        isFlag_ _ = False


asName s = map (\x -> if x == '_' then '-' else toLower x) $ if last s == '_' then init s else s

-- have are already assigned, want are a list of ones I might want
data Names = Names {have :: [String], want :: [String]}

-- error out if any name is by multiple have's, or one item would get no names
names :: [Names] -> [[String]]
names xs | not $ null bad = err "repeated names" $ unwords bad
    where bad = duplicates $ concatMap have xs

names xs | any null res = err "no available name" "?"
         | otherwise = res
    where
        bad = concatMap have xs ++ duplicates (concatMap want xs)
        res = map (\x -> have x ++ (want x \\ bad)) xs


duplicates :: Eq a => [a] -> [a]
duplicates xs = nub $ xs \\ nub xs


namesOn :: (a -> Names) -> ([String] -> a -> a) -> [a] -> [a]
namesOn f g xs = zipWith g (names $ map f xs) xs
