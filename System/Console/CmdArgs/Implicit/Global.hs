{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Implicit.Global(global) where

import System.Console.CmdArgs.Implicit.Local
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text
import System.Console.CmdArgs.Default

import Control.Monad
import Data.Char
import Data.Function
import Data.Generics.Any
import Data.List
import Data.Maybe


global :: Prog_ -> Mode (CmdArgs Any)
global x = setHelp x $ collapse $ assignGroups $ assignNames $ extraFlags x


---------------------------------------------------------------------
-- COLLAPSE THE FLAGS/MODES UPWARDS

collapse :: Prog_ -> Mode (CmdArgs Any)
collapse x | length ms == 1 = (head ms){modeNames=[progProgram x]}
           | length auto > 1 = err "prog" "Multiple automatic modes"
           | otherwise = (head $ map zeroMode auto ++ map emptyMode ms)
                {modeNames=[progProgram x], modeGroupModes = toGroup ms, modeHelp = progHelp x}
    where
        ms = map collapseMode $ progModes x
        auto = [m | (m,True) <- zip ms $ map modeDefault $ progModes x]


-- | A mode devoid of all it's contents
emptyMode :: Mode (CmdArgs Any) -> Mode (CmdArgs Any)
emptyMode x = x
    {modeCheck = \x -> if cmdArgsHasValue x then Left "No mode given and no default mode" else Right x
    ,modeGroupFlags = groupUncommonDelete $ modeGroupFlags x
    ,modeArgs=Nothing, modeHelpSuffix=[]}

-- | A mode whose help hides all it's contents
zeroMode :: Mode (CmdArgs Any) -> Mode (CmdArgs Any)
zeroMode x = x
    {modeGroupFlags = groupUncommonHide $ modeGroupFlags x
    ,modeArgs=fmap (\x -> x{argType=""}) $ modeArgs x
    ,modeHelpSuffix=[]}


collapseMode :: Mode_ -> Mode (CmdArgs Any)
collapseMode x =
    collapseArgs [x | x@Arg_{} <- modeFlags_ x] $
    collapseFlags [x | x@Flag_{} <- modeFlags_ x] $
    modeMode x


collapseFlags :: [Flag_] -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
collapseFlags xs x = x{modeGroupFlags = Group (pick Nothing) [] [(g, pick $ Just g) | g <- groups]}
    where
        pick x = map flagFlag $ filter ((==) x . flagGroup) xs
        groups = nub $ mapMaybe flagGroup xs


collapseArgs :: [Flag_] -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
collapseArgs [] x = x
collapseArgs xs x = x{modeCheck=chk, modeArgs = Just $ flagArg upd hlp}
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


incArgsSeen x = x{cmdArgsPrivate = CmdArgsPrivate $ getArgsSeen x + 1}
getArgsSeen CmdArgs{cmdArgsPrivate = CmdArgsPrivate i} = i


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
    where f m = m{modeFlags_ = modeFlags_ m ++ map (\x -> def{flagFlag=x, flagExplicit=True, flagGroup=grp}) flags}
          grp = if length (progModes p) > 1 then Just commonGroup else Nothing
          flags = flagHelpFormat undefined : flagVersion vers : if progVerbosity p then flagsVerbosity verb else []
          vers x = x{cmdArgsVersion = Just $ progSumm p}
          verb v x = x{cmdArgsVerbosity = Just v}


setHelp :: Prog_ -> Mode (CmdArgs Any) -> Mode (CmdArgs Any)
setHelp p = mapModes0 add ""
    where
        mapModes0 f pre m = f pre $ mapModes1 f pre m
        mapModes1 f pre m = m{modeGroupModes = fmap (mapModes0 f (pre ++ head (modeNames m) ++ " ")) $ modeGroupModes m}

        add pre m = changeHelp m $ \hlp txt x -> x{cmdArgsHelp=Just $ showText txt $ msg hlp}
            where msg hlp = Line (progSumm p) : Line "" : helpText hlp (prepare m{modeNames = map (pre++) $ modeNames m})

        prepare = mapModes1 (\_ m -> m{modeGroupFlags = groupCommonHide $ modeGroupFlags m}) ""


changeHelp :: Mode a -> (HelpFormat -> TextFormat -> a -> a) -> Mode a
changeHelp m upd = m{modeGroupFlags = fmap f $ modeGroupFlags m}
    where hlp = flagHelpFormat upd
          f flg = if flagNames hlp == flagNames flg then hlp else flg


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
