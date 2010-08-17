{-# LANGUAGE RecordWildCards, ViewPatterns, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- | This module takes the result of Structure, and traslates it to
--   the CmdArgs.Explicit format.
module System.Console.CmdArgs.Implicit.Step1(step1, Prog1(..), Mode1(..), Flag1(..)) where

import System.Console.CmdArgs.Implicit.Ann
import Data.Generics.Any
import System.Console.CmdArgs.Implicit.Capture

import Data.Char
import Data.List


data Prog1 = Prog1 [Ann] [Mode1] deriving Show
data Mode1 = Mode1 [Ann] Any [Flag1] deriving Show
data Flag1 = Flag1 [Ann] String Any deriving Show


step1 :: Capture -> Prog1
step1 = expand . groupnames . inherit . flatten


err x = error $ "CmdArgs.Implicit.Step1: " ++ x


mapMode :: (Mode1 -> Mode1) -> Prog1 -> Prog1
mapMode f (Prog1 a b) = Prog1 a $ map f b


---------------------------------------------------------------------
-- EXPAND FLAGS
-- Add FlagName properties where you can

-- For every flag/mode, assign it a name based on the type information, unless it has explicit
-- For every flag, assign it a short name if it doesn't have one and it would be unambiguous, and no explicit
-- Remove all explicit annotations
expand :: Prog1 -> Prog1
expand = mapMode (removeExplicit . assignShort . assignLong)


removeExplicit (Mode1 a b c) = Mode1 (del a) b $ map f c
    where f (Flag1 a b c) = Flag1 (del a) b c
          del = filter (/= Explicit)


assignShort (Mode1 a b c) = Mode1 a b [Flag1 ((s \\ dupe) ++ x) y z | (s,Flag1 x y z) <- zip poss c]
    where seen = [x | Flag1 a _ _ <- c, Name [x] <- a]
          dupe = concat poss \\ nub (concat poss)
          poss = map f c
          f (Flag1 a b c) = [Name [x] | Explicit `notElem` a, null [() | Name [_] <- a]
                                       , x <- take 1 [head x | Name x <- a], x `notElem` seen]


assignLong (Mode1 a b c) = Mode1 (add (ctor b) a) b $ map f c
    where f (Flag1 a b c) = Flag1 (add newname a) b c
               where newname = if FlagEnum `elem` a then ctor c else b
          add s xs = [Name ss | all g xs, Name ss `notElem` xs] ++ xs
               where ss = map (\x -> if x == '_' then '-' else toLower x) $ if last s == '_' then init s else s

          g Explicit = False
          g FlagArgs = False
          g FlagArgPos{} = False
          g _ = True


---------------------------------------------------------------------
-- GROUP NAMES
-- Make sure every mode/flag has a GroupName annotation

groupnames :: Prog1 -> Prog1
groupnames (Prog1 a b) = Prog1 a $ f onmode "" [Mode1 c d $ f onflag "" e | Mode1 c d e <- b]
    where
        onmode (Mode1 a b c) = (a, \a -> Mode1 a b c)
        onflag (Flag1 a b c) = (a, \a -> Flag1 a b c)

        f on name [] = []
        f on name (x:xs) = case [y | GroupName y <- c] of
            [] -> g (GroupName name:c) : f on name xs
            ys -> x : f on (last ys) xs
            where (c,g) = on x


---------------------------------------------------------------------
-- INHERIT
-- Deal with FlagInherit

inherit :: Prog1 -> Prog1
inherit (Prog1 a b) = Prog1 a $ f ask0 b
    where
        ask0 s = err $ "Field missing and not specified previously: " ++ show s

        f ask (x:xs) = x2 : f (\s -> let c = [b | b@(Flag1 _ a _) <- cs, a == s] in if null c then ask s else c) xs
            where x2@(Mode1 _ _ cs) = inheritMode ask x
        f ask [] = []


inheritMode :: (String -> [Flag1]) -> Mode1 -> Mode1
inheritMode ask (Mode1 a b c) = Mode1 a (foldr ($) b upd) (concat c2)
    where (c2,upd) = unzip $ map (inheritFlag ask) c


inheritFlag :: (String -> [Flag1]) -> Flag1 -> ([Flag1], Any -> Any)
inheritFlag ask (Flag1 a b c)
    | FlagInherit `notElem` a = ([Flag1 a b c], id)
    | or [typeOf c /= typeOf c2 | Flag1 _ _ c2 <- ys] = err $ "Field missing and previous instance has a different type:" ++ show b
    | Flag1 a2 b2 c2 : _ <- ys = (ys, setField (b2,c2))
    where ys = ask b

---------------------------------------------------------------------
-- FLATTEN
-- Separate the data in to Prog/Mode/Flag

flatten :: Capture -> Prog1
flatten = moveAnn . flattenProg


-- Move annotations from Prog to Mode if appropriate
-- 'Help' can be at the program level for multi-mode programs
moveAnn :: Prog1 -> Prog1
moveAnn (Prog1 as ms) = Prog1 prog [Mode1 (mode++a) b c | Mode1 a b c <- ms]
    where (prog,mode) = partition (\x -> case x of Help{} -> length ms > 1; _ -> isProgAnn x) as

isProgAnn ProgSummary{} = True
isProgAnn ProgProgram{} = True
isProgAnn ProgVerbosity{} = True
isProgAnn _ = False


flattenProg :: Capture -> Prog1
flattenProg (Ann a b) = let Prog1 x y = flattenProg b in Prog1 (x++[a]) y
flattenProg (Many xs) = Prog1 [] $ map flattenMode xs
flattenProg x@Ctor{} = Prog1 [] [flattenMode x]
flattenProg x = err $ "Unexpected in a program: " ++ show x


flattenMode :: Capture -> Mode1
flattenMode (Ann a b) = let Mode1 x y z = flattenMode b in Mode1 (x++[a]) y z
flattenMode (Ctor x ys) = Mode1 [] x [Flag1 a n b | (y,n) <- zip ys $ fields x, Flag1 a _ b <- flattenFlag y]
flattenMode x = err $ "Unexpected in a mode: " ++ show x


flattenFlag :: Capture -> [Flag1]
flattenFlag (Ann a b) = [Flag1 (x++[a]) y z | Flag1 x y z <- flattenFlag b]
flattenFlag (Value x) = [Flag1 [] "" x]
flattenFlag (Missing x) = [Flag1 [FlagInherit] "" x]
flattenFlag x@Ctor{} = [Flag1 [] "" $ flattenValue x]
flattenFlag (Many xs) = concatMap flattenFlag $ map (Ann FlagEnum) xs
flattenFlag x = err $ "Unexpected in a flag: " ++ show x


flattenValue :: Capture -> Any
flattenValue (Ctor x _) = x
flattenValue (Value x) = x
flattenValue x = err $ "Unexpected in a value: " ++ show x


{-
*** Exception:

Ann (ProgSummary "HLint v1.6.5, (C) Neil Mitchell 2006-2009")
Ann (ModeHelpSuffix ["To check all Haskell files in 'src' and generate a report type:","  hlint src --report"])
Ann (Text "Suggest improvements to Haskell source code")
Ann (ProgProgram "hlint")

(Ctor HLint
    [Ann (Text "Generate a report in HTML") (Ann (FldTyp "FILE") (Ann (FldEmpty "report.html") (Ctor [] [])))
    ,Ann (Text "Hint/ignore file to use") (Ann (FldTyp "FILE") (Ctor [] []))
    ,Ann (Text "Color the output (requires ANSI terminal)") (Ann (FldFlag "colour") (Ann (FldFlag "c") (Ctor False [])))
    ,Ann (Text "Ignore a particular hint") (Ann (FldTyp "MESSAGE") (Ctor [] []))
    ,Ann (Text "Show all ignored ideas") (Ctor False []),Ann (Text "Run in test mode") (Ctor False [])
    ,Ann (Text "CPP #define") (Ann (FldTyp "NAME[=VALUE]") (Ctor [] []))
    ,Ann (Text "CPP include path") (Ann (FldTyp "DIR") (Ctor [] []))
    ,Ann (FldTyp "FILE/DIR") (Ann FldArgs (Ctor [] []))]))))
-}
