{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- | This module takes the result of Capture, and deals with all the local
--   constraints.
module System.Console.CmdArgs.Implicit.Local(
    local, err,
    Prog_(..), Builtin_(..), Mode_(..), Flag_(..), Fixup(..), isFlag_,
    progHelpOutput, progVersionOutput
    ) where

import System.Console.CmdArgs.Implicit.Ann
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Implicit.Reader
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Annotate
import System.Console.CmdArgs.Default
import qualified Data.Generics.Any.Prelude as A

import Control.Monad
import Data.Char
import Data.Generics.Any
import Data.Maybe


data Prog_ = Prog_
    {progModes :: [Mode_]
    ,progSummary :: Maybe [String]
    ,progProgram :: String
    ,progHelp :: String -- only for multiple mode programs
    ,progVerbosityArgs :: (Maybe Builtin_, Maybe Builtin_) -- (verbose, quiet)
    ,progHelpArg :: Maybe Builtin_
    ,progVersionArg :: Maybe Builtin_
    ,progNoAtExpand :: Bool
    } deriving Show
instance Default Prog_ where
    def = Prog_ def def def def def (Just def) (Just def) def

progOutput f x = fromMaybe ["The " ++ progProgram x ++ " program"] $
    (builtinSummary =<< f x) `mplus` progSummary x

progHelpOutput = progOutput progHelpArg
progVersionOutput = progOutput progVersionArg


data Builtin_ = Builtin_
    {builtinNames :: [String]
    ,builtinExplicit :: Bool
    ,builtinHelp :: Maybe String
    ,builtinGroup :: Maybe String
    ,builtinSummary :: Maybe [String]
    } deriving Show
instance Default Builtin_ where
    def = Builtin_ def def def def def

data Mode_ = Mode_
    {modeFlags_ :: [Flag_]
    ,modeMode :: Mode (CmdArgs Any)
    ,modeDefault :: Bool
    ,modeGroup :: Maybe String
    ,modeExplicit :: Bool
    } deriving Show
instance Default Mode_ where
    def = Mode_ [] (modeEmpty $ error "Mode_ undefined") def def def

data Flag_
    = Flag_
        {flagField :: String
        ,flagFlag :: Flag (CmdArgs Any)
        ,flagExplicit :: Bool
        ,flagGroup :: Maybe String
        ,flagEnum :: Maybe String -- if you are an enum, what is your string value
        ,flagFixup :: Fixup
        }
    | Arg_
        {flagArg_ :: Arg (CmdArgs Any)
        ,flagArgPos :: Maybe Int
        ,flagArgOpt :: Maybe String
        ,flagFixup :: Fixup
        }
      deriving Show
instance Default Flag_ where
    def = Flag_ "" (error "Flag_ undefined") def def def def

newtype Fixup = Fixup (Any -> Any)

instance Default Fixup where def = Fixup id
instance Show Fixup where show _ = "Fixup"

isFlag_ Flag_{} = True
isFlag_ _ = False

withMode x f = x{modeMode = f $ modeMode x}
withFlagArg x f = x{flagArg_ = f $ flagArg_ x}
withFlagFlag x f = x{flagFlag = f $ flagFlag x}

err x y = error $ "System.Console.CmdArgs.Implicit, unexpected " ++ x ++ ": " ++ y
errFlag x y = err ("flag (" ++ x ++ ")") y


local :: Capture Ann -> Prog_
local = prog_ . defaultMissing


---------------------------------------------------------------------
-- CAPTURE THE STRUCTURE

prog_ :: Capture Ann -> Prog_
prog_ (Ann a b) = progAnn a $ prog_ b
prog_ (Many xs) = def{progModes=concatMap mode_ xs, progProgram=prog}
    where prog = map toLower $ typeShell $ fromCapture $ head xs
prog_ x@Ctor{} = prog_ $ Many [x]
prog_ x = err "program" $ show x


mode_ :: Capture Ann -> [Mode_]
mode_ (Ann Ignore _) = []
mode_ (Ann a b) = map (modeAnn a) $ mode_ b
mode_ o@(Ctor x ys) = [withMode def{modeFlags_=flgs} $ \x -> x{modeValue=embed $ fixup $ fromCapture o}]
    where flgs = concat $ zipWith flag_ (fields x) ys
          fixup x = foldl (\x (Fixup f) -> f x) x $ map flagFixup flgs
mode_ x = err "mode" $ show x


flag_ :: String -> Capture Ann -> [Flag_]
flag_ name (Ann Ignore _) = []
flag_ name (Ann a b) = map (flagAnn a) $ flag_ name b
flag_ name (Value x) = let (fix,flg) = value_ name x in [def{flagField=name, flagFlag=remap embed reembed flg, flagFixup=fix}]
flag_ name x@Ctor{} = flag_ name $ Value $ fromCapture x
flag_ name (Many xs) = concatMap (enum_ name) xs
flag_ name x = errFlag name $ show x


enum_ :: String -> Capture Ann -> [Flag_]
enum_ name (Ann Ignore _) = []
enum_ name (Ann a b) = map (flagAnn a) $ enum_ name b
enum_ name (Value x) = [def{flagField=name, flagFlag = flagNone [] (fmap upd) "", flagEnum=Just $ ctor x}]
    where upd v | not (A.isString x) && A.isList x = setField (name, getField name v `A.append` x) v
                | otherwise = setField (name,x) v
enum_ name x@Ctor{} = enum_ name $ Value $ fromCapture x
enum_ name x = errFlag name $ show x


-- Fixup (ends up in modeCheck) and the flag itself
value_ :: String -> Any -> (Fixup, Flag Any)
value_ name x
    | isNothing mty = errFlag name $ show x
    | readerBool ty =
        let f (Right x) = x
            upd b x = setField (name, f $ readerRead ty (getField name x) $ show b) x
        in (fixup, flagBool [] upd "")
    | otherwise =
        let upd s x = fmap (\c -> setField (name,c) x) $ readerRead ty (getField name x) s
        in (fixup, flagReq [] upd (readerHelp ty) "")
    where
        mty = reader x
        ty = fromJust mty
        fixup = Fixup $ \x -> setField (name,readerFixup ty $ getField name x) x


---------------------------------------------------------------------
-- CAPTURE THE ANNOTATIONS

progAnn :: Ann -> Prog_ -> Prog_
progAnn (ProgSummary a) x = x{progSummary=Just $ lines a}
progAnn (ProgProgram a) x = x{progProgram=a}
progAnn ProgVerbosity x = x{progVerbosityArgs=let f sel = Just $ fromMaybe def $ sel $ progVerbosityArgs x in (f fst, f snd)}
progAnn (Help a) x | length (progModes x) > 1 = x{progHelp=a}
progAnn (ProgHelpArg a) x = x{progHelpArg = builtinAnns (progHelpArg x) a}
progAnn (ProgVersionArg a) x = x{progVersionArg = builtinAnns (progVersionArg x) a}
progAnn (ProgVerbosityArgs a b) x = x{progVerbosityArgs=(builtinAnns (Just $ fromMaybe def $ fst $ progVerbosityArgs x) a, builtinAnns (Just $ fromMaybe def $ snd $ progVerbosityArgs x) b)}
progAnn ProgNoAtExpand x = x{progNoAtExpand=True}
progAnn a x | length (progModes x) == 1 = x{progModes = map (modeAnn a) $ progModes x}
progAnn a x = err "program" $ show a


builtinAnns = foldl (flip builtinAnn)

builtinAnn :: Ann -> Maybe Builtin_ -> Maybe Builtin_
builtinAnn _ Nothing = Nothing
builtinAnn Ignore _ = Nothing
builtinAnn Explicit (Just x) = Just x{builtinExplicit=True}
builtinAnn (Name a) (Just x) = Just x{builtinNames=a : builtinNames x}
builtinAnn (Help a) (Just x) = Just x{builtinHelp=Just a}
builtinAnn (GroupName a) (Just x) = Just x{builtinGroup=Just a}
builtinAnn (ProgSummary a) (Just x) = Just x{builtinSummary=Just $ lines a}
builtinAnn a x = err "builtin" $ show a


modeAnn :: Ann -> Mode_ -> Mode_
modeAnn (Help a) x = withMode x $ \x -> x{modeHelp=a}
modeAnn (ModeHelpSuffix a) x = withMode x $ \x -> x{modeHelpSuffix=a}
modeAnn ModeDefault x = x{modeDefault=True}
modeAnn (GroupName a) x = x{modeGroup=Just a}
modeAnn Explicit x = x{modeExplicit=True}
modeAnn (Name a) x = withMode x $ \x -> x{modeNames=a:modeNames x}
modeAnn a x = err "mode" $ show a


flagAnn :: Ann -> Flag_ -> Flag_
flagAnn (FlagType a) x@Arg_{} = withFlagArg x $ \x -> x{argType=a}
flagAnn (FlagType a) x@Flag_{} = withFlagFlag x $ \x -> x{flagType=a}
flagAnn (Help a) x@Flag_{} = withFlagFlag x $ \x -> x{flagHelp=a}
flagAnn (FlagArgPos a) x = toArg x $ Just a
flagAnn FlagArgs x = toArg x Nothing
flagAnn Explicit x@Flag_{} = x{flagExplicit=True}
flagAnn (FlagOptional a) x@Flag_{flagEnum=Nothing,flagFlag=Flag{flagInfo=FlagReq}} = withFlagFlag x $ \x -> x{flagInfo=FlagOpt a}
flagAnn (FlagOptional a) x@Arg_{} = x{flagArgOpt=Just a}
flagAnn (Name a) x@Flag_{} = withFlagFlag x $ \x -> x{flagNames = a : flagNames x}
flagAnn (GroupName a) x@Flag_{} = x{flagGroup=Just a}
flagAnn a x = errFlag (head $ words $ show x) $ show a

toArg :: Flag_ -> Maybe Int -> Flag_
toArg (Flag_ fld x False Nothing Nothing fix) pos
    | null (flagNames x), null (flagHelp x), Just y <- opt $ flagInfo x
    = Arg_ (Arg (flagValue x) (flagType x) (isNothing y)) pos y fix
    where
        opt FlagReq = Just Nothing
        opt (FlagOpt x) = Just (Just x)
        opt (FlagOptRare x) = Just Nothing
        opt _ = Nothing
toArg a _ = errFlag "args/argPos" $ show a
