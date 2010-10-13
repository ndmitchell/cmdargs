{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- | This module takes the result of Capture, and deals with all the local
--   constraints.
module System.Console.CmdArgs.Implicit.Local(
    local, err,
    Prog_(..), progSumm, Mode_(..), Flag_(..), isFlag_
    ) where

import System.Console.CmdArgs.Implicit.Ann
import System.Console.CmdArgs.Implicit.Type
import System.Console.CmdArgs.Implicit.Read
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Annotate
import System.Console.CmdArgs.Default

import Data.Char
import Data.Generics.Any
import Data.Maybe


data Prog_ = Prog_
    {progModes :: [Mode_]
    ,progSummary :: Maybe [String]
    ,progProgram :: String
    ,progHelp :: String -- only for multiple mode programs
    ,progVerbosity :: Bool
    } deriving Show
instance Default Prog_ where
    def = Prog_ def def def def def
progSumm x = fromMaybe ["The " ++ progProgram x ++ " program"] $ progSummary x

data Mode_ = Mode_
    {modeFlags_ :: [Flag_]
    ,modeMode :: Mode (CmdArgs Any)
    ,modeDefault :: Bool
    ,modeGroup :: Maybe String
    ,modeExplicit :: Bool
    } deriving Show
instance Default Mode_ where
    def = Mode_ [] m def def def
        where m = Mode (toGroup []) [] (error "Mode_ undefined") Right "" [] Nothing (toGroup [])

data Flag_
    = Flag_
        {flagField :: String
        ,flagFlag :: Flag (CmdArgs Any)
        ,flagExplicit :: Bool
        ,flagGroup :: Maybe String
        ,flagEnum :: Maybe String -- if you are an enum, what is your string value
        }
    | Arg_
        {flagArg_ :: Arg (CmdArgs Any)
        ,flagArgPos :: Maybe Int
        ,flagArgOpt :: Maybe String
        }
      deriving Show
instance Default Flag_ where
    def = Flag_ "" (error "Flag_ undefined") def def def

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
mode_ o@(Ctor x ys) = [withMode def{modeFlags_=concat $ zipWith flag_ (fields x) ys} $ \x -> x{modeValue=embed $ fromCapture o}]
mode_ x = err "mode" $ show x


flag_ :: String -> Capture Ann -> [Flag_]
flag_ name (Ann Ignore _) = []
flag_ name (Ann a b) = map (flagAnn a) $ flag_ name b
flag_ name (Value x) = [def{flagField=name, flagFlag = remap embed reembed $ value_ name x}]
flag_ name x@Ctor{} = flag_ name $ Value $ fromCapture x
flag_ name (Many xs) = map (enum_ name) xs
flag_ name x = errFlag name $ show x


enum_ :: String -> Capture Ann -> Flag_
enum_ name (Ann a b) = flagAnn a $ enum_ name b
enum_ name (Value x) = def{flagField=name, flagFlag = flagNone [] (fmap $ setField (name,x)) "", flagEnum=Just $ ctor x}
enum_ name x@Ctor{} = enum_ name $ Value $ fromCapture x
enum_ name x = errFlag name $ show x


value_ :: String -> Any -> Flag Any
value_ name x
    | isNothing mty = errFlag name $ show x
    | isReadBool ty =
        let upd b x = setField (name,addContainer ty (getField name x) (Any b)) x
        in flagBool [] upd ""
    | otherwise =
        let upd s x = fmap (\c -> setField (name,c) x) $ reader ty s $ getField name x
        in flagReq [] upd (readHelp ty) ""
    where
        mty = toReadContainer x
        ty = fromJust mty


---------------------------------------------------------------------
-- CAPTURE THE ANNOTATIONS

progAnn :: Ann -> Prog_ -> Prog_
progAnn (ProgSummary a) x = x{progSummary=Just $ lines a}
progAnn (ProgProgram a) x = x{progProgram=a}
progAnn ProgVerbosity x = x{progVerbosity=True}
progAnn (Help a) x | length (progModes x) > 1 = x{progHelp=a}
progAnn a x | length (progModes x) == 1 = x{progModes = map (modeAnn a) $ progModes x}
progAnn a x = err "program" $ show a


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
toArg (Flag_ fld x False Nothing Nothing) pos
    | null (flagNames x), null (flagHelp x), Just y <- opt $ flagInfo x
    = Arg_ (Arg (flagValue x) (flagType x)) pos y
    where
        opt FlagReq = Just Nothing
        opt (FlagOpt x) = Just (Just x)
        opt (FlagOptRare x) = Just Nothing
        opt _ = Nothing
toArg a _ = errFlag "args/argPos" $ show a
