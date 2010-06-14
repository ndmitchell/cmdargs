{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module System.Console.CmdArgs.Implicit.Type where

import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.Char
import Data.Function


data Mode a = Mode
    {modeVal :: a
    ,modeName :: String
    ,modeText :: String
    ,modeHelpSuffix :: [String]
    ,modeExplicit :: Bool
    ,modeDef :: Bool
    ,modeProg :: Maybe String
    ,modeFlags :: [Flag]
    }
    deriving Show -- FIXME: The Show should be the --help

modeDefault = Mode{modeText="",modeHelpSuffix=[],modeExplicit=False,modeDef=False,modeProg=Nothing}

data Flag = Flag
    {flagName :: String -- field name
    ,flagKey :: String -- disambiguator (equal to field name, apart from enums)
    ,flagArgs :: Maybe (Maybe Int) -- Nothing = all arguments, Just i = position i, 0-based
    ,flagType :: FlagType
    ,flagVal :: Dynamic -- FIXME: Remove, only used in default computation
    ,flagOpt :: Maybe String
    ,flagTyp :: String
    ,flagText :: String
    ,flagFlag :: [String]
    ,flagUnknown :: Bool -- place to put unknown args
    ,flagExplicit :: Bool
    ,flagGroup :: Maybe String
    }
    deriving Show

flagDefault = Flag{flagArgs=Nothing,flagOpt=Nothing,flagTyp="",flagText="",flagFlag=[],flagUnknown=False,flagExplicit=False,flagGroup=Nothing}


---------------------------------------------------------------------
-- STRUCTURED FLAGS


isFlagFlag = not . isFlagArgs
isFlagArgs = isJust . flagArgs
isFlagBool x = case flagType x of FlagBool{} -> True; _ -> False
isFlagOpt = isJust . flagOpt
flagTypDef def x = case flagTyp x of "" -> def; y -> y


-- Flag types
data FlagType
    = FlagBool Dynamic
    | FlagItem (String -> Maybe (Dynamic -> Dynamic))

instance Show FlagType where
    show (FlagBool x) = "FlagBool " ++ show x
    show (FlagItem x) = "FlagItem <function>"

toFlagType :: TypeRep -> Maybe FlagType
toFlagType typ
    | typ == typeOf True = Just $ FlagBool $ toDyn True
    | Just r <- toFlagTypeRead False typ = Just $ FlagItem r
    | a == typeRepTyCon (typeOf ""), Just r <- toFlagTypeRead True (head b) = Just $ FlagItem r
    | otherwise = Nothing
    where (a,b) = splitTyConApp typ

toFlagTypeRead :: Bool -> TypeRep -> Maybe (String -> Maybe (Dynamic -> Dynamic))
toFlagTypeRead list x
    | x == typeOf "" = with (\x -> [(x,"")])
    | x == typeOf (0 :: Int) = with (reads :: ReadS Int)
    | x == typeOf (0 :: Integer) = with (reads :: ReadS Integer)
    | x == typeOf (0 :: Float) = with (reads :: ReadS Float)
    | x == typeOf (0 :: Double) = with (reads :: ReadS Double)
    | otherwise = Nothing
    where
        with :: forall a . Typeable a => ReadS a -> Maybe (String -> Maybe (Dynamic -> Dynamic))
        with r = Just $ \x -> case r x of
            [(v,"")] -> Just $ \old -> if list then toDyn $ fromJust (fromDynamic old) ++ [v] else toDyn v
            _ -> Nothing
