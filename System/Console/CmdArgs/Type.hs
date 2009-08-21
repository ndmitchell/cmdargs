{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module System.Console.CmdArgs.Type where

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
    ,modeFlags :: [Flag]
    }

modeDefault = Mode{modeText="",modeHelpSuffix=[],modeExplicit=False}

data Flag = Flag
    {flagName :: String
    -- FIXME: flag = Either (Maybe Int) [String], either --flag or arg based
    ,flagArgs :: Maybe (Maybe Int) -- Nothing = all arguments, Just i = position i
    ,flagType :: FlagType
    ,flagVal :: Dynamic
    ,flagOpt :: Maybe String
    ,flagTyp :: String
    ,flagText :: String
    ,flagFlag :: [String]
    ,flagExplicit :: Bool
    }

flagDefault = Flag{flagArgs=Nothing,flagOpt=Nothing,flagTyp="",flagText="",flagFlag=[],flagExplicit=False}


---------------------------------------------------------------------
-- STRUCTURED FLAGS


isFlagFlag = not . isFlagArgs
isFlagArgs = isJust . flagArgs
isFlagBool x = case flagType x of FlagBool -> True; _ -> False
isFlagOpt = isJust . flagOpt
flagTypDef def x = case flagTyp x of "" -> def; y -> y


-- Flag types
data FlagType
    = FlagBool
    | FlagItem (String -> Maybe (Dynamic -> Dynamic))

toFlagType :: TypeRep -> Maybe FlagType
toFlagType typ
    | typ == typeOf True = Just FlagBool
    | Just r <- toFlagTypeRead False typ = Just $ FlagItem r
    | a == typeRepTyCon (typeOf ""), Just r <- toFlagTypeRead True (head b) = Just $ FlagItem r
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
