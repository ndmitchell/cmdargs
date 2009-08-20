{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}

module System.Console.CmdArgs.Type where

import Data.Dynamic
import Data.Data
import Data.List
import Data.Maybe
import Data.Char
import Data.Function


-- FIXME: Info should move in to UI once we have a proper type
data Info
    = FldName String -- the record name
    | FldType TypeRep -- the field type
    | FldValue Dynamic
    | FldEmpty String
    | FldArgs
    | FldArgPos Int
    | FldTyp String
    | ModName String -- the constructor name
    | Text String
    | Flag String
    | Explicit
    | HelpSuffix [String]
      deriving Show



type Flag = [Info]

data Mode a = Mode a [Info] [Flag]

---------------------------------------------------------------------
-- STRUCTURED FLAGS

-- Simple stuff
isFlagArgs xs = [] /= [() | FldArgs{} <- xs]
isFlagArgPos xs = [] /= [() | FldArgPos{} <- xs]
isFlagFlag xs = not $ isFlagArgs xs || isFlagArgPos xs
flagName xs = head [x | FldName x <- xs]
isFldFlag Flag{} = True; isFldFlag _ = False
fldFlags xs = [x | Flag x <- xs]
isExplicit xs = [] /= [() | Explicit{} <- xs]
isFlagOpt = isJust . flagOpt
flagOpt xs = listToMaybe [x | FldEmpty x <- xs]
flagText xs = unwords [x | Text x <- xs]
isFlagBool xs = case flagType xs of FlagBool -> True; _ -> False
hasFlagType = isJust . toFlagType
flagType = fromJust . toFlagType
modeName xs = head [x | ModName x <- xs]


-- Flag types
data FlagType
    = FlagBool
    | FlagItem (String -> Maybe (Dynamic -> Dynamic))

toFlagType :: Flag -> Maybe FlagType
toFlagType xs
    | typ == typeOf True = Just FlagBool
    | Just r <- toFlagTypeRead False typ = Just $ FlagItem r
    | a == typeRepTyCon (typeOf ""), Just r <- toFlagTypeRead True (head b) = Just $ FlagItem r
    where typ = head [x | FldType x <- xs]
          (a,b) = splitTyConApp typ

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
