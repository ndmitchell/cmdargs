
module System.Console.CmdArgs.Implicit.Read where

import System.Console.CmdArgs.Implicit.Any
import Data.Data


data ReadContainer
    = ReadList ReadAtom
    | ReadMaybe ReadAtom
    | ReadAtom ReadAtom

data ReadAtom
    = ReadBool
    | ReadInt
    | ReadInteger
    | ReadFloat
    | ReadDouble
    | ReadString
--    | ReadTuple [ReadAtom] -- Possible to add relatively easily

isReadBool ReadBool{} = True; isReadBool _ = False

fromReadContainer :: ReadContainer -> ReadAtom
fromReadContainer (ReadList x) = x
fromReadContainer (ReadMaybe x) = x
fromReadContainer (ReadAtom x) = x


toReadContainer :: Any -> Maybe ReadContainer
toReadContainer x = case tyConString $ typeRepTyCon t of
        "[]" | show t /= "[Char]" -> fmap ReadList $ toReadAtom $ fromListAny x
        "Maybe" -> fmap ReadMaybe $ toReadAtom $ fromMaybeAny x
        _ -> fmap ReadAtom $ toReadAtom x
    where t = anyType x


toReadAtom :: Any -> Maybe ReadAtom
toReadAtom x = case show $ anyType x of
    "Bool" -> Just ReadBool
    "Int" -> Just ReadInt
    "Integer" -> Just ReadInteger
    "Float" -> Just ReadFloat
    "Double" -> Just ReadDouble
    "[Char]" -> Just ReadString
    _ -> Nothing


-- | Both Any will be the same type as ReadContainer
reader :: ReadContainer -> String -> Any -> Either String Any
reader t s x = fmap (addContainer t x) $ readAtom (fromReadContainer t) s


-- | If c is the container type, and a is the atom type:
--   Type (c a) -> c a -> a -> c a
addContainer :: ReadContainer -> Any -> Any -> Any
addContainer (ReadAtom _) _ x = x
addContainer (ReadMaybe _) o x = justAny_ o x
addContainer (ReadList _) o x = appendAny o $ consAny x $ nilAny_ o


-- | The Any will be the type as ReadAtom
readAtom :: ReadAtom -> String -> Either String Any
readAtom ty s = case ty of
    ReadBool -> f False -- not very good, but should never be hit
    ReadInt -> f (0::Int)
    ReadInteger -> f (0::Integer)
    ReadFloat -> f (0::Float)
    ReadDouble -> f (0::Double)
    ReadString -> Right $ Any s
    where
        f t = case reads s of
            [(x,"")] -> Right $ Any $ x `asTypeOf` t
            _ -> Left $ "Could not read as type " ++ show (typeOf t) ++ ", " ++ show s
