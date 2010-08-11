
module System.Console.CmdArgs.Implicit.Read(isReadBool, toReadContainer, reader, addContainer, readHelp) where

import System.Console.CmdArgs.Implicit.Any
import System.Console.CmdArgs.Explicit
import Data.Char
import Data.Data
import Data.List


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
    | ReadEnum [(String,Any)]
--    | ReadTuple [ReadAtom] -- Possible to add relatively easily

isReadBool x = case fromReadContainer x of
    ReadBool{} -> True
    _ -> False

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
    _ -> toReadEnum x


toReadEnum :: Any -> Maybe ReadAtom
toReadEnum (Any x)
    | isAlgType t && all ((==) 0 . arity) cs
        = Just $ ReadEnum [(map toLower $ showConstr c, Any $ fromConstr c `asTypeOf` x) | c <- cs]
    | otherwise = Nothing
    where
        t = dataTypeOf x
        cs = dataTypeConstrs t
        arity c = length $ gmapQ Any $ fromConstr c `asTypeOf` x


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
    ReadBool -> maybe (Left $ "Could not read as boolean, " ++ show s) (Right . Any) $ parseBool s
    ReadInt -> f (0::Int)
    ReadInteger -> f (0::Integer)
    ReadFloat -> f (0::Float)
    ReadDouble -> f (0::Double)
    ReadString -> Right $ Any s
    ReadEnum xs -> readOne (map toLower s) xs
    where
        f t = case reads s of
            [(x,"")] -> Right $ Any $ x `asTypeOf` t
            _ -> Left $ "Could not read as type " ++ show (typeOf t) ++ ", " ++ show s


readOne :: String -> [(String,a)] -> Either String a
readOne a xs | null ys = Left $ "Could not read, expected one of: " ++ unwords (map fst xs)
             | length ys > 1 = Left $ "Ambiguous read, could be any of: " ++ unwords (map fst ys)
             | otherwise = Right $ snd $ head ys
    where ys = filter (\x -> a `isPrefixOf` fst x) xs


readHelp :: ReadContainer -> String
readHelp ty = case fromReadContainer ty of
    ReadBool -> "BOOL"
    ReadInt -> "INT"
    ReadInteger -> "INT"
    ReadFloat -> "NUM"
    ReadDouble -> "NUM"
    ReadString -> "ITEM"
    ReadEnum xs -> map toUpper $ tyconUQname $ show $ anyType $ snd $ head xs
