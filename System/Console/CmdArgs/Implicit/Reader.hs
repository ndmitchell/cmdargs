{-# LANGUAGE PatternGuards #-}

-- | FIXME: toReadContainer seems redundant? Shouldn't reader be entirely enough?
--          The whole API exported by this seems a little confused
module System.Console.CmdArgs.Implicit.Reader(isReadBool, toReadContainer, reader, addContainer, readHelp) where

import Data.Generics.Any
import qualified Data.Generics.Any.Prelude as A
import System.Console.CmdArgs.Explicit
import Data.Char
import Data.Either
import Data.List


data ReadContainer
    = ReadList ReadAtom
    | ReadMaybe ReadAtom
    | ReadAtom ReadAtom
    | ReadNewtype ReadAtom

data ReadAtom
    = ReadBool
    | ReadInt
    | ReadInteger
    | ReadFloat
    | ReadDouble
    | ReadString
    | ReadEnum [(String,Any)]
    | ReadTuple [ReadAtom]

isReadBool x = case fromReadContainer x of
    ReadBool{} -> True
    _ -> False

fromReadContainer :: ReadContainer -> ReadAtom
fromReadContainer (ReadList x) = x
fromReadContainer (ReadMaybe x) = x
fromReadContainer (ReadAtom x) = x
fromReadContainer (ReadNewtype x) = x


toReadContainer :: Any -> Maybe ReadContainer
toReadContainer x = case typeShell x of
        "[]" | typeName x /= "[Char]" -> fmap ReadList $ toReadAtom $ A.fromList x
        "Maybe" -> fmap ReadMaybe $ toReadAtom $ A.fromMaybe x
        _ | Just y <- toReadAtom x -> Just $ ReadAtom y
          | [xx] <- children x, length (ctors x) == 1, Just y <- toReadAtom xx -> Just $ ReadNewtype y
          | otherwise -> Nothing


toReadAtom :: Any -> Maybe ReadAtom
toReadAtom x = case typeName x of
    "Bool" -> Just ReadBool
    "Int" -> Just ReadInt
    "Integer" -> Just ReadInteger
    "Float" -> Just ReadFloat
    "Double" -> Just ReadDouble
    "[Char]" -> Just ReadString
    _ | A.isTuple x -> fmap ReadTuple $ mapM toReadAtom $ children $ compose0 x $ typeShell x
    _ -> toReadEnum x


toReadEnum :: Any -> Maybe ReadAtom
toReadEnum x
    | isAlgType x && all ((==) 0 . arity . compose0 x) cs
        = Just $ ReadEnum [(map toLower c, compose0 x c) | c <- cs]
    | otherwise = Nothing
    where cs = ctors x


-- | Both Any will be the same type as ReadContainer
reader :: ReadContainer -> String -> Any -> Either String Any
reader t s x = fmap (addContainer t x) $ readAtom (fromReadContainer t) s


-- | If c is the container type, and a is the atom type:
--   Type (c a) -> c a -> a -> c a
addContainer :: ReadContainer -> Any -> Any -> Any
addContainer (ReadAtom _) _ x = x
addContainer (ReadMaybe _) o x = A.just_ o x
addContainer (ReadList _) o x = A.append o $ A.cons x $ A.nil_ o
addContainer (ReadNewtype _) o x = recompose o [x]


-- | The Any will be the type as ReadAtom
readAtom :: ReadAtom -> String -> Either String Any
readAtom ty s = case ty of
    ReadBool -> maybe (Left $ "Could not read as boolean, " ++ show s) (Right . Any) $ parseBool s
    ReadInt -> f (0::Int)
    ReadInteger -> f (0::Integer)
    ReadFloat -> f (0::Float)
    ReadDouble -> f (0::Double)
    ReadString -> Right $ Any s
    ReadEnum xs -> readEnum (map toLower s) xs
    ReadTuple _ -> readTuple ty s
    where
        f t = case reads s of
            [(x,"")] -> Right $ Any $ x `asTypeOf` t
            _ -> Left $ "Could not read as type " ++ show (typeOf $ Any t) ++ ", " ++ show s


readEnum:: String -> [(String,a)] -> Either String a
readEnum a xs | null ys = Left $ "Could not read, expected one of: " ++ unwords (map fst xs)
              | Just (_,el) <- find (\x -> a == fst x) ys = Right el
              | length ys > 1 = Left $ "Ambiguous read, could be any of: " ++ unwords (map fst ys)
              | otherwise = Right $ snd $ head ys
    where ys = filter (\x -> a `isPrefixOf` fst x) xs


readTuple :: ReadAtom -> String -> Either String Any
readTuple ty s
    | length ss /= length ts = Left "Incorrect number of comma separated fields for tuple"
    | not $ null left = Left $ head left
    | otherwise = Right $ gen right
    where
        (left,right) = partitionEithers $ zipWith readAtom ts ss
        (ts,gen) = flatten ty
        ss = split s


split :: String -> [String]
split = lines . map (\x -> if x == ',' then '\n' else x)

flatten :: ReadAtom -> ([ReadAtom], [Any] -> Any)
flatten (ReadTuple xs) = (concat ns, A.tuple . zipWith ($) fs . unconcat ns)
    where (ns,fs) = unzip $ map flatten xs
flatten x = ([x], \[a] -> a)


unconcat :: [[w]] -> [a] -> [[a]]
unconcat [] [] = []
unconcat (w:ws) xs = x1 : unconcat ws x2
    where (x1,x2) = splitAt (length w) xs


readHelp :: ReadContainer -> String
readHelp = f . fromReadContainer
    where
        f ReadBool = "BOOL"
        f ReadInt = "INT"
        f ReadInteger = "INT"
        f ReadFloat = "NUM"
        f ReadDouble = "NUM"
        f ReadString = "ITEM"
        f (ReadEnum xs) = map toUpper $ typeShell $ snd $ head xs
        f (ReadTuple xs) = intercalate "," $ map f xs
