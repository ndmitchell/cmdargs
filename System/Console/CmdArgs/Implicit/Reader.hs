{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Implicit.Reader(Reader(..), reader) where

import Data.Generics.Any
import qualified Data.Generics.Any.Prelude as A
import System.Console.CmdArgs.Explicit
import Data.Char
import Data.List


data Reader = Reader
    {readerHelp :: String
    ,readerBool :: Bool
    ,readerParts :: Int
    ,readerRead :: Any -> String -> Either String Any
    }

-- reader tries to use the first argument to readerRead, but reader_ doesn't
readerRead_ r = readerRead r $ error "Invariant broken: reader/reader_"


reader :: Any -> Maybe Reader
reader x | A.isList x && not (A.isString x) = do
    r <- reader_ $ A.fromList x
    return r{readerRead = \o s -> fmap (A.append o . A.list_ x) $ readerRead_ r s}
reader x = reader_ x


reader_ :: Any -> Maybe Reader
reader_ x | A.isString x = Just $ Reader "ITEM" False 1 $ const $ Right . Any


reader_ x | typeName x == "Bool" = Just $ Reader "BOOL" True 1 $ const $ \s ->
    maybe (Left $ "Could not read as boolean, " ++ show s) (Right . Any) $ parseBool s


reader_ x
    | ty == "Int" = f "INT" (0::Int)
    | ty == "Integer" = f "INT" (0::Integer)
    | ty == "Float" = f "NUM" (0::Float)
    | ty == "Double" = f "NUM" (0::Double)
    where
        ty = typeName x
        f hlp t = Just $ Reader hlp False 1 $ const $ \s -> case reads s of
            [(x,"")] -> Right $ Any $ x `asTypeOf` t
            _ -> Left $ "Could not read as type " ++ show (typeOf $ Any t) ++ ", " ++ show s


reader_ x | A.isList x = do
    r <- reader_ $ A.fromList x
    return $ r{readerRead = const $ fmap (A.list_ x) . readerRead_ r}


reader_ x | A.isMaybe x = do
    r <- reader_ $ A.fromMaybe x
    return $ r{readerRead = const $ fmap (A.just_ x) . readerRead_ r}


reader_ x | isAlgType x && length xs > 1 && all ((==) 0 . arity . snd) xs
    = Just $ Reader (map toUpper $ typeShell x) (typeName x == "Bool") 1 $ const $ rd . map toLower
    where
        xs = [(map toLower c, compose0 x c) | c <- ctors x]

        rd s | null ys = Left $ "Could not read, expected one of: " ++ unwords (map fst xs)
             | Just (_,x) <- find ((==) s . fst) ys = Right x
             | length ys > 1 = Left $ "Ambiguous read, could be any of: " ++ unwords (map fst ys)
             | otherwise = Right $ snd $ head ys
            where ys = filter (isPrefixOf s . fst) xs


reader_ x | [c] <- ctors x, x <- compose0 x c = do
    let cs = children x
    rs <- mapM reader_ cs
    let n = sum $ map readerParts rs
    return $ Reader (uncommas $ map readerHelp rs) (map readerBool rs == [True]) n $ const $ \s ->
        let ss = commas s in
        if n == 1 then fmap (recompose x . return) $ readerRead_ (head $ filter ((==) 1 . readerParts) rs) s
        else if length ss /= n then Left "Incorrect number of commas for fields"
        else fmap (recompose x) $ sequenceEither $ zipWith readerRead_ rs $ map uncommas $ takes (map readerParts rs) ss


reader_ _ = Nothing


uncommas = intercalate ","
commas = lines . map (\x -> if x == ',' then '\n' else x)


takes [] _ = []
takes (i:is) xs = a : takes is b
    where (a,b) = splitAt i xs

sequenceEither = foldr f (Right [])
    where f (Left x) _ = Left x
          f _ (Left x) = Left x
          f (Right x) (Right xs) = Right (x:xs)
