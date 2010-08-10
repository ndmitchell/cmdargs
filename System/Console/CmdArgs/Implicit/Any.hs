{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module System.Console.CmdArgs.Implicit.Any where

import Control.Monad.State
import Data.Data
import Data.List
import Data.Maybe


---------------------------------------------------------------------
-- THE ANY TYPE

data Any = forall a . Data a => Any a
type AnyT t = Any

instance Show Any where
    show (Any x) = show $ typeOf x


anyConstr :: Any -> Constr
anyConstr (Any x) = toConstr x


fromAny :: Data a => Any -> a
fromAny (Any x) = case cast x of
    Just y -> y
    ~(Just y) -> error $ "CmdArgs.Implicit.Capture.fromAny: Failed to extract any, got " ++
                         show (typeOf x) ++ ", wanted " ++ show (typeOf y)


anyType :: Any -> TypeRep
anyType (Any x) = typeOf x

anyDataType :: Any -> DataType
anyDataType (Any x) = dataTypeOf x


---------------------------------------------------------------------
-- SYB UTILITIES

gmapI :: Data a => (forall d. Data d => Int -> d -> d) -> a -> a
gmapI f x = flip evalState 0 $ flip gmapM x $ \y -> do
    i <- get
    put $ i+1
    return $ f i y


fromConstrI :: Data a => (forall d. Data d => Int -> d) -> Constr -> a
fromConstrI f c = gmapI (\i _ -> f i) $ fromConstr c


getField :: Data a => String -> a -> Any
getField lbl x = gmapQi (fieldIndex lbl x) Any x


setField :: Data a => String -> a -> Any -> a
setField lbl x v = gmapI (\j y -> if j == i then fromAny v else y) x
    where i = fieldIndex lbl x


fieldIndex :: Data a => String -> a -> Int
fieldIndex lbl = fromMaybe (error $ "Failed to find field: " ++ lbl) . findIndex (== lbl) . constrFields . toConstr


---------------------------------------------------------------------
-- ANY FUNCTIONS
-- Those with at the end _ take a first parameter which is a witness

headAny :: AnyT [a] -> AnyT a
headAny (Any x)
    | isTypeList (Any x) && not (nullAny $ Any x)
    = gmapQi 0 Any x


tailAny :: AnyT [a] -> AnyT [a]
tailAny (Any x)
    | isTypeList (Any x) && not (nullAny $ Any x)
    = gmapQi 1 Any x


consAny :: AnyT a -> AnyT [a] -> AnyT [a]
consAny x (Any y)
    | anyType x == fromTypeList (Any y)
    = Any $ fromConstrI (\i -> fromAny $ if i == 0 then x else Any y) ctr `asTypeOf` y
    where Just ctr = readConstr (dataTypeOf y) "(:)"

nullAny :: AnyT [a] -> Bool
nullAny (Any x)
    | isTypeList $ Any x
    = showConstr (toConstr x) == "[]"


justAny_ :: AnyT (Maybe a) -> AnyT a -> AnyT (Maybe a)
justAny_ (Any o) x
    | fromTypeMaybe (Any o) == anyType x
    = Any $ fromConstrB (fromAny x) ctr `asTypeOf` o
    where Just ctr = readConstr (dataTypeOf o) "Just"


nilAny_ :: AnyT [a] -> AnyT [a]
nilAny_ (Any o) 
    | isTypeList $ Any o
    = Any $ fromConstrB undefined ctr `asTypeOf` o
    where Just ctr = readConstr (dataTypeOf o) "[]"


appendAny :: AnyT [a] -> AnyT [a] -> AnyT [a]
appendAny x y
    | anyType x == anyType y && nullAny x = y
    | otherwise = consAny (headAny x) (appendAny (tailAny x) y)


-- Checking functions for runtime type checking
isTypeList x = tyConString (typeRepTyCon $ anyType x) == "[]"
isTypeMaybe x = tyConString (typeRepTyCon $ anyType x) == "Maybe"
fromTypeList x | isTypeList x = head $ typeRepArgs $ anyType x
fromTypeMaybe x | isTypeMaybe x = head $ typeRepArgs $ anyType x
