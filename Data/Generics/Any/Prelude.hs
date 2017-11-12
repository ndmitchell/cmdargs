{-# LANGUAGE ViewPatterns #-}

module Data.Generics.Any.Prelude where

import Data.Generics.Any
import Data.Maybe


head :: AnyT [a] -> AnyT a
head (decompose -> ("(:)",[x,_])) = x

tail :: AnyT [a] -> AnyT [a]
tail (decompose -> ("(:)",[_,x])) = x

cons :: AnyT a -> AnyT [a] -> AnyT [a]
cons x y = compose y "(:)" [x,y]

uncons :: AnyT [a] -> Maybe (AnyT a, AnyT [a])
uncons x = case decompose x of
    ("[]",[]) -> Nothing
    ("(:)",[a,b]) -> Just (a,b)

null :: AnyT [a] -> Bool
null x | isList x = ctor x == "[]"

just_ :: AnyT (Maybe a) -> AnyT a -> AnyT (Maybe a)
just_ w x = compose w "Just" [x]

nil_ :: AnyT [a] -> AnyT [a]
nil_ w = compose w "[]" []

list_ :: AnyT [a] -> AnyT a -> AnyT [a]
list_ w x = cons x $ nil_ w

append :: AnyT [a] -> AnyT [a] -> AnyT [a]
append x y | typeOf x == typeOf y = f x y
    where f x y = case uncons x of
                       Nothing -> y
                       Just (a,b) -> cons a $ f b y

reverse :: AnyT [a] -> AnyT [a]
reverse xs | isList xs = rev xs (nil_ xs)
    where rev xs acc = case uncons xs of
                           Nothing -> acc
                           Just (x,xs) -> rev xs (cons x acc)


isString x = typeName x == "[Char]"
isList x = typeShell x == "[]"
isMaybe x = typeShell x == "Maybe"
isTuple x = isJust $ readTupleType $ typeShell x

fromList w = children (compose0 w "(:)") !! 0
fromMaybe w = children (compose0 w "Just") !! 0
fromTuple w = children (compose0 w $ typeShell w)

unit :: AnyT ()
unit = Any ()

-- Could use a witness and avoid switching on the list of tuples, but this
-- presents a nicer interface
tuple :: [Any] -> Any
tuple [] = unit
tuple [x] = x
-- $(2\7 tuple [$(1,$ Any x$)] = Any ($(1,$ x$)))
tuple [Any x1,Any x2] = Any (x1,x2)
tuple [Any x1,Any x2,Any x3] = Any (x1,x2,x3)
tuple [Any x1,Any x2,Any x3,Any x4] = Any (x1,x2,x3,x4)
tuple [Any x1,Any x2,Any x3,Any x4,Any x5] = Any (x1,x2,x3,x4,x5)
tuple [Any x1,Any x2,Any x3,Any x4,Any x5,Any x6] = Any (x1,x2,x3,x4,x5,x6)
tuple [Any x1,Any x2,Any x3,Any x4,Any x5,Any x6,Any x7] = Any (x1,x2,x3,x4,x5,x6,x7)
tuple _ = error "Data.Generics.Any: Tuples of 8 elements or more are not supported by Data.Data"
