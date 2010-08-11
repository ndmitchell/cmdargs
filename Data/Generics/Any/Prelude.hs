{-# LANGUAGE ViewPatterns #-}

module Data.Generics.Any.Prelude where

import Prelude hiding (head,tail,null)
import Data.Generics.Any
import Data.Maybe


head :: AnyT [a] -> AnyT a
head (decompose -> ("(:)",[x,_])) = x

tail :: AnyT [a] -> AnyT [a]
tail (decompose -> ("(:)",[_,x])) = x

cons :: AnyT a -> AnyT [a] -> AnyT [a]
cons x y = compose y "(:)" [x,y]

null :: AnyT [a] -> Bool
null x | isList x = ctor x == "[]"

just_ :: AnyT (Maybe a) -> AnyT a -> AnyT (Maybe a)
just_ w x = compose w "Just" [x]

nil_ :: AnyT [a] -> AnyT [a]
nil_ w = compose w "[]" []

append :: AnyT [a] -> AnyT [a] -> AnyT [a]
append x y | typeOf x == typeOf y = f x y
    where f x y | null x = y
                | otherwise = cons (head x) (f (tail x) y)


isList x = typeShell x == "[]"
isMaybe x = typeShell x == "Maybe"
isTuple x = isJust $ readTupleType $ typeShell x

fromList w = children (compose0 w "(:)") !! 0
fromMaybe w = children (compose0 w "Just") !! 0
fromTuple w = children (compose0 w $ typeShell w)
