{-# LANGUAGE PatternGuards, ScopedTypeVariables, ExistentialQuantification, DeriveDataTypeable #-}

-- | This module captures annotations on a value, and builds a 'Capture' value.
--   This module has two ways of writing annotations:
--
--   /Impure/: The impure method of writing annotations is susceptible to over-optimisation by GHC
--   - sometimes @\{\-\# OPTIONS_GHC -fno-cse \#\-\}@ will be required.
--
--   /Pure/: The pure method is more verbose, and lacks some type safety.
--
--   As an example of the two styles:
--
-- > data Foo = Foo {foo :: Int, bar :: Int}
--
--   @ impure = 'capture' $ Foo {foo = 12, bar = 'many' [1 '&=' \"inner\", 2]} '&=' \"top\"@
--
--   @ pure = 'capture_' $ 'record' Foo{} [foo := 12, bar :=+ ['atom' 1 '+=' \"inner\", 'atom' 2]] '+=' \"top\"@
--
--   Both evaluate to:
--
-- > Capture (Ann "top") (Ctor (Foo 12 1) [Value 12, Many [Ann "inner" (Value 1), Value 2]]
module System.Console.CmdArgs.Annotate(
    -- * Capture framework
    Capture(..), Any(..), fromCapture,
    -- * Impure
    capture, many, (&=),
    -- * Pure
    capture_, many_, (+=), atom, record, Annotate((:=),(:=+))
    ) where

import Data.Data(Data,Typeable)
import Data.List
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import Control.Exception
import Data.Generics.Any

infixl 2 &=, +=
infix 3 :=


-- | The result of capturing some annotations.
data Capture ann
    = Many [Capture ann] -- ^ Many values collapsed ('many' or 'many_')
    | Ann ann (Capture ann) -- ^ An annotation attached to a value ('&=' or '+=')
    | Value Any -- ^ A value (just a value, or 'atom')
    | Missing Any -- ^ A missing field (a 'RecConError' exception, or missing from 'record')
    | Ctor Any [Capture ann] -- ^ A constructor (a constructor, or 'record')
      deriving Show

instance Functor Capture where
    fmap f (Many xs) = Many $ map (fmap f) xs
    fmap f (Ann a x) = Ann (f a) $ fmap f x
    fmap f (Value x) = Value x
    fmap f (Missing x) = Missing x
    fmap f (Ctor x xs) = Ctor x $ map (fmap f) xs


-- | Return the value inside a capture.
fromCapture :: Capture ann -> Any
fromCapture (Many (x:_)) = fromCapture x
fromCapture (Ann _ x) = fromCapture x
fromCapture (Value x) = x
fromCapture (Missing x) = x
fromCapture (Ctor x _) = x


---------------------------------------------------------------------
-- IMPURE BIT

-- test = show $ capture $ many [Just ((66::Int) &= P 1 &= P 2), Nothing &= P 8] &= P 3

{-
Notes On Purity
---------------

There is a risk that things that are unsafe will be inlined. That can generally be
removed by NOININE on everything.

There is also a risk that things get commoned up. For example:

foo = trace "1" 1
bar = trace "1" 1
main = do
    evaluate foo
    evaluate bar

Will print "1" only once, since foo and bar share the same pattern. However, if
anything in the value is a lambda they are not seen as equal. We exploit this by
defining const_ and id_ as per this module.

Now anything wrapped in id_ looks different from anything else.
-}


{-
The idea is to keep a stack of either continuations, or values
If you encounter 'many' you become a value
If you encounter '&=' you increase the continuation
-}

{-# NOINLINE ref #-}
ref :: IORef [Either (Capture Any -> Capture Any) (Capture Any)]
ref = unsafePerformIO $ newIORef []

push = modifyIORef ref (Left id :)
pop = do x:xs <- readIORef ref; writeIORef ref xs; return x
modify f = modifyIORef ref $ \x -> case x of Left g : rest -> f g : rest ; _ -> error "Internal error in Capture"
add f = modify $ \x -> Left $ x . f
set x = modify $ \f -> Right $ f x


-- | Collapse multiple values in to one.
{-# NOINLINE many #-}
many :: Data val => [val] -> val
many xs = unsafePerformIO $ do
    ys <- mapM (force . Any) xs
    set $ Many ys
    return $ head xs


{-# NOINLINE addAnn #-}
addAnn :: (Data val, Data ann) => val -> ann -> val
addAnn x y = unsafePerformIO $ do
    add (Ann $ Any y)
    evaluate x
    return x


-- | Capture a value. Note that if the value is evaluated
--   more than once the result may be different, i.e.
--
-- > capture x /= capture x
{-# NOINLINE capture #-}
capture :: (Data val, Data ann) => val -> Capture ann
capture x = unsafePerformIO $ fmap (fmap fromAny) $ force $ Any x


force :: Any -> IO (Capture Any)
force x@(Any xx) = do
    push
    res <- try $ evaluate xx
    y <- pop
    case y of
        _ | Left (_ :: RecConError) <- res -> return $ Missing x
        Right r -> return r
        Left f | not $ isAlgType x -> return $ f $ Value x
               | otherwise -> do
            cs <- mapM force $ children x
            return $ f $ Ctor x cs


-- | Add an annotation to a value.
--
--   It is recommended that anyone making use of this function redefine
--   it with a more restrictive type signature to control the type of the
--   annotation (the second argument). Any redefinitions of this function
--   should add an INLINE pragma, to reduce the chance of incorrect
--   optimisations.
{-# INLINE (&=) #-}
(&=) :: (Data val, Data ann) => val -> ann -> val
(&=) x y = addAnn (id_ x) (id_ y)


{-# NOINLINE const_ #-}
const_ :: a -> b -> b
const_ f x = x

{-# INLINE id_ #-}
id_ :: a -> a
id_ x = const_ (\() -> ()) x


---------------------------------------------------------------------
-- PURE PART

-- | This type represents an annotated value. The type of the underlying value is not specified.
data Annotate ann
    = forall c f . (Data c, Data f) => (c -> f) := f -- ^ Construct a field, @fieldname := value@.
    | forall c f . (Data c, Data f) => (c -> f) :=+ [Annotate ann] -- ^ Add annotations to a field.
    | AAnn ann (Annotate ann)
    | AMany [Annotate ann]
    | AAtom Any
    | ACtor Any [Annotate ann]


-- | Add an annotation to a value.
(+=) :: Annotate ann -> ann -> Annotate ann
(+=) = flip AAnn

-- | Collapse many annotated values in to one.
many_ :: [Annotate a] -> Annotate a
many_ = AMany

-- | Lift a pure value to an annotation.
atom :: Data val => val -> Annotate ann
atom = AAtom . Any

-- | Create a constructor/record. The first argument should be
--   the type of field, the second should be a list of fields constructed
--   originally defined by @:=@ or @:=+@.
--
--   This operation is not type safe, and may raise an exception at runtime
--   if any field has the wrong type or label.
record :: Data a => a -> [Annotate b] -> Annotate b
record a b = ACtor (Any a) b

-- | Capture the annotations from an annotated value.
capture_ :: Show a => Annotate a -> Capture a
capture_ (AAnn a x) = Ann a (capture_ x)
capture_ (AMany xs) = Many (map capture_ xs)
capture_ (AAtom x) = Value x
capture_ (_ := c) = Value $ Any c
capture_ (_ :=+ c) = Many $ map capture_ c
capture_ (ACtor x xs)
    | not $ null rep = error $ "Some fields got repeated under " ++ show x ++ "." ++ ctor x ++ ": " ++ show rep
    | otherwise = Ctor x2 xs2
    where
        x2 = recompose x $ map fromCapture xs2
        xs2 = [fromMaybe (Missing c) $ lookup i is | let is = zip inds $ map capture_ xs, (i,c) <- zip [0..] $ children x]
        inds = zipWith fromMaybe [0..] $ map (fieldIndex x) xs
        rep = inds \\ nub inds


fieldIndex :: Any -> Annotate a -> Maybe Int
fieldIndex ctor (AAnn a x) = fieldIndex ctor x
fieldIndex ctor (f := _) = fieldIndex ctor (f :=+ [])
fieldIndex ctor (f :=+ _) | isJust res = res
                          | otherwise = error $ "Couldn't resolve field for " ++ show ctor
    where c = recompose ctor [Any $ throwInt i `asTypeOf` x | (i,Any x) <- zip [0..] (children ctor)]
          res = catchInt $ f $ fromAny c
fieldIndex _ _ = Nothing



data ExceptionInt = ExceptionInt Int deriving (Show, Typeable)
instance Exception ExceptionInt


throwInt :: Int -> a
throwInt i = throw (ExceptionInt i)


{-# NOINLINE catchInt #-}
catchInt :: a -> Maybe Int
catchInt x = unsafePerformIO $ do
    y <- try (evaluate x)
    return $ case y of
        Left (ExceptionInt z) -> Just z
        _ -> Nothing
