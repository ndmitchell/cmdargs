{-# LANGUAGE PatternGuards, ScopedTypeVariables #-}

-- | This module does all the tricky/unsafe bits of CmdArgs.
--   It captures annotations on the data structure in the most direct way
--   possible.
module System.Console.CmdArgs.Implicit.Capture(
    Capture(..), capture, many, (&=)
    ) where

{-
Notes on purity:

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


import Data.Data(Data)
import Data.IORef
import System.IO.Unsafe
import Control.Exception
import Data.Generics.Any


-- test = show $ capture $ many [Just ((66::Int) &= P 1 &= P 2), Nothing &= P 8] &= P 3


infixl 2 &=

data Capture a
    = Many [Capture a]
    | Ann a (Capture a)
    | Value Any
    | Missing Any -- a RecConError
    | Ctor Any [Capture a]
      deriving Show

instance Functor Capture where
    fmap f (Many xs) = Many $ map (fmap f) xs
    fmap f (Ann a x) = Ann (f a) $ fmap f x
    fmap f (Value x) = Value x
    fmap f (Missing x) = Missing x
    fmap f (Ctor x xs) = Ctor x $ map (fmap f) xs


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
many :: Data a => [a] -> a
many xs = unsafePerformIO $ do
    ys <- mapM (force . Any) xs
    set $ Many ys
    return $ head xs


{-# NOINLINE addAnn #-}
addAnn :: (Data a, Data b) => a -> b -> a
addAnn x y = unsafePerformIO $ do
    add (Ann $ Any y)
    evaluate x
    return x


{-# NOINLINE capture #-}
capture :: Data a => Any -> Capture a
capture x = unsafePerformIO $ fmap (fmap fromAny) $ force x


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


-- | Add an annotation to a value. Note that if the value is evaluated
--   more than once the annotation will only be available the first time.
--
--   If exporting this function with a more restrictive type signature
--   add an INLINE pragma (to reduce the chance of CSE occuring).
{-# INLINE (&=) #-}
(&=) :: (Data a, Data b) => a -> b -> a
(&=) x y = addAnn (id_ x) (id_ y)


{-# NOINLINE const_ #-}
const_ :: a -> b -> b
const_ f x = x

{-# INLINE id_ #-}
id_ :: a -> a
id_ x = const_ (\() -> ()) x

