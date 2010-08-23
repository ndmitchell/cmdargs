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
import System.Console.CmdArgs.Implicit.Ann
import Data.Generics.Any


-- test = show $ capture $ many [Just ((66::Int) &= P 1 &= P 2), Nothing &= P 8] &= P 3


infixl 2 &=

data Capture
    = Many [Capture]
    | Ann Ann Capture
    | Value Any
    | Missing Any -- a RecConError
    | Ctor Any [Capture]
      deriving Show
      
{-
The idea is to keep a stack of either continuations, or values
If you encounter 'many' you become a value
If you encounter '&=' you increase the continuation
-}

{-# NOINLINE ref #-}
ref :: IORef [Either (Capture -> Capture) Capture]
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


-- | Add an annotation to a value. Note that if the value is evaluated
--   more than once the annotation will only be available the first time.
{-# NOINLINE addAnn #-}
addAnn :: Data a => a -> Ann -> a
addAnn x y = unsafePerformIO $ do
    add (Ann y)
    evaluate x
    return x


{-# NOINLINE capture #-}
capture :: Any -> Capture
capture x = unsafePerformIO $ force x


force :: Any -> IO Capture
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


{-# INLINE (&=) #-}
(&=) :: Data a => a -> Ann -> a
(&=) x y = addAnn (id_ x) (id_ y)


{-# NOINLINE const_ #-}
const_ :: a -> b -> b
const_ f x = x

{-# INLINE id_ #-}
id_ :: a -> a
id_ x = const_ (\() -> ()) x

