
-- | This module does all the tricky/unsafe bits of CmdArgs.
--   It captures annotations on the data structure in the most direct way
--   possible.
module System.Console.CmdArgs.Implicit.Capture(
    Capture(..), capture, many, (&=)
    ) where

import Data.Data
import Data.IORef
import System.IO.Unsafe
import Control.Exception
import System.Console.CmdArgs.Implicit.Ann
import System.Console.CmdArgs.Implicit.Any


-- test = show $ capture $ many [Just ((66::Int) &= P 1 &= P 2), Nothing &= P 8] &= P 3


infixl 2 &=

data Capture
    = Many [Capture]
    | Ann Ann Capture
    | Value Any
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
many :: Data a => [a] -> a
many xs = unsafePerformIO $ do
    ys <- mapM force xs
    set $ Many ys
    return $ head xs


-- | Add an annotation to a value. Note that if the value is evaluated
--   more than once the annotation will only be available the first time.
(&=) :: Data a => a -> Ann -> a
(&=) x y = unsafePerformIO $ do
    add (Ann y)
    evaluate x
    return x


capture :: Data a => a -> Capture
capture x = unsafePerformIO $ force x


force :: Data a => a -> IO Capture
force x = do
    push
    evaluate x
    y <- pop
    case y of
        Right r -> return r
        Left f | not $ isAlgType $ dataTypeOf x -> return $ f $ Value $ Any x
               | otherwise -> do
            cs <- sequence $ gmapQ force x
            return $ f $ Ctor (Any x) cs
