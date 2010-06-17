{-# LANGUAGE ExistentialQuantification #-}

-- | This module does all the tricky/unsafe bits of CmdArgs
module System.Console.CmdArgs.Implicit.Capture(
    Capture(..), Any(..), capture, one, (&=)
    ) where

import Data.Data
import Data.IORef
import System.IO.Unsafe
import Control.Exception


data Prop = P Int deriving Show
-- FIXME: import Prop from elsewhere

-- test = show $ capture $ one [Just ((66::Int) &= P 1 &= P 2), Nothing &= P 8] &= P 3


infixl 2 &=

data Any = forall a . Data a => Any a

instance Show Any where
    show (Any x) = show $ typeOf x

data Capture
    = One [Capture]
    | Prop Prop Capture
    | Value Any
    | Ctor Constr [Capture]
      deriving Show
      
{-
The idea is to keep a stack of either continuations, or values
If you encounter 'one' you become a value
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


one :: Data a => [a] -> a
one xs = unsafePerformIO $ do
    ys <- mapM force xs
    set $ One ys
    return $ head xs


(&=) :: Data a => a -> Prop -> a
(&=) x y = unsafePerformIO $ do
    add (Prop y)
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
            let c = toConstr x
            cs <- sequence $ gmapQ force x
            return $ f $ Ctor c cs
