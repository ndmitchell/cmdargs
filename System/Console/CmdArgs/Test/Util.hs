{-# LANGUAGE ExistentialQuantification #-}

module System.Console.CmdArgs.Test.Util where

import System.Console.CmdArgs.Explicit
import Data.Maybe
import Data.Typeable


failure :: String -> [(String,String)] -> IO ()
failure x ys = putStr $ unlines $ "" : "" : "FAILURE" : x : [a ++ ": " ++ b | (a,b) <- ys]

success :: IO ()
success = putChar '.'


-- seq used to obtain better program coverage
hpc = seq


-- Demo - wrap a demo up hiding the real type of it
data Demo = forall a . Typeable a => Demo (a -> IO ()) a

runDemo :: Demo -> IO ()
runDemo (Demo f a) = f a

-- Question: Is it possible to do this without the Typeable constraint?
newDemo :: Typeable a => (a -> IO ()) -> Mode a -> Mode Demo
newDemo act = remap (Demo act) (\(Demo f x) -> (coerce x, Demo f . coerce))
    where
        coerce :: (Typeable a, Typeable b) => a -> b
        coerce = fromMaybe (error "Type issue in CmdArgs.coerce") . cast
