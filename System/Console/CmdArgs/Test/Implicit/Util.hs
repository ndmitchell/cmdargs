{-# LANGUAGE ScopedTypeVariables #-}

module System.Console.CmdArgs.Test.Implicit.Util where

import System.Console.CmdArgs
import Control.Monad
import System.Environment
import Control.Exception


tester x = (map modeValue x, (===), fails)
    where
        (===) args v = do
            res <- withArgs args $ cmdArgs "" x
            when (res /= v) $
                error $ "Mismatch on flags " ++ show args

        fails args = do
            res <- try $ withArgs args $ cmdArgs "" x
            case res of
                Left (e :: SomeException) -> return ()
                Right _ -> error $ "Expected failure " ++ show args
