
module System.Console.CmdArgs.Test.Explicit(test) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util


test :: IO ()
test = do
    testUnnamedOnly
    testFlags

testUnnamedOnly = do
    let m = mode [] "" [flagUnnamed (upd "") ""]
    checkFail m ["-f"]
    checkFail m ["--test"]
    checkGood m ["fred","bob"] ["fred","bob"]
    checkGood m ["--","--test"] ["--test"]
    checkGood m [] []

testFlags = do
    let m = mode [] "" [flagNone ["test","t"] ("test":) "", flagNone ["more","m"] ("more":) ""]
    checkFail m ["-q"]
    checkGood m ["--test"] ["test"]
    checkGood m ["-t"] ["test"]
    checkGood m ["--mo"] ["more"]
    checkGood m ["-tm"] ["test","more"]


---------------------------------------------------------------------
-- UTILITIES

upd pre s x = Right $ (pre++s):x

checkFail :: Mode [String] -> [String] -> IO ()
checkFail m xs = case process m xs of
    Right a -> failure "Succeeded when should have failed" [("Args",show xs),("Result",show a)]
    _ -> success

checkGood :: Mode [String] -> [String] -> [String] -> IO ()
checkGood m xs ys = case process m xs of
    Left err -> failure "Failed when should have succeeded" [("Args",show xs),("Error",err)]
    Right a | reverse a /= ys -> failure "Wrong parse" [("Args",show xs),("Wanted",show ys),("Got",show $ reverse a)]
    _ -> success
