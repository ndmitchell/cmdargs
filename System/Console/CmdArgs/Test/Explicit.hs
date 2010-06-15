
module System.Console.CmdArgs.Test.Explicit(test) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util


test :: IO ()
test = do
    testUnnamedOnly
    testFlags
    testModes

testUnnamedOnly = do
    let m = mode [] "" [flagArg (upd "") ""]
    checkFail m ["-f"]
    checkFail m ["--test"]
    checkGood m ["fred","bob"] ["fred","bob"]
    checkGood m ["--","--test"] ["--test"]
    checkGood m [] []

testFlags = do
    let m = mode [] "" [flagNone ["test","t"] ("test":) "", flagNone ["more","m"] ("more":) ""
                       ,flagReq ["color","colour","bobby"] (upd "color") "" ""
                       ,flagOpt "" ["bob","z"] (upd "bob") "" ""
                       ,flagBool ["x","xxx"] (upb "xxx") ""
                       ,flagArg (upd "") ""]
    checkFail m ["-q"]
    checkGood m ["--test"] ["test"]
    checkGood m ["-t"] ["test"]
    checkFail m ["-t="]
    checkFail m ["--test=value"]
    checkFail m ["--bo"]
    checkGood m ["--bobb=r"] ["colorr"]
    checkGood m ["--bob"] ["bob"]
    checkGood m ["--bob=foo"] ["bobfoo"]
    checkGood m ["--bob","foo"] ["bob","foo"]
    checkGood m ["-zfoo"] ["bobfoo"]
    checkGood m ["-z=foo"] ["bobfoo"]
    checkGood m ["-z","foo"] ["bob","foo"]
    checkGood m ["--mo"] ["more"]
    checkGood m ["-tm"] ["test","more"]
    checkGood m ["--col=red"] ["colorred"]
    checkGood m ["--col","red","-t"] ["colorred","test"]

testModes = do
    let m = modes [("test", mode ["test"] "" [flagNone ["bob"] ("bob":) ""])
                  ,("dist", mode ["dist"] "" [flagNone ["bob"] ("bob":) "", flagReq ["bill"] (upd "bill") "" "", flagArg (upd "") ""])]
    checkFail m []
    checkFail m ["tess"]
    checkGood m ["test","--b"] ["test","bob"]
    checkGood m ["t","--bo"] ["test","bob"]
    checkGood m ["dist","--bob"] ["dist","bob"]
    checkFail m ["dist","--bill"]
    checkGood m ["dist","--bill","foo"] ["dist","billfoo"]

---------------------------------------------------------------------
-- UTILITIES

upd pre s x = Right $ (pre++s):x
upb pre s x = (pre ++ show s):x

checkFail :: Mode [String] -> [String] -> IO ()
checkFail m xs = case process m xs of
    Right a -> failure "Succeeded when should have failed" [("Args",show xs),("Result",show a)]
    Left a -> length (show a) `hpc` success

checkGood :: Mode [String] -> [String] -> [String] -> IO ()
checkGood m xs ys = case process m xs of
    Left err -> failure "Failed when should have succeeded" [("Args",show xs),("Error",err)]
    Right a | reverse a /= ys -> failure "Wrong parse" [("Args",show xs),("Wanted",show ys),("Got",show $ reverse a)]
    _ -> success
