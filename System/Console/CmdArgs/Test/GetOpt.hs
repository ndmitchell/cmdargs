{-# LANGUAGE DeriveDataTypeable #-}

module System.Console.CmdArgs.Test.GetOpt where

import Data.Data
import System.Console.CmdArgs.GetOpt
import qualified System.Console.CmdArgs.Explicit as Explicit
import System.Console.CmdArgs.Test.Util


data Flag = Verbose | Version | Name String | Output String | Arg String deriving (Show,Data,Typeable)

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

tester :: [String] -> (String,String)
tester cmdline = case getOpt Permute options cmdline of
                        (o,n,[]  ) -> let x = "options=" ++ show o ++ "  args=" ++ show n in (x,x)
                        (_,_,errs) -> ("failed", unlines errs ++ usageInfo header options)
   where header = "Usage: foobar [OPTION...] files..."

mode = (convert "GetOpt compatibility demo" options){Explicit.modeNames=["getopt"]}

demo = [newDemo print mode]


test = do
    tester ["foo","-v"] === "options=[Verbose]  args=[\"foo\"]"
    tester ["foo","--","-v"] === "options=[]  args=[\"foo\",\"-v\"]"
    tester ["-?o","--name","bar","--na=baz"] === "options=[Version,Output \"stdout\",Name \"bar\",Name \"baz\"]  args=[]"
    tester ["--ver","foo"] === "failed"

a === b | fst a == b = success
        | otherwise = failure "Mismatch in GetOpt" [("Wanted",b),("Got",fst a)]
