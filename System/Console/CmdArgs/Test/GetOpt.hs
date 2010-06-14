
module System.Console.CmdArgs.Test.GetOpt where

import System.Console.CmdArgs.GetOpt
import System.Console.CmdArgs.Test.Util


data Flag = Verbose | Version | Name String | Output String | Arg String   deriving Show

options :: [OptDescr Flag]
options =
   [Option ['v']     ["verbose"]           (NoArg Verbose)      "verbosely list files",
    Option ['V','?'] ["version","release"] (NoArg Version)      "show version info",
    Option ['o']     ["output"]            (OptArg out "FILE")  "use FILE for dump",
    Option ['n']     ["name"]              (ReqArg Name "USER") "only dump USER's files"]

out :: Maybe String -> Flag
out Nothing  = Output "stdout"
out (Just o) = Output o

tester :: ArgOrder Flag -> [String] -> String
tester order cmdline = case getOpt order options cmdline of
                        (o,n,[]  ) -> "options=" ++ show o ++ "  args=" ++ show n
                        (_,_,errs) -> "failed"

test = do
    tester Permute ["foo","-v"] === "options=[Verbose]  args=[\"foo\"]"
    tester Permute ["foo","--","-v"] === "options=[]  args=[\"foo\",\"-v\"]"
    tester Permute ["-?o","--name","bar","--na=baz"] === "options=[Version,Output \"stdout\",Name \"bar\",Name \"baz\"]  args=[]"
    tester Permute ["--ver","foo"] === "failed"

a === b | a == b = success
        | otherwise = failure "Mismatch in GetOpt" [("Wanted",b),("Got",a)]
