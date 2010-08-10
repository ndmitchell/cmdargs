{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module System.Console.CmdArgs.Test.Implicit.Tests where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util

data Test1
    = Reading {maybeInt :: Maybe Int, listDouble :: [Double], maybeStr :: Maybe String, float :: Float}
    | Other
      deriving (Show,Eq,Data,Typeable)

reading = Reading def def def (def &= args)

mode1 = cmdArgsMode $ modes [reading,Other]  &= help "Testing various corner cases"

test = do
    let Tester{fails,(===)} = tester "Test" mode1
    fails []
    ["reading"] === reading
    ["reading","--maybeint=12"] === reading{maybeInt = Just 12}
    ["reading","--maybeint=12","--maybeint=14"] === reading{maybeInt = Just 14}
    fails ["reading","--maybeint"]
    fails ["reading","--maybeint=test"]
    ["reading","--listdouble=1","--listdouble=3","--listdouble=2"] === reading{listDouble=[1,3,2]}
    fails ["reading","--maybestr"]
    ["reading","--maybestr="] === reading{maybeStr=Just ""}
    ["reading","--maybestr=test"] === reading{maybeStr=Just "test"}
    ["reading","12.5"] === reading{float=12.5}
    ["reading","12.5","18"] === reading{float=18}
