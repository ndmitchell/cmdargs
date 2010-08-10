{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module System.Console.CmdArgs.Test.Implicit.Tests where
import System.Console.CmdArgs
import System.Console.CmdArgs.Test.Implicit.Util

data Test1
    = Reading {maybeInt :: Maybe Int, listDouble :: [Double], maybeStr :: Maybe String, float :: Float
              ,bool :: Bool, maybeBool :: Maybe Bool, listBool :: [Bool]}
    | Other
      deriving (Show,Eq,Data,Typeable)

reading = Reading def def def (def &= args) def def def

mode1 = cmdArgsMode $ modes [reading,Other]  &= help "Testing various corner cases (1)"

test1 = do
    let Tester{fails,(===)} = tester "Test1" mode1
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
    ["reading","--bool"] === reading{bool=True}
    ["reading","--maybebool"] === reading{maybeBool=Just True}
    ["reading","--maybebool=off"] === reading{maybeBool=Just False}
    ["reading","--listbool","--listbool=true","--listbool=false"] === reading{listBool=[True,True,False]}
    fails ["reading","--listbool=fred"]


-- from bug #230
data Test2 = Cmd1 {bs :: [String]}
           | Cmd2 {bar :: Int}
             deriving (Show, Eq, Data, Typeable)

mode2 = cmdArgsMode $ modes [Cmd1 [], Cmd2 42] &= help "Testing various corner cases (2)"

test2 = do
    let Tester{fails,(===)} = tester "Test2" mode2
    fails []
    ["cmd1","-btest"] === Cmd1 ["test"]
    ["cmd2","-b14"] === Cmd2 14

