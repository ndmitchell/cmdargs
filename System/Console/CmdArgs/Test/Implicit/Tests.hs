{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TemplateHaskell, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-unused-binds #-}

module System.Console.CmdArgs.Test.Implicit.Tests(test, demos) where

import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit(modeHelp)
import System.Console.CmdArgs.Test.Implicit.Util
import System.Console.CmdArgs.Quote
import Data.Int


-- from bug #256 and #231
data Test1
    = Test1 {maybeInt :: Maybe Int, listDouble :: [Double], maybeStr :: Maybe String, float :: Float
            ,bool :: Bool, maybeBool :: Maybe Bool, listBool :: [Bool], int64 :: Int64}
      deriving (Show,Eq,Data,Typeable)

def1 = Test1 def def def (def &= args) def def def def
mode1 = cmdArgsMode def1

$(cmdArgsQuote [d|
    mode1_ = cmdArgsMode# def1_
    def1_ = Test1 def def def (def &=# args) def def def def
    |])

test1 = do
    let Tester{..} = testers "Test1" [mode1,mode1_]
    [] === def1
    ["--maybeint=12"] === def1{maybeInt = Just 12}
    ["--maybeint=12","--maybeint=14"] === def1{maybeInt = Just 14}
    fails ["--maybeint"]
    fails ["--maybeint=test"]
    ["--listdouble=1","--listdouble=3","--listdouble=2"] === def1{listDouble=[1,3,2]}
    fails ["--maybestr"]
    ["--maybestr="] === def1{maybeStr=Just ""}
    ["--maybestr=test"] === def1{maybeStr=Just "test"}
    ["12.5"] === def1{float=12.5}
    ["12.5","18"] === def1{float=18}
    ["--bool"] === def1{bool=True}
    ["--maybebool"] === def1{maybeBool=Just True}
    ["--maybebool=off"] === def1{maybeBool=Just False}
    ["--listbool","--listbool=true","--listbool=false"] === def1{listBool=[True,True,False]}
    ["--int64=12"] === def1{int64=12}
    fails ["--listbool=fred"]
    invalid $ \_ -> def1{listBool = def &= opt "yes"}


-- from bug #230
data Test2 = Cmd1 {bs :: [String]}
           | Cmd2 {bar :: Int}
             deriving (Show, Eq, Data, Typeable)

mode2 = cmdArgsMode $ modes [Cmd1 [], Cmd2 42]

test2 = do
    let Tester{..} = tester "Test2" mode2
    fails []
    ["cmd1","-btest"] === Cmd1 ["test"]
    ["cmd2","-b14"] === Cmd2 14


-- various argument position
data Test3 = Test3 {pos1_1 :: [Int], pos1_2 :: [String], pos1_rest :: [String]}
             deriving (Show, Eq, Data, Typeable)

mode3 = cmdArgsMode $ Test3 (def &= argPos 1) (def &= argPos 2 &= opt "foo") (def &= args)

$(cmdArgsQuote [d| mode3_ = cmdArgsMode# $ Test3 (def &=# argPos 1) (def &=# argPos 2 &=# opt "foo") (def &=# args) |])


test3 = do
    let Tester{..} = testers "Test3" [mode3,mode3_]
    fails []
    fails ["a"]
    ["a","1"] === Test3 [1] ["foo"] ["a"]
    ["a","1","c"] === Test3 [1] ["c"] ["a"]
    ["a","1","c","d"] === Test3 [1] ["c"] ["a","d"]
    invalid $ \_ -> Test3 def def (def &= help "help" &= args)


-- from bug #222
data Test4 = Test4 {test_4 :: [String]}
             deriving (Show, Eq, Data, Typeable)

mode4 = cmdArgsMode $ Test4 (def &= opt "hello" &= args)

test4 = do
    let Tester{..} = tester "Test4" mode4
    [] === Test4 ["hello"]
    ["a"] === Test4 ["a"]
    ["a","b"] === Test4 ["a","b"]


-- from #292, automatic enumerations
data ABC = Abacus | Arbitrary | B | C deriving (Eq,Show,Data,Typeable)
data Test5 = Test5 {choice :: ABC} deriving (Eq,Show,Data,Typeable)

mode5 = cmdArgsMode $ Test5 B

test5 = do
    let Tester{..} = tester "Test5" mode5
    [] === Test5 B
    fails ["--choice=A"]
    ["--choice=c"] === Test5 C
    ["--choice=C"] === Test5 C
    ["--choice=Aba"] === Test5 Abacus
    ["--choice=abacus"] === Test5 Abacus
    ["--choice=c","--choice=B"] === Test5 B

-- tuple support
data Test6 = Test6 {val1 :: (Int,Bool), val2 :: [(Int,(String,Double))]} deriving (Eq,Show,Data,Typeable)
val6 = Test6 def def

mode6 = cmdArgsMode val6

test6 = do
    let Tester{..} = tester "Test6" mode6
    [] === val6
    ["--val1=1,True"] === val6{val1=(1,True)}
    ["--val1=84,off"] === val6{val1=(84,False)}
    fails ["--val1=84"]
    fails ["--val1=84,off,1"]
    ["--val2=1,2,3","--val2=5,6,7"] === val6{val2=[(1,("2",3)),(5,("6",7))]}

-- from #333, add default fields
data Test7 = Test71 {shared :: Int}
           | Test72 {unique :: Int, shared :: Int}
           | Test73 {unique :: Int, shared :: Int}
             deriving (Eq,Show,Data,Typeable)

mode7 = cmdArgsMode $ modes [Test71{shared = def &= name "rename"}, Test72{unique=def}, Test73{}]

test7 = do
    let Tester{..} = tester "Test7" mode7
    fails []
    ["test71","--rename=2"] === Test71 2
    ["test72","--rename=2"] === Test72 0 2
    ["test72","--unique=2"] === Test72 2 0
    ["test73","--rename=2"] === Test73 0 2
    ["test73","--unique=2"] === Test73 2 0

-- from #252, grouping
data Test8 = Test8 {test8a :: Int, test8b :: Int, test8c :: Int}
           | Test81
           | Test82
             deriving (Eq,Show,Data,Typeable)

mode8 = cmdArgsMode $ modes [Test8 1 (2 &= groupname "More flags") 3 &= groupname "Mode1", Test81, Test82 &= groupname "Mode2"]
mode8_ = cmdArgsMode_ $ modes_ [record Test8{} [atom (1::Int), atom (2::Int) += groupname "More flags", atom (3::Int)] += groupname "Mode1"
                               ,record Test81{} []
                               ,record Test82{} [] += groupname "Mode2"]

test8 = do
    let Tester{..} = testers "Test8" [mode8,mode8_]
    isHelp ["-?"] ["Flags:","  --test8a=INT","More flags:","  --test8b=INT"]
    fails []
    ["test8","--test8a=18"] === Test8 18 2 3

-- bug from Sebastian Fischer, enums with multiple fields
data XYZ = X | Y | Z deriving (Eq,Show,Data,Typeable)
data Test9 = Test91 {foo :: XYZ}
           | Test92 {foo :: XYZ}
             deriving (Eq,Show,Data,Typeable)

mode9 = cmdArgsMode $ modes [Test91 {foo = enum [X &= help "pick X (default)", Y &= help "pick Y"]} &= auto, Test92{}]
mode9_ = cmdArgsMode_ $ modes_ [record Test91{} [enum_ foo [atom X += help "pick X (default)", atom Y += help "pick Y"]] += auto, record Test92{} []]

test9 = do
    let Tester{..} = testers "Test9" [mode9,mode9_]
    [] === Test91 X
    ["test91","-x"] === Test91 X
    ["test91","-y"] === Test91 Y
    fails ["test91","-z"]
    ["test92","-x"] === Test92 X
    ["test92","-y"] === Test92 Y
    ["test92"] === Test92 X
    invalid $ \_ -> modes [Test91 {foo = enum [X &= help "pick X (default)"] &= opt "X"}]

-- share common fields in the help message
data Test10 = Test101 {food :: Int}
            | Test102 {food :: Int, bard :: Int}
              deriving (Eq,Show,Data,Typeable)

mode10 = cmdArgsMode $ modes [Test101 def, Test102 def def]

test10 = do
    let Tester{..} = tester "Test10" mode10
    isHelp ["-?=one"] ["  -f --food=INT"]
    isHelpNot ["-?=one"] ["  -b --bard=INT"]

-- test for GHC over-optimising
data Test11 = Test11A {test111 :: String}
            | Test11B {test111 :: String}
              deriving (Eq,Show,Data,Typeable)

test11A = Test11A { test111 = def &= argPos 0 }
test11B = Test11B { test111 = def &= argPos 0 }
mode11 = cmdArgsMode $ modes [test11A, test11B]

mode11_ = cmdArgsMode_ $ modes_
    [record Test11A{} [test111 := def += argPos 0]
    ,record Test11B{} [test111 := def += argPos 0]]

test11 = do
    let Tester{..} = testers "Test11" [mode11,mode11_]
    fails []
    ["test11a","test"] === Test11A "test"
    ["test11b","test"] === Test11B "test"


-- #351, check you can add name annotations to modes
data Test12 = Test12A | Test12B deriving (Eq,Show,Data,Typeable)

mode12 = cmdArgsMode $ modes [Test12A &= name "check", Test12B]
mode12_ = cmdArgsMode $ modes [Test12A &= name "check" &= explicit, Test12B]

test12 = do
    let Tester{..} = tester "Test12" mode12
    fails []
    ["test12a"] === Test12A
    ["check"] === Test12A
    ["test12b"] === Test12B
    fails ["t"]
    let Tester{..} = tester "Test12" mode12_
    fails []
    fails ["test12a"]
    ["check"] === Test12A
    ["test12b"] === Test12B
    ["t"] === Test12B


-- the ignore annotation and versionArg [summary]
data Test13 = Test13A {foo13 :: Int, bar13 :: Either Int Int}
            | Test13B {foo13 :: Int}
            | Test13C {foo13 :: Int}
              deriving (Eq,Show,Data,Typeable)

mode13 = cmdArgsMode $ modes [Test13A 1 (Left 1 &= ignore), Test13B 1 &= ignore, Test13C{}]
                       &= versionArg [summary "Version text here"]
                       &= summary "Help text here"

test13 = do
    let Tester{..} = tester "Test13" mode13
    fails ["test13b"]
    fails ["test13a --bar13=1"]
    ["test13a","--foo13=13"] === Test13A 13 (Left 1)
    ["test13c","--foo13=13"] === Test13C 13
    isHelp ["--help"] ["Help text here"]
    isVersion ["--version"] "Version text here"
    fails ["--numeric-version"]

-- check a list becomes modes not an enum
data Test14 = Test14A | Test14B | Test14C deriving (Eq,Show,Data,Typeable)

mode14 = cmdArgsMode $ modes [Test14A, Test14B, Test14C]

test14 = do
    let Tester{..} = tester "Test14" mode14
    fails []
    ["test14a"] === Test14A
    fails ["--test14a"]

-- custom help flags
data Test15 = Test15 {test15a :: Bool} deriving (Eq,Show,Data,Typeable)

mode15 = cmdArgsMode $ Test15 (False &= name "help")
         &= helpArg [groupname "GROUP", name "h", name "nohelp", explicit, help "whatever"] &= versionArg [ignore]
         &= verbosityArgs [ignore] [explicit,name "silent"]

$(cmdArgsQuote [d|
    mode15_ = cmdArgsMode# $ Test15 (False &=# name "help")
              &=# helpArg [groupname "GROUP", name "h", name "nohelp", explicit, help "whatever"] &=# versionArg [ignore]
              &=# verbosityArgs [ignore] [explicit,name "silent"]
    |])

test15 = do
    let Tester{..} = testers "Test15" [mode15,mode15_]
    invalid $ \_ -> Test15 (False &= name "help")
    ["--help"] === Test15 True
    ["-t"] === Test15 True
    fails ["-?"]
    isHelp ["--nohelp"] ["  -h --nohelp  whatever"]
    isHelp ["-h"] []
    isHelp ["-h"] ["GROUP:"]
    fails ["--version"]
    fails ["--verbose"]
    fails ["--quiet"]
    isVerbosity ["--help","--silent"] Quiet

-- check newtype support
newtype MyInt = MyInt Int deriving (Eq,Show,Data,Typeable)

data Test16 = Test16 {test16a :: MyInt, test16b :: [MyInt]} deriving (Eq,Show,Data,Typeable)

mode16 = cmdArgsMode $ Test16 (MyInt 12) [] &= summary "The Glorious Glasgow Haskell Compilation System, version 7.6.3"

test16 = do
    let Tester{..} = tester "Test16" mode16
    [] === Test16 (MyInt 12) []
    isVersion ["--numeric-version"] "7.6.3"
    fails ["--test16a"]
    ["--test16a=5"] === Test16 (MyInt 5) []
    ["--test16b=5","--test16b=82"] === Test16 (MyInt 12) [MyInt 5, MyInt 82]

-- #552, @ directives not expanded after -- symbols
-- not actually checked because this path doesn't go through processArgs
data Test17 = Test17 {test17_ :: [String]} deriving (Eq,Show,Data,Typeable)

mode17 = cmdArgsMode $ Test17 ([] &= args) &= noAtExpand &= summary "bzip2 3.5-windows version"

test17 = do
    let Tester{..} = tester "Test17" mode17
    [] === Test17 []
    ["test","of","this"] === Test17 ["test","of","this"]
    ["test","--","@foo"] === Test17 ["test","@foo"]
    isVersion ["--numeric-version"] "3.5-windows"


data Debuggable = This | That deriving (Eq,Show,Data,Typeable)
data Test18 = Test18 {test18_ :: [Debuggable]} deriving (Eq,Show,Data,Typeable)

mode18 = cmdArgsMode $ Test18 $ enum [[] &= ignore, [This] &= name "debug-this", [That] &= name "debug-that"]

test18 = do
    let Tester{..} = tester "Test18" mode18
    [] === Test18 []
    ["--debug-this","--debug-that","--debug-this"] === Test18 [This,That,This]

-- #610, check performance for long lists (took ~20s before)

data Test19 = Test19 {test19_ :: [String]} deriving (Eq,Show,Data,Typeable)

mode19 = cmdArgsMode $ Test19 ([] &= args)

test19 = do
    let Tester{..} = tester "Test19" mode19
    let args = map show [1..1000]
    args === Test19 args


-- #615, newtype wrappers of lists/Maybe should accumulate properly

newtype Test20A = Test20A [String] deriving (Eq,Show,Data,Typeable)
data Test20 = Test20 {test20_ :: Test20A} deriving (Eq,Show,Data,Typeable)

mode20 = cmdArgsMode $ Test20 (Test20A [] &= args)

test20 = do
    let Tester{..} = tester "Test20" mode20
    ["a","b","c"] === Test20 (Test20A ["a","b","c"])


-- #626, don't reverse values too much

newtype Test21A = Test21A [String] deriving (Eq,Show,Data,Typeable)
data Test21 = Test21 {test21A :: Test21A, test21B :: [String], test21C :: [Int]} deriving (Eq,Show,Data,Typeable)

mode21 = cmdArgsMode $ Test21 (Test21A ["a","b","c"]) ["A","B","C"] [1,2,3]

test21 = do
    let Tester{..} = tester "Test21" mode21
    [] === Test21 (Test21A ["a","b","c"]) ["A","B","C"] [1,2,3]


-- For some reason, these must be at the end, otherwise the Template Haskell
-- stage restriction kicks in.

test = test1 >> test2 >> test3 >> test4 >> test5 >> test6 >> test7 >> test8 >> test9 >> test10 >>
       test11 >> test12 >> test13 >> test14 >> test15 >> test16 >> test18 >> test19 >> test20 >>
       test21
demos = zipWith f [1..]
        [toDemo mode1, toDemo mode2, toDemo mode3, toDemo mode4, toDemo mode5, toDemo mode6
        ,toDemo mode7, toDemo mode8, toDemo mode9, toDemo mode10, toDemo mode11, toDemo mode12
        ,toDemo mode13, toDemo mode14, toDemo mode15, toDemo mode16, toDemo mode17, toDemo mode18
        ,toDemo mode19, toDemo mode20, toDemo mode21]
    where f i x = x{modeHelp = "Testing various corner cases (" ++ show i ++ ")"}
