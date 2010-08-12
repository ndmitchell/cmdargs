{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module System.Console.CmdArgs.Test.Implicit.Tests where
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit(modeHelp)
import System.Console.CmdArgs.Test.Implicit.Util

test = test1 >> test2 >> test3 >> test4 >> test5 >> test6
demos = zipWith f [1..] [toDemo mode1, toDemo mode2, toDemo mode3, toDemo mode4, toDemo mode5, toDemo mode6]
    where f i x = x{modeHelp = "Testing various corner cases (" ++ show i ++ ")"}


-- from bug #256 and #231
data Test1
    = Test1 {maybeInt :: Maybe Int, listDouble :: [Double], maybeStr :: Maybe String, float :: Float
            ,bool :: Bool, maybeBool :: Maybe Bool, listBool :: [Bool]}
      deriving (Show,Eq,Data,Typeable)

def1 = Test1 def def def (def &= args) def def def
mode1 = cmdArgsMode $ def1

test1 = do
    let Tester{fails,(===)} = tester "Test1" mode1
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
    fails ["--listbool=fred"]


-- from bug #230
data Test2 = Cmd1 {bs :: [String]}
           | Cmd2 {bar :: Int}
             deriving (Show, Eq, Data, Typeable)

mode2 = cmdArgsMode $ modes [Cmd1 [], Cmd2 42]

test2 = do
    let Tester{fails,(===)} = tester "Test2" mode2
    fails []
    ["cmd1","-btest"] === Cmd1 ["test"]
    ["cmd2","-b14"] === Cmd2 14


-- various argument position
data Test3 = Test3 {pos1_1 :: [Int], pos1_2 :: [String], pos1_rest :: [String]}
             deriving (Show, Eq, Data, Typeable)

mode3 = cmdArgsMode $ Test3 (def &= argPos 1) (def &= argPos 2 &= opt "foo") (def &= args)

test3 = do
    let Tester{fails,(===)} = tester "Test3" mode3
    fails []
    fails ["a"]
    ["a","1"] === Test3 [1] ["foo"] ["a"]
    ["a","1","c"] === Test3 [1] ["c"] ["a"]
    ["a","1","c","d"] === Test3 [1] ["c"] ["a","d"]


-- from bug #222
data Test4 = Test4 {test_4 :: [String]}
             deriving (Show, Eq, Data, Typeable)

mode4 = cmdArgsMode $ Test4 (def &= opt "hello" &= args)

test4 = do
    let Tester{(===)} = tester "Test4" mode4
    [] === Test4 ["hello"]
    ["a"] === Test4 ["a"]
    ["a","b"] === Test4 ["a","b"]


-- from #292, automatic enumerations
data ABC = Abacus | Arbitrary | B | C deriving (Eq,Show,Data,Typeable)
data Test5 = Test5 {choice :: ABC} deriving (Eq,Show,Data,Typeable)

mode5 = cmdArgsMode $ Test5 B

test5 = do
    let Tester{(===),fails} = tester "Test5" mode5
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
    let Tester{(===),fails} = tester "Test6" mode6
    [] === val6
    ["--val1=1,True"] === val6{val1=(1,True)}
    ["--val1=84,off"] === val6{val1=(84,False)}
    fails ["--val1=84"]
    fails ["--val1=84,off,1"]
    ["--val2=1,2,3","--val2=5,6,7"] === val6{val2=[(1,("2",3)),(5,("6",7))]}
