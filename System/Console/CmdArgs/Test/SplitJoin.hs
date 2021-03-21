{-# LANGUAGE QuasiQuotes #-}

module System.Console.CmdArgs.Test.SplitJoin(test) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import Control.Monad



test = do
    forM_ tests $ \(src,parsed) -> do
        let a = splitArgs src
            b1 = joinArgs parsed
            b2 = joinArgs $ splitArgs b1
        if a == parsed then return () else failure "splitArgs" [("Given   ",src),("Expected",show parsed),("Found   ",show a)]
        if b1 == b2 then return () else failure "joinArgs" [("Given   ",show parsed),("Expected",b1),("Found   ",b2)]
    success

{-
newtype CmdLine = CmdLine String deriving Show

instance Arbitrary CmdLine where
    arbitrary = fmap CmdLine $ listOf $ elements "abcd \\/\'\""

generateTests :: IO ()
generateTests = withTempFile $ \src -> do
    writeFile src "import System.Environment\nmain = print =<< getArgs\n"
    quickCheckWith stdArgs{chatty=False} $ \(CmdLine x) -> unsafePerformIO $ do
        putStr $ ",(,) " ++ (show x) ++ " "
        system $ "runghc \"" ++ src ++ "\" " ++ x
        return True


withTempFile :: (FilePath -> IO a) -> IO a
withTempFile f = bracket
    (do (file,h) <- openTempFile "." "cmdargs.hs"; hClose h; return file)
    removeFile
    f
-}

-- Pregenerate the QuickCheck tests and run them through the system console
-- Not done each time for three reasons
-- * Avoids an extra dependency on QuickCheck + process
-- * Slow to run through the command line
-- * Can't figure out how to read the output, without adding more escaping (which breaks the test)
tests =
    [f "" []
    ,f "c" ["c"]
    ,f "b" ["b"]
    ,f "\\" ["\\"]
    ,f "'//" ["'//"]
    ,f "a" ["a"]
    ,f "cda" ["cda"]
    ,f "b'" ["b'"]
    ,f "" []
    ,f " " []
    ,f "/b" ["/b"]
    ,f "\"b/\"d a'b'b" ["b/d","a'b'b"]
    ,f "d'c a\"/\\" ["d'c","a/\\"]
    ,f "d" ["d"]
    ,f "bb' " ["bb'"]
    ,f "b'\\" ["b'\\"]
    ,f "\"\\ac" ["\\ac"]
    ,f "\\'\"abbb\"c/''' \\ c" ["\\'abbbc/'''","\\","c"]
    ,f "/bbdbb a " ["/bbdbb","a"]
    ,f "b\" d" ["b d"]
    ,f "" []
    ,f "\\cc/''\\b\\ccc\\'\\b\\" ["\\cc/''\\b\\ccc\\'\\b\\"]
    ,f "/" ["/"]
    ,f "///\"b\\c/b\"cd//c'\"" ["///b\\c/bcd//c'"]
    ,f "\\\"d\\\\' /d\\\\/bb'a /\\d" ["\"d\\\\'","/d\\\\/bb'a","/\\d"]
    ,f "c/ \\''/c b\\'" ["c/","\\''/c","b\\'"]
    ,f "dd'b\\\\\\' /c'aaa\"" ["dd'b\\\\\\'","/c'aaa"]
    ,f "b'd''\\/ b\\'b'db/'cd " ["b'd''\\/","b\\'b'db/'cd"]
    ,f "a\"ba\\/\\ " ["aba\\/\\ "]
    ,f "b\"'dd'c /b/c\"bbd \"\"\\ad'\"c\\\"" ["b'dd'c /b/cbbd","\\ad'c\""]
    ,f "da 'c\\\\acd/'dbaaa///dccbc a \\" ["da","'c\\\\acd/'dbaaa///dccbc","a","\\"]
    ,f "a'ac \"da\"" ["a'ac","da"]
    ,f "\"'\\\"/\"\"b\\b  \"'\"\"ccd'a\"/c /da " ["'\"/\"b\\b","'\"ccd'a/c /da "]
    ,f "d\"\\c\\\\cb c/\"aa' b\"\\/d \"'c c/" ["d\\c\\\\cb c/aa'","b\\/d 'c","c/"]
    ,f "dbc\\/\"\"//c/\"accda" ["dbc\\///c/accda"]
    ,f "aca a'' \\ c b'\\/d\\" ["aca","a''","\\","c","b'\\/d\\"]
    ,f "dc\"bc/a\\ccdd\\\\aad\\c'ab '\\cddcdba" ["dcbc/a\\ccdd\\\\aad\\c'ab '\\cddcdba"]
    ,f " c'\"ba \"b\\dc\"" ["c'ba b\\dc"]
    ,f "a\\acd/a \"'c /'c'" ["a\\acd/a","'c /'c'"]
    ,f " ac ddc/\"\"a/\\bd\\d c'cac\"c\\a/a''c" ["ac","ddc/a/\\bd\\d","c'cacc\\a/a''c"]
    ,f "b/cd\"//bb\"/daaab/  b b \"'     d\"a\" 'd b" ["b/cd//bb/daaab/","b","b","'     da 'd b"]
    ,f "a\"cc'cd\"\\'ad '\"dcc acb\"\\\\" ["acc'cd\\'ad","'dcc acb\\\\"]
    ,f "/bc/bc'/\"d  \"a/\"\\ad aba\\da" ["/bc/bc'/d  a/\\ad aba\\da"]
    ,f "b\\a" ["b\\a"]
    ,f "/dc ''c'a\"'/'\\ /'cd\\'d/'db/b\"' cabacaaa\"\"dd" ["/dc","''c'a'/'\\ /'cd\\'d/'db/b'","cabacaaadd"]
    ,f "\"ac\\\"c'/c'b\"b\"b'd\"c\"\"" ["ac\"c'/c'bbb'dc"]
    ,f "/ 'ccc\"d\\dc'\"'\\  b" ["/","'cccd\\dc''\\","b"]
    ,f "  '\"/\\cc\\/c '\\\\" ["'/\\cc\\/c '\\\\"]
    ,f "\\ \\' ' /d  \"cc\\\\//da\"d'a/a\"ca\\\\\"\\cb c\"d'b 'acb" ["\\","\\'","'","/d","cc\\\\//dad'a/aca\\\\cb","cd'b 'acb"]
    ,f "a\"\"d'\"a\"\\ \\c db'da/d\\c\"a/ aa c/db" ["ad'a\\","\\c","db'da/d\\ca/ aa c/db"]
    ,f " d\\" ["d\\"]
    ,f "d c b'/\\/'\"/'a'aa\"a\"/ad\\/" ["d","c","b'/\\/'/'a'aaa/ad\\/"]
    ,f "  a \\' /" ["a","\\'","/"]
    ,f "'/ c" ["'/","c"]
    ,f "acd 'bcab /ba'daa'/ba/\"dcdadbcacb" ["acd","'bcab","/ba'daa'/ba/dcdadbcacb"]
    ,f "a\\\"dd'a c\"a\"\"ac\\" ["a\"dd'a","ca\"ac\\"]
    ,f "\"dba /'bb\\ d ba '/c' \"dd\\' cbcd c /b/\\b///" ["dba /'bb\\ d ba '/c' dd\\'","cbcd","c","/b/\\b///"]
    ,f "a'c/c \"ccb '/d\\abd/bc  " ["a'c/c","ccb '/d\\abd/bc  "]
    ,f "\\da\"\\//add\\\\ c" ["\\da\\//add\\\\ c"]
    ,f "c/\\\"//  a/\"ac\"//''ba\"c/\\bc\\\"d\"bc/d" ["c/\"//","a/ac//''bac/\\bc\"dbc/d"]
    ,f "/d/ a   dc'\\ \"" ["/d/","a","dc'\\",""]
    ,f " \"dc//b\\cd/ \\ac\"b\"b\"d\"\"\"dd\"\" ' a\\'/ \"/'/\\a/abd\\ddd" ["dc//b\\cd/ \\acbbd\"dd","'","a\\'/","/'/\\a/abd\\ddd"]
    ,f "\\'  ' d\"b bbc" ["\\'","'","db bbc"]
    ,f "'ba\\a'db/bd d\\'b\\ \\/a'da' " ["'ba\\a'db/bd","d\\'b\\","\\/a'da'"]
    ,f "\\b\\cc\"\"d' dd ddcb\"d" ["\\b\\ccd'","dd","ddcbd"]
    ,f "d\"dc'\\d\"/'\\\"b\\c'c\" db' \\'b/\"a' / da'\"/ab'\\ c\\bc\\//dbcb\\" ["ddc'\\d/'\"b\\c'c db' \\'b/a'","/","da'/ab'\\ c\\bc\\//dbcb\\"]
    ,f " b ddbbbbc\"da\\c\"'\\" ["b","ddbbbbcda\\c'\\"]
    ,f "b/\"d dacd'/'\\\"''a a /'\\c'b ab\\  dda\\c'abdd'a\"//d \\\\\\ d\"\"" ["b/d dacd'/'\"''a a /'\\c'b ab\\  dda\\c'abdd'a//d","\\\\\\","d"]
    ,f "/c\"\" dd'a'/b\\/'\"'/" ["/c","dd'a'/b\\/''/"]
    ,f "/\"'\"\"'cc a a\\dd''\\'b" ["/'\"'cc","a","a\\dd''\\'b"]
    ,f "c\"dcd''aba\" \" /'" ["cdcd''aba"," /'"]
    ,f "'\"/''\\\\d'/ad\\baadabdca\\ /\\'''bd\\/\"'/' aca \\  \\a'\\ cd\"d /bdcd''cac" ["'/''\\\\d'/ad\\baadabdca\\ /\\'''bd\\/'/'","aca","\\","\\a'\\","cdd /bdcd''cac"]
    ,f "\" /\"da" [" /da"]
    ,f "'\"ca/'d/d/d\\ca\"/\"\" ddac cc\" ''a c''bd\"bc'dc\\/\"b\"a\\\"\"a/\\ " ["'ca/'d/d/d\\ca/","ddac","cc ''a c''bdbc'dc\\/ba\"a/\\ "]
    ,f "\\\\d'ad ' ''\"cd/a \"\"\\'\\\"'dc\\" ["\\\\d'ad","'","''cd/a \"\\'\"'dc\\"]
    ,f " ab  c'\\a" ["ab","c'\\a"]
    ,f "b" ["b"]
    ,f "''c dc c\\'d'ab'd\"\\\"cca\"b'da\"dbcdbd\"cd'/d \\cd'\"d  \"\"b cdc''/\\\"b'" ["''c","dc","c\\'d'ab'd\"ccab'dadbcdbdcd'/d","\\cd'd  \"b","cdc''/\"b'"]
    ,f " \"'cb dbddbdd/" ["'cb dbddbdd/"]
    ,f "a/\"d// dd/cc/\"cc\"d\" d\\/a a \\c\"  \\\\/\"\\ bcc'ac'\"\\c//d\"da/\\aac\\b\"c/'b\"\"bbd/\\" ["a/d// dd/cc/ccd","d\\/a","a","\\c  \\\\/\\","bcc'ac'\\c//dda/\\aac\\bc/'b\"bbd/\\"]
    ,f "b\"ddccd\"a\"/ba\"" ["bddccda/ba"]
    ,f " \"  c/b/'/bdd  cb d'c a'\"'a d\\\\db//\\\"' c'/'c\\/aa" ["  c/b/'/bdd  cb d'c a''a","d\\\\db//\"'","c'/'c\\/aa"]
    ,f "\\caab" ["\\caab"]
    ,f "bb\"'\"/d'bad 'd\\/'\\b//\\\\ \\d''c\"c b\\b/\\" ["bb'/d'bad","'d\\/'\\b//\\\\","\\d''cc b\\b/\\"]
    ,f " c'a\"  \\cab\"bd\"dcd\"/cb/\"\"b\"b'\"d" ["c'a  \\cabbddcd/cb/bb'd"]
    ,f "\\/ \"c'ca" ["\\/","c'ca"]
    ,f "  d' /c'bc\"'/'\\\\dca'cc\"'\"''/d cb//'a \"bd ab\"dcaadc\\\"'d\\\"/a\"a\\\"ba//b/ d/dbac/d\\caa\"bc/ " ["d'","/c'bc'/'\\\\dca'cc'''/d cb//'a bd","abdcaadc\"'d\"/aa\"ba//b/","d/dbac/d\\caabc/ "]
    ,f "/\"\\db'd/ ca\"ad b\\\\\"cd/a bbc\\ " ["/\\db'd/ caad","b\\cd/a bbc\\ "]
    ,f "cdc bd'/\"c''c d \\\"aa \\d\\ bb'b/ /b/a/c'acda\\'\"\"c \"bbbaa/'/a \\aca\"'/ac' " ["cdc","bd'/c''c d \"aa \\d\\ bb'b/ /b/a/c'acda\\'\"c","bbbaa/'/a \\aca'/ac'"]
    ,f "ad/'b\\d /cc\"\"ab \\ \"'  ''b\\\"/\\  a\"'d\"\\ddacdbbabb b b  //' acd\"c\\d'd\\b\"'\\\"aaba/bda/c'// \\b" ["ad/'b\\d","/ccab","\\","'  ''b\"/\\  a'd\\ddacdbbabb b b  //' acdc\\d'd\\b'\"aaba/bda/c'// \\b"]
    ,f "bac cc \"ac\"/ca/ '\"\" b/b d /cd'\\'bb\" \\ \"b '/ b c ' c''\"a/ad\\ " ["bac","cc","ac/ca/","'","b/b","d","/cd'\\'bb \\ b","'/","b","c","'","c''a/ad\\ "]
    ,f "baa'  b'b''\\dab/'c" ["baa'","b'b''\\dab/'c"]
    ,f "cb\\\\ " ["cb\\\\"]
    ,f "/b'a''d\"b\"   'c'b ba\\'b\" bb" ["/b'a''db","'c'b","ba\\'b bb"]
    ,f "b /\"ca\\cbac " ["b","/ca\\cbac "]
    ,f " \"\"/\"bcaa\"\"a' \\/bb \"a\\\"'\"" ["/bcaa\"a'","\\/bb","a\"'"]
    ,f "\"c /''c\"\\badc/\\daa/\\ c\"a c\\ \\/cab \"b\"\\ ba\"\"/d/cd'a ad'c/ad\"' a\\d/d\\c\\'cdccd/\"a'/\"b///ac\"" ["c /''c\\badc/\\daa/\\","ca c\\ \\/cab b\\ ba\"/d/cd'a","ad'c/ad' a\\d/d\\c\\'cdccd/a'/b///ac"]
    ,f "/cbbd\"/b' /dd\"/c\\ca/'\"\\ cc  \\d\"aca/\"b caa\\d\\'\"b'b  dc\"cd\\'c\" 'd/ac\"cacc\"" ["/cbbd/b' /dd/c\\ca/'\\ cc  \\daca/b caa\\d\\'b'b","dccd\\'c","'d/accacc"]
    ,f "bc/bd\\ca\\bcacca\"\"\\c/\\ /\"\"a/\"c'//b'\\d/a/'ab/cbd/cacb//b \\d\"aac\\d'\"/" ["bc/bd\\ca\\bcacca\\c/\\","/a/c'//b'\\d/a/'ab/cbd/cacb//b \\daac\\d'/"]
    ,f "bbac bdc/d\\\"/db\"dbdb\"a \" /\"/'a\\acacbcc c'//\\//b\"ca\"bcca c\\/aaa/c/bccbccaa  \"\" cdccc/bddcbc c''" ["bbac","bdc/d\"/dbdbdba"," //'a\\acacbcc","c'//\\//bcabcca","c\\/aaa/c/bccbccaa","","cdccc/bddcbc","c''"]
    ]
    where f = (,)
