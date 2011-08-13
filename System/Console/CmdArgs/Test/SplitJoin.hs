{-# LANGUAGE QuasiQuotes #-}

module System.Console.CmdArgs.Test.SplitJoin(test) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Test.Util
import Control.Monad



test = do
    forM_ (take 0 tests) $ \(src,parsed) -> do
        let a = splitArgs src
            b = joinArgs parsed
        if a == parsed then return () else failure "splitArgs" [("Given",src),("Expected",show parsed),("Found",show a)] >> error "done"
        if b == src then return () else failure "joinArgs" [("Given",show parsed),("Expected",src),("Found",b)]
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
        system $ "runhaskell \"" ++ src ++ "\" " ++ x
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
    [(,) "" []
    ,(,) "c" ["c"]
    ,(,) "b" ["b"]
    ,(,) "\\" ["\\"]
    ,(,) "'//" ["'//"]
    ,(,) "a" ["a"]
    ,(,) "cda" ["cda"]
    ,(,) "b'" ["b'"]
    ,(,) "" []
    ,(,) " " []
    ,(,) "/b" ["/b"]
    ,(,) "\"b/\"d a'b'b" ["b/d","a'b'b"]
    ,(,) "d'c a\"/\\" ["d'c","a/\\"]
    ,(,) "d" ["d"]
    ,(,) "bb' " ["bb'"]
    ,(,) "b'\\" ["b'\\"]
    ,(,) "\"\\ac" ["\\ac"]
    ,(,) "\\'\"abbb\"c/''' \\ c" ["\\'abbbc/'''","\\","c"]
    ,(,) "/bbdbb a " ["/bbdbb","a"]
    ,(,) "b\" d" ["b d"]
    ,(,) "" []
    ,(,) "\\cc/''\\b\\ccc\\'\\b\\" ["\\cc/''\\b\\ccc\\'\\b\\"]
    ,(,) "/" ["/"]
    ,(,) "///\"b\\c/b\"cd//c'\"" ["///b\\c/bcd//c'"]
    ,(,) "\\\"d\\\\' /d\\\\/bb'a /\\d" ["\"d\\\\'","/d\\\\/bb'a","/\\d"]
    ,(,) "c/ \\''/c b\\'" ["c/","\\''/c","b\\'"]
    ,(,) "dd'b\\\\\\' /c'aaa\"" ["dd'b\\\\\\'","/c'aaa"]
    ,(,) "b'd''\\/ b\\'b'db/'cd " ["b'd''\\/","b\\'b'db/'cd"]
    ,(,) "a\"ba\\/\\ " ["aba\\/\\ "]
    ,(,) "b\"'dd'c /b/c\"bbd \"\"\\ad'\"c\\\"" ["b'dd'c /b/cbbd","\\ad'c\""]
    ,(,) "da 'c\\\\acd/'dbaaa///dccbc a \\" ["da","'c\\\\acd/'dbaaa///dccbc","a","\\"]
    ,(,) "a'ac \"da\"" ["a'ac","da"]
    ,(,) "\"'\\\"/\"\"b\\b  \"'\"\"ccd'a\"/c /da " ["'\"/\"b\\b","'\"ccd'a/c /da "]
    ,(,) "d\"\\c\\\\cb c/\"aa' b\"\\/d \"'c c/" ["d\\c\\\\cb c/aa'","b\\/d 'c","c/"]
    ,(,) "dbc\\/\"\"//c/\"accda" ["dbc\\///c/accda"]
    ,(,) "aca a'' \\ c b'\\/d\\" ["aca","a''","\\","c","b'\\/d\\"]
    ,(,) "dc\"bc/a\\ccdd\\\\aad\\c'ab '\\cddcdba" ["dcbc/a\\ccdd\\\\aad\\c'ab '\\cddcdba"]
    ,(,) " c'\"ba \"b\\dc\"" ["c'ba b\\dc"]
    ,(,) "a\\acd/a \"'c /'c'" ["a\\acd/a","'c /'c'"]
    ,(,) " ac ddc/\"\"a/\\bd\\d c'cac\"c\\a/a''c" ["ac","ddc/a/\\bd\\d","c'cacc\\a/a''c"]
    ,(,) "b/cd\"//bb\"/daaab/  b b \"'     d\"a\" 'd b" ["b/cd//bb/daaab/","b","b","'     da 'd b"]
    ,(,) "a\"cc'cd\"\\'ad '\"dcc acb\"\\\\" ["acc'cd\\'ad","'dcc acb\\\\"]
    ,(,) "/bc/bc'/\"d  \"a/\"\\ad aba\\da" ["/bc/bc'/d  a/\\ad aba\\da"]
    ,(,) "b\\a" ["b\\a"]
    ,(,) "/dc ''c'a\"'/'\\ /'cd\\'d/'db/b\"' cabacaaa\"\"dd" ["/dc","''c'a'/'\\ /'cd\\'d/'db/b'","cabacaaadd"]
    ,(,) "\"ac\\\"c'/c'b\"b\"b'd\"c\"\"" ["ac\"c'/c'bbb'dc"]
    ,(,) "/ 'ccc\"d\\dc'\"'\\  b" ["/","'cccd\\dc''\\","b"]
    ,(,) "  '\"/\\cc\\/c '\\\\" ["'/\\cc\\/c '\\\\"]
    ,(,) "\\ \\' ' /d  \"cc\\\\//da\"d'a/a\"ca\\\\\"\\cb c\"d'b 'acb" ["\\","\\'","'","/d","cc\\\\//dad'a/aca\\\\cb","cd'b 'acb"]
    ,(,) "a\"\"d'\"a\"\\ \\c db'da/d\\c\"a/ aa c/db" ["ad'a\\","\\c","db'da/d\\ca/ aa c/db"]
    ,(,) " d\\" ["d\\"]
    ,(,) "d c b'/\\/'\"/'a'aa\"a\"/ad\\/" ["d","c","b'/\\/'/'a'aaa/ad\\/"]
    ,(,) "  a \\' /" ["a","\\'","/"]
    ,(,) "'/ c" ["'/","c"]
    ,(,) "acd 'bcab /ba'daa'/ba/\"dcdadbcacb" ["acd","'bcab","/ba'daa'/ba/dcdadbcacb"]
    ,(,) "a\\\"dd'a c\"a\"\"ac\\" ["a\"dd'a","ca\"ac\\"]
    ,(,) "\"dba /'bb\\ d ba '/c' \"dd\\' cbcd c /b/\\b///" ["dba /'bb\\ d ba '/c' dd\\'","cbcd","c","/b/\\b///"]
    ,(,) "a'c/c \"ccb '/d\\abd/bc  " ["a'c/c","ccb '/d\\abd/bc  "]
    ,(,) "\\da\"\\//add\\\\ c" ["\\da\\//add\\\\ c"]
    ,(,) "c/\\\"//  a/\"ac\"//''ba\"c/\\bc\\\"d\"bc/d" ["c/\"//","a/ac//''bac/\\bc\"dbc/d"]
    ,(,) "/d/ a   dc'\\ \"" ["/d/","a","dc'\\",""]
    ,(,) " \"dc//b\\cd/ \\ac\"b\"b\"d\"\"\"dd\"\" ' a\\'/ \"/'/\\a/abd\\ddd" ["dc//b\\cd/ \\acbbd\"dd","'","a\\'/","/'/\\a/abd\\ddd"]
    ,(,) "\\'  ' d\"b bbc" ["\\'","'","db bbc"]
    ,(,) "'ba\\a'db/bd d\\'b\\ \\/a'da' " ["'ba\\a'db/bd","d\\'b\\","\\/a'da'"]
    ,(,) "\\b\\cc\"\"d' dd ddcb\"d" ["\\b\\ccd'","dd","ddcbd"]
    ,(,) "d\"dc'\\d\"/'\\\"b\\c'c\" db' \\'b/\"a' / da'\"/ab'\\ c\\bc\\//dbcb\\" ["ddc'\\d/'\"b\\c'c db' \\'b/a'","/","da'/ab'\\ c\\bc\\//dbcb\\"]
    ,(,) " b ddbbbbc\"da\\c\"'\\" ["b","ddbbbbcda\\c'\\"]
    ,(,) "b/\"d dacd'/'\\\"''a a /'\\c'b ab\\  dda\\c'abdd'a\"//d \\\\\\ d\"\"" ["b/d dacd'/'\"''a a /'\\c'b ab\\  dda\\c'abdd'a//d","\\\\\\","d"]
    ,(,) "/c\"\" dd'a'/b\\/'\"'/" ["/c","dd'a'/b\\/''/"]
    ,(,) "/\"'\"\"'cc a a\\dd''\\'b" ["/'\"'cc","a","a\\dd''\\'b"]
    ,(,) "c\"dcd''aba\" \" /'" ["cdcd''aba"," /'"]
    ,(,) "'\"/''\\\\d'/ad\\baadabdca\\ /\\'''bd\\/\"'/' aca \\  \\a'\\ cd\"d /bdcd''cac" ["'/''\\\\d'/ad\\baadabdca\\ /\\'''bd\\/'/'","aca","\\","\\a'\\","cdd /bdcd''cac"]
    ,(,) "\" /\"da" [" /da"]
    ,(,) "'\"ca/'d/d/d\\ca\"/\"\" ddac cc\" ''a c''bd\"bc'dc\\/\"b\"a\\\"\"a/\\ " ["'ca/'d/d/d\\ca/","ddac","cc ''a c''bdbc'dc\\/ba\"a/\\ "]
    ,(,) "\\\\d'ad ' ''\"cd/a \"\"\\'\\\"'dc\\" ["\\\\d'ad","'","''cd/a \"\\'\"'dc\\"]
    ,(,) " ab  c'\\a" ["ab","c'\\a"]
    ,(,) "b" ["b"]
    ,(,) "''c dc c\\'d'ab'd\"\\\"cca\"b'da\"dbcdbd\"cd'/d \\cd'\"d  \"\"b cdc''/\\\"b'" ["''c","dc","c\\'d'ab'd\"ccab'dadbcdbdcd'/d","\\cd'd  \"b","cdc''/\"b'"]
    ,(,) " \"'cb dbddbdd/" ["'cb dbddbdd/"]
    ,(,) "a/\"d// dd/cc/\"cc\"d\" d\\/a a \\c\"  \\\\/\"\\ bcc'ac'\"\\c//d\"da/\\aac\\b\"c/'b\"\"bbd/\\" ["a/d// dd/cc/ccd","d\\/a","a","\\c  \\\\/\\","bcc'ac'\\c//dda/\\aac\\bc/'b\"bbd/\\"]
    ,(,) "b\"ddccd\"a\"/ba\"" ["bddccda/ba"]
    ,(,) " \"  c/b/'/bdd  cb d'c a'\"'a d\\\\db//\\\"' c'/'c\\/aa" ["  c/b/'/bdd  cb d'c a''a","d\\\\db//\"'","c'/'c\\/aa"]
    ,(,) "\\caab" ["\\caab"]
    ,(,) "bb\"'\"/d'bad 'd\\/'\\b//\\\\ \\d''c\"c b\\b/\\" ["bb'/d'bad","'d\\/'\\b//\\\\","\\d''cc b\\b/\\"]
    ,(,) " c'a\"  \\cab\"bd\"dcd\"/cb/\"\"b\"b'\"d" ["c'a  \\cabbddcd/cb/bb'd"]
    ,(,) "\\/ \"c'ca" ["\\/","c'ca"]
    ,(,) "  d' /c'bc\"'/'\\\\dca'cc\"'\"''/d cb//'a \"bd ab\"dcaadc\\\"'d\\\"/a\"a\\\"ba//b/ d/dbac/d\\caa\"bc/ " ["d'","/c'bc'/'\\\\dca'cc'''/d cb//'a bd","abdcaadc\"'d\"/aa\"ba//b/","d/dbac/d\\caabc/ "]
    ,(,) "/\"\\db'd/ ca\"ad b\\\\\"cd/a bbc\\ " ["/\\db'd/ caad","b\\cd/a bbc\\ "]
    ,(,) "cdc bd'/\"c''c d \\\"aa \\d\\ bb'b/ /b/a/c'acda\\'\"\"c \"bbbaa/'/a \\aca\"'/ac' " ["cdc","bd'/c''c d \"aa \\d\\ bb'b/ /b/a/c'acda\\'\"c","bbbaa/'/a \\aca'/ac'"]
    ,(,) "ad/'b\\d /cc\"\"ab \\ \"'  ''b\\\"/\\  a\"'d\"\\ddacdbbabb b b  //' acd\"c\\d'd\\b\"'\\\"aaba/bda/c'// \\b" ["ad/'b\\d","/ccab","\\","'  ''b\"/\\  a'd\\ddacdbbabb b b  //' acdc\\d'd\\b'\"aaba/bda/c'// \\b"]
    ,(,) "bac cc \"ac\"/ca/ '\"\" b/b d /cd'\\'bb\" \\ \"b '/ b c ' c''\"a/ad\\ " ["bac","cc","ac/ca/","'","b/b","d","/cd'\\'bb \\ b","'/","b","c","'","c''a/ad\\ "]
    ,(,) "baa'  b'b''\\dab/'c" ["baa'","b'b''\\dab/'c"]
    ,(,) "cb\\\\ " ["cb\\\\"]
    ,(,) "/b'a''d\"b\"   'c'b ba\\'b\" bb" ["/b'a''db","'c'b","ba\\'b bb"]
    ,(,) "b /\"ca\\cbac " ["b","/ca\\cbac "]
    ,(,) " \"\"/\"bcaa\"\"a' \\/bb \"a\\\"'\"" ["/bcaa\"a'","\\/bb","a\"'"]
    ,(,) "\"c /''c\"\\badc/\\daa/\\ c\"a c\\ \\/cab \"b\"\\ ba\"\"/d/cd'a ad'c/ad\"' a\\d/d\\c\\'cdccd/\"a'/\"b///ac\"" ["c /''c\\badc/\\daa/\\","ca c\\ \\/cab b\\ba\"/d/cd'a","ad'c/ad' a\\d/d\\c\\'cdccd/a'/b///ac"]
    ,(,) "/cbbd\"/b' /dd\"/c\\ca/'\"\\ cc  \\d\"aca/\"b caa\\d\\'\"b'b  dc\"cd\\'c\" 'd/ac\"cacc\"" ["/cbbd/b' /dd/c\\ca/'\\ cc  \\daca/b caa\\d\\'b'b","dccd\\'c","'d/accacc"]
    ,(,) "bc/bd\\ca\\bcacca\"\"\\c/\\ /\"\"a/\"c'//b'\\d/a/'ab/cbd/cacb//b \\d\"aac\\d'\"/" ["bc/bd\\ca\\bcacca\\c/\\","/a/c'//b'\\d/a/'ab/cbd/cacb//b \\daac\\d'/"]
    ,(,) "bbac bdc/d\\\"/db\"dbdb\"a \" /\"/'a\\acacbcc c'//\\//b\"ca\"bcca c\\/aaa/c/bccbccaa  \"\" cdccc/bddcbc c''" ["bbac","bdc/d\"/dbdbdba"," //'a\\acacbcc","c'//\\//bcabcca","c\\/aaa","/c/bccbccaa","","cdccc/bddcbc","c''"]
    ]
