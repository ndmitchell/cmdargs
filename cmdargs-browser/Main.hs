{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Launch
import Control.Monad.IO.Class
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Control.Monad
import Control.Arrow
import System.Environment
import System.FilePath
import Data.Char
import Data.Maybe
import Data.List
import System.Console.CmdArgs.Helper
import System.Console.CmdArgs.Explicit
import Paths_cmdargs_browser


type LBString = LBS.ByteString
type BString = BS.ByteString
lbsUnpack = LBS.unpack
bsUnpack = BS.unpack
txtUnpack = Text.unpack


main :: IO ()
main = do
    args <- getArgs
    wait <- newEmptyMVar
    mc <- receive
    thread <- forkIO $ run $ liftIO . talk ("--verbose" `elem` args) mc wait
    res <- takeMVar wait
    killThread thread
    reply res


talk :: Bool -> Mode a -> MVar (Either String [String]) -> Request -> IO Response
talk verbose mode wait r = handle err $ do
    when verbose $ comment $ bsUnpack (rawPathInfo r) ++ " " ++ maybe "" show argument
    case path of
        ["res",x] -> do
            dir <- getDataDir
            return $ ResponseFile status200 [noCache, (hContentType, fromString $ mime $ takeExtension x)] (dir </> x) Nothing
        ["ok"] -> exit $ Right $ splitArgs $ fromMaybe "" argument
        ["cancel"] -> exit $ Left "User pressed cancel"
        ["check"] -> respond status200 [] $ fromString $ fromMaybe "" $ check mode 0 $ splitArgs $ fromMaybe "" argument
        [] -> respond status200 [noCache, (hContentType, fromString "text/html")] $ fromString $ contents mode
        _ -> respond status404 [] $ fromString $ "URL not found: " ++ bsUnpack (rawPathInfo r)
    where
        respond a b c = do evaluate $ LBS.length c; return $ responseLBS a b c -- try and get decent error messages
        err (x :: SomeException) = respond status500 [noCache, (hContentType, fromString "text/plain")] $ fromString $ "EXCEPTION:\n" ++ show x
        path = map txtUnpack $ pathInfo r
        argument = fmap bsUnpack $ join $ lookup (fromString "arg") (queryString r)
        exit val = do putMVar wait val; respond status200 [(hContentType, fromString "text/plain")] $ fromString ""
        noCache = (hCacheControl, fromString "no-cache")


check :: Mode a -> Int -> [String] -> Maybe String
check mode skip args = either Just (const Nothing) $ process (dropArgs skip mode) args
    where
        dropArgs i m = m{modeGroupModes = fmap (dropArgs i) $ modeGroupModes m
                        ,modeArgs = (drop i *** id) $ modeArgs m}


mime ".png" = "image/png"
mime ".css" = "text/css"
mime ".js" = "text/javascript"
mime ".html" = "text/html"
mime ".htm" = "text/html"
mime _ = "text/plain"


contents mode = unlines
    ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    ,"<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>"
    ,"<head>"
    ,"<title>" ++ prog ++ " arguments</title>"
    ,"<link type='image/png' rel='icon' href='/res/favicon.png' />"
    ,"<script src='/res/jquery-1.4.2.js'></script>"
    ,"<script>"
    ,"var mode = " ++ fromJ (json mode) ++ ";"
    ,"</script>"
    ,"<script src='/res/cmdargs.js'></script>"
    ,"<link type='text/css' rel='stylesheet' href='res/cmdargs.css' />"
    ,"</head>"
    ,"<body>"
    ,"<div id='body'>Loading...</div>"
    ,"</body>"
    ,"</html>"
    ]
    where
        prog = head $ modeNames mode ++ ["program"]


---------------------------------------------------------------------
-- JSON conversion

class JSON a where json :: a -> J

newtype J = J {fromJ :: String}

(@=) :: JSON a => String -> a -> (String,J)
(@=) a b = (a, json b)

jsonFields :: [(String, J)] -> J
jsonFields xs = J $ "{" ++ intercalate "," [a ++ ":" ++ b | (a,J b) <- xs] ++ "}"

instance JSON Bool where json = J . map toLower . show
instance JSON [Char] where json = J . show
instance JSON J where json = id

instance JSON a => JSON [a] where
    json xs = J $ "[" ++ intercalate "," (map (fromJ . json) xs) ++ "]"

instance JSON a => JSON (Maybe a) where
    json Nothing = J "null"
    json (Just x) = json x

instance (JSON a, JSON b) => JSON (a, b) where
    json (a, b) = J $ "[" ++ fromJ (json a) ++ "," ++ fromJ (json b) ++ "]"


instance JSON (Mode a) where
    json Mode{..} = jsonFields
        ["modes" @= modeGroupModes
        ,"names" @= modeNames
        ,"expandAt" @= modeExpandAt
        ,"help" @= modeHelp
        ,"helpSuffix" @= modeHelpSuffix
        ,"args" @= modeArgs
        ,"flags" @= modeGroupFlags
        ]

instance JSON a => JSON (Group a) where
    json Group{..} = jsonFields
        ["unnamed" @= groupUnnamed
        ,"hidden" @= groupHidden
        ,"named" @= groupNamed
        ]

instance JSON (Flag a) where
    json Flag{..} = jsonFields
        ["names" @= flagNames
        ,"info" @= flagInfo
        ,"type" @= flagType
        ,"help" @= flagHelp
        ]

instance JSON FlagInfo where
    json FlagReq = jsonFields ["type" @= "req"]
    json (FlagOpt x) = jsonFields ["type" @= "opt", "value" @= x]
    json (FlagOptRare x) = jsonFields ["type" @= "rare", "value" @= x]
    json FlagNone = jsonFields ["type" @= "none"]

instance JSON (Arg a) where
    json Arg{..} = jsonFields
        ["type" @= argType
        ,"require" @= argRequire
        ]
