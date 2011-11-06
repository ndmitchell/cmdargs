
module Main where

import Network.Wai
import Network.Wai.Handler.Launch
import Control.Monad.IO.Class
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Control.Monad
import Control.Arrow
import System.Environment
import System.FilePath
import Data.Maybe
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
talk verbose mode wait r = do
    when verbose $ comment $ bsUnpack (rawPathInfo r) ++ " " ++ maybe "" show argument
    case path of
        ["res",x] -> do
            dir <- getDataDir
            return $ ResponseFile statusOK [noCache, headerContentType $ fromString $ mime $ takeExtension x] (dir </> x) Nothing
        ["ok"] -> exit $ Right $ splitArgs $ fromMaybe "" argument
        ["cancel"] -> exit $ Left "User pressed cancel"
        ["check"] -> return $ responseLBS statusOK [] $ fromString $ fromMaybe "" $ check mode 0 $ splitArgs $ fromMaybe "" argument
        [] -> return $ responseLBS statusOK [noCache, headerContentType $ fromString "text/html"] $ fromString $ contents mode
        _ -> return $ responseLBS status404 [] $ fromString $ "URL not found: " ++ bsUnpack (rawPathInfo r)
    where
        path = map txtUnpack $ pathInfo r
        argument = fmap bsUnpack $ join $ lookup (fromString "arg") (queryString r)
        exit val = do putMVar wait val; return $ responseLBS statusOK [headerContentType $ fromString "text/plain"] $ fromString ""
        noCache = headerCacheControl $ fromString "no-cache"


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
    ,"<link type='image/png' rel='icon' href='/res/cmdargs.png' />"
    ,"<script src='/res/jquery-1.4.2.js'></script>"
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
