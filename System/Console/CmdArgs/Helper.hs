{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}

-- | Module for implementing CmdArgs helpers. A CmdArgs helper is an external program,
--   that helps a user construct the command line arguments. To use a helper set the
--   environment variable @$CMDARGS_HELPER@ (or @$CMDARGS_HELPER_/YOURPROGRAM/@) to
--   one of:
--
-- * @echo /foo/@ will cause @/foo/@ to be used as the command arguments.
--
-- * @cmdargs-browser@ will cause a web browser to appear to help entering the arguments.
--   For this command to work, you will need to install the @cmdargs-browser@ package:
--   <http://hackage.haskell.org/package/cmdargs-browser>
module System.Console.CmdArgs.Helper(
    -- * Called by the main program
    execute,
    -- * Called by the helper program
    Unknown, receive, reply, comment
    ) where
-- Should really be under Explicit, but want to export it top-level as Helper

import System.Console.CmdArgs.Explicit.Type
import System.Console.CmdArgs.Explicit.SplitJoin
import System.Process
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Maybe
import System.Exit
import System.IO
import System.IO.Unsafe


hOut h x = do
    hPutStrLn h x
    hFlush h


-- | Run a remote command line entry.
execute
    :: String -- ^ Name of the command to run, e.g. @echo argument@, @cmdargs-browser@
    -> Mode a -- ^ Mode to run remotely
    -> [String] -- ^ Initial set of command line flags (not supported by all helpers)
    -> IO (Either String [String]) -- ^ Either an error message, or a list of flags to use
execute cmd mode args
    | "echo" == takeWhile (not . isSpace) cmd = return $ Right $ splitArgs $ drop 4 cmd
    | otherwise = withBuffering stdout NoBuffering $ do
        (Just hin, Just hout, _, _) <- createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe}
        -- none of the buffering seems necessary in practice, but better safe than sorry
        hSetBuffering hin LineBuffering
        hSetBuffering hout LineBuffering
        (m, ans) <- saveMode mode
        hOut hin m
        loop ans hin hout
    where
        loop ans hin hout = do
            x <- hGetLine hout
            if "Result " `isPrefixOf` x then
                return $ read $ drop 7 x
             else if "Send " `isPrefixOf` x then do
                hOut hin =<< ans (drop 5 x)
                loop ans hin hout
             else if "#" `isPrefixOf` x then do
                hOut stdout x
                loop ans hin hout
             else
                return $ Left $ "Unexpected message from program: " ++ show x


withBuffering hndl mode act = bracket
    (do old <- hGetBuffering hndl; hSetBuffering hndl mode; return old)
    (hSetBuffering hndl)
    (const act)


-- | Unknown value, representing the values stored within the 'Mode' structure. While the values
--   are not observable, they behave identically to the original values.
newtype Unknown = Unknown {fromUnknown :: Value} -- wrap Value so the Pack instance doesn't leak


-- | Receive information about the mode to display.
receive :: IO (Mode Unknown)
receive = do
    m <- getLine
    return $ remap2 Unknown fromUnknown $ loadMode m $ \msg -> unsafePerformIO $ do
        hOut stdout $ "Send " ++ msg
        getLine


-- | Send a reply with either an error, or a list of flags to use. This function exits the helper program.
reply :: Either String [String] -> IO ()
reply x = do
    hOut stdout $ "Result " ++ show x
    exitWith ExitSuccess


-- | Send a comment which will be displayed on the calling console, mainly useful for debugging.
comment :: String -> IO ()
comment x = hOut stdout $ "# " ++ x


---------------------------------------------------------------------
-- IO MAP

data IOMap a = IOMap (IORef (Int,[(Int,a)]))

newIOMap :: IO (IOMap a)
newIOMap = fmap IOMap $ newIORef (0, [])

addIOMap :: IOMap a -> a -> IO Int
addIOMap (IOMap ref) x = atomicModifyIORef ref $ \(i,xs) -> let j = i+1 in ((j,(j,x):xs), j)

getIOMap :: IOMap a -> Int -> IO a
getIOMap (IOMap ref) i = do (_,xs) <- readIORef ref; return $ fromJust $ lookup i xs


---------------------------------------------------------------------
-- SERIALISE A MODE

newtype Value = Value Int


{-# NOINLINE toValue #-}
toValue :: Mode a -> Mode Value
-- fairly safe, use of a table and pointers from one process to another, but referentially transparent
toValue x = unsafePerformIO $ do
    -- the ref accumulates, so is a space leak
    -- but it will all disappear after the helper goes, so not too much of an issue
    mp <- newIOMap
    let embed x = unsafePerformIO $ fmap Value $ addIOMap mp x
        proj (Value x) = unsafePerformIO $ getIOMap mp x
    return $ remap2 embed proj x


saveMode :: Mode a -> IO (String, String -> IO String) -- (value, ask questions from stdin)
saveMode m = do
    mp <- newIOMap
    res <- add mp $ pack $ toValue m
    return $ (show res, fmap show . get mp . read)
    where
        add :: IOMap (Pack -> Pack) -> Pack -> IO Pack
        add mp x = flip transformM x $ \x -> case x of
            Func (NoShow f) -> do i <- addIOMap mp f; return $ FuncId i
            x -> return x

        get :: IOMap (Pack -> Pack) -> (Int,Pack) -> IO Pack
        get mp (i,x) = do
            f <- getIOMap mp i
            add mp $ f x


loadMode :: String -> (String -> String) -> Mode Value -- given serialised, question asker, give me a value
loadMode x f = unpack $ rep $ read x
    where
        rep :: Pack -> Pack
        rep x = flip transform x $ \x -> case x of
            FuncId i -> Func $ NoShow $ \y -> rep $ read $ f $ show (i,y)
            x -> x


-- Support data types

data Pack = Ctor String [(String, Pack)]
          | List [Pack]
          | Char Char
          | Int Int
          | Func (NoShow (Pack -> Pack))
          | FuncId Int -- Never passed to pack/unpack, always transfromed away by saveMode/loadMode
          | String String
          | None -- ^ Never generated, only used for reading in bad cases
            deriving (Show,Read)

newtype NoShow a = NoShow a
instance Show (NoShow a) where -- deliberately leave the methods unimplemented
instance Read (NoShow a) where -- deliberately leave the methods unimplemented


transformM, descendM :: Monad m => (Pack -> m Pack) -> Pack -> m Pack
transformM f x = f =<< descendM (transformM f) x
descendM f x = let (a,b) = uniplate x in liftM b $ mapM f a

transform, descend :: (Pack -> Pack) -> Pack -> Pack
transform f = f . descend (transform f)
descend f x = let (a,b) = uniplate x in b $ map f a

uniplate :: Pack -> ([Pack], [Pack] -> Pack)
uniplate (List xs) = (xs, List)
uniplate (Ctor x ys) = (map snd ys, Ctor x . zip (map fst ys))
uniplate x = ([], const x)


class Packer a where
    pack :: a -> Pack
    unpack :: Pack -> a

add a b = (a, pack b)
ctor x (Ctor y xs) | x == y = xs
ctor _ _ = []
get a b = unpack $ fromMaybe None $ lookup a b


-- General instances

instance Packer a => Packer [a] where
    pack xs = if length ys == length zs && not (null ys) then String zs else List ys
        where ys = map (pack) xs
              zs = [x | Char x <- ys]

    unpack (String xs) = unpack $ List $ map Char xs
    unpack (List xs) = map (unpack) xs
    unpack _ = []

instance (Packer a, Packer b) => Packer (a -> b) where
    pack f = Func $ NoShow $ pack . f . unpack
    unpack (Func (NoShow f)) = unpack . f . pack

instance Packer Value where
    pack (Value x) = pack x
    unpack x = Value $ unpack x

instance Packer Char where
    pack = Char
    unpack (Char x) = x
    unpack _ = ' '

instance Packer Int where
    pack = Int
    unpack (Int x) = x
    unpack _ = -1

instance (Packer a, Packer b) => Packer (a,b) where
    pack (a,b) = Ctor "(,)" [add "fst" a, add "snd" b]
    unpack x = (get "fst" y, get "snd" y)
        where y = ctor "(,)" x

instance Packer a => Packer (Maybe a) where
    pack Nothing = Ctor "Nothing" []
    pack (Just x) = Ctor "Just" [add "fromJust" x]
    unpack x@(Ctor "Just" _) = Just $ get "fromJust" $ ctor "Just" x
    unpack _ = Nothing

instance (Packer a, Packer b) => Packer (Either a b) where
    pack (Left x) = Ctor "Left" [add "fromLeft" x]
    pack (Right x) = Ctor "Right" [add "fromRight" x]
    unpack x@(Ctor "Left" _) = Left $ get "fromLeft" $ ctor "Left" x
    unpack x@(Ctor "Right" _) = Right $ get "fromRight" $ ctor "Right" x
    unpack _ = Left $ unpack None

instance Packer Bool where
    pack True = Ctor "True" []
    pack _ = Ctor "False" []
    unpack (Ctor "True" _) = True
    unpack _ = False


-- CmdArgs specific

instance Packer a => Packer (Group a) where
    pack Group{..} = Ctor "Group"
        [add "groupUnnamed" groupUnnamed
        ,add "groupHidden" groupHidden
        ,add "groupNamed" groupNamed]
    unpack x = let y = ctor "Group" x in Group
        {groupUnnamed = get "groupUnnamed" y
        ,groupHidden = get "groupHidden" y
        ,groupNamed = get "groupNamed" y}       

instance Packer a => Packer (Mode a) where
    pack Mode{..} = Ctor "Mode"
        [add "modeGroupModes" modeGroupModes
        ,add "modeNames" modeNames
        ,add "modeHelp" modeHelp
        ,add "modeHelpSuffix" modeHelpSuffix
        ,add "modeArgs" modeArgs
        ,add "modeGroupFlags" modeGroupFlags
        ,add "modeValue" modeValue
        ,add "modeCheck" modeCheck
        ,add "modeReform" modeReform
        ,add "modeExpandAt" modeExpandAt]
    unpack x = let y = ctor "Mode" x in Mode
        {modeGroupModes = get "modeGroupModes" y
        ,modeNames = get "modeNames" y
        ,modeHelp = get "modeHelp" y
        ,modeHelpSuffix = get "modeHelpSuffix" y
        ,modeArgs = get "modeArgs" y
        ,modeGroupFlags = get "modeGroupFlags" y
        ,modeValue = get "modeValue" y
        ,modeCheck = get "modeCheck" y
        ,modeReform = get "modeReform" y
        ,modeExpandAt = get "modeExpandAt" y}

instance Packer a => Packer (Flag a) where
    pack Flag{..} = Ctor "Flag"
        [add "flagNames" flagNames
        ,add "flagInfo" flagInfo
        ,add "flagType" flagType
        ,add "flagHelp" flagHelp
        ,add "flagValue" flagValue]
    unpack x = let y = ctor "Flag" x in Flag
        {flagNames = get "flagNames" y
        ,flagInfo = get "flagInfo" y
        ,flagType = get "flagType" y
        ,flagHelp = get "flagHelp" y
        ,flagValue = get "flagValue" y}

instance Packer a => Packer (Arg a) where
    pack Arg{..} = Ctor "Arg"
        [add "argType" argType
        ,add "argRequire" argRequire
        ,add "argValue" argValue]
    unpack x = let y = ctor "Arg" x in Arg
        {argType = get "argType" y
        ,argRequire = get "argRequire" y
        ,argValue = get "argValue" y}

instance Packer FlagInfo where
    pack FlagReq = Ctor "FlagReq" []
    pack (FlagOpt x) = Ctor "FlagOpt" [add "fromFlagOpt" x]
    pack (FlagOptRare x) = Ctor "FlagOptRare" [add "fromFlagOpt" x]
    pack FlagNone = Ctor "FlagNone" []
    unpack x@(Ctor name _) = case name of
        "FlagReq" -> FlagReq
        "FlagOpt" -> FlagOpt $ get "fromFlagOpt" $ ctor name x
        "FlagOptRare" -> FlagOpt $ get "fromFlagOpt" $ ctor name x
        "FlagNone" -> FlagNone
    unpack _ = FlagNone
