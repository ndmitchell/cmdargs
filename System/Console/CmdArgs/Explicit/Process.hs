
module System.Console.CmdArgs.Explicit.Process(process, processValue, processArgs) where

import System.Console.CmdArgs.Explicit.Type
import Control.Arrow
import Data.List
import Data.Maybe
import System.Environment
import System.Exit


-- | Process the flags obtained by @getArgs@ with a mode. Displays
--   an error and exits with failure if the command line fails to parse, or returns
--   the associated value. Implemented in terms of 'process'.
processArgs :: Mode a -> IO a
processArgs m = do
    xs <- getArgs
    case process m xs of
        Left x -> do putStrLn x; exitFailure
        Right x -> return x


-- | Process a list of flags (usually obtained from @getArgs@) with a mode. Displays
--   an error and exits with failure if the command line fails to parse, or returns
--   the associated value. Implemeneted in terms of 'process'.
processValue :: Mode a -> [String] -> a
processValue m xs = case process m xs of
    Left x -> error x
    Right x -> x


-- | Process a list of flags (usually obtained from @getArgs@) with a mode. Returns
--   @Left@ and an error message if the command line fails to parse, or @Right@ and
--   the associated value.
process :: Mode a -> [String] -> Either String a
process = processMode


processMode :: Mode a -> [String] -> Either String a
processMode m args =
    case find of
        Ambiguous xs -> Left $ ambiguous "mode" a xs
        Found x -> processMode x as
        NotFound
            | isNothing (modeArgs m) && args /= [] &&
              not (null $ modeModes m) && not ("-" `isPrefixOf` concat args)
                -> Left $ missing "mode" $ concatMap modeNames $ modeModes m
            | otherwise -> either Left (modeCheck m) $ processFlags m (modeValue m) args
    where
        (find,a,as) = case args of
            [] -> (NotFound,"",[])
            x:xs -> (lookupName (map (modeNames &&& id) $ modeModes m) x, x, xs)


data S a = S
    {val :: a
    ,args :: [String]
    ,errs :: [String]
    }

err :: S a -> String -> S a
err s x = s{errs=x:errs s}

upd :: S a -> (a -> Either String a) -> S a
upd s f = case f $ val s of
    Left x -> err s x
    Right x -> s{val=x}


processFlags :: Mode a -> a -> [String] -> Either String a
processFlags mode val_ args_ = f $ S val_ args_ []
    where
        f s | not $ null $ errs s = Left $ last $ errs s
            | null $ args s = Right $ val s
            | otherwise = f (processFlag mode s)


pickFlags long mode = [(filter (\x -> (length x > 1) == long) $ flagNames flag,(flagInfo flag,flag)) | flag <- modeFlags mode]


processFlag :: Mode a -> S a -> S a
processFlag mode s_@S{args=('-':'-':xs):ys} | xs /= "" =
    case lookupName (pickFlags True mode) a of
        Ambiguous poss -> err s $ ambiguous "flag" ("--" ++ a) poss
        NotFound -> err s $ "Unknown flag: --" ++ a
        Found (arg,flag) -> case arg of
            FlagNone | null b -> upd s $ flagValue flag ""
                     | otherwise -> err s $ "Unhandled argument to flag, none expected: --" ++ xs
            FlagReq | null b && null ys -> err s $ "Flag requires argument: --" ++ xs
                    | null b -> upd s{args=tail ys} $ flagValue flag $ head ys
                    | otherwise -> upd s $ flagValue flag $ tail b
            _ | null b -> upd s $ flagValue flag $ fromFlagOpt arg
              | otherwise -> upd s $ flagValue flag $ tail b
    where
        s = s_{args=ys}
        (a,b) = break (== '=') xs


processFlag mode s_@S{args=('-':x:xs):ys} | x /= '-' =
    case lookupName (pickFlags False mode) [x] of
        Ambiguous poss -> err s $ ambiguous "flag" ['-',x] poss
        NotFound -> err s $ "Unknown flag: -" ++ [x]
        Found (arg,flag) -> case arg of
            FlagNone | "=" `isPrefixOf` xs -> err s $ "Unhandled argument to flag, none expected: -" ++ [x]
                     | otherwise -> upd s_{args=['-':xs|xs/=""] ++ ys} $ flagValue flag ""
            FlagReq | null xs && null ys -> err s $ "Flag requires argument: -" ++ [x]
                    | null xs -> upd s_{args=tail ys} $ flagValue flag $ head ys
                    | otherwise -> upd s_{args=ys} $ flagValue flag $ if "=" `isPrefixOf` xs then tail xs else xs
            FlagOpt x | null xs -> upd s_{args=ys} $ flagValue flag x
                      | otherwise -> upd s_{args=ys} $ flagValue flag $ if "=" `isPrefixOf` xs then tail xs else xs
            FlagOptRare x | "=" `isPrefixOf` xs -> upd s_{args=ys} $ flagValue flag $ tail xs
                          | otherwise -> upd s_{args=['-':xs|xs/=""] ++ ys} $ flagValue flag x
    where
        s = s_{args=ys}


processFlag mode s_ =
    case modeArgs mode of
        Nothing -> err s $ "Unhandled argument, none expected: " ++ x
        Just arg -> case argValue arg x (val s) of
            Left e -> err s $ "Unhandled argument, " ++ e ++ ": " ++ x
            Right v -> s{val=v}
    where
        x:ys = if head (args s_) == "--" then tail (args s_) else args s_
        s = s_{args=ys}


---------------------------------------------------------------------
-- UTILITIES

ambiguous typ got xs = "Ambiguous " ++ typ ++ " '" ++ got ++ "', could be any of: " ++ unwords xs
missing typ xs = "Missing " ++ typ ++ ", wanted any of: " ++ unwords xs


data LookupName a = NotFound
                  | Ambiguous [Name]
                  | Found a

-- different order to lookup so can potentially partially-apply it
lookupName :: [([Name],a)] -> Name -> LookupName a
lookupName names value =
    case (match (==), match isPrefixOf) of
        ([],[]) -> NotFound
        ([],[x]) -> Found $ snd x
        ([],xs) -> Ambiguous $ map fst xs
        ([x],_) -> Found $ snd x
        (xs,_) -> Ambiguous $ map fst xs
    where
        match op = [(head ys,v) | (xs,v) <- names, let ys = filter (op value) xs, ys /= []]
    
