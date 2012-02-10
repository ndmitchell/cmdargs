{-# LANGUAGE RecordWildCards #-}
module System.Console.CmdArgs.Explicit.Process(process) where

import System.Console.CmdArgs.Explicit.Type
import Control.Arrow
import Data.List
import Data.Maybe


-- | Process a list of flags (usually obtained from @getArgs@/@expandArgsAt@) with a mode. Returns
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
            | null (fst $ modeArgs m) && isNothing (snd $ modeArgs m) && args /= [] &&
              not (null $ modeModes m) && not ("-" `isPrefixOf` concat args)
                -> Left $ missing "mode" $ concatMap modeNames $ modeModes m
            | otherwise -> either Left (modeCheck m) $ processFlags m (modeValue m) args
    where
        (find,a,as) = case args of
            [] -> (NotFound,"",[])
            x:xs -> (lookupName (map (modeNames &&& id) $ modeModes m) x, x, xs)


data S a = S
    {val :: a -- The value you are accumulating
    ,args :: [String] -- The arguments you are processing through
    ,argsCount :: Int -- The number of unnamed arguments you have seen
    ,errs :: [String] -- The errors you have seen
    }

stop :: Mode a -> S a -> Maybe (Either String a)
stop mode S{..}
    | not $ null errs = Just $ Left $ last errs
    | null args = Just $ if argsCount >= mn then Right val else
        Left $ "Expected " ++ (if Just mn == mx then "exactly" else "at least") ++ show mn ++ " unnamed arguments, but got only " ++ show argsCount
    | otherwise = Nothing
    where (mn, mx) = argsRange mode

err :: S a -> String -> S a
err s x = s{errs=x:errs s}

upd :: S a -> (a -> Either String a) -> S a
upd s f = case f $ val s of
    Left x -> err s x
    Right x -> s{val=x}


processFlags :: Mode a -> a -> [String] -> Either String a
processFlags mode val_ args_ = f $ S val_ args_ 0 []
    where f s = fromMaybe (f $ processFlag mode s) $ stop mode s


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


processFlag mode s_@S{args="--":ys} = f s_{args=ys}
    where f s | isJust $ stop mode s = s
              | otherwise = f $ processArg mode s

processFlag mode s = processArg mode s

processArg mode s_@S{args=x:ys, argsCount=count} = case argsPick mode count of
    Nothing -> err s $ "Unhandled argument, " ++ str ++ " expected: " ++ x
        where str = if count == 0 then "none" else "at most " ++ show count
    Just arg -> case argValue arg x (val s) of
            Left e -> err s $ "Unhandled argument, " ++ e ++ ": " ++ x
            Right v -> s{val=v}
    where
        s = s_{args=ys, argsCount=count+1}


-- find the minimum and maximum allowed number of arguments (Nothing=infinite)
argsRange :: Mode a -> (Int, Maybe Int)
argsRange Mode{modeArgs=(lst,end)} = (mn,mx)
    where mn = length $ dropWhile (not . argRequire) $ reverse $ lst ++ maybeToList end
          mx = if isJust end then Nothing else Just $ length lst


argsPick :: Mode a -> Int -> Maybe (Arg a)
argsPick Mode{modeArgs=(lst,end)} i = if i < length lst then Just $ lst !! i else end


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
    
