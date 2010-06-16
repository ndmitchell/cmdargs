
module System.Console.CmdArgs.Explicit.Process(process) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Console.CmdArgs.Explicit.Type


process :: Mode a -> [String] -> Either String a
process = processMode


processMode :: Mode a -> [String] -> Either String a
processMode m@Modes{} (a:args) =
    case lookupName (modesList m) a of
        Ambiguous xs -> Left $ ambiguous "mode" a xs
        Found x -> processMode x args
        NotFound 
            | Just def <- modesDefault m
            , Found y <- lookupName (modesList m) def
                -> processMode y (a:args)
            | otherwise -> Left $ unexpected "mode" a $ concatMap fst $ modesList m
processMode m@Modes{} [] = case modesDefault m of
    Nothing -> Left $ missing "mode" $ concatMap fst $ modesList m
    Just y -> processMode m [y]

processMode m@Mode{} args = processFlags (modeFlags m) (modeValue m) args


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


processFlags :: [Flag a] -> a -> [String] -> Either String a
processFlags flags val_ args_ = f $ S val_ args_ []
    where
        f s | not $ null $ errs s = Left $ last $ errs s
            | null $ args s = Right $ val s
            | otherwise = f (processFlag flags s)


pickFlags long flags = [(filter (\x -> (length x > 1) == long) name,(arg,flag)) | flag@Flag{flagInfo=FlagNamed arg name} <- flags]


processFlag :: [Flag a] -> S a -> S a
processFlag flags s_@S{args=('-':'-':xs):ys} | xs /= "" =
    case lookupName (pickFlags True flags) a of
        Ambiguous poss -> err s $ ambiguous "flag" ("--" ++ a) poss
        NotFound -> err s $ "Unknown flag: --" ++ a
        Found (arg,flag) -> case arg of
            ArgNone | null b -> upd s $ flagValue flag ""
                    | otherwise -> err s $ "Unhandled argument to flag, none expected: --" ++ xs
            ArgReq | null b && null ys -> err s $ "Flag requires argument: --" ++ xs
                   | null b -> upd s{args=tail ys} $ flagValue flag $ head ys
                   | otherwise -> upd s $ flagValue flag $ tail b
            _ | null b -> upd s $ flagValue flag $ fromArgOpt arg
              | otherwise -> upd s $ flagValue flag $ tail b
    where
        s = s_{args=ys}
        (a,b) = break (== '=') xs


processFlag flags s_@S{args=('-':x:xs):ys} | x /= '-' =
    case lookupName (pickFlags False flags) [x] of
        Ambiguous poss -> err s $ ambiguous "flag" ['-',x] poss
        NotFound -> err s $ "Unknown flag: -" ++ [x]
        Found (arg,flag) -> case arg of
            ArgNone | "=" `isPrefixOf` xs -> err s $ "Unhandled argument to flag, none expected: -" ++ [x]
                    | otherwise -> upd s_{args=['-':xs|xs/=""] ++ ys} $ flagValue flag ""
            ArgReq | null xs && null ys -> err s $ "Flag requires argument: -" ++ [x]
                   | null xs -> upd s_{args=tail ys} $ flagValue flag $ head ys
                   | otherwise -> upd s_{args=ys} $ flagValue flag $ if "=" `isPrefixOf` xs then tail xs else xs
            ArgOpt x | null xs -> upd s_{args=ys} $ flagValue flag x
                     | otherwise -> upd s_{args=ys} $ flagValue flag $ if "=" `isPrefixOf` xs then tail xs else xs
            ArgOptRare x | "=" `isPrefixOf` xs -> upd s_{args=ys} $ flagValue flag $ tail xs
                         | otherwise -> upd s_{args=['-':xs|xs/=""] ++ ys} $ flagValue flag x
    where
        s = s_{args=ys}


processFlag flags s_ =
    case nopos of
        Nothing -> err s $ "Unhandled argument, none expected: " ++ x
        Just flag -> case flagValue flag x (val s) of
            Left e -> err s $ "Unhandled argument, " ++ e ++ ": " ++ x
            Right v -> s{val=v}
    where
        x:ys = if head (args s_) == "--" then tail (args s_) else args s_
        s = s_{args=ys}
        nopos = listToMaybe [flag | flag@Flag{flagInfo=FlagUnnamed} <- flags]


---------------------------------------------------------------------
-- UTILITIES

ambiguous typ got xs = "Ambiguous " ++ typ ++ " '" ++ got ++ "', could be any of: " ++ unwords xs
missing typ xs = "Missing " ++ typ ++ ", wanted any of: " ++ unwords xs
unexpected typ got xs = "Unexpected " ++ typ ++ " '" ++ got ++ "', wanted any of: " ++ unwords xs


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
    
