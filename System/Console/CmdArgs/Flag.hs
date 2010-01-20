{-# LANGUAGE PatternGuards #-}

module System.Console.CmdArgs.Flag where

import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad
import Data.Function

import System.Console.CmdArgs.Type


data Action = Update String (Dynamic -> Dynamic)
            | Error String

instance Show Action where
    show (Update x y) = "Update " ++ show x ++ " <function>"
    show (Error x) = "Error " ++ show x

getAction :: [Action] -> String -> Maybe Action
getAction as x = listToMaybe [a | a@(Update s _) <- as, s == x]

hasAction :: [Action] -> String -> Bool
hasAction as = isJust . getAction as


---------------------------------------------------------------------
-- HELP INFORMATION FOR A FLAG

helpFlag :: Flag -> [((String,String,String), Maybe String)]
helpFlag xs =
    [((unwords (map ("-"++) short)
      ,unwords (map ("--"++) long) ++ val
      ,flagText xs ++ maybe "" (\x -> " (default=" ++ x ++ ")") (defaultFlag xs))
     ,flagGroup xs)
    | isFlagFlag xs]
    where
        (short,long) = partition ((==) 1 . length) $ flagFlag xs
        val = if isFlagBool xs then ""
              else ['['|opt] ++ "=" ++ flagTypDef "VALUE" xs ++ [']'|opt]
        opt = isFlagOpt xs


-- Given a flag, see what argument positions it should have
-- with the Int being a sort order
helpFlagArgs :: Flag -> [(Int,String)]
helpFlagArgs xs = case (flagArgs xs, flagTypDef "FILE" xs) of
    (Just Nothing,x) -> [(maxBound :: Int,"[" ++ x ++ "]")]
    (Just (Just i),x) -> [(i,x)]
    _ -> []


defaultFlag :: Flag -> Maybe String
defaultFlag x = if res `elem` [Just "",Just "0"] then Nothing else res
    where
        res = flagOpt x `mplus` val
        val = case flagVal x of
                x | Just v <- fromDynamic x -> Just (v :: String)
                  | Just v <- fromDynamic x -> Just $ show (v :: Int)
                  | Just v <- fromDynamic x -> Just $ show (v :: Integer)
                  | Just v <- fromDynamic x -> Just $ show (v :: Float)
                  | Just v <- fromDynamic x -> Just $ show (v :: Double)
                _ -> Nothing


---------------------------------------------------------------------
-- PROCESS A FLAG

parseFlags :: [Flag] -> [String] -> [Action]
parseFlags flags = f 0
    where
        f seen [] = case sortBy (comparing fst) [(i,n) | Flag{flagTyp=n, flagArgs=Just (Just i),flagOpt=Nothing} <- flags, i >= seen] of
            [] -> []
            x -> [Error $ "Not enough non-flag arguments, expected " ++ show ((length x)+seen) ++
                          ", but got " ++ show seen ++
                          "; missing: " ++ (intercalate " " $ map snd x)]

        f seen (x:xs) = act : f (seen + if "-" `isPrefixOf` x then 0 else 1) ys
            where (act,ys) = case sortBy (compare `on` fst) $ mapMaybe (\flag -> parseFlag flag seen (x:xs)) flags of
                    [] -> (Error $ "Unknown flag: " ++ x, xs)
                    r1:r2:_ | fst r1 == fst r2 -> (Error $ "Ambiguous flag: " ++ x, xs)
                    r:_ -> snd r


data Priority = PriExactFlag | PriPrefixFlag | PriFilePos | PriFile | PriUnknown
                deriving (Eq,Ord,Show)


parseFlag :: Flag -> Int -> [String] -> Maybe (Priority, (Action, [String]))

parseFlag flag seen (x:xs) | flagUnknown flag = Just (PriUnknown, (Update (flagName flag) (addMany x), xs))

parseFlag flag seen (('-':x:xs):ys) | xs /= "" && x `elem` expand = parseFlag flag seen (['-',x]:('-':xs):ys)
    where expand = [x | isFlagBool flag, [x] <- flagFlag flag]

parseFlag flag seen (('-':x:xs):ys) | x /= '-' = parseFlag flag seen (x2:ys)
    where x2 = '-':'-':x:['='| xs /= [] && head xs /= '=']++xs

parseFlag flag seen (('-':'-':x):xs)
    | not $ any (a `isPrefixOf`) (flagFlag flag) = Nothing
    | otherwise = Just $ (,) (if a `elem` flagFlag flag then PriExactFlag else PriPrefixFlag) $
    case flagType flag of
        FlagBool r
            | drop 1 b `elem` ["","true","yes","on","enabled","1"] -> upd (const r) xs
            | drop 1 b `elem` ["false","no","off","disabled","0"] -> upd (const (toDyn False)) xs
            | otherwise -> err "couldn't parse argument" xs
        FlagItem r ->
            if not (isFlagOpt flag) && null b && (null xs || "-" `isPrefixOf` head xs)
            then err "needs an argument" xs
            else let (text,rest) = case flagOpt flag of
                        Just v | null b -> (v, xs)
                        _ | null b -> (head xs, tail xs)
                        _ -> (drop 1 b, xs)
                 in case r text of
                        Nothing -> err "couldn't parse argument" rest
                        Just v -> upd v rest
    where
        (a,b) = break (== '=') x
        err msg rest = (Error $ "Error on flag " ++ show x ++ ", flag " ++ msg, rest)
        upd v rest = (Update (flagName flag) v, rest)

parseFlag flag seen (x:xs) = case flagArgs flag of
    Just Nothing -> upd (addMany x) PriFile
    Just (Just i) | i == seen -> upd (addOne x) PriFilePos
    _ -> Nothing
    where upd op p = Just (p, (Update (flagName flag) op, xs))

addMany x v = toDyn $ fromDyn v [""] ++ [x]
addOne x v = toDyn x
