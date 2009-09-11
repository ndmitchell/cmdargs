
module System.Console.CmdArgs.Expand(defaults,expand,autoFlags) where

import System.Console.CmdArgs.Type
import Data.Dynamic
import Data.List
import Data.Maybe
import Data.Char
import Data.Function


---------------------------------------------------------------------
-- PRESUPPLIED ARGS

autoFlags :: [Flag]
autoFlags =
    [(f "!help" "?" "help" "Show usage information (optional format)")
        {flagType=fromJust $ toFlagType (typeOf ""),flagOpt=Just "",flagTyp="FORMAT"}
    ,f "!version" "V" "version" "Show version information"
    ,f "!verbose" "v" "verbose" "Higher verbosity"
    ,f "!quiet" "q" "quiet" "Lower verbosity"
    ]
    where f name short long text = flagDefault
            {flagName=name,flagKey=name,flagFlag=[short,long],flagText=text,flagType=FlagBool (toDyn True),flagVal=toDyn False,flagExplicit=True}

---------------------------------------------------------------------
-- FLAG DEFAULTS

-- FIXME: Add the string (default=foo) in the appropriate places
defaults :: a -> Flag -> Flag
defaults = error "todo" 


---------------------------------------------------------------------
-- FLAG EXPANSION
-- Introduce more long/short names

-- (keyname,([flags],explicit))
type FlagNames = [(String,([String],Bool))]

-- Error if:
--   Two things with the same FldName have different FldFlag or Explicit
--   Two fields without the same FldName have different FldFlag
expand :: [Mode a] -> [Mode a]
expand xs | not $ checkFlags ys = error "Flag's don't meet their condition"
          | otherwise = xs3
    where
        xs3 = map (\x -> x{modeFlags=[if isFlagArgs c then c else c{flagFlag=fst $ fromJust $ lookup (flagKey c) ys2} | c <- modeFlags x]}) xs2
        ys2 = assignShort $ assignLong ys
        ys = sort $ nub [(flagKey x, (flagFlag x, flagExplicit x)) | x <- map modeFlags xs2, x <- x, isFlagFlag x]
        xs2 = map (\x -> x{modeFlags = autoFlags ++ modeFlags x}) xs


checkFlags :: FlagNames -> Bool
checkFlags xs | any ((/=) 1 . length) $ groupBy ((==) `on` fst) xs = error "Two record names have different flags"
              | nub names /= names = error "One flag has been assigned twice"
              | otherwise = True
    where names = concatMap (fst . snd) xs


assignLong :: FlagNames -> FlagNames
assignLong xs = map f xs
    where
        seen = concatMap (fst . snd) xs
        f (name,(already,False)) | name `notElem` seen = (name,(g name:already,False))
        f x = x
        g xs | "_" `isSuffixOf` xs = g $ init xs
        g xs = [if x == '_' then '-' else x | x <- xs]


assignShort :: FlagNames -> FlagNames
assignShort xs = zipWith (\x (a,(b,c)) -> (a,(maybe [] (return . return) x ++ b,c))) good xs
    where
        seen = concat $ filter ((==) 1 . length) $ concatMap (fst . snd) xs
        guesses = map guess xs :: [Maybe Char]
        dupes = let gs = catMaybes guesses in nub $ gs \\ nub gs
        good = [if maybe True (`elem` (dupes++seen)) g then Nothing else g | g <- guesses] :: [Maybe Char]

        -- guess at a possible short flag
        guess (name,(already,False)) | all ((/=) 1 . length) already = Just $ head $ head already
        guess _ = Nothing


