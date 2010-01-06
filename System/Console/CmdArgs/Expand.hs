
module System.Console.CmdArgs.Expand(defaults,expand,autoFlags) where

import System.Console.CmdArgs.Type
import Data.Dynamic
import Data.Ord
import Data.List
import Data.Maybe
import Data.Char
import Data.Function


---------------------------------------------------------------------
-- PRESUPPLIED ARGS

autoFlags_def :: String -> String -> String -> String -> Flag
autoFlags_def name short long text = flagDefault
            {flagName=name,flagKey=name,flagFlag=[short,long],flagText=text,flagType=FlagBool (toDyn True),flagVal=toDyn False,flagExplicit=True}

autoFlagsVerbosity :: [Flag]
autoFlagsVerbosity =
    [ f "!verbose" "v" "verbose" "Higher verbosity"
    , f "!quiet" "q" "quiet" "Lower verbosity"
    ]
    where f = autoFlags_def

autoFlagsHelp :: [Flag]
autoFlagsHelp = [ (autoFlags_def "!help" "?" "help" "Show usage information (optional format)")
        {flagType=fromJust $ toFlagType (typeOf ""),flagOpt=Just "",flagTyp="FORMAT"}
                ]

autoFlagsVersion :: [Flag]
autoFlagsVersion =
    [autoFlags_def "!version" "V" "version" "Show version information"]

autoFlags :: [Flag]
autoFlags = autoFlagsHelp ++ autoFlagsVersion ++ autoFlagsVerbosity

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
expand xs | isNothing (firstgood $ z yss) = error "Flags don't meet their condition: " ++ (firsterr $ z yss)
          | otherwise = xs3
    where
        xs3 = map (\x -> x{modeFlags=[if isFlagArgs c then c else c{flagFlag=fst $ fromJust $ lookup (flagKey c) (ys2 goodflags)} | c <- modeFlags x]}) goodflags
        ys2 e = assignShort $ assignLong $ ys e
        ys e = sort $ nub [(flagKey x, (flagFlag x, flagExplicit x)) | x <- map modeFlags e, x <- x, isFlagFlag x]

        goodflags = fromJust $ firstgood $ z yss
        firstgood zs = let g = filter (either (const False) (const True)) zs
                           Right g' = head g
                       in if null g then Nothing else Just g'
        firsterr zs = let Right e = head zs in e -- only called if no firstgood
        z es = map (\e -> maybe (Right e) Left $ checkFlags $ ys e) es
        yss = map (xs2 . concat) afseqs
        afseqs = reverse $ sortBy (comparing length) $ subsequences [ autoFlagsHelp
                                                                    , autoFlagsVersion
                                                                    , autoFlagsVerbosity
                                                                    ]

        xs2 af = map (\x -> x{modeFlags = autflg x af ++ modeFlags x}) xs
        autflg m f = if modeExplicit m then [] else f


-- Check validity of flags, Nothing if OK, Just errstring on error
checkFlags :: FlagNames -> Maybe String
checkFlags xs | any ((/=) 1 . length) grouped = Just group_err
              | nub names /= names = Just dupflg_err
              | otherwise = Nothing
    where names = concatMap (fst . snd) xs
          grouped = groupBy ((==) `on` fst) xs
          dups = filter ((/=) 1 . length) grouped
          group_err = intercalate "\n" $ flip map dups
                      (\d -> "Record name " ++ (fst $ head d) ++ " has different flags:" ++ (
                             intercalate " v.s. " $ map (show . fst . snd) d))
          dupnames = filter ((/=) 1 . length) $ group $ sort names
          dupflg_err = intercalate "\n" $ flip map dupnames
                       (\d -> "Flag " ++ (head d) ++ " has been assigned to " ++
                              (show $ length d) ++ " different arguments")

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


