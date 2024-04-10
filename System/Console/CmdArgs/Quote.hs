{-# LANGUAGE TemplateHaskell, PatternGuards, MagicHash, CPP #-}

-- | This module provides a quotation feature to let you write command line
--   arguments in the impure style, but have them translated into the pure style,
--   as per "System.Console.CmdArgs.Implicit". An example:
--
-- > {-# LANGUAGE TemplateHaskell, DeriveDataTypeable, MagicHash #-}
-- > import System.Console.CmdArgs.Implicit
-- > import System.Console.CmdArgs.Quote
-- >
-- > data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)
-- >
-- > $(cmdArgsQuote [d|
-- >     sample = Sample{hello = def &=# help "World argument" &=# opt "world"}
-- >                    &=# summary "Sample v1"
-- >
-- >     run = cmdArgs# sample :: IO Sample
-- >     |])
-- >
-- > main = print =<< run
--
--   Inside 'cmdArgsQuote' you supply the command line parser using attributes in the
--   impure style. If you run with @-ddump-splices@ (to see the Template Haskell output),
--   you would see:
--
-- > run = cmdArgs_
-- >     (record Sample{} [hello := def += help "World argument" += opt "world"]
-- >         += summary "Sample v1")
-- >     :: IO Sample
--
--   /Stubs/
--
--   To define the original parser you may use either the standard impure annotations ('(&=)', 'modes'), or
--   the stub annotations versions defined in this module ('(&=#)', 'modes'). The stub versions do not include
--   a "Data" constraint, so can be used in situations where the Data instance is not yet available - typically
--   when defining the parser in the same module as the data type on GHC 7.2 and above. The stub versions should
--   never be used outside 'cmdArgsQuote' and will always raise an error.
--
--   /Explicit types/
--
--   There will be a limited number of situations where an impure parser will require additional types, typically
--   on the result of 'cmdArgs' if the result is used without a fixed type - for example if you 'show' it. Most users
--   will not need to add any types. In some cases you may need to remove some explicit types, where the intermediate
--   type of the annotations has changed - but again, this change should be rare.
--
--   /Completeness/
--
--   The translation is not complete, although works for all practical instances I've tried. The translation works
--   by first expanding out the expression (inlining every function defined within the quote, inlining let bindings),
--   then performs the translation. This scheme leads to two consequences: 1) Any expensive computation executed inside
--   the quotation to produce the command line flags may be duplicated (a very unlikely scenario). 2) As I do not yet
--   have expansion rules for all possible expressions, the expansion (and subsequently the translation) may fail.
--   I am interested in any bug reports where the feature does not work as intended.
module System.Console.CmdArgs.Quote(
    -- * Template Haskell quotation function
    cmdArgsQuote,
    -- * Stub versions of the impure annotations
    (&=#), modes#, cmdArgsMode#, cmdArgs#, enum#
    ) where

import Language.Haskell.TH
import Control.Arrow
import Control.Monad
import Data.Data
import Data.Maybe
import System.Console.CmdArgs.Implicit

stub name = error $
    "System.Console.CmdArgs.Quote." ++ name ++
    ": this function is provided only for use inside cmdArgsQuote, and should never be called"

-- | Version of '&=' without a 'Data' context, only to be used within 'cmdArgsQuote'.
(&=#) :: a -> Ann -> a
(&=#) = stub "(&=#)"

-- | Version of 'modes' without a 'Data' context, only to be used within 'cmdArgsQuote'.
modes# :: [a] -> a
modes# = stub "modes#"

-- | Version of 'cmdArgsMode' without a 'Data' context, only to be used within 'cmdArgsQuote'.
cmdArgsMode# :: a -> Mode (CmdArgs a)
cmdArgsMode# = stub "cmdArgsMode#"

-- | Version of 'cmdArgs' without a 'Data' context, only to be used within 'cmdArgsQuote'.
cmdArgs# :: a -> IO a
cmdArgs# = stub "cmdArgs#"

-- | Version of 'enum' without a 'Data' context, only to be used within 'cmdArgsQuote'.
enum# :: [a] -> a
enum# = stub "enum#"


-- | Quotation function to turn an impure version of "System.Console.CmdArgs.Implicit" into a pure one.
--   For details see "System.Console.CmdArgs.Quote".
cmdArgsQuote :: Q [Dec] -> Q [Dec]
cmdArgsQuote x = do
    x <- x
    translate $ rename $ simplify $ inline x


-- | Apply the rewrite rules
translate :: [Dec] -> Q [Dec]
translate = descendBiM f
    where
        dull = ['Just, 'Left, 'Right, '(:)] -- Prelude constructors of non-zero arity

        f (RecConE x xs) = return $
            let args = [anns (InfixE (Just $ VarE lbl) (ConE '(:=)) (Just val)) as | (lbl,x) <- xs, let (val, as) = asAnns x]
            in VarE 'record `AppE` RecConE x [] `AppE` ListE args

        f x | (ConE x, xs@(_:_)) <- asApps x, x `notElem` dull = do
            names <- forM [1..length xs] $ \i -> newName $ "_" ++ nameBase x ++ show i
            let (vals, ass) = unzip $ map asAnns xs
                bind = [ValD (VarP name) (NormalB val) [] | (name,val) <- zip names vals]
                args = [anns (VarE 'atom `AppE` VarE name) as | (name,as) <- zip names ass]
            return $ LetE bind $ VarE 'record `AppE` (ConE x `apps` map VarE names) `AppE` ListE args

        f x = descendM f x

        apps x [] = x
        apps x (y:ys) = apps (x `AppE` y) ys

        asApps (AppE x y) = let (a,b) = asApps x in (a,b++[y])
        asApps x = (x,[])

        anns x [] = x
        anns x (a:as) = anns (InfixE (Just x) (VarE '(+=)) (Just a)) as

        asAnns (InfixE (Just x) (VarE op) (Just y)) | op == '(+=) = let (a,b) = asAnns x in (a,b++[y])
        asAnns (AppE (AppE (VarE op) x) y) | op == '(+=) = let (a,b) = asAnns x in (a,b++[y])
        asAnns x = (x, [])


-- | Move from the old names to the new names, sufficient for where that is the full translation
rename :: [Dec] -> [Dec]
rename = transformBi f
    where
        rep = let f a b c = [(a,c),(b,c)] in concat
            [f '(&=) '(&=#) '(+=)
            ,f 'modes 'modes# 'modes_
            ,f 'enum 'enum# 'enum_
            ,f 'cmdArgsMode 'cmdArgsMode# 'cmdArgsMode_
            ,f 'cmdArgs 'cmdArgs# 'cmdArgs_]

        f (VarE x) | Just x <- lookup x rep = VarE x
        f x = x


-- | Simplify the syntax tree - things like application of a lambda
simplify :: [Dec] -> [Dec]
simplify = transformBi f
    where
#if MIN_VERSION_template_haskell(2,22,0)
        f (AppE (LamE [VisAP (VarP v)] bod) x) = f $ subst v x bod
#else
        f (AppE (LamE [VarP v] bod) x) = f $ subst v x bod
#endif
        f x = x

        subst v x bod = transform f bod
            where f (VarE v2) | v == v2 = x
                  f x = x


-- | Evaluate through all locally defined functions and let expressions, at most once per defn
inline :: [Dec] -> [Dec]
inline xs = map (dec $ addEnv xs []) xs
    where
        newEnv = concatMap $ \x -> case x of
            FunD x [Clause ps (NormalB e) ds] -> [(x, LamE ps $ let_ ds e)]
            ValD (VarP x) (NormalB e) ds -> [(x, let_ ds e)]
            _ -> []

        addEnv xs env = without [] (newEnv xs) ++ env
            where
                -- create an environment where everything in ns is missing, recursively drop one thing each time
                without ns new = [(n, exp (new2 ++ env) e) | (n,e) <- new, n `notElem` ns, let new2 = without (n:ns) new]


        dec env (FunD n cs) = FunD n $ map (clause env) cs
        dec env (ValD p x ds) = ValD p (body (addEnv ds env) x) ds

        clause env (Clause ps x ds) = Clause ps (body (addEnv ds env) x) ds

        body env (GuardedB xs) = GuardedB $ map (second $ exp env) xs
        body env (NormalB x) = NormalB $ exp env x

        -- FIXME: propagating the env ignores variables shadowed by LamE/CaseE
        exp env (LetE ds x) = LetE ds $ exp (addEnv ds env) x
        exp env (VarE x) | Just x <- lookup x env = x
        exp env x = descend (exp env) x

        let_ ds e = if null ds then e else LetE ds e


---------------------------------------------------------------------
-- MINI UNIPLATE - Avoid the dependency just for one small module

descendBi :: (Data a, Data b) => (b -> b) -> a -> a
descendBi f x | Just f <- cast f = f x
              | otherwise = gmapT (descendBi f) x

descend :: Data a => (a -> a) -> a -> a
descend f = gmapT (descendBi f)

transform :: Data a => (a -> a) -> a -> a
transform f = f . descend (transform f)

transformBi :: (Data a, Data b) => (b -> b) -> a -> a
transformBi f = descendBi (transform f)

descendBiM :: (Data a, Data b, Monad m) => (b -> m b) -> a -> m a
descendBiM f x | Just x <- cast x = liftM (fromJust . cast) $ f x -- guaranteed safe
               | otherwise = gmapM (descendBiM f) x

descendM :: (Data a, Monad m) => (a -> m a) -> a -> m a
descendM f = gmapM (descendBiM f)
