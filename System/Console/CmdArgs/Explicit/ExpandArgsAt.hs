{-# LANGUAGE RecordWildCards #-}
module System.Console.CmdArgs.Explicit.ExpandArgsAt(expandArgsAt) where

import System.FilePath


-- | Expand @\@@ directives in a list of arguments, usually obtained from 'getArgs'.
--   As an example, given the file @test.txt@ with the lines @hello@ and @world@:
--
-- > expandArgsAt ["@test.txt","!"] == ["hello","world","!"]
--
--   Any @\@@ directives in the files will be recursively expanded (raising an error
--   if there is infinite recursion).
--
--   To supress @\@@ expansion, pass any @\@@ arguments after @--@.
expandArgsAt :: [String] -> IO [String]
expandArgsAt args = do
        ebefore <- mapM (f [] ".") before
        return $ concat ebefore ++ after
    where
        (before,after) = break (== "--") args

        f seen dir ('@':x)
            | x `elem` seen = error $ unlines $
                "System.Console.CmdArgs.Explicit.expandArgsAt, recursion in @ directives:" :
                map ("  "++) (reverse $ x:seen)
            | length seen > 15 = error $ unlines $
                "System.Console.CmdArgs.Explicit.expandArgsAt, over 15 @ directives deep:" :
                map ("  "++) (reverse seen)
            | otherwise = do
                src <- readFile $ dir </> x
                fmap concat $ mapM (f (x:seen) (takeDirectory x)) $ lines src
        f _ _ x = return [x]
