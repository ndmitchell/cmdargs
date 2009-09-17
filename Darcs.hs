{-# LANGUAGE DeriveDataTypeable #-}
module Darcs where
import System.Console.CmdArgs

{-
COMMENTS

* The code in this file is not implemented in CmdArgs, it's a discussion point.

* I haven't specified advanced/common on each flag, but it's easy to add.
-}

data LongComment = Skip | Edit | Prompt

data Darcs
    = Help
        {helpCommands :: [String]}
    | Add
        {boring :: Bool
        ,case_ok :: Bool
        ,reserved_ok :: Bool
        ,recurse :: Bool
        ,date_trick :: Bool
        ,repodir :: Maybe String
        ,dry_run :: Bool
        ,umask :: Maybe String
        ,fileDirs :: [String]}
    | Remove
        {repodir :: Maybe Stsring
        ,umask :: Maybe String
        ,fileDirs :: [String]}
    | Mv
        {case_ok :: Bool
        ,reserved_ok :: Bool
        ,repodir :: Maybe String
        ,fileDirs :: [String]
        ,umask :: Maybe String}
    | Replace
        {token_chars :: [String]
        ,force :: Bool
        .repodir :: Maybe String
        ,umask :: Maybe String
        ,oldToken :: String
        ,newToken :: String
        ,argFiles :: [String]}
    | Revert
        {all_ :: Bool
        ,interactive :: Bool
        ,repodir :: Maybe String
        ,umask :: Maybe String}
        ,fileDirs :: [String]
    | Unrevert
        {ignore_times :: Bool
        ,all_ :: Bool
        ,interactive :: Bool
        ,repodir :: Maybe String
        ,umask :: Maybe String}
    | Whatsnew
        {summary :: Bool
        ,unified :: Bool
        ,look_for_adds :: Bool
        ,repodir :: Maybe String
        ,boring :: Bool
        ,fileDirs :: [String]}
    | Record
        {patch_name :: Maybe String
        ,author :: Maybe String
        ,test :: Bool
        ,leave_test_directory
        ,all_ :: Bool
        ,pipe :: Bool
        ,interactive :: Bool
        ,ask_deps :: Bool
        ,long_comment :: LongComment
        ,look_for_adds :: Bool
        ,repodir :: Maybe String
        ,logfile :: [String]
        ,delete_logfile :: Bool
        ,compress :: Bool
        ,ignore_times :: Bool
        ,umask :: Maybe String
        ,set_scripts_executable :: Bool
        }
    | Unrecord
        {from_match :: [String]
        ,from_patch :: [String]
        ,from_tag :: [String]
        ,last_ :: Maybe Int
        ,matches :: [String]
        ,patches :: [String]
        ,no_deps :: Bool
        ,prompt_for_dependencies :: Bool
        ,all_ :: Bool
        ,interactive :: Bool
        ,repodir :: Maybe String
        ,compress :: Bool
        ,umask :: Maybe String}
    | Amend_Record
        {match :: Maybe String
        ,patch :: Maybe String
        ,index_ :: Maybe (Int,Int)
        ,test :: Bool
        ,leave_test_directory :: Bool]
        ,all_ :: Bool
        ,interactive :: Bool
        ,author :: Maybe String
        ,patch_name :: Maybe String
        ,long_comment :: LongComment
        ,look_for_adds :: Bool
        ,compress :: Bool
        ,ignore_times :: Bool
        ,umask :: Maybe String
        ,set_scripts_executable :: Bool}
    | Mark_Conflicts
        {ignore_times :: Bool
        ,repodir :: Maybe String
        ,umask :: Maybe String}
    | Tag
        {tagname :: String
        ,patch_name :: Maybe String
        ,author :: Maybe String
        ,checkpoint :: Bool
        ,pipe :: Bool
        ,interactive :: Bool
        ,long_comment :: LongComment
        ,repodir :: Maybe String
        ,compress :: Bool
        ,umask :: Maybe String}
    | Setpref
        {pref :: String
        ,value :: String
        ,repodir :: Maybe String
        ,umask :: Maybe String}
    | ...
    | Show_Contents
        {match :: Maybe String
        ,patch :: Maybe String
        ,tag :: Maybe String
        ,index_ :: Maybe (Int,Int)
        ,repodir :: Maybe String
        ,argFiles :: [String]}
    | Show_Files
        {files :: Bool
        ,directories :: Bool
        ,pending :: Bool
        ,null_ :: Bool
        ,repodir :: Maybe String}
    | Show_Repo
        {repodir :: Maybe String
        ,files :: Bool
        ,xml_output :: Bool}
    | Show_Authors
        {repodir :: Maybe String}
    | Show_Tags
        {repodir :: Maybe String}
    | ...


darcs =
    [modeGroup "Commands:"
    ,mode $ Help
        {helpCommands = [] &= args & typ "[<DARCS_COMMAND> [DARCS_SUBCOMMAND]]"}
        & helpSuffix textHelp & text "Display help about darcs and darcs commands."
    ,modeGroup "Changing and querying the working copy:"
    ,mode $ Add
        {boring = False &= text "don't skip boring files"
        ,case_ok = False &= text "don't refuse to add files differing only in case"
        ,reserved_ok = False &= text "don't refuse to add files with Windows-reserved names"
        ,recursive = False &= flag "r" & text "add contents of subdirectories" &
                     negative (flag "not-recursive" & text "don't add contents of subdirectories")
        ,date_trick = False &= text "files with date appended to avoid conflict [EXPERIMENTAL]" &
                      negative (flag "no-date-trick" & text "don't use experimental date appending trick")
        ,dry_run = False &= text "don't actually take the action"
        ,repodir = Nothing &= typ "DIRECTORY" & text "specify the repository directory in which to run"
        ,umask = Nothing &= typ "UMASK" & text "specify umask to use when writing"
        ,fileDirs = [] &= typ "<FILE or DIRECTORY>"}
        & helpSuffix textAdd & text "Add one or more new files or directories."
    ,mode $ Remove {}
        & helpSuffix textRemove & text "Remove one or more files or directories from the repository."
    ,mode $ Mv {}
        & helpSuffix textMv & text "Move/rename one or more files or directories."
    ,mode $ Replace
        {token_chars = [] & typ "\"[CHARS]\"" & text "define token to contain these characters"
        ,force = False & flag "f" & text "proceed with replace even if 'new' token already" &
                 negative (flag "no-force" & text "don't force the replace if it looks scary")
        ,argFiles = [] & typ "<FILE>"}
        & helpSuffix textReplace & text "Replace a token with a new value for that token."
    ,mode $ Revert
        {all_ = False &= flag "a" & text "answer yes to all patches"
        ,interactive = False &= flag "i" & text "prompt user interactively"}
        & helpSuffix textRevert & text "Revert to the recorded version (not always reversible)."
    ,mode $ Unrevert
        {ignore_times = False &= text "don't trust the file modification times"}
        & helpSuffix textUnrevert & text "Undo the last revert (may fail if changes after the revert)."
    ,mode $ Whatsnew
        {summary = False &= flag "s" & text "summarize changes"
        ,unified = False &= flag "u" & text "output patch in a darcs-specific format similar to diff -u"
        ,look_for_adds = False &= flag "l" & text "look for (non-boring) files that could be added" &
                         negative (flag "dont-look-for-adds" & text "don't look for any files that could be added [DEFAULT]")}
        & helpSuffix textWhatsnew & text "List unrecorded changes in the working tree."
    ,modeGroup "Copying changes between the working copy and the repository:"
    ,mode $ Record
        {patch_name = Nothing &= flag "m" & typ "PATCHNAME" & text "name of patch"
        ,author = Nothing &= flag "A" & typ "EMAIL" & text "specify author id"
        ,test = True &= text "run the test script" &
                negative (text "don't run the test script")
        ,leave_test_directory = False &= text "don't remove the test directory" &
                                negative (flag "remote-test-directory" & text "remove the test directory")
        ,pipe = False &= text "ask user interactively for the patch metadata"
        ,ask_deps = False &= text "ask for extra dependencies" &
                    negative (text "don't ask for extra dependencies")
        ,long_comment = Prompt &= enum
            [Prompt &= text "prompt for whether to edit the long comment"
            ,Edit &= text "edit the long comment by default"
            ,Skip &= text "don't give a long comment"]
        ,look_for_adds = False &= flag "l" & text "look for (non-boring) files that could be added" &
                         negative (text "don't look for any files that could be added")
        ,logfile = [] &= typ "FILE" & text "give patch name and comment in file"
        ,delete_logfile = False &= text "delete the logfile when done"
        ,compress = False &= text "create compressed patches" &
                    negative (flag "dont-compress" & text "don't create compressed patches")
        ,ignore_times = False &= text "don't trust the file modification times"
        ,set_scripts_executable = False &= text "make scripts executable" &
                                  negative (flag "dont-set-scripts-executable" & text "don't make scripts executable")}
        &= helpSuffix textRecord & text "Save changes in the working copy to the repository as a patch."
    ,mode $ Unrecord
        {from_match = [] &= typ "PATTERN" & text "select changes starting with a patch matching PATTERN"
        ,from_patch = [] &= typ "REGEXP" & text "select changes starting with a patch matching REGEXP"
        ,from_tag = [] &= typ "REGEXP" & text "select changes starting with a tag matching REGEXP"
        ,last_ = Nothing &= typ "NUMBER" & text "select the last NUMBER patches"
        ,matches = [] &= typ "PATTERN" & text "select patches matching PATTERN"
        ,patches = [] &= flag "p" & typ "REGEXP" & text "select patches matching REGEXP"
        ,tags = [] &= flag "t" & typ "REGEXP" & text "select tags matching REGEXP"
        ,no_deps = False &= text "don't automatically fulfill dependencies"
        ,prompt_for_dependencies = True &= text "prompt about patches that are depended on by matched patches" &
                                   negative (flag "dont-prompt-for-dependencies" & text "don't ask about patches that are depended on by matched patches (with --match or --patch)"} &
        &= helpSuffix textUnrecord & text "Remove recorded patches without changing the working copy."
    ,mode $ Amend_Record
        {match = Nothing &= typ "PATTERN" & text "select a single patch matching PATTERN"
        ,patch = Nothing &= flag "p" & typ "REGEXP" & "text select a single patch matching REGEXP"
        ,index_ = Nothing &= typ "N-M" & text "select a range of patches" & parse parseRange}
        &= helpSuffix textAmendRecord & text "Replace a patch with a better version before it leaves your repository."
    ,mode $ Mark_Conflicts {}
        &= helpSuffix textMarkConflicts & text "Mark any unresolved conflicts in working copy, for manual resolution."
    ,modeGroup "Direct modification of the repository:"
    ,mode $ Tag
        {tagName = "" & typ "TAGNAME" & argPos 1}
        &= helpSuffix textTag & text "Name the current repository state for future reference."
    ,mode $ Setpref
        {pref = "" & typ "<PREF>" & argPos 1
        ,value = "" & typ "<VALUE>" & argPos 2}
        &= helpSuffix textSetpref & text "Set the value of a preference (test, predist, boringfile or binariesfile)."
    ,modeGroup "Querying the repository:"
    ,...
    ,modeSub "show" (text "Show information which is stored by darcs.")
        [mode $ Show_Contents{}
            &= text "Outputs a specific version of a file."
        ,mode $ Show_Files
            {files = True &= text "include files in output" & negative (text "don't include files in output")
            ,directories = True &= text "include directories in output" & negative (text "don't include directories in output")
            ,pending = True &= text "reflect pending patches in output" & negative (text "only included recorded patches in output")
            ,null_ = False &= flag "0" & text "separate file names by NUL characters"}
        ,mode $ Show_Repo
            {xml_output = False &= text "generate XML formatted output"}
            &= text "Show repository summary information"
        ,mode $ Show_Authors {}
            &= text "Show all authors in the repository."
        ,mode $ Show_Tags {}
            &= text "Show all tags in the repository."
        ]
    ,
    ...
    ]


data DarcsCommon = DarcsCommon
    {match :: Bool
    ,disable :: Bool
    ,help :: Bool
    ,debug :: Bool
    ,debug_verbose :: Bool
    ,debug_http :: Bool
    ,verbose :: Bool
    ,quiet :: Bool
    ,standard_verbosity :: Bool
    ,timings :: Bool
    ,posthook :: [String]
    ,no_posthook :: Bool
    ,prompt_posthook :: Bool
    ,prehook :: [String]
    .no_prehook :: Bool
    ,prompt_prehook :: Bool
    }


darcsCommon = mode $ DarcsCommon
    {match = False &= simple & text "shows a summary of how to use patch matching rules"
    ,disable = False &= simple & text "disable this command"
    ,help = False &= simple & flag "h" & "shows brief description of command and its arguments"
    ,debug = False &= text "give only debug output"
    ,debug_verbose = False &= text "give debug and verbose output"
    ,debug_http = False &= text "give debug output for curl and libwww"
    ,verbose = False &= flag "v" & text "give verbose output"
    ,quiet = False &= flag "q" & text "suppress informational output"
    ,standard_verbosity = False &= "neither verbose nor quiet output"
    ,timings = False &= "provide debugging timings information"
    ,posthook = [] &= text "specify command to run after this darcs command"
    ,no_posthook = False &= text "don't run posthook command"
    ,prompt_posthook = True &= text "prompt before running posthook" &
                       negative (flag "run-posthook" & text "run posthook command without prompting")
    ,prehook = [] &= text "specify command to run before this darcs command"
    ,no_prehook = False &= text "don't run prehook command"
    ,prompt_prehook = True &= text "prompt before running prehook" &
                      negative (flag "run-prehook" & text "run prehook command without prompting")


options = no_auto_short


main = print =<< cmdArgs "darcs version 2.2.1 (release)" modes



parseRange :: String -> Either String (Int,Int)
parseRange x | b /= [], [(a2,"")] <- reads a, [(b2,"")] <- b = Right (a2,b2)
             | otherwise = Left $ "Failed to parse a range: " ++ show x
    where (a,b) = break (== '-') x


textHelp =
    ["Without arguments, `darcs help' prints a categorized list of darcs commands " ++
     "and a short description of each one.  With an extra argument, `darcs help foo' " ++
     "prints detailed help about the darcs command foo."]

textAdd =
    ["Generally a repository contains both files that should be version " ++
     "controlled (such as source code) and files that Darcs should ignore " ++
     "(such as executables compiled from the source code).  The `darcs add' " ++
     "command is used to tell Darcs which files to version control." ++
    ,""
    ,"When an existing project is first imported into a Darcs repository, it " ++
     "is common to run `darcs add -r *' or `darcs record -l' to add all " ++
     "initial source files into darcs." ++
    ,""
    ,"Adding symbolic links (symlinks) is not supported." ++
    ,""
    ,"Darcs will ignore all files and folders that look `boring'.  The " ++
     "--boring option overrides this behaviour."
    ,"Darcs will not add file if another file in the same folder has the " ++
     "same name, except for case.  The --case-ok option overrides this " ++
     "behaviour.  Windows and OS X usually use filesystems that do not allow " ++
     "files a folder to have the same name except for case (for example, " ++
     "`ReadMe' and `README').  If --case-ok is used, the repository might be " ++
     "unusable on those systems!" ++
    ,""
    ,"The --date-trick option allows you to enable an experimental trick to " ++
     "make add conflicts, in which two users each add a file or directory " ++
     "with the same name, less problematic.  While this trick is completely " ++
     "safe, it is not clear to what extent it is beneficial."]

textRemove =
    ["Remove should be called when you want to remove a file from your project, " ++
     "but don't actually want to delete the file.  Otherwise just delete the " ++
     "file or directory, and darcs will notice that it has been removed. " ++
     "Be aware that the file WILL be deleted from any other copy of the " ++
     "repository to which you later apply the patch."]

textMv =
    ["Darcs mv needs to be called whenever you want to move files or " ++
     "directories. Unlike remove, mv actually performs the move itself in your " ++
     "working copy."]

textReplace =
    ["Replace allows you to change a specified token wherever it " ++
     "occurs in the specified files.  The replace is encoded in a " ++
     "special patch and will merge as expected with other patches. " ++
     "Tokens here are defined by a regexp specifying the characters " ++
     "which are allowed.  By default a token corresponds to a C identifier."]

textRevert =
    ["Revert is used to undo changes made to the working copy which have " ++
     "not yet been recorded.  You will be prompted for which changes you " ++
     "wish to undo. The last revert can be undone safely using the unrevert " ++
     "command if the working copy was not modified in the meantime."]

textUnrevert =
    ["Unrevert is a rescue command in case you accidentally reverted " ++
     "something you wanted to keep (for example, accidentally typing `darcs " ++
     "rev -a' instead of `darcs rec -a'). "
    ,""
    ,"This command may fail if the repository has changed since the revert " ++
     "took place.  Darcs will ask for confirmation before executing an " ++
     "interactive command that will *definitely* prevent unreversion."]

textWhatsnew =
    ["The `darcs whatsnew' command lists unrecorded changes to the working " ++
     "tree.  If you specify a set of files and directories, only unrecorded " ++
     "changes to those files and directories are listed."
    ,""
    ,"With the --summary option, the changes are condensed to one line per " ++
     "file, with mnemonics to indicate the nature and extent of the change. " ++
     "The --look-for-adds option causes candidates for `darcs add' to be " ++
     "included in the summary output."
    ,""
    ,"By default, `darcs whatsnew' uses Darcs' internal format for changes. " ++
     "To see some context (unchanged lines) around each change, use the " ++
     "--unified option.  To view changes in conventional `diff' format, use " ++
     "the `darcs diff' comand; but note that `darcs whatsnew' is faster."
    ,""
    ,"This command exits unsuccessfully (returns a non-zero exit status) if " ++
     "there are no unrecorded changes."]

textRecord =
    ["Record is used to name a set of changes and record the patch to the repository."]

textUnrecord =
    ["Unrecord does the opposite of record in that it makes the changes from " ++
     "patches active changes again which you may record or revert later.  The " ++
     "working copy itself will not change."
    ,""
    ,"Beware that you should not use this command if you are going to " ++
     "re-record the changes in any way and there is a possibility that " ++
     "another user may have already pulled the patch."]

textAmendRecord =
    ["Amend-record updates a `draft' patch with additions or improvements, " ++
     "resulting in a single `finished' patch.  This is better than recording " ++
     "the additions and improvements as separate patches, because then " ++
     "whenever the `draft' patch is copied between repositories, you would " ++
     "need to make sure all the extra patches are copied, too. "
    ,""
     "Do not copy draft patches between repositories, because a finished " ++
     "patch cannot be copied into a repository that contains a draft of the " ++
     "same patch.  If this has already happened, `darcs obliterate' can be " ++
     "used to remove the draft patch."
    ,""
     "Do not run amend-record in repository that other developers can pull " ++
     "from, because if they pull while an amend-record is in progress, their " ++
     "repository may be corrupted."
    ,""
     "When recording a draft patch, it is a good idea to start the name with " ++
     "`DRAFT:' so that other developers know it is not finished.  When " ++
     "finished, remove it with `darcs amend-record --edit-description'."
    ,""
     "Like `darcs record', if you call amend-record with files as arguments, " ++
     "you will only be asked about changes to those files.  So to amend a " ++
     "patch to foo.c with improvements in bar.c, you would run:"
    ,""
     "    darcs amend-record --match 'touch foo.c' bar.c"
    ,""
     "It is usually a bad idea to amend another developer's patch.  To make " ++
     "amend-record only ask about your own patches by default, you can add " ++
     "something like `amend-record match David Roundy' to ~/.darcs/defaults, " ++
     "where `David Roundy' is your name."

textMarkConflicts =
    ["Darcs requires human guidance to unify changes to the same part of a " ++
     "source file.  When a conflict first occurs, darcs will add both " ++
     "choices to the working tree, delimited by markers."
    ,""
    ,"However, you might revert or manually delete these markers without " ++
     "actually resolving the conflict.  In this case, `darcs mark-conflicts' " ++
     "is useful to show where any unresolved conflicts.  It is also useful " ++
     "if `darcs apply' is called with --apply-conflicts, where conflicts " ++
     "aren't marked initially."
    ,""
    ,"Any unrecorded changes to the working tree *will* be lost forever when " ++
     "you run this command!  You will be prompted for confirmation before " ++
     "this takes place."
    ,""
    ,"This command was historically called `resolve', and this deprecated " ++
     "alias still exists for backwards-compatibility."]

textTag =
    ["The `darcs tag' command names the current repository state, so that it " ++
     "can easily be referred to later.  Every `important' state should be " ++
     "tagged; in particular it is good practice to tag each stable release " ++
     "with a number or codename.  Advice on release numbering can be found " ++
     "at http://producingoss.com/en/development-cycle.html.
    ,""
    ,"To reproduce the state of a repository `R' as at tag `t', use the " ++
     "command `darcs get --tag t R'.  The command `darcs show tags' lists " ++
     "all tags in the current repository.
    ,""
    ,"Tagging also provides significant performance benefits: when Darcs " ++
     "reaches a shared tag that depends on all antecedent patches, it can " ++
     "simply stop processing.
    ,""
    ,"Like normal patches, a tag has a name, an author, a timestamp and an " ++
     "optional long description, but it does not change the working tree. " ++
     "A tag can have any name, but it is generally best to pick a naming " ++
     "scheme and stick to it.
    ,""
    ,"The `darcs tag' command accepts the --pipe and --checkpoint options, " ++
     "which behave as described in `darcs record' and `darcs optimize' " ++
     "respectively."]
