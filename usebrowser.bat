@echo off
set cmdargs=
if exist c:\spacework\cmdargs set cmdargs=c:\spacework\cmdargs
if exist c:\Neil\cmdargs set cmdargs=c:\Neil\cmdargs
if "%cmdargs%" == "" goto fail

set CMDARGS_HELPER="(cd %cmdargs%\cmdargs-browser && runghc -i..:Paths Main.hs)"
goto finish

:fail
echo Failed to find cmdargs repo on this machine

:finish
