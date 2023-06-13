echo off

rem ########################################################################## #
rem Frame  (©2023 Lelanthran Manickum)                                         #
rem                                                                            #
rem This program comes with ABSOLUTELY NO WARRANTY. This is free software      #
rem and you are welcome to redistribute it under certain conditions;  see      #
rem the LICENSE file for details.                                              #
rem ########################################################################## #

set DBPATH=%TEMP%\frame
set PROG=%CD%\recent\bin\x86_64-w64-mingw32\frame.exe --dbpath=%DBPATH%

set NC=[0m
set INV=[7m
set RED=[31m
set GREEN=[32m
set BLUE=[34m
set CYAN=[36m
set YELLOW=[33m
set HI_ON="%RED%%INV%"

set STMT_NUM=0

set LIMIT=%1

echo Removing %DBPATH%
rmdir /q /s  %DBPATH%

call :execute %PROG% create
call :execute %PROG% status
call :execute %PROG% history

echo Created

call :execute %PROG% replace --message="Root: replacement"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% push one --message="One: message one"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% append --message="\nMessage: 1\n"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% up
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% push two --message="Two: message two"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% top
call :execute %PROG% down one
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% back 1
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% back 1
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% push three --message="three: three"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% pop 
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% push four --message="four: four"
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% delete root/two 
call :execute %PROG% status
call :execute %PROG% history

call :execute %PROG% top
call :execute %PROG% status
call :execute %PROG% history


call :execute %PROG% push five --message="five: five"
call :execute %PROG% up
call :execute %PROG% push six --message="six: six"
call :execute %PROG% up
call :execute %PROG% push seven --message="seven: seven"
call :execute %PROG% up
call :execute %PROG% push eight --message="eight: eight"
call :execute %PROG% up
call :execute %PROG% push eighteen --message="eight: eight"
call :execute %PROG% up
call :execute %PROG% push eighty --message="eight: eight"
call :execute %PROG% up
call :execute %PROG% down one
call :execute %PROG% push one --message="new one"
call :execute %PROG% up
call :execute %PROG% push two --message="new one"
call :execute %PROG% up
call :execute %PROG% push three --message="new one"
call :execute %PROG% up
rem execute %PROG% push four --message="new one"
rem execute %PROG% up
call :execute %PROG% push five --message="new one"
call :execute %PROG% up
call :execute %PROG% push six --message="new one"
call :execute %PROG% up
call :execute %PROG% push seven --message="new one"
call :execute %PROG% up
call :execute %PROG% push eight --message="new one"
call :execute %PROG% up
call :execute %PROG% push nine --message="new one"
call :execute %PROG% up
call :execute %PROG% push ten --message="new one"
call :execute %PROG% up
call :execute %PROG% status
call :execute %PROG% list

rem Current node is root/one
call :execute %PROG% match --from-root "e"
call :execute %PROG% match --from-root "ei"
call :execute %PROG% match --from-root "eigh"

call :execute %PROG% match "e"
call :execute %PROG% match "ei"
call :execute %PROG% match "eigh"
call :execute %PROG% match "eigh" --invert
call :execute %PROG% status
call :execute %PROG% match "one/egh" > t
rem if [ `wc -l t | cut -f 1 -d \   ` -ne 1 ]; then
rem   die expected failed match
rem fi

rem Rename root/one/five
call :execute %PROG% switch root/one/five
call :execute %PROG% rename 'FIVE'
rem Current node is root/one/FIVE
call :execute %PROG% status

call :execute %PROG% match --from-root "one/ei"

call :execute %PROG% tree


exit 0

:execute
echo %RED%Executing%NC% %GREEN% %STMT_NUM%:%NC%
echo %BLUE%%*%NC%
set /A "STMT_NUM+=1"
%* --dbpath=%DBPATH% || goto :end
if %STMT_NUM% GEQ %LIMIT% goto :end
goto :eof

:end
exit

