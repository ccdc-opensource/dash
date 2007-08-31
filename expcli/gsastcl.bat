@ECHO OFF
REM a batch file to a DOS command and pause
if "%gsas%" == "" set gsas=c:\gsas
set PGPLOT_FONT=%gsas%\pgl\grfont.dat
set SYMOP=%gsas%\data\symop.dat
SETLOCAL
CALL :MATCH "%~1" "genles.exe *$"
IF ERRORLEVEL 1 GOTO L.S.
	"%~1" "%~2" "%~3" "%~4" "%~5" "%~6" "%~7"
	PAUSE
	GOTO ENDIF
:L.S.
	COPY /Y "%~2.EXP" "%~2.EXP.BAK"
	"%~1" "%~2" "%~3" "%~4" "%~5" "%~6" "%~7"
	ECHO. 
	SET /P _STR="Keep '%~2.EXP' file of this run? ( <Y>/N ) >"
	CALL :MATCH "%_STR%" "^ *n"
	IF ERRORLEVEL 1 COPY /Y "%~2.EXP.BAK" "%~2.EXP"
:ENDIF
del expgui.lck
exit

:MATCH
SET _PAT=%~2
ECHO %~1 | FINDSTR /I /R /C:"%_PAT%" > NUL && EXIT /B 1
EXIT /B 0
