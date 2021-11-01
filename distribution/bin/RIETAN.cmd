@ECHO OFF
REM Batch file to run RIETAN 2000/FP and display the resulting *.itx if any
CLS

IF x%2 == x GOTO usage
IF NOT EXIST "%~1" GOTO usage
IF NOT EXIST "%~2" GOTO usage

SETLOCAL
SET RIETAN=%~dp1
SET RIETANEXE=%~1 
SET SAMPLE=%~n2
CD %~dp2

SET REFINE=1
SET LST2CIF=1
SET ORFFE=
IF x%3 == x2 (
	SET ORFFE=1
	SET LST2CIF=
)
IF x%3 == x1 SET REFINE=
IF x%3 == x0 SET LST2CIF=

IF NOT DEFINED REFINE GOTO end_refine
IF EXIST "%SAMPLE%.itx" MOVE /Y "%SAMPLE%.itx" "%SAMPLE%.sav"
IF DEFINED ORFFE (
	IF EXIST "%SAMPLE%.xyz" MOVE /Y "%SAMPLE%.xyz" "%SAMPLE%.xyz.BAK"
)
ECHO RIETAN is now running ...
REM  Input *.ins: Standard input (record length of 80).
REM        *.int: X-ray/neutron diffraction data.
REM        *.bkg: Background intensities.
REM        *.ffe: Input data created by ORFFE for imposing constraints on interatomic distances and/or bond angles.
REM        *.fba: Data created by PRIMA for MEM-based whole-pattern fitting.
REM        *.ffi: Initial integrated intensities for Le Bail refinement.
REM Output *.itx: Data for plotting Rietveld-refinement patterns or a simulated pattern.
REM        *.hkl: Data for Fourier/D synthesis by FOUSYN.
REM        *.xyz: Data for calculating interatomic distances and bond angles by ORFFE.
REM        *.fos: Data for MEM analysis by PRIMA.
REM        *.ffo: Integrated intensities resulting from Le Bail refinement.
REM        *.vcs: VICS (VIsualization of Crystal Structures) text file.
REM        *.lst: Standard output.
COPY /Y "%SAMPLE%.ins" "%SAMPLE%.ins.BAK"
"%RIETANEXE%" "%SAMPLE%.ins" "%SAMPLE%.int" "%SAMPLE%.bkg" "%SAMPLE%.itx" "%SAMPLE%.hkl" "%SAMPLE%.xyz" "%SAMPLE%.fos" "%SAMPLE%.ffe" "%SAMPLE%.fba" "%SAMPLE%.ffi" "%SAMPLE%.ffo" "%SAMPLE%.vcs" | "%RIETAN%tee.exe" "%SAMPLE%.lst"
IF ERRORLEVEL 1 GOTO error
:end_refine

IF NOT DEFINED LST2CIF GOTO end_cif
IF EXIST "%SAMPLE%.lst" (
	IF EXIST "%SAMPLE%.cif" MOVE /Y "%SAMPLE%.cif" "%SAMPLE%.cif.BAK"
	set LST2CIF=%RIETAN%
	"%RIETAN%lst2cif.exe" "%SAMPLE%.lst"
	IF ERRORLEVEL 1 GOTO error
)
:end_cif

IF NOT DEFINED ORFFE GOTO end_orffe
ECHO ORFFE is now running ...
REM  Input *.xyz: ORFFE instructions.
REM Output *.dst: Standard output.
REM        *.ffe: Data for imposing restraints on interatomic distances and/or bond angles (a copy of *.dst).
IF NOT EXIST "%SAMPLE%.xyz" (
	ECHO Cannot find "%SAMPLE%.xyz", make sure NDA set correctly
	GOTO end_orffe
)
IF EXIST "%SAMPLE%.ffe" MOVE /Y "%SAMPLE%.ffe" "%SAMPLE%.ffe.BAK"
REM "%RIETAN%orffe.exe" "%SAMPLE%.xyz" | "%RIETAN%tee.exe" "%SAMPLE%.dst"
"%RIETAN%orffe.exe" "%SAMPLE%.xyz" > "%SAMPLE%.dst"
IF ERRORLEVEL 1 GOTO error
ECHO ORFFE Done
:end_orffe

IF NOT DEFINED REFINE GOTO end_refine2
IF NOT EXIST "%SAMPLE%.itx" GOTO end_plt
IF NOT EXIST "%RIETAN%gnuplot\bin\wgnuplot.exe" (
	ECHO.
	ECHO Cannot run gnuplot!
	ECHO No such file: "%RIETAN%gnuplot\bin\wgnuplot.exe"
	GOTO end_plt
)
IF EXIST "%SAMPLE%.plt" "%RIETAN%gnuplot\bin\wgnuplot.exe" /noend "%SAMPLE%.plt"
:end_plt
IF EXIST "%SAMPLE%.sav" DEL "%SAMPLE%.sav"
ECHO. 
SET /P _STR="Keep ins file of this run? ( <Y>/N ) >"
CALL :MATCH "%_STR%" "^ *n"
IF ERRORLEVEL 1 COPY /Y "%SAMPLE%.ins.BAK" "%SAMPLE%.ins"
GOTO end
:end_refine2

pause

:end
exit /B 0

:usage
ECHO Usage: %~nx0 Path_to_RIETAN.exe ins_File [task-code]
ECHO.
ECHO   task-code: 0 rietan; 1 lst2cif; 2 rietan+orffe; others rietan+lst2cif
ECHO.

:error
pause
exit /B 1

:MATCH
SET _PAT=%~2
ECHO %~1 | FINDSTR /I /R /C:"%_PAT%" > NUL && EXIT /B 1
EXIT /B 0
