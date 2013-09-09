@echo off

@REM Push DASH files into a Windows release layout.
@REM 1. The Release dash.exe in the current hierarchy is copied.
@REM 2. Other files are exported from CVS, using the specified revision.

REM this script needs to work in the top level directory

cd ..

call setup.bat

if X%1==X goto usage
    SET OUTPUTDIR=%1

if "%2"=="" goto usage
    SET CVSTAG=%2

if "%3"=="" goto usage
    SET CVSDOCTAG=%3

REM =================================================================================
REM DO some error-checking before we start copying files:

if exist %OUTPUTDIR% goto outputdir_ok
    REM attempt to create the output directory:
    mkdir %OUTPUTDIR%
    if not exist %OUTPUTDIR% goto nooutputdir

:outputdir_ok
    SET DASH_EXE=%BUILD_DIRECTORY%\dash\Release\DASH.exe

if not exist %DASH_EXE% goto noexe

REM For now, take these from the HEAD branch

SET ZMCONV_EXE=%BUILD_DIRECTORY%\makezmatrix\Release\makezmatrix.exe
SET ZMCONV_ROOT_DIR=%BUILD_DIRECTORY%

SET SPECIAL_POSN_EXE=%BUILD_DIRECTORY%\utilities\special_positions\release\special_positions.exe
SET SPECIAL_POSN_ROOT_DIR=%BUILD_DIRECTORY%

SET MERCURY_FOLDER=%DISTRIBUTION_DIRECTORY%\mercury\latest

REM =================================================================================
REM Copy files from dash project

    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/ExtinctionSymbol.exe"
REM        CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/special_positions.exe"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/SpaceGroupSymbols.dat"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/TOPAS.inc"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/RIETAN.cmd"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/RIETAN2000.tem"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%                          "dash/RIETANFP.tem"

    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%\icons                          "dash/icons"
    MKDIR "%OUTPUTDIR%\Unsupported Extras"
    CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Unsupported Extras"             "dash/Unsupported Extras/MDash.exe"
    CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Unsupported Extras"             "dash/Unsupported Extras/MDASH.pdf"
    CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Unsupported Extras"             "dash/Unsupported Extras/ReadMe.txt"
    CALL %BUILD_DIRECTORY%\export_command.bat %OUTPUTDIR%\expcli                         "dash/expcli"

REM Make Documentation areas

    MKDIR %OUTPUTDIR%\Documentation
    MKDIR %OUTPUTDIR%\Documentation\manual
    MKDIR %OUTPUTDIR%\Documentation\manual\output
    MKDIR "%OUTPUTDIR%\Documentation\Extinction Symbol Documents"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial1\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial1\Data files"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial2\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial2\Data files"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial3\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial3\Data files"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial4\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial4\Data files"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial5\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial5\Data files"
    MKDIR %OUTPUTDIR%\Documentation\Tutorial6\
    MKDIR "%OUTPUTDIR%\Documentation\Tutorial6\Data files"

REM copy tutorial files
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial1\Data files"    "dash/Tutorial_1.xye"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial1\Data files"    "dash/Tutorial_1.mol2"
    
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial2\Data files"    "dash/Tutorial_2.xye"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial2\Data Files"    "dash/Tutorial_2.mol2"
    
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3.xye"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-trans.mol2"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-cis.mol2"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelA.zmatrix"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelB.zmatrix"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelC.zmatrix"

        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4.xye"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4-half.mol2"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4-full.mol2"
    
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5.xye"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5.mol2"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5-2.mol2"
        
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6.raw"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6-atoms.mol2"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6-frags.mol2"

REM Copy Extinction Symbol Docs
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/Advanced.asc"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/Dopmmm.hkl"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/ESMANUAL for DASH.DOC"
        CALL %BUILD_DIRECTORY%\export_command.bat "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/parameter_input.asc"
    
    
REM =================================================================================
REM Copy files from doc_rep/dash

    SET CVS_REPOSITORY=":ssh;username=%USERNAME%;hostname=basalt:/cvs/doc_rep"
    SET CVSCMD=%CVS_EXECUTABLE% -d %CVS_REPOSITORY% -Q export



    %CVSCMD% -r %CVSDOCTAG% -d %OUTPUTDIR%\Documentation\manual\output\images "dash/output/images"
    %CVSCMD% -r %CVSDOCTAG% -d %OUTPUTDIR%\Documentation\manual\output\portable_html "dash/output/portable_html"
    %CVSCMD% -r %CVSDOCTAG% -d %OUTPUTDIR%\Documentation\manual\output\ "dash/output/pdf/dash_31.pdf"
    
REM == TODO: Extinction Symbol Docs ==

REM =================================================================================
REM Copy files from makezmatrix

set THIS_BUILD_DIRECTORY=%BUILD_DIRECTORY%

if not exist %ZMCONV_EXE% goto nozmconv

    set WD=%cd%
    cd %ZMCONV_ROOT_DIR%
    call copy_qt_exe.bat %ZMCONV_EXE% %OUTPUTDIR%\zmconv
REM  put the special positions exe in the same place as 
REM  ZMCONV to use same distributed libraries
    COPY %SPECIAL_POSN_EXE% %OUTPUTDIR%\zmconv 
    cd %WD%
    set BUILD_DIRECTORY=%THIS_BUILD_DIRECTORY%
    goto start_mercury
:nozmconv    
    echo Could not locate the zmconv executable %ZMCONV_EXE%
    echo distribution will be incomplete
    
:start_mercury

if not exist %MERCURY_FOLDER% goto nomercury
    echo %BUILD_DIRECTORY%\copy_directory.bat "%MERCURY_FOLDER%" "%OUTPUTDIR%\mercury"
    call %BUILD_DIRECTORY%\copy_directory.bat "%MERCURY_FOLDER%" "%OUTPUTDIR%\mercury"

    goto start_dash

:nomercury
    echo Could not locate the mercury executable %MERCURY_FOLDER%
    echo distribution will be incomplete
    
:start_dash    
    copy %DASH_EXE% %OUTPUTDIR%
    

    REM /q doesn't list names of each file copied
    REM /y over-writes any existing files
    REM /k preserves modification dates 
    set XCOPY_ARGS=/Q /Y /K
    
    REM Copy the Intel fortran DLLs
    
    REM TODO:  Hremove need for ugly hard coded path!!!
    set INTEL_DLL_FOLDER="C:\Program Files (x86)\Common Files\Intel\Shared Libraries\redist\ia32\compiler"
    
    XCOPY %XCOPY_ARGS% %INTEL_DLL_FOLDER%\libifcoremd.dll %OUTPUTDIR%
    XCOPY %XCOPY_ARGS% %INTEL_DLL_FOLDER%\libmmd.dll %OUTPUTDIR%
    XCOPY %XCOPY_ARGS% %INTEL_DLL_FOLDER%\svml_dispmd.dll %OUTPUTDIR%
    call copy_qt_exe.bat %DASH_EXE% %OUTPUTDIR%    
:start_mercury    

    set CURRENT_DIRECTORY=%cd%
    cd %OUTPUTDIR%\Mercury
    RENAME mercury.exe dash_mercury.exe
    
    cd %CURRENT_DIRECTORY%
    
    goto out

:nooutputdir
    echo Could not create the output directory %OUTPUTDIR%.  Note this is referenced from
    echo the root build directory, not the mogul subdirectory.
    echo Please check permissions.
    goto out

:noexe
    echo Could not locate the dash executable %DASH_EXE% from %cd%
    goto out

:usage
    echo Usage: %0 outputdir cvstag cvsdoctag
    echo -------------------------------------------------------------
    echo The script will create a DASH export hierarchy, for use
    echo with a PC installer program.
    echo -------------------------------------------------------------
    goto out

:out

    REM back to the initial directory
    cd dash
