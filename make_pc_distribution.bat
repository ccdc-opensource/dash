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
    SET DASH_EXE=%cd%\dash\Release\DASH.exe

if not exist %DASH_EXE% goto noexe

REM For now, take these from the HEAD branch

SET ZMCONV_EXE=D:\builds\DASH_3_2_BRANCH\makezmatrix\Release\makezmatrix.exe
SET ZMCONV_ROOT_DIR=D:\builds\DASH_3_2_BRANCH

SET SPECIAL_POSN_EXE=D:\builds\DASH_3_2_BRANCH\utilities\special_positions\release\special_positions.exe
SET SPECIAL_POSN_ROOT_DIR=D:\builds\DASH_3_2_BRANCH

SET MERCURY_EXE=D:\builds\CSDS_NOV_2009_BRANCH\mercury\mercury_quick_build\Release\mercury_quick_build.exe
SET MERCURY_ROOT_DIR=D:\builds\CSDS_NOV_2009_BRANCH

REM =================================================================================
REM Copy files from dash_rep/dash

    SET CVS_REPOSITORY=":ssh;username=%USERNAME%;hostname=basalt:/cvs/dash_rep"
    SET CVSCMD=%CVS_EXECUTABLE% -d %CVS_REPOSITORY% -Q export
    SET EXPORTSPEC=-r %CVSTAG%

    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/ExtinctionSymbol.exe"
!    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/special_positions.exe"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/SpaceGroupSymbols.dat"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/TOPAS.inc"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/RIETAN.cmd"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/RIETAN2000.tem"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%                          "dash/RIETANFP.tem"


    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%\icons                          "dash/icons"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Unsupported Extras"             "dash/Unsupported Extras"
    %CVSCMD% %EXPORTSPEC% -d %OUTPUTDIR%\expcli                         "dash/expcli"

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
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial1\Data files"    "dash/Tutorial_1.xye"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial1\Data files"    "dash/Tutorial_1.mol2"
    
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial2\Data files"    "dash/Tutorial_2.xye"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial2\Data Files"    "dash/Tutorial_2.mol2"
    
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3.xye"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-trans.mol2"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-cis.mol2"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelA.zmatrix"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelB.zmatrix"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial3\Data files"    "dash/Tutorial_3-ModelC.zmatrix"

    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4.xye"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4-half.mol2"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial4\Data files"    "dash/Tutorial_4-full.mol2"
    
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5.xye"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5.mol2"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial5\Data files"    "dash/Tutorial_5-2.mol2"
        
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6.raw"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6-atoms.mol2"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Tutorial6\Data files"    "dash/Tutorial_6-frags.mol2"

REM Copy Extinction Symbol Docs
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/Advanced.asc"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/Dopmmm.hkl"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/ESMANUAL for DASH.DOC"
    %CVSCMD% %EXPORTSPEC% -d "%OUTPUTDIR%\Documentation\Extinction Symbol Documents" "dash/Extinction Symbol Documents/parameter_input.asc"
    
    
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

if not exist %MERCURY_EXE% goto nomercury
    set WD=%cd%
    cd %MERCURY_ROOT_DIR%
    call copy_qt_exe.bat %MERCURY_EXE% %OUTPUTDIR%\Mercury
    cd %WD%
    set BUILD_DIRECTORY=%THIS_BUILD_DIRECTORY%
    goto start_dash

:nomercury
    echo Could not locate the mercury executable %MERCURY_EXE%
    echo distribution will be incomplete
    
:start_dash    
    copy %DASH_EXE% %OUTPUTDIR%
    

	  REM /q doesn't list names of each file copied
    REM /y over-writes any existing files
    REM /k preserves modification dates 
    set XCOPY_ARGS=/Q /Y /K
    
    REM Copy the Intel fortran DLLs
    
    REM TODO:	Hremove need for ugly hard coded path!!!
    set INTEL_DLL_FOLDER="C:\Program Files\Intel\Compiler\11.1\046\bin\ia32"
    XCOPY %XCOPY_ARGS% %INTEL_DLL_FOLDER%\lib*.dll %OUTPUTDIR%
    
:start_mercury    

    set CURRENT_DIRECTORY=%cd%
    cd %OUTPUTDIR%\Mercury
    RENAME mercury_quick_build.exe dash_mercury.exe
    
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
