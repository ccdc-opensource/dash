# Microsoft Developer Studio Project File - Name="PCDash" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=PCDash - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PCDash.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PCDash.mak" CFG="PCDash - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PCDash - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "PCDash - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "PCDash - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt /winapp
# ADD F90 /browser /compile_only /include:"Release/" /include:"c:\wint\lib.vf" /include:"c:\wint\include" /nologo /warn:nofileopt /winapp
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /i "c:\wint\include" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib kernel32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 winter.lib comdlg32.lib winspool.lib winmm.lib shell32.lib advapi32.lib opengl32.lib glu32.lib version.lib kernel32.lib /nologo /subsystem:windows /profile /machine:I386 /out:"C:\Program Files\DASH\PCDash.exe" /libpath:"c:\wint\lib.vf"

!ELSEIF  "$(CFG)" == "PCDash - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD F90 /browser /check:power /compile_only /debug:full /include:"Debug/" /include:"c:\wint\lib.vf" /include:"c:\wint\include" /nologo /traceback /warn:argument_checking /warn:nofileopt /warn:unused
# SUBTRACT F90 /check:bounds /check:format /check:output_conversion /check:overflow /check:underflow /warn:declarations /warn:truncated_source
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W4 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /i "c:\wint\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib kernel32.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /pdbtype:sept
# ADD LINK32 winter.lib winmm.lib version.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /pdbtype:sept /libpath:"c:\wint\lib.vf"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "PCDash - Win32 Release"
# Name "PCDash - Win32 Debug"
# Begin Group "Body"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Basic_IO.f90
DEP_F90_BASIC=\
	".\DRUID_HEADER.mod"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Basic_Strings.f90
# End Source File
# Begin Source File

SOURCE=.\Basic_Vectors.f90
# End Source File
# Begin Source File

SOURCE=.\BeginSa.f90
DEP_F90_BEGIN=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Ccslmain.for
DEP_F90_CCSLM=\
	".\Reflns.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Chi_sq_plot.F90
DEP_F90_CHI_S=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\create_fob.for
DEP_F90_CREAT=\
	".\IZMCheck.inc"\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Dialog_Routines.f90
DEP_F90_DIALO=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DoAtomPositions.for
# End Source File
# Begin Source File

SOURCE=.\error_message.f90
DEP_F90_ERROR=\
	".\Reflns.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Eval.for
DEP_F90_EVAL_=\
	".\IZMCheck.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Fcn.f90
DEP_F90_FCN_F=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Ffcalc.for
DEP_F90_FFCAL=\
	".\params.inc"\
	".\SGinc\ffcalc_001.inc"\
	".\SGinc\ffcalc_002.inc"\
	".\SGinc\ffcalc_039.inc"\
	".\SGinc\ffcalc_040.inc"\
	".\SGinc\ffcalc_044.inc"\
	".\SGinc\ffcalc_050.inc"\
	".\SGinc\ffcalc_052.inc"\
	".\SGinc\ffcalc_057.inc"\
	".\SGinc\ffcalc_058.inc"\
	".\SGinc\ffcalc_061.inc"\
	".\SGinc\ffcalc_064.inc"\
	".\SGinc\ffcalc_065.inc"\
	".\SGinc\ffcalc_066.inc"\
	".\SGinc\ffcalc_067.inc"\
	".\SGinc\ffcalc_069.inc"\
	".\SGinc\ffcalc_112.inc"\
	".\SGinc\ffcalc_115.inc"\
	".\SGinc\ffcalc_116.inc"\
	".\SGinc\ffcalc_143.inc"\
	".\SGinc\ffcalc_164.inc"\
	".\SGinc\ffcalc_176.inc"\
	".\SGinc\ffcalc_212.inc"\
	".\SGinc\ffcalc_266.inc"\
	".\SGinc\ffcalc_269.inc"\
	".\SGinc\ffcalc_284.inc"\
	".\SGinc\ffcalc_290.inc"\
	".\SGinc\ffcalc_292.inc"\
	".\SGinc\ffcalc_298.inc"\
	".\SGinc\ffcalc_304.inc"\
	".\SGinc\ffcalc_356.inc"\
	".\SGinc\ffcalc_365.inc"\
	".\SGinc\ffcalc_369.inc"\
	".\SGinc\ffcalc_430.inc"\
	".\SGinc\ffcalc_431.inc"\
	".\SGinc\ffcalc_432.inc"\
	".\SGinc\ffcalc_433.inc"\
	".\SGinc\ffcalc_434.inc"\
	".\SGinc\ffcalc_435.inc"\
	".\SGinc\ffcalc_449.inc"\
	".\SGinc\ffcalc_451.inc"\
	".\SGinc\ffcalc_462.inc"\
	".\SGinc\ffcalc_468.inc"\
	".\SGinc\ffcalc_469.inc"\
	".\SGinc\ffcalc_471.inc"\
	".\SGinc\ffcalc_481.inc"\
	".\SGinc\ffcalc_483.inc"\
	".\SGinc\ffcalc_485.inc"\
	".\SGinc\ffcalctop.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Fortic.for
DEP_F90_FORTI=\
	".\params.inc"\
	".\Reflns.inc"\
	".\SGinc\ffcalctop.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\FORTY.FOR
DEP_F90_FORTY=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\Reflns.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fou.for
# End Source File
# Begin Source File

SOURCE=.\Frac2cart.for
# End Source File
# Begin Source File

SOURCE=.\Generate_TicMarks.for
DEP_F90_GENER=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\statlog.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\get_logref.for
DEP_F90_GET_L=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\statlog.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\getdsl.f90
DEP_F90_GETDS=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\GETHCV.FOR
DEP_F90_GETHC=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\GETPIK.FOR
DEP_F90_GETPI=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Initialisation.f90
DEP_F90_INITI=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Interface.f90
DEP_F90_INTER=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mag.for
# End Source File
# Begin Source File

SOURCE=.\Main_Field_Changed_Routines.f90
DEP_F90_MAIN_=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\POLY_COLOURS.INC"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MCBack.f90
DEP_F90_MCBAC=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\multipeak_chisq.for
DEP_F90_MULTI=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\multipeak_fitter.for
DEP_F90_MULTIP=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\MultiRun.f90
DEP_F90_MULTIR=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mvec.for
# End Source File
# Begin Source File

SOURCE=.\Pawley.f90
DEP_F90_PAWLE=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pawley_error_check.f90
DEP_F90_PAWLEY=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PCDruid_Main.f90
DEP_F90_PCDRU=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PCDruid_Resource.F90
# End Source File
# Begin Source File

SOURCE=.\Pf.for
DEP_F90_PF_FO=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\PF_Simplex.for
# End Source File
# Begin Source File

SOURCE=.\plot_test.F90
DEP_F90_PLOT_=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PolyFitter_Subs.f90
DEP_F90_POLYF=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PolyLoadFiles.f90
DEP_F90_POLYL=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Pr.for
DEP_F90_PR_FO=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Profile_Plot.f90
DEP_F90_PROFI=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PutSimplexChisq.f90
DEP_F90_PUTSI=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_one_zm.for
# End Source File
# Begin Source File

SOURCE=.\Res2Mol2.f90
DEP_F90_RES2M=\
	".\Variables.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_Defaults.f90
# End Source File
# Begin Source File

SOURCE=.\SA_main.f90
DEP_F90_SA_MA=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\IZMCheck.inc"\
	".\Lattice.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sa_move_status.f90
DEP_F90_SA_MO=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_output.f90
DEP_F90_SA_OU=\
	".\DRUID_HEADER.mod"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sa_plot.f90
DEP_F90_SA_PL=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_Profile_Plot.f90
DEP_F90_SA_PR=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\statlog.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sa_refresh.f90
DEP_F90_SA_RE=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Sa_simopt.for
# End Source File
# Begin Source File

SOURCE=.\sa_simplex.f90
DEP_F90_SA_SI=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_structure_output.for
DEP_F90_SA_ST=\
	".\GLBVAR.INC"\
	".\IZMCheck.inc"\
	".\Lattice.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_SUBS.FOR
DEP_F90_SA_SU=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\sa_upload.f90
# End Source File
# Begin Source File

SOURCE=.\SAsoln_store.f90
DEP_F90_SASOL=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\SAsummary.f90
DEP_F90_SASUM=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SGDecode.for
# End Source File
# Begin Source File

SOURCE=.\Tutorials.f90
DEP_F90_TUTOR=\
	".\DRUID_HEADER.mod"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Upload_Cell_Constants.f90
DEP_F90_UPLOA=\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Upload_Widths.f90
DEP_F90_UPLOAD=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\UserInputChecks.f90
DEP_F90_USERI=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Utilities.for
# End Source File
# Begin Source File

SOURCE=.\Valchi.for
DEP_F90_VALCH=\
	".\AllFFCalc.inc"\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\statlog.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\valchipro.f90
DEP_F90_VALCHI=\
	".\AllFFCalc.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Vicar.for
# End Source File
# Begin Source File

SOURCE=.\Wifd99.for
DEP_F90_WIFD9=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Wizard_routines.f90
DEP_F90_WIZAR=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# End Group
# Begin Group "Body Includes"

# PROP Default_Filter "inc;INC"
# Begin Source File

SOURCE=.\AllFFCalc.inc
# End Source File
# Begin Source File

SOURCE=.\DialogPosCmn.inc
# End Source File
# Begin Source File

SOURCE=.\GET_LOGREF.inc
# End Source File
# Begin Source File

SOURCE=.\GLBVAR.INC
# End Source File
# Begin Source File

SOURCE=.\IZMCheck.inc
# End Source File
# Begin Source File

SOURCE=.\Lattice.inc
# End Source File
# Begin Source File

SOURCE=.\params.inc
# End Source File
# Begin Source File

SOURCE=.\POLY_COLOURS.INC
# End Source File
# Begin Source File

SOURCE=.\posns.inc
# End Source File
# Begin Source File

SOURCE=.\Reflns.inc
# End Source File
# Begin Source File

SOURCE=.\statlog.inc
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\BACK.BMP
# End Source File
# Begin Source File

SOURCE=.\res\done.ico
# End Source File
# Begin Source File

SOURCE=.\res\Druid.ico
# End Source File
# Begin Source File

SOURCE=.\res\EJECT.BMP
# End Source File
# Begin Source File

SOURCE=.\res\EmbossedProfile.bmp
# End Source File
# Begin Source File

SOURCE=.\res\fast_forward.BMP
# End Source File
# Begin Source File

SOURCE=.\res\fullrange.bmp
# End Source File
# Begin Source File

SOURCE=.\res\OpenFile.bmp
# End Source File
# Begin Source File

SOURCE=.\res\PAUSE.BMP
# End Source File
# Begin Source File

SOURCE=.\res\PawRefOptions.bmp
# End Source File
# Begin Source File

SOURCE=.\PCDruid_resource.RC
# End Source File
# Begin Source File

SOURCE=.\res\PLAY.BMP
# End Source File
# Begin Source File

SOURCE=.\res\PolyFitterWizard.bmp
# End Source File
# Begin Source File

SOURCE=.\res\PolyFitterWizardFull.bmp
# End Source File
# Begin Source File

SOURCE=.\res\PolyFitterWizardSmall.bmp
# End Source File
# Begin Source File

SOURCE=.\res\previous_track.BMP
# End Source File
# Begin Source File

SOURCE=.\res\STOP.BMP
# End Source File
# Begin Source File

SOURCE=.\res\temperature1.bmp
# End Source File
# Begin Source File

SOURCE=.\res\TOOLBAR1.BMP
# End Source File
# Begin Source File

SOURCE=.\res\WidthEquations.bmp
# End Source File
# Begin Source File

SOURCE=.\res\wizard.ico
# End Source File
# Begin Source File

SOURCE=.\res\WizardWelcome.bmp
# End Source File
# Begin Source File

SOURCE=.\res\ZOOM.BMP
# End Source File
# End Group
# Begin Group "SGinc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\SGinc\ffcalc_001.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_002.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_039.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_040.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_044.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_050.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_052.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_057.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_058.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_061.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_064.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_065.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_066.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_067.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_069.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_112.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_115.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_116.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_143.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_164.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_176.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_212.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_266.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_269.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_284.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_290.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_292.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_298.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_304.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_356.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_365.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_369.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_430.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_431.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_432.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_433.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_434.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_435.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_449.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_451.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_462.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_468.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_469.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_471.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_481.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_483.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalc_485.inc
# End Source File
# Begin Source File

SOURCE=.\SGinc\ffcalctop.inc
# End Source File
# End Group
# Begin Group "Licensing"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\licensing.f90
DEP_F90_LICEN=\
	".\DRUID_HEADER.mod"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# End Group
# Begin Source File

SOURCE=.\Buglist.txt
# End Source File
# Begin Source File

SOURCE=.\Interface.txt
# End Source File
# Begin Source File

SOURCE=.\UnusedCode.txt
# End Source File
# End Target
# End Project
