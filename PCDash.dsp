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
# ADD F90 /browser /check:power /compile_only /debug:full /include:"Debug/" /include:"c:\wint\lib.vf" /include:"c:\wint\include" /math_library:fast /nologo /optimize:0 /traceback /warn:argument_checking /warn:nofileopt /warn:unused
# SUBTRACT F90 /assume:buffered_io /assume:noaccuracy_sensitive /check:bounds /check:format /check:output_conversion /check:overflow /check:underflow /warn:declarations /warn:truncated_source /fast
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

SOURCE=.\Ccslmain.f90
DEP_F90_CCSLM=\
	".\DRUID_HEADER.mod"\
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

SOURCE=.\create_fob.f90
DEP_F90_CREAT=\
	".\IZMCheck.inc"\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Declarations.f90
DEP_F90_DECLA=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Dialog_Routines.f90
DEP_F90_DIALO=\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVAR.f90
# End Source File
# Begin Source File

SOURCE=.\DICVO2.f90
DEP_F90_DICVO=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO3.f90
DEP_F90_DICVO3=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO4.f90
DEP_F90_DICVO4=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO5.f90
DEP_F90_DICVO5=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO6.f90
DEP_F90_DICVO6=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO7.f90
DEP_F90_DICVO7=\
	".\Debug\DICVAR.MOD"\
	".\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVOL91.f90
DEP_F90_DICVOL=\
	".\Debug\DICVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\DoAtomPositions.f90
# End Source File
# Begin Source File

SOURCE=.\error_message.f90
DEP_F90_ERROR=\
	".\params.inc"\
	".\Reflns.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Eval.f90
DEP_F90_EVAL_=\
	".\IZMCheck.inc"\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Fcn.f90
# End Source File
# Begin Source File

SOURCE=.\Ffcalc.f90
DEP_F90_FFCAL=\
	".\params.inc"\
	".\SGinc\ffcalc_001.f90"\
	".\SGinc\ffcalc_002.f90"\
	".\SGinc\ffcalc_039.f90"\
	".\SGinc\ffcalc_040.f90"\
	".\SGinc\ffcalc_044.f90"\
	".\SGinc\ffcalc_050.f90"\
	".\SGinc\ffcalc_052.f90"\
	".\SGinc\ffcalc_057.f90"\
	".\SGinc\ffcalc_058.f90"\
	".\SGinc\ffcalc_061.f90"\
	".\SGinc\ffcalc_064.f90"\
	".\SGinc\ffcalc_065.f90"\
	".\SGinc\ffcalc_066.f90"\
	".\SGinc\ffcalc_067.f90"\
	".\SGinc\ffcalc_069.f90"\
	".\SGinc\ffcalc_112.f90"\
	".\SGinc\ffcalc_115.f90"\
	".\SGinc\ffcalc_116.f90"\
	".\SGinc\ffcalc_143.f90"\
	".\SGinc\ffcalc_164.f90"\
	".\SGinc\ffcalc_176.f90"\
	".\SGinc\ffcalc_212.f90"\
	".\SGinc\ffcalc_266.f90"\
	".\SGinc\ffcalc_269.f90"\
	".\SGinc\ffcalc_284.f90"\
	".\SGinc\ffcalc_290.f90"\
	".\SGinc\ffcalc_292.f90"\
	".\SGinc\ffcalc_298.f90"\
	".\SGinc\ffcalc_304.f90"\
	".\SGinc\ffcalc_356.f90"\
	".\SGinc\ffcalc_365.f90"\
	".\SGinc\ffcalc_369.f90"\
	".\SGinc\ffcalc_430.f90"\
	".\SGinc\ffcalc_431.f90"\
	".\SGinc\ffcalc_432.f90"\
	".\SGinc\ffcalc_433.f90"\
	".\SGinc\ffcalc_434.f90"\
	".\SGinc\ffcalc_435.f90"\
	".\SGinc\ffcalc_449.f90"\
	".\SGinc\ffcalc_451.f90"\
	".\SGinc\ffcalc_462.f90"\
	".\SGinc\ffcalc_468.f90"\
	".\SGinc\ffcalc_469.f90"\
	".\SGinc\ffcalc_471.f90"\
	".\SGinc\ffcalc_481.f90"\
	".\SGinc\ffcalc_483.f90"\
	".\SGinc\ffcalc_485.f90"\
	".\SGinc\ffcalctop.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Fortic.f90
DEP_F90_FORTI=\
	".\params.inc"\
	".\Reflns.inc"\
	".\SGinc\ffcalctop.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\forty.f90
DEP_F90_FORTY=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\Reflns.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Fou.f90
# End Source File
# Begin Source File

SOURCE=.\Frac2cart.f90
# End Source File
# Begin Source File

SOURCE=.\Generate_TicMarks.f90
DEP_F90_GENER=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\statlog.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\get_logref.f90
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

SOURCE=.\gethcv.f90
DEP_F90_GETHC=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\getpik.f90
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
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mag.f90
# End Source File
# Begin Source File

SOURCE=.\Main_Field_Changed_Routines.f90
DEP_F90_MAIN_=\
	".\Debug\DICVAR.MOD"\
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

SOURCE=.\MCBack.f90
DEP_F90_MCBAC=\
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

SOURCE=.\multipeak_chisq.f90
DEP_F90_MULTI=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\multipeak_fitter.f90
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

SOURCE=.\Mvec.f90
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

SOURCE=.\PCDash_Main.f90
DEP_F90_PCDAS=\
	".\DialogPosCmn.inc"\
	".\DRUID_HEADER.mod"\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PCDruid_Resource.F90
# End Source File
# Begin Source File

SOURCE=.\Pf.f90
DEP_F90_PF_F9=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\PF_Simplex.f90
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

SOURCE=.\Pr.f90
DEP_F90_PR_F9=\
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

SOURCE=.\read_one_zm.f90
DEP_F90_READ_=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Res2Mol2.f90
DEP_F90_RES2M=\
	".\Debug\SAMVAR.MOD"\
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
	".\params.inc"\
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
	".\params.inc"\
	".\Variables.mod"\
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

SOURCE=.\Sa_simopt.f90
# End Source File
# Begin Source File

SOURCE=.\sa_simplex.f90
DEP_F90_SA_SI=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_structure_output.f90
DEP_F90_SA_ST=\
	".\GLBVAR.INC"\
	".\IZMCheck.inc"\
	".\Lattice.inc"\
	".\params.inc"\
	".\statlog.inc"\
	".\Variables.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\sa_subs.f90
DEP_F90_SA_SU=\
	".\DRUID_HEADER.mod"\
	".\params.inc"\
	".\Variables.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\samabo.f90
DEP_F90_SAMAB=\
	".\Debug\SAMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\SAMVAR.f90
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

SOURCE=.\SGDecode.f90
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

SOURCE=.\Utilities.f90
# End Source File
# Begin Source File

SOURCE=.\Valchi.f90
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

SOURCE=.\variables.f90
DEP_F90_VARIA=\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Vicar.f90
# End Source File
# Begin Source File

SOURCE=.\Wifd99.f90
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

SOURCE=.\res\DASH.ico
# End Source File
# Begin Source File

SOURCE=.\res\done.ico
# End Source File
# Begin Source File

SOURCE=.\res\Druid.ico
# End Source File
# Begin Source File

SOURCE=.\res\Druid_2.ico
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
