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
# ADD F90 /alignment:commons /assume:buffered_io /assume:noaccuracy_sensitive /compile_only /include:"Release/" /include:"c:\wint\lib.vf" /include:"c:\wint\include" /math_library:fast /nologo /optimize:5 /warn:nofileopt
# SUBTRACT F90 /browser /fpp /winapp /fast
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
# ADD LINK32 winter.lib winmm.lib version.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /map /machine:I386 /out:"Debug\gDASH.exe" /libpath:"c:\wint\lib.vf"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "PCDash - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt /winapp
# ADD F90 /assume:buffered_io /assume:noaccuracy_sensitive /browser /check:bounds /compile_only /debug:full /define:"ONTBUG" /include:"Debug/" /include:"c:\wint\lib.vf" /include:"c:\wint\include" /math_library:fast /nologo /traceback /warn:argument_checking /warn:nofileopt /warn:truncated_source /warn:unused /fast
# SUBTRACT F90 /check:format /check:output_conversion /check:overflow /check:underflow /fpp /warn:declarations
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W4 /Gm /GX /ZI /O2 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /i "c:\wint\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib kernel32.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /pdbtype:sept
# ADD LINK32 winter.lib winmm.lib version.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /out:"Debug/CDASH.exe" /pdbtype:sept /libpath:"c:\wint\lib.vf"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "PCDash - Win32 Release"
# Name "PCDash - Win32 Debug"
# Begin Group "Body"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\Align.f90
DEP_F90_ALIGN=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\ATMVAR.f90
# End Source File
# Begin Source File

SOURCE=.\Basic_Date.f90
# End Source File
# Begin Source File

SOURCE=.\Basic_Files.f90
DEP_F90_BASIC=\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Basic_IO.f90
DEP_F90_BASIC_=\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Basic_Strings.f90
# End Source File
# Begin Source File

SOURCE=.\Basic_Vectors.f90
# End Source File
# Begin Source File

SOURCE=.\Ccslmain.f90
DEP_F90_CCSLM=\
	".\params.inc"\
	".\Reflns.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Chi_sq_plot.F90
DEP_F90_CHI_S=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\create_fob.f90
DEP_F90_CREAT=\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\RRVAR.MOD"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Declarations.f90
DEP_F90_DECLA=\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Dialog_Routines.f90
DEP_F90_DIALO=\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVAR.f90
# End Source File
# Begin Source File

SOURCE=.\DICVO2.f90
DEP_F90_DICVO=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO3.f90
DEP_F90_DICVO3=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO4.f90
DEP_F90_DICVO4=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO5.f90
DEP_F90_DICVO5=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO6.f90
DEP_F90_DICVO6=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVO7.f90
DEP_F90_DICVO7=\
	".\Release\DICVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\DICVOL91.f90
DEP_F90_DICVOL=\
	".\Release\DICVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\error_message.f90
DEP_F90_ERROR=\
	".\params.inc"\
	".\Reflns.inc"\
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Eval.f90
DEP_F90_EVAL_=\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Fcn.f90
# End Source File
# Begin Source File

SOURCE=.\Ffcalc.f90
DEP_F90_FFCAL=\
	".\Release\REFVAR.mod"\
	".\SGinc\ffcalctop.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\Fortic.f90
DEP_F90_FORTI=\
	".\params.inc"\
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\forty.f90
DEP_F90_FORTY=\
	".\params.inc"\
	".\Reflns.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
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
	".\params.inc"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\get_logref.f90
DEP_F90_GET_L=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Init_Routines.f90
DEP_F90_INIT_=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\PRJVAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Initialisation.f90
DEP_F90_INITI=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Interface.f90
DEP_F90_INTER=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\licensing.f90
DEP_F90_LICEN=\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\LoadRAWFiles.f90
DEP_F90_LOADR=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\LoadSDI.f90
DEP_F90_LOADS=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mag.f90
# End Source File
# Begin Source File

SOURCE=.\Main_Field_Changed_Routines.f90
DEP_F90_MAIN_=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DICVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MCBack.f90
DEP_F90_MCBAC=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\MONKEY1.F90
DEP_F90_MONKE=\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\multipeak_chisq.f90
DEP_F90_MULTI=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\multipeak_fitter.f90
DEP_F90_MULTIP=\
	".\params.inc"\
	".\Reflns.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\MultiRun.f90
DEP_F90_MULTIR=\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Mvec.f90
# End Source File
# Begin Source File

SOURCE=.\Pawley.f90
DEP_F90_PAWLE=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\pawley_error_check.f90
DEP_F90_PAWLEY=\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PCDash_Main.f90
DEP_F90_PCDAS=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\Release\DICVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PRJVAR.mod"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
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
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PF_Simplex.f90
DEP_F90_PF_SI=\
	".\params.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\plot_test.F90
DEP_F90_PLOT_=\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PO_VAR.f90
# End Source File
# Begin Source File

SOURCE=.\PolyFitter_Subs.f90
DEP_F90_POLYF=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PolyLoadFiles.f90
DEP_F90_POLYL=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	".\statlog.inc"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Pr.f90
DEP_F90_PR_F9=\
	".\params.inc"\
	".\Reflns.inc"\
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\PRJVAR.f90
# End Source File
# Begin Source File

SOURCE=.\Profile_Plot.f90
DEP_F90_PROFI=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ProjectSave.f90
DEP_F90_PROJE=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\PRJVAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\read_one_zm.f90
DEP_F90_READ_=\
	".\Release\ATMVAR.mod"\
	".\Release\SAMVAR.MOD"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\REFVAR.f90
# End Source File
# Begin Source File

SOURCE=.\Res2Mol2.f90
DEP_F90_RES2M=\
	".\Lattice.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\SAMVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\Rietveld.f90
DEP_F90_RIETV=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\RRVAR.MOD"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\RR_simopt.f90
DEP_F90_RR_SI=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\RRVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\RRVAR.f90
# End Source File
# Begin Source File

SOURCE=.\SA_Begin.f90
DEP_F90_SA_BE=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_Defaults.f90
DEP_F90_SA_DE=\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_Dialogues.f90
DEP_F90_SA_DI=\
	".\GLBVAR.INC"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SAMVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_main.f90
DEP_F90_SA_MA=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SAMVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_move_status.f90
DEP_F90_SA_MO=\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_output.f90
DEP_F90_SA_OU=\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_refresh.f90
DEP_F90_SA_RE=\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_simopt.f90
DEP_F90_SA_SI=\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_soln_store.f90
DEP_F90_SA_SO=\
	".\params.inc"\
	".\Release\VARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_structure_output.f90
DEP_F90_SA_ST=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_subs.f90
DEP_F90_SA_SU=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SA_summary.f90
DEP_F90_SA_SUM=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\POLY_COLOURS.INC"\
	".\Release\ATMVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\samabo.f90
DEP_F90_SAMAB=\
	".\Release\ATMVAR.mod"\
	".\Release\SAMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\SAMVAR.f90
# End Source File
# Begin Source File

SOURCE=.\SGDecode.f90
# End Source File
# Begin Source File

SOURCE=.\SOLVAR.f90
# End Source File
# Begin Source File

SOURCE=.\SpaceGroupDetermination.f90
DEP_F90_SPACE=\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Tutorials.f90
DEP_F90_TUTOR=\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Upload_Cell_Constants.f90
DEP_F90_UPLOA=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Upload_Widths.f90
DEP_F90_UPLOAD=\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\UserInputChecks.f90
DEP_F90_USERI=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Utilities.f90
# End Source File
# Begin Source File

SOURCE=.\Valchi.f90
DEP_F90_VALCH=\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\ATMVAR.mod"\
	".\Release\PO_VAR.mod"\
	".\Release\REFVAR.mod"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	
# End Source File
# Begin Source File

SOURCE=.\valchipro.f90
DEP_F90_VALCHI=\
	".\params.inc"\
	".\Release\REFVAR.mod"\
	
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
	".\Release\REFVAR.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Wizard_routines.f90
DEP_F90_WIZAR=\
	".\GLBVAR.INC"\
	".\Lattice.inc"\
	".\params.inc"\
	".\Release\DICVAR.mod"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\VARIABLES.mod"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\XtalFile.f90
DEP_F90_XTALF=\
	".\GLBVAR.INC"\
	".\Release\DRUID_HEADER.mod"\
	".\Release\RRVAR.MOD"\
	".\Release\SOLVAR.MOD"\
	".\Release\VARIABLES.mod"\
	".\Release\ZMVAR.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\ZMVAR.f90
# End Source File
# End Group
# Begin Group "Body Includes"

# PROP Default_Filter "inc;INC"
# Begin Source File

SOURCE=.\GLBVAR.INC
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

SOURCE=.\Reflns.inc
# End Source File
# Begin Source File

SOURCE=.\statlog.inc
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\DASH.ico
# End Source File
# Begin Source File

SOURCE=.\res\OpenFile.bmp
# End Source File
# Begin Source File

SOURCE=.\PCDruid_resource.RC
# End Source File
# Begin Source File

SOURCE=.\res\PolyFitterWizard.bmp
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

SOURCE=.\res\WizardWelcome.bmp
# End Source File
# End Group
# Begin Group "SGinc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\SGinc\ffcalctop.inc
# End Source File
# End Group
# Begin Source File

SOURCE=.\Buglist.txt
# End Source File
# Begin Source File

SOURCE=.\Interface.txt
# End Source File
# Begin Source File

SOURCE=.\ToDoBeforeRelease.txt
# End Source File
# Begin Source File

SOURCE=.\UnusedCode.txt
# End Source File
# End Target
# End Project
