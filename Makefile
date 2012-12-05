#############################################################################
# Makefile for building: dash.x
# Modified by hand based on that generated by qmake (2.01a) (Qt 4.4.2) 
#           on: Wed Feb 11 19:14:40 2009
# Project:  dash.pro
# Template: app
# Command: /local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/bin/qmake -unix CONFIG+=no_autoqmake CCDC_DIRECTORY=dash -o Makefile dash.pro
#############################################################################

####### Compiler, tools and options

DBGFLG        = -g -DONTBUG -Wall
OPTFLG        = -O3 # -Wuninitialized
ifeq ($(findstring release, $(QTDIR)), release)
	VERFLAG	= $(OPTFLG)
else
	VERFLAG	= $(DBGFLG)
endif
FF            = g95
RC            = $(WINTER)/bin/rc
CC            = gcc
CXX           = g++
DEFINES       = -DCCDC_STD_EXPORT= -DCCDC_THREAD_SUPPORT
FFLAGS        = -cpp -ffree-line-length-huge -fmod=obj $(VERFLAG) #-fstatic
RFLAGS        = -cg95 -i$(WINTER)/include 
CFLAGS        = -pipe -Wall -W $(DEFINES) $(VERFLAG)
CXXFLAGS      = -pipe -Wall -W $(DEFINES) $(VERFLAG)
INCPATH       = -I. -I$(WINTER)/lib.g95
LINK          = g95
LFLAGS        = $(VERFLAG)
LIBS          = $(SUBLIBS)  -L$(WINTER)/lib.g95 -lwint -Llibs -lXm -lXt -lX11 -lm
AR            = ar cqs
RANLIB        = 
QMAKE         = /local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/bin/qmake
TAR           = tar -cf
COMPRESS      = gzip -9f
COPY          = cp -f
SED           = sed
COPY_FILE     = $(COPY)
COPY_DIR      = $(COPY) -r
INSTALL_FILE  = install -m 644 -p
INSTALL_DIR   = $(COPY_DIR)
INSTALL_PROGRAM = install -m 755 -p
DEL_FILE      = rm -f
SYMLINK       = ln -sf
DEL_DIR       = rmdir
MOVE          = mv -f
CHK_DIR_EXISTS= test -d
MKDIR         = mkdir -p

####### Output directory

OBJECTS_DIR   = obj/

####### Files

SOURCES       = ATMVAR.f90 \
		Align.f90 \
		Basic_Date.f90 \
		Basic_Files.f90 \
		Basic_IO.f90 \
		Basic_Strings.f90 \
		Basic_Vectors.f90 \
		BatchMode.f90 \
		Ccslmain.f90 \
		CCDC_LICENSE_BINDINGS.f90 \
                DICVAR.f90 \
		DICVO2.f90 \
		DICVO3.f90 \
		DICVO4.f90 \
		DICVO5.f90 \
		DICVO6.f90 \
		DICVO7.f90 \
		DICVOL91.f90 \
		Declarations.f90 \
		Dialog_Routines.f90 \
		Eval.f90 \
		External_RR.f90 \
		FWHM.f90 \
		Fcn.f90 \
		Ffcalc.f90 \
		Fortic.f90 \
		Fou.f90 \
		Frac2cart.f90 \
		GSAS.f90 \
		Generate_TicMarks.f90 \
		Init_Routines.f90 \
		Initialisation.f90 \
		Interface.f90 \
		LoadRAWFiles.f90 \
		LoadSDI.f90 \
		MCBack.f90 \
		MDB.f90 \
		Mag.f90 \
		Main_Field_Changed_Routines.f90 \
		Mogul.f90 \
		MultiModal.f90 \
		MultiRun.f90 \
		Mvec.f90 \
		PCDash_Main.f90 \
		PF_Simplex.f90 \
		PO_VAR.f90 \
		PRJVAR.f90 \
		Pawley.f90 \
		Pf.f90 \
		PolyFitter_Subs.f90 \
		PolyLoadFiles.f90 \
		Pr.f90 \
		Profile_Plot.f90 \
		ProjectSave.f90 \
		REFVAR.f90 \
		RIETAN.f90 \
		RRVAR.f90 \
		RR_simopt.f90 \
		Res2Mol2.f90 \
		Rietveld.f90 \
		SAMVAR.f90 \
		SA_Begin.f90 \
		SA_Defaults.f90 \
		SA_Dialogues.f90 \
		SA_main.f90 \
		SA_move_status.f90 \
		SA_output.f90 \
		SA_refresh.f90 \
		SA_restrain.f90 \
		SA_simopt.f90 \
		SA_soln_store.f90 \
		SA_structure_output.f90 \
		SA_subs.f90 \
		SA_summary.f90 \
		SGDecode.f90 \
		SOLVAR.f90 \
		SingleCrystal.f90 \
		SpaceGroupDetermination.f90 \
		TAVAR.f90 \
		TOPAS.f90 \
		Tutorials.f90 \
		Upload_Cell_Constants.f90 \
		Upload_Widths.f90 \
		UserInputChecks.f90 \
		Utilities.f90 \
		Valchi.f90 \
		Vicar.f90 \
		Wifd99.f90 \
		Wizard_routines.f90 \
		XtalFile.f90 \
		ZMVAR.f90 \
		create_fob.f90 \
		error_message.f90 \
		forty.f90 \
		get_logref.f90 \
		licensing.f90 \
		multipeak_chisq.f90 \
		multipeak_fitter.f90 \
		pawley_error_check.f90 \
		read_one_zm.f90 \
		samabo.f90 \
		valchipro.f90 \
		variables.f90 \
		Chi_sq_plot.F90 \
		MONKEY1.F90 \
		PCDruid_Resource.F90 \
		plot_test.F90 \
		for_g95.f90
INCLS      = GLBVAR.INC \
		Lattice.inc \
		params.inc \
		Poly_Colours.inc \
		Reflns.inc \
		SA_restrain.inc \
		statlog.inc \
		SGinc/ffcalctop.inc 
RESSRC        = ./PCDruid_resource.RC \
		res/bimodal.ico \
		res/ChiSqd3.bmp \
		res/cooling.bmp \
		res/DASH.ico \
		res/DeleteFile.bmp \
		res/Del_abc.bmp \
		res/Del_PFR.bmp \
		res/Del_TickMarks.bmp \
		res/Down.bmp \
		res/FitPeak.bmp \
		res/Hourglass2.bmp \
		res/new.bmp \
		res/open.bmp \
		res/OpenFile.bmp \
		res/PolyFitterWizard.bmp \
		res/save.bmp \
		res/TOOLBAR1.BMP \
		res/trimodal.ico \
		res/Up.bmp \
		res/WidthEquations.bmp \
		res/wizard.bmp \
		res/WizardWelcome.bmp
MODS       = obj/atmvar.mod \
		obj/ccdc_license_bindings.mod \
                obj/dicvar.mod \
		obj/druid_header.mod \
		obj/po_var.mod \
		obj/prjvar.mod \
		obj/rrvar.mod \
		obj/refvar.mod \
		obj/samvar.mod \
		obj/solvar.mod \
		obj/tavar.mod \
		obj/zmvar.mod \
		obj/variables.mod \
		obj/for_g95.mod
OBJECTS       = obj/ATMVAR.o \
		obj/Align.o \
		obj/Basic_Date.o \
		obj/Basic_Files.o \
		obj/Basic_IO.o \
		obj/Basic_Strings.o \
		obj/Basic_Vectors.o \
		obj/BatchMode.o \
		obj/CCDC_LICENSE_BINDINGS.o \
                obj/Ccslmain.o \
		obj/DICVAR.o \
		obj/DICVO2.o \
		obj/DICVO3.o \
		obj/DICVO4.o \
		obj/DICVO5.o \
		obj/DICVO6.o \
		obj/DICVO7.o \
		obj/DICVOL91.o \
		obj/Declarations.o \
		obj/Dialog_Routines.o \
		obj/Eval.o \
		obj/External_RR.o \
		obj/FWHM.o \
		obj/Fcn.o \
		obj/Ffcalc.o \
		obj/Fortic.o \
		obj/Fou.o \
		obj/Frac2cart.o \
		obj/GSAS.o \
		obj/Generate_TicMarks.o \
		obj/Init_Routines.o \
		obj/Initialisation.o \
		obj/Interface.o \
		obj/LoadRAWFiles.o \
		obj/LoadSDI.o \
		obj/MCBack.o \
		obj/MDB.o \
		obj/Mag.o \
		obj/Main_Field_Changed_Routines.o \
		obj/Mogul.o \
		obj/MONKEY1.o \
		obj/MultiModal.o \
		obj/MultiRun.o \
		obj/Mvec.o \
		obj/PCDash_Main.o \
		obj/PF_Simplex.o \
		obj/PO_VAR.o \
		obj/PRJVAR.o \
		obj/Pawley.o \
		obj/Pf.o \
		obj/PolyFitter_Subs.o \
		obj/PolyLoadFiles.o \
		obj/Pr.o \
		obj/Profile_Plot.o \
		obj/ProjectSave.o \
		obj/REFVAR.o \
		obj/RIETAN.o \
		obj/RRVAR.o \
		obj/RR_simopt.o \
		obj/Res2Mol2.o \
		obj/Rietveld.o \
		obj/SAMVAR.o \
		obj/SA_Begin.o \
		obj/SA_Defaults.o \
		obj/SA_Dialogues.o \
		obj/SA_main.o \
		obj/SA_move_status.o \
		obj/SA_output.o \
		obj/SA_refresh.o \
		obj/SA_restrain.o \
		obj/SA_simopt.o \
		obj/SA_soln_store.o \
		obj/SA_structure_output.o \
		obj/SA_subs.o \
		obj/SA_summary.o \
		obj/SGDecode.o \
		obj/SOLVAR.o \
		obj/SingleCrystal.o \
		obj/SpaceGroupDetermination.o \
		obj/TAVAR.o \
		obj/TOPAS.o \
		obj/Tutorials.o \
		obj/Upload_Cell_Constants.o \
		obj/Upload_Widths.o \
		obj/UserInputChecks.o \
		obj/Utilities.o \
		obj/Valchi.o \
		obj/Vicar.o \
		obj/Wifd99.o \
		obj/Wizard_routines.o \
		obj/XtalFile.o \
		obj/ZMVAR.o \
		obj/create_fob.o \
		obj/error_message.o \
		obj/forty.o \
		obj/get_logref.o \
		obj/licensing.o \
		obj/multipeak_chisq.o \
		obj/multipeak_fitter.o \
		obj/pawley_error_check.o \
		obj/read_one_zm.o \
		obj/samabo.o \
		obj/valchipro.o \
		obj/variables.o \
		obj/Chi_sq_plot.o \
		obj/PCDruid_Resource.o \
		obj/plot_test.o \
		obj/for_g95.o
RESOBJ        = ./PCDruid_resource.o
DIST          = /local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/common/g++.conf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/common/unix.conf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/common/linux.conf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/qconfig.pri \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/qt_functions.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/qt_config.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/exclusive_builds.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/default_pre.prf \
		../cppbuilds_shared/setup_listdir.pri \
		../cppbuilds_shared/common_start.pri \
		../cppbuilds_shared/dependencies.pri \
		../cppbuilds_shared/platforms.pri \
		../cppbuilds_shared/common_end.pri \
		../cppbuilds_shared/ccdc_top.pri \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/debug.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/default_post.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/warn_on.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/resources.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/uic.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/yacc.prf \
		/local/buildman/tools/qt/qt-4.4.2-linux-deb4-debug/mkspecs/features/lex.prf \
		dash.pro

QMAKE_TARGET  = dash.x
DESTDIR       = 
TARGET        = dash.x

first: all
####### Implicit rules

.SUFFIXES: .o .c .cpp .cc .cxx .C

.cpp.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o "$@" "$<"

.cc.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o "$@" "$<"

.cxx.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o "$@" "$<"

.C.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o "$@" "$<"

.c.o:
	$(CC) -c $(CFLAGS) $(INCPATH) -o "$@" "$<"

####### Build rules

all: Makefile $(TARGET)

$(TARGET):  $(OBJECTS_DIR) $(MODS) $(OBJECTS)  $(RESOBJ)
	$(LINK) $(LFLAGS) -o $(TARGET) $(OBJECTS) $(RESOBJ) $(OBJCOMP) $(LIBS)

$(OBJECTS_DIR):
	@$(CHK_DIR_EXISTS) $(OBJECTS_DIR) || $(MKDIR) $(OBJECTS_DIR)

#qmake:  FORCE
#	@$(QMAKE) -unix CONFIG+=no_autoqmake CCDC_DIRECTORY=dash -o Makefile dash.pro

dist: 
	@$(CHK_DIR_EXISTS) obj/dash.x1.0.0 || $(MKDIR) obj/dash.x1.0.0 
	$(COPY_FILE) --parents $(SOURCES) $(DIST) obj/dash.x1.0.0/ && (cd `dirname obj/dash.x1.0.0` && $(TAR) dash.x1.0.0.tar dash.x1.0.0 && $(COMPRESS) dash.x1.0.0.tar) && $(MOVE) `dirname obj/dash.x1.0.0`/dash.x1.0.0.tar.gz . && $(DEL_FILE) -r obj/dash.x1.0.0


clean:compiler_clean 
	-$(DEL_FILE) $(OBJECTS) $(MODS) $(RESOBJ)
	-$(DEL_FILE) *~ core *.core


####### Sub-libraries

distclean: clean
	-$(DEL_FILE) $(TARGET) 
	-$(DEL_FILE) Makefile


compiler_rcc_make_all:
compiler_rcc_clean:
compiler_uic_make_all:
compiler_uic_clean:
compiler_image_collection_make_all: qmake_image_collection.cpp
compiler_image_collection_clean:
	-$(DEL_FILE) qmake_image_collection.cpp
compiler_yacc_decl_make_all:
compiler_yacc_decl_clean:
compiler_yacc_impl_make_all:
compiler_yacc_impl_clean:
compiler_lex_make_all:
compiler_lex_clean:
compiler_clean: 

####### Compile

obj/atmvar.mod obj/ATMVAR.o: ATMVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/ATMVAR.o ATMVAR.f90

obj/ccdc_license_bindings.mod obj/CCDC_LICENSE_BINDINGS.o: CCDC_LICENSE_BINDINGS.f90
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/CCDC_LICENSE_BINDINGS.o CCDC_LICENSE_BINDINGS.f90

obj/dicvar.mod obj/DICVAR.o: DICVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVAR.o DICVAR.f90

obj/druid_header.mod obj/PCDruid_Resource.o: PCDruid_Resource.F90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PCDruid_Resource.o PCDruid_Resource.F90

obj/po_var.mod obj/PO_VAR.o: PO_VAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PO_VAR.o PO_VAR.f90

obj/prjvar.mod obj/PRJVAR.o: PRJVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PRJVAR.o PRJVAR.f90

obj/refvar.mod obj/REFVAR.o: REFVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/REFVAR.o REFVAR.f90

obj/rrvar.mod obj/RRVAR.o: RRVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/RRVAR.o RRVAR.f90

obj/samvar.mod obj/SAMVAR.o: SAMVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SAMVAR.o SAMVAR.f90

obj/solvar.mod obj/SOLVAR.o: SOLVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SOLVAR.o SOLVAR.f90

obj/tavar.mod obj/TAVAR.o: TAVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/TAVAR.o TAVAR.f90

obj/variables.mod obj/variables.o: variables.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/variables.o variables.f90

obj/zmvar.mod obj/ZMVAR.o: ZMVAR.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/ZMVAR.o ZMVAR.f90

obj/for_g95.mod obj/for_g95.o: for_g95.f90 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/for_g95.o for_g95.f90

obj/Align.o: Align.f90 $(INCLS) $(MODS) 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Align.o Align.f90

obj/Basic_Date.o: Basic_Date.f90 $(INCLS) $(MODS) 
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Basic_Date.o Basic_Date.f90

obj/Basic_Files.o: Basic_Files.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Basic_Files.o Basic_Files.f90

obj/Basic_IO.o: Basic_IO.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Basic_IO.o Basic_IO.f90

obj/Basic_Strings.o: Basic_Strings.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Basic_Strings.o Basic_Strings.f90

obj/Basic_Vectors.o: Basic_Vectors.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Basic_Vectors.o Basic_Vectors.f90

obj/BatchMode.o: BatchMode.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/BatchMode.o BatchMode.f90

obj/Ccslmain.o: Ccslmain.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Ccslmain.o Ccslmain.f90

obj/DICVO2.o: DICVO2.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO2.o DICVO2.f90

obj/DICVO3.o: DICVO3.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO3.o DICVO3.f90

obj/DICVO4.o: DICVO4.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO4.o DICVO4.f90

obj/DICVO5.o: DICVO5.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO5.o DICVO5.f90

obj/DICVO6.o: DICVO6.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO6.o DICVO6.f90

obj/DICVO7.o: DICVO7.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVO7.o DICVO7.f90

obj/DICVOL91.o: DICVOL91.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/DICVOL91.o DICVOL91.f90

obj/Declarations.o: Declarations.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Declarations.o Declarations.f90

obj/Dialog_Routines.o: Dialog_Routines.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Dialog_Routines.o Dialog_Routines.f90

obj/Eval.o: Eval.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Eval.o Eval.f90

obj/External_RR.o: External_RR.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/External_RR.o External_RR.f90

obj/FWHM.o: FWHM.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/FWHM.o FWHM.f90

obj/Fcn.o: Fcn.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Fcn.o Fcn.f90

obj/Ffcalc.o: Ffcalc.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Ffcalc.o Ffcalc.f90

obj/Fortic.o: Fortic.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Fortic.o Fortic.f90

obj/Fou.o: Fou.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Fou.o Fou.f90

obj/Frac2cart.o: Frac2cart.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Frac2cart.o Frac2cart.f90

obj/GSAS.o: GSAS.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/GSAS.o GSAS.f90

obj/Generate_TicMarks.o: Generate_TicMarks.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Generate_TicMarks.o Generate_TicMarks.f90

obj/Init_Routines.o: Init_Routines.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Init_Routines.o Init_Routines.f90

obj/Initialisation.o: Initialisation.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Initialisation.o Initialisation.f90

obj/Interface.o: Interface.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Interface.o Interface.f90

obj/LoadRAWFiles.o: LoadRAWFiles.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/LoadRAWFiles.o LoadRAWFiles.f90

obj/LoadSDI.o: LoadSDI.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/LoadSDI.o LoadSDI.f90

obj/MCBack.o: MCBack.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/MCBack.o MCBack.f90

obj/MDB.o: MDB.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/MDB.o MDB.f90

obj/Mag.o: Mag.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Mag.o Mag.f90

obj/Main_Field_Changed_Routines.o: Main_Field_Changed_Routines.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Main_Field_Changed_Routines.o Main_Field_Changed_Routines.f90

obj/Mogul.o: Mogul.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Mogul.o Mogul.f90

obj/MultiModal.o: MultiModal.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/MultiModal.o MultiModal.f90

obj/MultiRun.o: MultiRun.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/MultiRun.o MultiRun.f90

obj/Mvec.o: Mvec.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Mvec.o Mvec.f90

obj/PCDash_Main.o: PCDash_Main.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PCDash_Main.o PCDash_Main.f90

obj/PF_Simplex.o: PF_Simplex.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PF_Simplex.o PF_Simplex.f90

obj/Pawley.o: Pawley.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Pawley.o Pawley.f90

obj/Pf.o: Pf.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Pf.o Pf.f90

obj/PolyFitter_Subs.o: PolyFitter_Subs.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PolyFitter_Subs.o PolyFitter_Subs.f90

obj/PolyLoadFiles.o: PolyLoadFiles.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/PolyLoadFiles.o PolyLoadFiles.f90

obj/Pr.o: Pr.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Pr.o Pr.f90

obj/Profile_Plot.o: Profile_Plot.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Profile_Plot.o Profile_Plot.f90

obj/ProjectSave.o: ProjectSave.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/ProjectSave.o ProjectSave.f90

obj/RIETAN.o: RIETAN.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/RIETAN.o RIETAN.f90

obj/RR_simopt.o: RR_simopt.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/RR_simopt.o RR_simopt.f90

obj/Res2Mol2.o: Res2Mol2.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Res2Mol2.o Res2Mol2.f90

obj/Rietveld.o: Rietveld.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Rietveld.o Rietveld.f90

obj/SA_Begin.o: SA_Begin.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_Begin.o SA_Begin.f90

obj/SA_Defaults.o: SA_Defaults.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_Defaults.o SA_Defaults.f90

obj/SA_Dialogues.o: SA_Dialogues.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_Dialogues.o SA_Dialogues.f90

obj/SA_main.o: SA_main.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_main.o SA_main.f90

obj/SA_move_status.o: SA_move_status.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_move_status.o SA_move_status.f90

obj/SA_output.o: SA_output.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_output.o SA_output.f90

obj/SA_refresh.o: SA_refresh.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_refresh.o SA_refresh.f90

obj/SA_restrain.o: SA_restrain.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_restrain.o SA_restrain.f90

obj/SA_simopt.o: SA_simopt.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_simopt.o SA_simopt.f90

obj/SA_soln_store.o: SA_soln_store.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_soln_store.o SA_soln_store.f90

obj/SA_structure_output.o: SA_structure_output.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_structure_output.o SA_structure_output.f90

obj/SA_subs.o: SA_subs.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_subs.o SA_subs.f90

obj/SA_summary.o: SA_summary.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SA_summary.o SA_summary.f90

obj/SGDecode.o: SGDecode.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SGDecode.o SGDecode.f90

obj/SingleCrystal.o: SingleCrystal.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SingleCrystal.o SingleCrystal.f90

obj/SpaceGroupDetermination.o: SpaceGroupDetermination.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/SpaceGroupDetermination.o SpaceGroupDetermination.f90

obj/TOPAS.o: TOPAS.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/TOPAS.o TOPAS.f90

obj/Tutorials.o: Tutorials.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Tutorials.o Tutorials.f90

obj/Upload_Cell_Constants.o: Upload_Cell_Constants.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Upload_Cell_Constants.o Upload_Cell_Constants.f90

obj/Upload_Widths.o: Upload_Widths.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Upload_Widths.o Upload_Widths.f90

obj/UserInputChecks.o: UserInputChecks.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/UserInputChecks.o UserInputChecks.f90

obj/Utilities.o: Utilities.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Utilities.o Utilities.f90

obj/Valchi.o: Valchi.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Valchi.o Valchi.f90

obj/Vicar.o: Vicar.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Vicar.o Vicar.f90

obj/Wifd99.o: Wifd99.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Wifd99.o Wifd99.f90

obj/Wizard_routines.o: Wizard_routines.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Wizard_routines.o Wizard_routines.f90

obj/XtalFile.o: XtalFile.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/XtalFile.o XtalFile.f90

obj/create_fob.o: create_fob.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/create_fob.o create_fob.f90

obj/error_message.o: error_message.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/error_message.o error_message.f90

obj/forty.o: forty.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/forty.o forty.f90

obj/get_logref.o: get_logref.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/get_logref.o get_logref.f90

obj/licensing.o: licensing.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/licensing.o licensing.f90

obj/multipeak_chisq.o: multipeak_chisq.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/multipeak_chisq.o multipeak_chisq.f90

obj/multipeak_fitter.o: multipeak_fitter.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/multipeak_fitter.o multipeak_fitter.f90

obj/pawley_error_check.o: pawley_error_check.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/pawley_error_check.o pawley_error_check.f90

obj/read_one_zm.o: read_one_zm.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/read_one_zm.o read_one_zm.f90

obj/samabo.o: samabo.f90 $(INCLS) $(MODS)  
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/samabo.o samabo.f90

obj/valchipro.o: valchipro.f90 $(INCLS) $(MODS)
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/valchipro.o valchipro.f90

obj/Chi_sq_plot.o: Chi_sq_plot.F90 $(INCLS) $(MODS)
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/Chi_sq_plot.o Chi_sq_plot.F90

obj/MONKEY1.o: MONKEY1.F90 $(INCLS) $(MODS)
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/MONKEY1.o MONKEY1.F90

obj/plot_test.o: plot_test.F90 $(INCLS) $(MODS)
	$(FF) -c $(FFLAGS) $(INCPATH) -o obj/plot_test.o plot_test.F90

./PCDruid_resource.o: $(RESSRC)
	$(RC) $(RFLAGS) PCDruid_resource.RC

####### Install

install:   FORCE

uninstall:   FORCE

FORCE:

