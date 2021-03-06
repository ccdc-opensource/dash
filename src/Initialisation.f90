! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
#ifndef _WIN32
      MODULE CCDC_LINUX_LIBC_BINDINGS
      use ISO_C_BINDING
      use IFPORT
      INTERFACE
            function readlink(path, buf, bufsize) bind(C, NAME = 'readlink')
                  import
                  integer(C_SIZE_T) :: readlink
                  character(KIND = C_CHAR), intent(IN) :: path(*)
                  character(KIND = C_CHAR) :: buf(*)
                  integer(C_SIZE_T), value :: bufsize
            end function
      END INTERFACE
      END MODULE CCDC_LINUX_LIBC_BINDINGS
#endif

      SUBROUTINE ReadCurrentExecutableFullPath()
! Based on previous code and on idea from
! https://community.intel.com/t5/Intel-Fortran-Compiler/How-to-get-an-executable-s-location/td-p/749967
#ifdef _WIN32
      USE KERNEL32
#else
      use ISO_C_BINDING
      use IFPORT
      use CCDC_LINUX_LIBC_BINDINGS
#endif
      USE VARIABLES

      IMPLICIT NONE

#ifdef _WIN32
      INTEGER IDummy
      INTEGER(4) tProcess, tSize
      CHARACTER(MaxPathLength) tCurrentExecutable
      tSize = MaxPathLength
      IDummy = GetModuleFileName(tProcess, tCurrentExecutable, LOC(tSize))
      CurrentExecutableFullPath = TRIM(tCurrentExecutable)
#else

      integer :: pid, i, idx
      integer(C_SIZE_T) :: szret
      character(MaxPathLength) :: path
      character(KIND = C_CHAR) :: cbuf(MaxPathLength)
      pid = GETPID()
      write (path, '(i0)') pid
      path = '/proc/'//TRIM(path)//'/exe'
      szret = readlink(TRIM(path)//C_NULL_CHAR, cbuf, SIZE(cbuf, KIND = C_SIZE_T))
      if (szret == -1) stop 'Error reading link'

      path = ''
      do i = 1, SIZE(cbuf)
            if (cbuf(i) == C_NULL_CHAR) exit
            path(i:i) = cbuf(i)
      enddo
      CurrentExecutableFullPath = TRIM(path)
#endif
      END SUBROUTINE ReadCurrentExecutableFullPath

      SUBROUTINE InitialisePathLookupVariables
 
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) tInstallationDirectory
      CHARACTER(MaxPathLength) tCurrentExecutableDirectory
      CHARACTER(MaxPathLength) tCurrentExecutableFileName

      CALL ReadCurrentExecutableFullPath()
! tCurrentExecutable should now contain the full path to DASH.exe irrespective of the way
! DASH has been invoked.
      CALL SplitPath(CurrentExecutableFullPath, tCurrentExecutableDirectory, tCurrentExecutableFileName)
      IF (LEN_TRIM(tCurrentExecutableDirectory) .EQ. 0) tCurrentExecutableDirectory = '.'
      
! Store the directory where DASH was invoked from      
      CALL IOsDirName(StartUpDirectory)
      CALL IOsDirChange('..')
! If we were started from a directory whose parent has a share directory, we're probably being debugged
! In this case, consider the directory we were started in as the dash-distribution/bin directory 
      IF (IOsDirExists('share')) THEN
          Bindirectory = StartupDirectory 
      ELSE
! In any other case, use the directory where DASH.exe is and use that as the dash distribution bin directory
          BinDirectory = tCurrentExecutableDirectory
      END IF

! Cd in the bin directory and re-read the full directory name
      CALL IOsDirChange(BinDirectory)
      CALL IOsDirName(BinDirectory)
      CALL IOsDirChange('..')
      CALL IOsDirName(tInstallationDirectory)
! Finally, return to the directory from where we were invoked
      CALL IOsDirChange(StartUpDirectory)

! For debugging purposes
!      InstallationDirectory = 'D:\default\dash-install'
      ShareDashDirectory = TRIM(tInstallationDirectory)//DIRSPACER//"share"//DIRSPACER//"dash"
      DocumentationRoot = TRIM(tInstallationDirectory)//DIRSPACER//"share"//DIRSPACER//"doc"//DIRSPACER//"DASH"

#ifdef _WIN32
      CALL IOsVariable('APPDATA', AppDataDirectory)
#else
      CALL IOsVariable('HOME', AppDataDirectory)
#endif

      END SUBROUTINE InitialisePathLookupVariables
!
!*****************************************************************************
!
      SUBROUTINE Init_StdOut
! Selects the 'working directory' (which is changed every time the directory is changed
! when e.g. a Z-matrix is loaded, so this is not really '_the_' working directory).
! Checks if in that directory (but, as said, that directory changes all the time)
! temporary files can be created.
 
      USE WINTERACTER
      USE VARIABLES
#ifdef _WIN32
      USE KERNEL32
#endif
      IMPLICIT NONE

      INTEGER :: IFlags
      CHARACTER(MaxPathLength) :: Dirname

      DO WHILE (.TRUE.)
        IFlags = DirChange + DirCreate
        Dirname = ' '
        CALL WSelectDir(IFlags,Dirname,"Select working directory for DASH...")
        IF (LEN_TRIM(Dirname) .EQ. 0) THEN
          CALL WindowCloseWrap
          STOP
        ENDIF
! Open the file
        OPEN(UNIT = 6, FILE = 'dash.out', STATUS = 'UNKNOWN', ERR = 110)
        RETURN
 110    CALL ErrorMessage("DASH problem: Could not open temporary files"//CHAR(13)// &
                          "in the directory "//DirName(1:LEN_TRIM(DirName))//CHAR(13)//&
                          "Please pick an alternative directory for your DASH run.")
      ENDDO

      END SUBROUTINE Init_StdOut


      SUBROUTINE SelectDASHDialog(IDialogIdentifier)

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER IDialogIdentifier

      IF ( IDialogIdentifier .LE. 0 ) THEN
        CALL DebugErrorMessage("SelectDASHDialog: Bad dialog identifier")
        RETURN
      ENDIF
      CALL LoadDASHDialog(IDialogIdentifier)
      
      END SUBROUTINE SelectDASHDialog

      SUBROUTINE LoadDASHDialog(IDialogIdentifier)

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER IDialogIdentifier

      INTEGER MAX_DIALOG_IDENTIFIERS
      PARAMETER ( MAX_DIALOG_IDENTIFIERS = 5000 )
      INTEGER DIALOG_STATE(MAX_DIALOG_IDENTIFIERS)
      LOGICAL ROOT_OPEN
      COMMON / DASH_DIALOG_STATE / DIALOG_STATE, ROOT_OPEN

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( IDialogIdentifier .LE. 0 ) THEN
        CALL DebugErrorMessage("LoadDASHDialog: Bad dialog identifier")
        RETURN
      ENDIF


      IF ( DIALOG_STATE(IDialogIdentifier) .EQ. 0 ) THEN

        IF ( IN_BATCH .AND. (.NOT. ROOT_OPEN) ) THEN

! Open root window in hidden mode to allow dialogs being initialised properly.
! This is required, as set in 3.1.1 of Winteracter User Guide.
! Otherwise LoadDASHDialog will generate ErrLoadDialog while
! loading most dialogs. As error-check is usually not carried out,
! further operations on these dialogs may lead to more errors, even crash.

          CALL WindowOpen(FLAGS = HideWindow + SysMenuOn + MinButton + MaxButton + StatusBar, X = WInfoScreen(1)/10, &
                        Y = (WInfoScreen(2)/100) + 365, WIDTH = (WInfoScreen(1)*4)/5, &
                        HEIGHT = (WInfoScreen(2)*3)/8, MENUID = IDR_MENU1, &
                        TITLE = "DASH", NCOL256=128)
          ROOT_OPEN = .TRUE.
        ENDIF

        CALL WDialogLoad(IDialogIdentifier)
        DIALOG_STATE(IDialogIdentifier) = 1
      ELSE
        CALL WDialogSelect(IDialogIdentifier)
      ENDIF

      END SUBROUTINE LoadDASHDialog


      SUBROUTINE UnloadDASHDialog(IDialogIdentifier)

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER IDialogIdentifier

      INTEGER MAX_DIALOG_IDENTIFIERS
      PARAMETER ( MAX_DIALOG_IDENTIFIERS = 5000 )
      INTEGER DIALOG_STATE(MAX_DIALOG_IDENTIFIERS)
      LOGICAL ROOT_OPEN
      COMMON / DASH_DIALOG_STATE / DIALOG_STATE, ROOT_OPEN

      IF ( IDialogIdentifier .LE. 0 ) THEN
        CALL DebugErrorMessage("UnloadDASHDialog: Bad dialog identifier")
        RETURN
      ENDIF

      IF ( DIALOG_STATE(IDialogIdentifier) .EQ. 1 ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDialogIdentifier)
        CALL WDialogUnload()
        DIALOG_STATE(IDialogIdentifier) = 0
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE UnloadDASHDialog


      SUBROUTINE InitialiseDASHDialogState

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER MAX_DIALOG_IDENTIFIERS
      PARAMETER ( MAX_DIALOG_IDENTIFIERS = 5000 )
      INTEGER DIALOG_STATE(MAX_DIALOG_IDENTIFIERS)
      LOGICAL ROOT_OPEN
      COMMON / DASH_DIALOG_STATE / DIALOG_STATE, ROOT_OPEN

      INTEGER I

      DO I = 1, MAX_DIALOG_IDENTIFIERS
        DIALOG_STATE(I) = 0
      END DO

      END SUBROUTINE InitialiseDASHDialogState

!
!*****************************************************************************
!
! Rather than continually load/unload the various widgets, upload them all only once
! This way, the state can be memorised from session to session
      SUBROUTINE PolyFitter_UploadDialogues

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

! Problem here: apparently, WinterActer can only deal with up to 39 dialogue windows being
! loaded. We have quite a few more than that. As a result, some dialogues need to be
! swapped in and out of memory. This is ugly and error-prone, but that's the way it is.
! Currently those dialogues are IDD_SX_Page1a, IDD_PW_Page3a, D_SAW_Page6a and IDD_RR_External.

      CALL LoadDASHDialog(IDD_Structural_Information)         !  1
      CALL LoadDASHDialog(IDD_Configuration)                  !  2
      CALL LoadDASHDialog(IDD_Index_Preparation)              !  3
      CALL LoadDASHDialog(IDD_DV_Results)                     !  4
! Set the colours of the grid manually
      CALL LoadDASHDialog(IDD_Plot_Option_Dialog)             !  5
      CALL LoadDASHDialog(IDD_Polyfitter_Wizard_01)           !  6
      CALL LoadDASHDialog(IDD_PW_Page1)                       !  7
      CALL LoadDASHDialog(IDD_PW_Page3)                       !  8
      CALL LoadDASHDialog(IDD_PW_Page4)                       !  9
      CALL LoadDASHDialog(IDD_PW_Page5)                       ! 10
      CALL LoadDASHDialog(IDD_PW_Page6)                       ! 11
      CALL LoadDASHDialog(IDD_PW_Page7)                       ! 12
      CALL LoadDASHDialog(IDD_PW_Page8)                       ! 13
      CALL LoadDASHDialog(IDD_PW_Page8b)                      ! 14
      CALL LoadDASHDialog(IDD_PW_Page9)                       ! 15
      CALL LoadDASHDialog(IDD_PW_Page10)                      ! 16
      CALL LoadDASHDialog(IDD_SX_Page1)                       ! 17
      CALL LoadDASHDialog(IDD_SX_Page2)                       ! 18
      CALL LoadDASHDialog(IDD_Pawley_Status)                  ! 19
      CALL LoadDASHDialog(IDD_SAW_Page1)                      ! 20
      CALL LoadDASHDialog(IDD_zmEdit)                         ! 21
      CALL LoadDASHDialog(IDD_zmEditRotations)                ! 22
      CALL LoadDASHDialog(IDD_SAW_Page2)                      ! 23
      CALL LoadDASHDialog(IDD_SA_Modal_input2)                ! 24
      CALL LoadDASHDialog(IDD_ModalDialog)                    ! 25
      CALL LoadDASHDialog(IDD_SA_input3_2)                    ! 26
      CALL LoadDASHDialog(IDD_SA_input4)                      ! 27
      CALL LoadDASHDialog(IDD_SA_input5)                      ! 28
      CALL LoadDASHDialog(IDD_SA_Action1)                     ! 29
      CALL LoadDASHDialog(IDD_Summary)                        ! 30
      CALL LoadDASHDialog(IDD_SAW_Page5)                      ! 31
      CALL LoadDASHDialog(IDD_SAW_Page6)                      ! 32
!      CALL LoadDASHDialog(IDD_Parameter_Status_2)
      CALL LoadDASHDialog(IDD_OutputSolutions)                ! 33
      CALL LoadDASHDialog(IDD_Rietveld2)                      ! 34
      CALL LoadDASHDialog(IDD_RR_PO_Dialog)                   ! 35
      CALL LoadDASHDialog(IDD_SAW_Page7_TOPAS)                ! 36
      CALL LoadDASHDialog(IDD_SAW_Page7_GSAS)                 ! 37
      CALL LoadDASHDialog(IDD_SAW_Page7_RIETAN)               ! 38

      END SUBROUTINE PolyFitter_UploadDialogues
!
!*****************************************************************************
!
      SUBROUTINE PolyFitterInitialise

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      CHARACTER(LEN=128) lintem
      INTEGER I, II, nl
!O      DATA LPosSG/1,1,3,38,73,108,349,430,455,462,489,531/
      DATA CrystalSystemString /'Triclinic   ', 'Monoclinic-a', 'Monoclinic-b', &
                                'Monoclinic-c', 'Orthorhombic', 'Tetragonal  ', &
                                'Trigonal    ', 'Rhombohedral', 'Hexagonal   ', &
                                'Cubic       '/

      LPosSG( 1) =   1
      LPosSG( 2) =   3
      LPosSG( 3) =  38 
      LPosSG( 4) =  73
      LPosSG( 5) = 108
      LPosSG( 6) = 349
      LPosSG( 7) = 430
      LPosSG( 8) = 455
      LPosSG( 9) = 462
      LPosSG(10) = 489
      LPosSG(11) = MaxSPGR+1
! Get the space group symbols ...
      OPEN(110,FILE=TRIM(ShareDashDirectory)//DIRSPACER//'SpaceGroupSymbols.dat',STATUS='OLD', ERR = 999)
      i = 0
 10   lintem=' '
      READ(110,1100,END=100) lintem
      nl = LEN_TRIM(lintem)
 1100 FORMAT(A)
      IF (lintem(1:1) .EQ. '-') GOTO 100
      IF (nl .LT. 70) THEN
        DO ii = nl+1, 70
          lintem(ii:ii) = ' '
        ENDDO
      ENDIF
      i = i + 1
!    14:a1     P 21/b 1 1    -P 2xab           PMC$I1A000$P2A660 
!    14:a2     P 21/n 1 1    -P 2xn            PMC$I1A000$P2A666 
!    14:a3     P 21/c 1 1    -P 2xac           PMC$I1A000$P2A606 
!   165        P -3 c 1      -P 3 2"c          PRC$I3C000$P2F006 
!   166:H      R -3 m:H      -R 3 2"           RRC$I3C000$P2F000 
!   167:H      R -3 c:H      -R 3 2"c          RRC$I3C000$P2F006 
!   146:R      R 3:R          P 3*             PRN$P3Q000 
!12345678901234567890123456789012345678901234567890123456789012345678901234567890
!         1         2         3         4         5         6         7         8
      SGNumStr(i) = lintem(4:13)
      SGHMaStr(i) = lintem(15:26)
!U      SGHalStr(i) = lintem(29:46)
      SGShmStr(i) = lintem(47:70)
      GOTO 10
 100  IF (I .NE. MaxSPGR) THEN
        CALL ErrorMessage('Number of space groups in space-group file has changed.')
        CALL WindowCloseWrap
        STOP
      ENDIF
      CLOSE(110)
! Initialise space group to P 1
      NumberSGTable = 1
! Initialise crystal system to Triclinic, LatBrav = 1
      LatBrav = 1
      CALL Upload_CrystalSystem
      RETURN
 999  CONTINUE
! Failure, so exit gracefully
      CALL ErrorMessage("Sorry, DASH is not installed correctly: could not find the file"//CHAR(13) &
                          //'SpaceGroupSymbols.dat'//CHAR(13)// &
                          "in the installation directory"//CHAR(13)//&
                          TRIM(ShareDashDirectory))              
      CALL WindowCloseWrap
      STOP

      END SUBROUTINE PolyFitterInitialise
!
!*****************************************************************************
!
      SUBROUTINE InitialiseVariables(batch_setting)

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE ZMVAR
      USE REFVAR
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      LOGICAL           ChildWinAutoClose
      COMMON /ChWinAC/  ChildWinAutoClose(1:20)

      INTEGER*8         ChildWinHandler
      LOGICAL                                  ChildWinHandlerSet
      COMMON /ChWinHan/ ChildWinHandler(1:20), ChildWinHandlerSet(1:20)

      LOGICAL         MseBtnPressed, OldEventWaiting
      COMMON /Events/ MseBtnPressed, OldEventWaiting

      DATA MseBtnPressed   / .FALSE. /
      DATA OldEventWaiting / .FALSE. /

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      LOGICAL         InSA
      COMMON /SADATA/ InSA

      REAL           UR,     UI
      COMMON /FFTDA/ UR(15), UI(15)

      LOGICAL            ShowAgain
      INTEGER                       Counter
      COMMON  / DBGMSG / ShowAgain, Counter

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      REAL                PeakFindPos
      INTEGER                                           nPeaksFound
      COMMON / PEAKFIND / PeakFindPos(1:MaxPeaksFound), nPeaksFound

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL batch_setting
      REAL, EXTERNAL :: WaveLengthOf, dSpacing2TwoTheta
      INTEGER iWidth, iHeight
      PARAMETER (iWidth = 300, iHeight = 1)
      INTEGER tData(1:iWidth,1:iHeight)
      INTEGER I, J
      INTEGER iRed, iGreen, iBlue, iRGBvalue
      REAL    UM, TH

! The initialisations should be split up into 'one off initialisations' (at the start up
! of DASH only) and 'whenever a new project file is opened'
      in_batch = batch_setting
      SXMaxResolution = 1.75
      iRietveldMethod = INTERNAL_RB
      iRietveldMethodOpt = 1
      iMcMailleNgridOpt = 1
      lRebin = .FALSE.
      iBinWidth = 1
      RR_SA_Sol = 1
      
! Use Kabova (2017) settings as defaults
      NS = 73
      NT = 56
      RT = 0.27
      
      ISeed1 = 315
      ISeed2 = 159
      DO I = 1, MVAR
        ModalFlag(I) = 0 ! 0 = not a torsion angle
      ENDDO
      ShowAgain = .TRUE.
      Counter   = 0

      IF ( .not. in_batch ) THEN
        CALL SelectDASHDialog(IDD_SA_input3_2)
        CALL WDialogPutInteger(IDF_SA_NS, NS)
        CALL WDialogPutInteger(IDF_SA_NT, NT)
        CALL WDialogPutInteger(IDF_SA_RandomSeed1, ISeed1)
        CALL WDialogPutInteger(IDF_SA_RandomSeed2, ISeed2)
      ENDIF

      CALL Clear_Zmatrices
      ChiHandle = -1
      PI     = 4.0*ATAN(1.0)
      RAD    = PI/180.0
      DEG    = 180.0/PI
      TWOPI  = 2.0*PI
      FOURPI = 4.0*PI
      PIBY2  = PI/2.0
      ALOG2  = ALOG(2.0)
      SQL2X8 = SQRT(8.0*ALOG2)
      VALMUB = 0.2695
! Initialise some variables needed for Fast Fourier Transform
      UM = 0.5
      DO I = 1, 15
        UM = 0.5*UM
        TH = TWOPI*UM
        UR(I) = COS(TH)
        UI(I) = SIN(TH)
      ENDDO
      CALL CalCosArx
! Initialise path to viewer and argument for viewer. These will be overwritten if
! the configuration file is found and used.
      VIEWARG = ''
      ViewerExecutable = ''
      MOGULEXE = ' '
      DICVOLEXE = ''
      McMailleEXE = ''
      TOPASEXE = ''
      EXPGUIEXE = ''
      RIETANEXE = ''
      GSASINS = ''
      Rietan_FP = .FALSE.
      
      IF ( .NOT. in_batch ) then
        CALL GetPathToMercury
        CALL GetPathToMogulFromRegistry
        CALL GetPathToTopasFromRegistry
        CALL GetPathToExpguiFromRegistry
      ENDIF

      DO I = 0, maxfrg
        izmoid(0,I) = 0
        izmbid(0,I) = 0
      ENDDO
      CALL Clear_SA ! Sets NumOf_SA_Runs to 0
      CALL Update_Solutions
!      nPeaksFound = 0

      IF ( .NOT. in_batch ) THEN
        CALL SelectDASHDialog(IDD_SAW_Page5)
        CALL WDialogFieldState(IDB_Prog3, Disabled)
      ENDIF

      UseQuaternions = .TRUE.
! Initialise arrays to do with administration of open child windows
      ChildWinAutoClose = .FALSE.
      ChildWinHandlerSet = .FALSE.
      InSA = .FALSE.
      DashRawFile = ' '
      DashHcvFile = 'polyp.hcv'
      DashPikFile = 'polyp.pik'
      DashTicFile = 'polyp.tic'
      UseConfigFile = .TRUE.
      IDCurrent_Cursor_Mode = ID_Peak_Fitting_Mode
      DataSetChange = 0
      NumInternalDSC = -1
      ZeroPoint = 0.0
      PastPawley = .FALSE.
      NoWavelengthInXYE = .FALSE.
      T0 = 0.0

      ChiMult = 5.0 ! Dodgy: this must agree with the default specified in the resource file...
      MaxRuns = 25  ! Dodgy: this must agree with the default specified in the resource file...
      MaxMoves = 10000000 ! Dodgy: this must agree with the default specified in the resource file...
      CALL Set_Wavelength(WaveLengthOf('Cu'))
! Now initialise the maximum resolution in the dialogue window
      DefaultMaxResolution = DASHDefaultMaxResolution
      CALL Update_TruncationLimits
      
      IF ( .NOT. in_batch ) THEN 
        CALL SelectDASHDialog(IDD_Index_Preparation)
        CALL WDialogPutReal(IDF_eps, 0.03, '(F5.3)')
        CALL SelectDASHDialog(IDD_Configuration)
        CALL WDialogPutString(IDF_ViewExe, ViewerExecutable)
        CALL WDialogPutString(IDF_ViewArg, ViewArg)
        CALL WDialogPutString(IDF_MogulExe, MogulExe)
        CALL WDialogPutString(IDF_TopasExe, TopasExe)
        CALL WDialogPutString(IDF_EXPGUIExe, ExpguiExe)
      ENDIF

      CALL Set_AutoLocalMinimisation(.TRUE.)
      CALL Set_SimplexOnly(.FALSE.)
      CALL Set_UseHydrogensDuringAutoLocalMinimise(.TRUE.)
      CALL Set_UseCrystallographicCoM(.TRUE.)
      CALL Set_AutoAlign(.TRUE.)
      Call Set_HydrogenTreatment(2) ! Absorb

      CALL Set_SavePRO(.FALSE.)
      CALL Set_SaveRES(.FALSE.)
      CALL Set_SaveCCL(.FALSE.)
      CALL Set_SaveCIF(.FALSE.)
      CALL Set_SaveCSSR(.FALSE.)
      CALL Set_SavePDB(.FALSE.)
      CALL Set_OutputChi2vsMoves(.FALSE. )
      CALL Set_SavePrjAtEnd(.TRUE.)
      CALL Set_SaveParamAtEnd(.FALSE.)


      RandomInitVal = .TRUE.
      SA_SimplexDampingFactor = 0.1
      
      IF ( .NOT. in_batch ) THEN
        CALL SelectDASHDialog(IDD_SA_Modal_input2)
        CALL WDialogPutCheckBoxLogical(IDF_RandomInitVal, RandomInitVal)
! Grey out the "Previous Results >" button in the DICVOL Wizard window
        CALL SelectDASHDialog(IDD_PW_Page8)
        CALL WDialogFieldState(IDB_PrevRes,Disabled)
! Temp: Hide run topas in background, as requested in RT6522
        CALL SelectDASHDialog(IDD_Configuration)
        CALL WDialogFieldState(IDC_TOPAS_in_background, DialogHidden)
! Hide bkg term
        CALL SelectDASHDialog(IDD_PW_Page6)
        CALL WDialogFieldState(IDF_LABEL4, DialogHidden)
        CALL WDialogFieldState(IDF_BKG_TERM_TOPAS, DialogHidden)
        CALL WDialogFieldState(IDF_LABEL5, DialogHidden)
        CALL WDialogFieldState(IDF_BKG_TERM_GSAS, DialogHidden)
        CALL WDialogFieldState(IDF_LABEL6, DialogHidden)
        CALL WDialogFieldState(IDF_BKG_TERM_RIETAN, DialogHidden)
        CALL SelectDASHDialog(IDD_PW_Page4)
        CALL WDialogFieldState(IDB_PO, DialogHidden)
        CALL WDialogFieldState(IDF_GSAS_Import_ins, DialogHidden)
! Grey out 'Remove background' button on toolbar
        CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOff)
      ENDIF

      nPeaksFound = 0
      SlimValue = 1.0
      ScalFac   = 0.01
      BackRef   = .TRUE.
      JRadOption = 1 ! Initialise to X-ray lab data
      IXPos_IDD_Wizard = 0.5  * (WInfoScreen(1) - 756.0)
      IYPos_IDD_Wizard = 0.01 * WInfoScreen(2)
      NumOfRef = 0
      MARKER_SIZE = 0.35
      CHAR_SIZE = 1.0
      XPG1 = 0.12
      XPG2 = 0.95
      YPG1 = 0.12
      YPG2 = 0.93
      KolNumPGWindow       = 220
      KolNumMain           = 221
      KolNumObs            = 222
      KolNumCal            = 223
      KolNumDif            = 224
      KolNumMTic           = 225
      KolNumCTic           = 226
      KolNumPanelVLite     = 227
      KolNumPanelLite      = 228
      KolNumPanelDark      = 229
      KolNumPanelVDark     = 230
      KolNumPanelOuter     = 231
      KolNumRectSelect     = 232
      KolNumLargeCrossHair = 233
      KolNumPeakFit        = 234
      KolNumPeakPos        = 235
      KolNumBack           = 236
      KolPGWindow       = Win_RGB(253, 253, 248)
      KolMain           = Win_RGB( 20,  20, 150)
      KolObs            = Win_RGB(255,   0,   0)
      KolCal            = Win_RGB(  0,   0, 255)
      KolDif            = Win_RGB(200, 100, 200)
      KolMTic           = Win_RGB(  0,   0,   0)
      KolCTic           = Win_RGB(160,  32, 240)
      KolPanelVLite     = Win_RGB(245, 245, 245)
      KolPanelLite      = Win_RGB(235, 235, 235)
      KolPanelDark      = Win_RGB(210, 210, 210)
      KolPanelVDark     = Win_RGB(170, 170, 170)
      KolPanelOuter     = Win_RGB(190, 190, 190)
      KolRectSelect     = Win_RGB(150, 150,   5)
      KolLargeCrossHair = Win_RGB(150, 150,   5)
      KolPeakFit        = Win_RGB(  0, 131, 131)
      KolPeakPos        = Win_RGB( 50,  50, 200)
      KolBack           = Win_RGB(164, 211, 105)

      CALL ReadConfigurationFile
      ! Only need to initialise colour and bitmap for gui mode
      IF (.NOT. in_batch) THEN

        CALL IGrPaletteRGB(KolNumPGWindow,KolPGWindow%IRed,&
                                          KolPGWindow%IGreen,&
                                          KolPGWindow%IBlue)
        CALL IGrPaletteRGB(KolNumMain,KolMain%IRed,&
                                      KolMain%IGreen,&
                                      KolMain%IBlue)
        CALL IGrPaletteRGB(KolNumObs,KolObs%IRed,&
                                     KolObs%IGreen,&
                                     KolObs%IBlue)
        CALL IGrPaletteRGB(KolNumCal,KolCal%IRed,&
                                     KolCal%IGreen,&
                                     KolCal%IBlue)
        CALL IGrPaletteRGB(KolNumDif,KolDif%IRed,&
                                     KolDif%IGreen,&
                                     KolDif%IBlue)
        CALL IGrPaletteRGB(KolNumMTic,KolMTic%IRed,&
                                      KolMTic%IGreen,&
                                      KolMTic%IBlue)
        CALL IGrPaletteRGB(KolNumCTic,KolCTic%IRed,&
                                      KolCTic%IGreen,&
                                      KolCTic%IBlue)
        CALL IGrPaletteRGB(KolNumPanelVLite,KolPanelVLite%IRed,&
                                            KolPanelVLite%IGreen,&
                                            KolPanelVLite%IBlue)
        CALL IGrPaletteRGB(KolNumPanelLite,KolPanelLite%IRed,&
                                           KolPanelLite%IGreen,&
                                           KolPanelLite%IBlue)
        CALL IGrPaletteRGB(KolNumPanelDark,KolPanelDark%IRed,&
                                           KolPanelDark%IGreen,&
                                           KolPanelDark%IBlue)
        CALL IGrPaletteRGB(KolNumPanelVDark,KolPanelVDark%IRed,&
                                            KolPanelVDark%IGreen,&
                                            KolPanelVDark%IBlue)
        CALL IGrPaletteRGB(KolNumPanelOuter,KolPanelOuter%IRed,&
                                            KolPanelOuter%IGreen,&
                                            KolPanelOuter%IBlue)
        CALL IGrPaletteRGB(KolNumRectSelect,KolRectSelect%IRed,&
                                            KolRectSelect%IGreen,&
                                            KolRectSelect%IBlue)
        CALL IGrPaletteRGB(KolNumLargeCrossHair,KolLargeCrossHair%IRed,&
                                                KolLargeCrossHair%IGreen,&
                                                KolLargeCrossHair%IBlue)
        CALL IGrPaletteRGB(KolNumPeakFit,KolPeakFit%IRed,&
                                         KolPeakFit%IGreen,&
                                         KolPeakFit%IBlue)
        CALL IGrPaletteRGB(KolNumPeakPos,KolPeakPos%IRed,&
                                         KolPeakPos%IGreen,&
                                         KolPeakPos%IBlue)
        CALL IGrPaletteRGB(KolNumBack,   KolBack%IRed,&
                                         KolBack%IGreen,&
                                         KolBack%IBlue)
  ! Initialise bitmap 'Temperature1.bmp'
  ! Rather than loading it from file, it is now calculated.
        CALL WBitMapCreate(bmIHANDLE, iWidth, iHeight)
        DO J = 1, iHeight
          DO I = 1, iWidth
  ! Red
            SELECT CASE (I)
              CASE (  1: 53)
                iRed   = NINT( 0.0015 * ((FLOAT(I-  0))**3) - 0.1796 * ((FLOAT(I-  0))**2) + 1.3012 * (FLOAT(I-  0)) + 228.37)
              CASE ( 54:278)
                iRed   = IDNINT( -0.0000168D00 * ((DBLE(I- 53))**3) + 0.0001469D00 * ((DBLE(I- 53))**2) + 1.8535D00 * (DBLE(I- 53)) + 8.5478D00)
              CASE (279:300)
                iRed   =                                   242
            END SELECT
  ! Green
            SELECT CASE (I)
              CASE (  1: 52)
                iGreen = NINT( 0.0015 * ((FLOAT(I-  0))**3) - 0.1796 * ((FLOAT(I-  0))**2) + 1.3012 * (FLOAT(I-  0)) + 228.37)
              CASE ( 53:245)
                iGreen =                                    18
              CASE (246:300)
                iGreen = NINT(-0.0015 * ((FLOAT(I-245))**3) + 0.0662 * ((FLOAT(I-245))**2) + 5.1575 * (FLOAT(I-245)) + 12.951)
            END SELECT
  ! Blue
            SELECT CASE (I)
              CASE (  1: 53)
                iBlue  = NINT( 0.0008772 * ((FLOAT(I-  0))**3) - 0.072826 * ((FLOAT(I-  0))**2) + 0.42372 * (FLOAT(I-  0)) + 252.365)
              CASE ( 54:248)
                iBlue  = IDNINT( 0.0000232D00 * ((DBLE(I- 53))**3) - 0.010817D00 * ((DBLE(I- 53))**2) + 0.23341D00 * (DBLE(I- 53)) + 197.682D00)
              CASE (249:300)
                iBlue  = NINT(-0.0004 * ((FLOAT(I-248))**3) + 0.013  * ((FLOAT(I-248))**2) + 1.5058 * (FLOAT(I-248)) + 0.4329)
            END SELECT
            iRGBvalue = (iBlue*256*256) + (iGreen*256) + iRed
            tData(I,J) = iRGBvalue
          ENDDO    
        ENDDO              
        CALL WBitMapGetData(bmIHANDLE, tData)

      ENDIF ! in_batch

      END SUBROUTINE InitialiseVariables
!
!*****************************************************************************
!
      SUBROUTINE WriteConfigurationFile
!
! Adding options to the configuration file
! means that the configuration files would not be compatible.
! This can be partially solved by adding new variables at the end only and by programming it such
! that DASH ignores the remainder of the configuration file. That's what it does at the moment.
!
      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL, EXTERNAL :: SavePDB, SaveCSSR, SaveCCL, SaveCIF, SaveRES,  &
                           Get_ColourFlexibleTorsions, ConnectPointsObs,  &
                           PlotObservedErrorBars, PlotDifferenceErrorBars, &
                           PlotBackground, PlotPeakFitDifferenceProfile,  &
                           DASHWDialogGetCheckBoxLogical,                     &
                           Get_SavePRO, Get_OutputChi2vsMoves,            &
                           Get_DivideByEsd, Get_SavePrjAtEnd,             &
                           Get_SaveParamAtEnd
      LOGICAL, EXTERNAL :: Get_WriteWavelength2XYEFile
      LOGICAL, EXTERNAL :: Get_ShowCumChiSqd, Get_AutoAlign
      REAL, EXTERNAL :: WavelengthOf, PlotEsdMultiplier
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(MaxPathLength) DefaultWorkingDir
      INTEGER   RecNr
      INTEGER*4 tInteger
      REAL*4    tReal
      INTEGER RW, hFile

      !C Don't write out a configuration file when in batch mode.
      !C The problem is that a few variables are read straight from the GUI windows--and
      !C that is not possible in batch mode.
      IF ( in_batch ) &
        RETURN
      RW = 0
      hFile = 10
      tFileName = TRIM(AppDataDirectory)//DIRSPACER//'D3.cfg'
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=tFileName,ACCESS='DIRECT',RECL=cRECLMult,FORM='UNFORMATTED',ERR=999)
      RecNr = 1
! Write a header
      CALL FileWriteString(hFile, RecNr, ProgramVersion//' configuration file')
      CALL FileWriteLogical(hFile, RecNr, UseConfigFile)
      IF (.NOT. UseConfigFile) GOTO 999
! Save all colour definitions
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPGWindow)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumMain)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumObs)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumCal)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumDif)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumMTic)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumCTic)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelVLite)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelLite)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelDark)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelVDark)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelOuter)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumRectSelect)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumLargeCrossHair)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPeakFit)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPeakPos)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumBack)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IBlue)
! Show error bars YES / NO
      CALL FileWriteLogical(hFile, RecNr, PlotObservedErrorBars())
! Show background YES / NO
      CALL FileWriteLogical(hFile, RecNr, PlotBackground())
! Connect data points with lines YES / NO
      CALL FileWriteLogical(hFile, RecNr, ConnectPointsObs())
! Plot peak fit difference YES / NO
      CALL FileWriteLogical(hFile, RecNr, PlotPeakFitDifferenceProfile())
! Save the default working directory
      DefaultWorkingDir = 'D:\cvsDASH\dash\Debug'
      CALL FileWriteString(hFile, RecNr, DefaultWorkingDir)
! Save defaults for background subtraction
      CALL SelectDASHDialog(IDD_PW_Page6)
  ! Number of iterations
      CALL DASHWDialogGetInteger(IDF_NumOfIterations, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
  ! Window
      CALL DASHWDialogGetInteger(IDF_WindowWidth, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
  ! Use Monte Carlo YES / NO
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_UseMCYN))
  ! Use spline smooth YES / NO
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_UseMCYN))
! Save default wavelength
      CALL FileWriteReal(hFile, RecNr, WavelengthOf('Cu'))
! Save default maximum resolution
      CALL FileWriteReal(hFile, RecNr, DASHDefaultMaxResolution)
! Save the viewer
      CALL SelectDASHDialog(IDD_Configuration)

      CALL DASHWDialogGetString(IDF_ViewExe, ViewerExecutable)
      IF (ViewerExecutable .EQ. MercuryExecutable) THEN
            CALL FileWriteString(hFile, RecNr, '')
      ELSE
            CALL FileWriteString(hFile, RecNr, ViewerExecutable)
      ENDIF
! and the viewer arguments
      CALL DASHWDialogGetString(IDF_ViewArg, ViewArg)
      CALL FileWriteString(hFile, RecNr, ViewArg)
! Save hydrogen treatment 1 = ignore, 2 = absorb, 3 = explicit
      CALL FileWriteInteger(hFile, RecNr, HydrogenTreatment)
! Colour flexible torsions (in Z-matrix viewer) YES / NO
      CALL FileWriteLogical(hFile, RecNr, Get_ColourFlexibleTorsions())
! Save YES / NO which molecular file formats are to be written out when a best solution is found
      CALL FileWriteLogical(hFile, RecNr, SavePDB())  ! 1. .pdb  ?
      CALL FileWriteLogical(hFile, RecNr, SaveCSSR()) ! 2. .cssr ?
      CALL FileWriteLogical(hFile, RecNr, SaveCCL())  ! 3. .ccl  ?
      CALL FileWriteLogical(hFile, RecNr, SaveCIF())  ! 4. .cif  ?
      CALL FileWriteLogical(hFile, RecNr, SaveRES())  ! 5. .res  ?
! Save YES / NO if .pro file is to be written out when a best solution is found
      CALL FileWriteLogical(hFile, RecNr, Get_SavePRO())
! Auto local minimisation at the end of every run YES / NO
      CALL FileWriteLogical(hFile, RecNr, AutoMinimise)
! Save output profile chi**2 versus moves plot YES / NO
      CALL FileWriteLogical(hFile, RecNr, Get_OutputChi2vsMoves())
! Save the damping factor for the local minimisation
      CALL FileWriteReal(hFile, RecNr, SA_SimplexDampingFactor)
! Save the seeds for the random number generator
      CALL SelectDASHDialog(IDD_SA_input3_2)
      CALL DASHWDialogGetInteger(IDF_SA_RandomSeed1, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL DASHWDialogGetInteger(IDF_SA_RandomSeed2, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL DASHWDialogGetInteger(IDF_SA_MaxRepeats, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL DASHWDialogGetReal(IDF_SA_ChiTest, tReal)
      CALL FileWriteReal(hFile, RecNr, tReal)
      CALL DASHWDialogGetReal(IDF_MaxMoves1, tReal)
      CALL FileWriteReal(hFile, RecNr, tReal)
      CALL DASHWDialogGetInteger(IDF_MaxMoves2, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL SelectDASHDialog(IDD_SAW_Page5)
! Atom labels for SA solutions overlay. Two options: 
! 1. "Element symbol + solution number"
! 2. "Original atom labels"
      CALL DASHWDialogGetRadioButton(IDF_UseSolutionNr, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
! Atom colours for SA solutions overlay. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL DASHWDialogGetRadioButton(IDF_ColourBySolution, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
! Following is new in DASH 2.1
! Use hydrogens for auto local minimise
      CALL FileWriteLogical(hFile, RecNr, UseHAutoMin)
! Plot cumulative chi-squared      
      CALL FileWriteLogical(hFile, RecNr, Get_ShowCumChiSqd())
! Following is new in DASH 2.2
! Divide difference by ESDs      
      CALL FileWriteLogical(hFile, RecNr, Get_DivideByEsd())
! Auto align      
      CALL FileWriteLogical(hFile, RecNr, Get_AutoAlign())
! Following is new in DASH 3.0
! Save the mogul path
      CALL SelectDASHDialog(IDD_Configuration)
      CALL DASHWDialogGetString(IDF_MogulExe, MOGULEXE)
      CALL FileWriteString(hFile, RecNr, MOGULEXE)
! Use Mogul
      CALL FileWriteLogical(hFile, RecNr, UseMogul)
! Single crystal options
      CALL SelectDASHDialog(IDD_SX_Page2)
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_RecalcESDs))
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_IgnLT))
      CALL DASHWDialogGetReal(IDF_CutOff, tReal)
      CALL FileWriteReal(hFile, RecNr, tReal)
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_AvgFriedelPairs))
! Following is new in DASH 3.1
      CALL SelectDASHDialog(IDD_Configuration)
      CALL DASHWDialogGetString(IDF_DICVOLExe, DICVOLEXE)
      CALL FileWriteString(hFile, RecNr, DICVOLEXE)
      CALL DASHWDialogGetString(IDF_TOPASExe, TOPASEXE)
      CALL FileWriteString(hFile, RecNr, TOPASEXE)
      CALL DASHWDialogGetString(IDF_EXPGUIExe, EXPGUIEXE)
      CALL FileWriteString(hFile, RecNr, EXPGUIEXE)
      CALL DASHWDialogGetString(IDF_RIETANExe, RIETANEXE)
      CALL FileWriteString(hFile, RecNr, RIETANEXE)
      CALL DASHWDialogGetString(IDF_McMailleExe, McMailleEXE)
      CALL FileWriteString(hFile, RecNr, McMailleEXE)
      CALL FileWriteLogical(hFile, RecNr, Get_WriteWavelength2XYEFile())
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_BuiltIn_Mercury))
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDC_cif_for_viewer))
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDC_Sort_H_Down))
! Save defaults for background subtraction
      CALL SelectDASHDialog(IDD_PW_Page6)
  ! Use window smooth YES / NO
      CALL FileWriteLogical(hFile, RecNr, DASHWDialogGetCheckBoxLogical(IDF_UseSmooth))
  ! Window
      CALL DASHWDialogGetInteger(IDF_SmoothWindow, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
! Save .dash file at end of SA?
      CALL FileWriteLogical(hFile, RecNr, Get_SavePrjAtEnd())
! Save .tbl file at end of SA?
      CALL FileWriteLogical(hFile, RecNr, Get_SaveParamAtEnd())

! Save difference profile?
      CALL FileWriteLogical(hFile, RecNr, PlotDifferenceErrorBars())
      CALL FileWriteReal(hFile, RecNr, PlotEsdMultiplier())

  999 CLOSE(hFile)

      END SUBROUTINE WriteConfigurationFile
!
!*****************************************************************************
!
      SUBROUTINE ReadConfigurationFile

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      REAL, EXTERNAL :: WavelengthOf
      REAL, EXTERNAL :: dSpacing2TwoTheta
      INTEGER, EXTERNAL :: GetBFIOError
      LOGICAL, EXTERNAL :: SetRRMethodRadioState
      CHARACTER(MaxPathLength) tFileName
      INTEGER   RecNr, RW
      INTEGER   hFile
      CHARACTER(MaxPathLength) tString
      CHARACTER(MaxPathLength) MainVersionStr
      CHARACTER(MaxPathLength) SubVersionStr
      INTEGER*4 tInteger
      LOGICAL*4 tLogical
      REAL*4    tReal
      LOGICAL   FExists
      INTEGER   tLen, MainVersionLen, SubVersionLen

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      RW = 1
      tFileName = TRIM(AppDataDirectory)//DIRSPACER//'D3.cfg'
      INQUIRE(FILE=tFileName,EXIST=FExists)
      IF (.NOT. FExists) RETURN
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=tFileName,ACCESS='DIRECT',RECL=cRECLMult,FORM='UNFORMATTED',ERR=999)
      RecNr = 1
! Read the header
      CALL FileReadString(hFile,RecNr,tString) ! E.g. 'DASH 2.1.1 configuration file'
      ! Extract the main version number
      tLen = LEN_TRIM(tString)
      tString = tString(6:tLen) ! Removes 'DASH '
      CALL GetSubString(tString, '.', MainVersionStr)
      MainVersionLen = LEN_TRIM(MainVersionStr)
      CALL GetSubString(tString, '.', SubVersionStr)
      SubVersionLen = LEN_TRIM(SubVersionStr)
      CALL FileReadLogical(hFile, RecNr, UseConfigFile)
      IF (.NOT. UseConfigFile) THEN
        CALL DebugErrorMessage('Config file not used.')
        GOTO 999
      ENDIF
! Read all colour definitions
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPGWindow)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPGWindow%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumMain)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolMain%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumObs)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolObs%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumCal)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolCal%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumDif)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolDif%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumMTic)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolMTic%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumCTic)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolCTic%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelVLite)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVLite%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelLite)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelLite%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelDark)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelDark%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelVDark)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelVDark%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPanelOuter)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPanelOuter%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumRectSelect)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolRectSelect%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumLargeCrossHair)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolLargeCrossHair%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPeakFit)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakFit%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumPeakPos)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolPeakPos%IBlue)
      CALL FileRWInteger(hFile, RecNr, RW, KolNumBack)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IRed)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IGreen)
      CALL FileRWInteger(hFile, RecNr, RW, KolBack%IBlue)

      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
! Show error bars YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_ErrorBar_Check, tLogical)
! Show background YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_background_check, tLogical)
! Connect data points with lines YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_ConnectObsPoints, tLogical)
! Plot peak fit difference YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_PlotPeakFitDif, tLogical)
! Read the default working directory
      CALL FileReadString(hFile, RecNr, tString)
! Read defaults for background subtraction
      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_PW_Page6)
      CALL FileReadInteger(hFile, RecNr, tInteger)      ! Number of iterations
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_NumOfIterations, tInteger)
      CALL FileReadInteger(hFile, RecNr, tInteger)      ! Window
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_WindowWidth, tInteger)
      CALL FileReadLogical(hFile, RecNr, tLogical)      ! Use Monte Carlo YES / NO
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_UseMCYN, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)      ! Use spline smooth YES / NO
! Read default wavelength
      CALL FileReadReal(hFile, RecNr, tReal)
      CALL Set_Wavelength(tReal)
! Read default maximum resolution
      CALL FileReadReal(hFile, RecNr, DefaultMaxResolution)
! Now initialise the maximum resolution in the dialogue window
      CALL Update_TruncationLimits
! Read the viewer
      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_Configuration)

      CALL FileReadString(hFile, RecNr, ViewerExecutable)
      IF (( .NOT. IN_BATCH ) .AND. (LEN_TRIM(ViewerExecutable) .GT. 0)) THEN
        CALL WDialogPutString(IDF_ViewExe, ViewerExecutable)
      ELSE 
        CALL WDialogPutString(IDF_ViewExe, MercuryExecutable)
      ENDIF
! and the viewer arguments
      CALL FileReadString(hFile, RecNr, ViewArg)

      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutString(IDF_ViewArg, ViewArg)
! Read hydrogen treatment. This was a LOGICAL for versions below DASH 2.2
      IF ( (MainVersionStr .EQ. "1") .OR.    &
          ((MainVersionStr .EQ. "2") .AND. ((SubVersionStr .EQ. "0") .OR. (SubVersionStr .EQ. "1") .OR. (SubVersionStr .EQ. "2"))) ) THEN
        CALL FileReadLogical(hFile, RecNr, tLogical)
        IF ( tLogical ) THEN
          CALL Set_HydrogenTreatment(3) ! Explicit
        ELSE
          CALL Set_HydrogenTreatment(1) ! Ignore
        ENDIF
      ELSE
        CALL FileReadInteger(hFile, RecNr, tInteger)
        CALL Set_HydrogenTreatment(tInteger)
      ENDIF
! Colour flexible torsions (in Z-matrix viewer) YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_ColFlexTors, tLogical)
 
! Read YES / NO which molecular file formats are to be written out when a best solution is found
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 1. .pdb  ?
      CALL Set_SavePDB(tLogical)
      
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 2. .cssr ?
      CALL Set_SaveCSSR(tLogical)

      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 3. .ccl  ?
      CALL Set_SaveCCL(tLogical)

      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 4. .cif  ?
      CALL Set_SaveCIF(tLogical)
      
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 5. .res  ?
      CALL Set_SaveRES(tLogical)
      
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 6. .pro  ?
      CALL Set_SavePRO(tLogical)



! Auto local minimisation at the end of every run in multirun YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)

      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_AutoLocalOptimise, tLogical)
      ENDIF

! Read output profile chi**2 versus moves plot YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_OutputChi2vsMoves(tLogical)

! Read the damping factor for the local minimisation
      CALL FileReadReal(hFile,RecNr,SA_SimplexDampingFactor)
! Read the seeds for the random number generator

      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_SA_input3_2)
  
      CALL FileReadInteger(hFile, RecNr, tInteger)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_SA_RandomSeed1, tInteger)

      CALL FileReadInteger(hFile, RecNr, tInteger)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_SA_RandomSeed2, tInteger)

      CALL FileReadInteger(hFile, RecNr, tInteger)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_SA_MaxRepeats, tInteger)
      
      CALL FileReadReal(hFile, RecNr, tReal)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutReal(IDF_SA_ChiTest, tReal)

      CALL FileReadReal(hFile, RecNr, tReal)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutReal(IDF_MaxMoves1, tReal)

      CALL FileReadInteger(hFile, RecNr, tInteger)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutInteger(IDF_MaxMoves2, tInteger)

      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_SAW_Page5)
! Atom labels for SA solutions overlay. Two options: 
! 1. "Element symbol + solution number"
! 2. "Original atom labels"
      CALL FileReadInteger(hFile, RecNr, tInteger)
      IF ( .NOT. IN_BATCH ) THEN
        SELECT CASE (tInteger)
          CASE (1)
            CALL WDialogPutRadioButton(IDF_UseSolutionNr)
          CASE (2)
            CALL WDialogPutRadioButton(IDF_UseOriginal)
        END SELECT
      END IF

! Atom colours for SA solutions overlay. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL FileReadInteger(hFile, RecNr, tInteger)

      IF ( .NOT. IN_BATCH ) THEN
        SELECT CASE (tInteger)
          CASE (1)
            CALL WDialogPutRadioButton(IDF_ColourBySolution)
          CASE (2)
            CALL WDialogPutRadioButton(IDF_ColourByElement)
        END SELECT
      ENDIF
! Following is new in DASH 2.1
! Use hydrogens for auto local minimise
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
      CALL Set_UseHydrogensDuringAutoLocalMinimise(tLogical)
! Plot cumulative chi-squared 
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
        CALL WDialogPutCheckBoxLogical(IDF_ShowCumChiSqd, tLogical)
      ENDIF
! Following is new in DASH 2.2
! Divide difference by ESDs
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_DivDiffByEsd, tLogical)
! Auto align      
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_AutoAlign(tLogical)
! Following is new in DASH 3.0
! Read the Mogul path
      CALL FileReadString(hFile, RecNr, MOGULEXE)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
 
      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_Configuration)
        CALL WDialogPutString(IDF_MogulExe, MOGULEXE)
      ENDIF

! Use Mogul
      CALL FileReadLogical(hFile, RecNr, UseMogul)
! Single crystal options
      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_SX_Page2)
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_RecalcESDs, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_IgnLT, tLogical)
      CALL FileReadReal(hFile, RecNr, tReal)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutReal(IDF_CutOff, tReal)
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_AvgFriedelPairs, tLogical)
! Following is new in DASH 3.1
! Read the DICVOL04 path
      CALL FileReadString(hFile, RecNr, DICVOLEXE)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_Configuration)
        CALL WDialogPutString(IDF_DICVOLExe, DICVOLEXE)
      ENDIF
      CALL FileReadString(hFile, RecNr, TOPASEXE)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutString(IDF_TOPASExe, TOPASEXE)
      CALL FileReadString(hFile, RecNr, EXPGUIEXE)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutString(IDF_EXPGUIExe, EXPGUIEXE)
      CALL FileReadString(hFile, RecNr, RIETANEXE)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutString(IDF_RIETANExe, RIETANEXE)
      CALL FileReadString(hFile, RecNr, McMailleEXE)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutString(IDF_McMailleExe, McMailleEXE)
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDC_wl_in_xye, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_BuiltIn_Mercury, tLogical)
      IF ( .NOT. IN_BATCH ) THEN
        IF (tLogical) THEN
          CALL WDialogFieldState(IDF_Use_Client, Enabled)
          CALL WDialogFieldState(IDBBROWSE, Disabled)
          CALL WDialogFieldState(IDF_ViewExe, Disabled)
          CALL WDialogFieldState(IDF_ViewArg, Disabled)
        ELSE
          CALL WDialogFieldState(IDF_Use_Client, Disabled)
          CALL WDialogFieldState(IDBBROWSE, Enabled)
          CALL WDialogFieldState(IDF_ViewExe, Enabled)
          CALL WDialogFieldState(IDF_ViewArg, Enabled)
        ENDIF
      ENDIF

      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) THEN
        CALL WDialogPutCheckBoxLogical(IDC_cif_for_viewer, tLogical)
! Update state of related radio/check
        CALL SelectDASHDialog(IDD_PW_Page7)
        IF ( LEN_TRIM(DICVOLEXE) .GT. 0 ) THEN
          CALL WDialogFieldState(IDF_RADIO2, Enabled)
        ELSE
          CALL WDialogFieldState(IDF_RADIO2, Disabled)
        ENDIF
        IF ( LEN_TRIM(McMailleEXE) .GT. 0 ) THEN
          CALL WDialogFieldState(IDF_RADIO3, Enabled)
        ELSE
          CALL WDialogFieldState(IDF_RADIO3, Disabled)
        ENDIF
! As loaded by WizardWindowShow, IDD_SAW_Page6a has to be handled there
        CALL SelectDASHDialog(IDD_SAW_Page6)
        IF ( SetRRMethodRadioState() ) CALL WDialogPutRadioButton(IDF_RADIO1)
        IF ( tLogical ) THEN
          CALL SelectDASHDialog(IDD_Summary)
          CALL WDialogFieldState(IDF_ColourBySolution, Disabled)
          CALL WDialogPutRadioButton(IDF_ColourByElement)
          CALL SelectDASHDialog(IDD_SAW_Page5)
          CALL WDialogFieldState(IDF_ColourBySolution, Disabled)
          CALL WDialogPutRadioButton(IDF_ColourByElement)
        ELSE
          CALL SelectDASHDialog(IDD_Summary)
          CALL WDialogFieldState(IDF_ColourBySolution, Enabled)
          CALL SelectDASHDialog(IDD_SAW_Page5)
          CALL WDialogFieldState(IDF_ColourBySolution, Enabled)
        ENDIF
      ENDIF

      CALL FileReadLogical(hFile, RecNr, tLogical)
      IF ( .NOT. IN_BATCH ) THEN
        CALL SelectDASHDialog(IDD_Configuration)
        CALL WDialogPutCheckBoxLogical(IDC_Sort_H_Down, tLogical)
      ENDIF

! Read defaults for background subtraction
      IF ( .NOT. IN_BATCH ) &
        CALL SelectDASHDialog(IDD_PW_Page6)
      CALL FileReadLogical(hFile, RecNr, tLogical)      ! Use Monte Carlo YES / NO
      IF ( .NOT. IN_BATCH ) & 
        CALL WDialogPutCheckBoxLogical(IDF_UseSmooth, tLogical)
      
      IF ( .NOT. IN_BATCH ) THEN
        IF ( tLogical ) THEN
          CALL WDialogFieldState(IDF_LABEL3, Enabled)
          CALL WDialogFieldState(IDF_SmoothWindow, Enabled)
        ELSE
          CALL WDialogFieldState(IDF_LABEL3, Disabled)
          CALL WDialogFieldState(IDF_SmoothWindow, Disabled)
        ENDIF
      ENDIF

      CALL FileReadInteger(hFile, RecNr, tInteger)      ! Window

      IF ( .NOT. IN_BATCH ) THEN
        CALL WDialogPutInteger(IDF_SmoothWindow, tInteger)
        CALL SelectDASHDialog(IDD_SA_input4)
      ENDIF

      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_SavePrjAtEnd(tLogical)

      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_SaveParamAtEnd(tLogical)

      CALL FileReadLogical(hFile, RecNr, tLogical)

      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutCheckBoxLogical(IDF_DifferenceErrorBar_Check, tLogical)

      CALL FileReadReal(hFile, RecNr, tReal)

      IF ( .NOT. IN_BATCH ) &
        CALL WDialogPutReal(IDF_ErrorMultiplier_RealEntry, tReal)

      CLOSE(hFile)
      RETURN
  999 CALL DebugErrorMessage('Error while opening config file')
      CLOSE(hFile)

      END SUBROUTINE ReadConfigurationFile
!
!*****************************************************************************
! 
