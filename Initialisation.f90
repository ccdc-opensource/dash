!
!*****************************************************************************
!
      SUBROUTINE GetInstallationDirectory
 
      USE WINTERACTER
      USE VARIABLES
      USE KERNEL32

      IMPLICIT NONE

      CHARACTER(MaxPathLength) tString
      CHARACTER(MaxPathLength) tFile
      INTEGER*4 tProcess, tSize

! Determine the directory where DASH.exe resides and store it in "InstallationDirectory"
      tSize = MaxPathLength
      tProcess = 0 ! this program
      CALL GetModuleFileName(tProcess,tString,LOC(tSize))
! tString should now contain the full path to DASH.exe irrespective of the way
! DASH has been invoked.
      CALL SplitPath(tString,InstallationDirectory,tFile)
      IF (LEN_TRIM(InstallationDirectory) .EQ. 0) InstallationDirectory = '.'//DIRSPACER
      CALL IOsDirChange(InstallationDirectory)
      CALL IOsDirName(InstallationDirectory)
      InstallationDirectory = InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//DIRSPACER

      END SUBROUTINE GetInstallationDirectory
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
      USE KERNEL32

      IMPLICIT NONE

      INTEGER :: IFlags
      CHARACTER(MaxPathLength) :: Dirname

      DO WHILE (.TRUE.)
        IFlags = DirChange + DirCreate
        Dirname = ' '
        CALL WSelectDir(IFlags,Dirname,"Select working directory for DASH...")
        IF (LEN_TRIM(Dirname) .EQ. 0) THEN
          CALL WindowClose
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
!
!*****************************************************************************
!
! Rather than continually load/unload the various widgets, upload them all only once
! This way, the state can be memorised from session to session
      SUBROUTINE PolyFitter_UploadDialogues

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CALL WDialogLoad(IDD_Background_Fit)
      CALL WDialogLoad(IDD_Structural_Information)
      CALL WDialogLoad(IDD_Configuration)
      CALL WDialogLoad(IDD_Index_Preparation)
      CALL WDialogLoad(IDD_DV_Results)
! Set the colours of the grid manually
      CALL WDialogLoad(IDD_Plot_Option_Dialog)
      CALL WDialogLoad(IDD_Polyfitter_Wizard_01)
      CALL WDialogLoad(IDD_PW_Page1)
      CALL WDialogLoad(IDD_PW_Page2)
      CALL WDialogLoad(IDD_PW_Page3)
      CALL WDialogLoad(IDD_PW_Page3a)
      CALL WDialogLoad(IDD_PW_Page4)
      CALL WDialogLoad(IDD_PW_Page5)
      CALL WDialogLoad(IDD_PW_Page6)
      CALL WDialogLoad(IDD_PW_Page7)
      CALL WDialogLoad(IDD_PW_Page8)
      CALL WDialogLoad(IDD_PW_Page9)
      CALL WDialogLoad(IDD_PW_Page10)
      CALL WDialogLoad(IDD_SX_Page1)
      CALL WDialogLoad(IDD_SX_Page1a)
      CALL WDialogLoad(IDD_SX_Page2)
      CALL WDialogLoad(IDD_Pawley_Status)
      CALL WDialogLoad(IDD_Pawley_ErrorLog)
      CALL WDialogLoad(IDD_SAW_Page1)
      CALL WDialogLoad(IDD_zmEdit)
      CALL WDialogLoad(IDD_zmEditRotations)
      CALL WDialogLoad(IDD_SAW_Page2)
      CALL WDialogLoad(IDD_SA_Modal_input2)
      CALL WDialogLoad(IDD_ModalDialog)
      CALL WDialogLoad(IDD_SA_input3_2)
      CALL WDialogLoad(IDD_SA_Action1)
      CALL WDialogLoad(IDD_Summary)
      CALL WDialogLoad(IDD_SAW_Page5)
      CALL WDialogLoad(IDD_SAW_Page6)
      CALL WDialogLoad(IDD_Parameter_Status_2)
      CALL WDialogLoad(IDD_OutputSolutions)
      CALL WDialogLoad(IDD_Rietveld2)
      CALL WDialogLoad(IDD_RR_PO_Dialog)

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
      OPEN(110,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//'SpaceGroupSymbols.dat',STATUS='OLD', ERR = 999)
      i = 0
 10   lintem=' '
      READ(110,1100,END=100) nl, lintem
 1100 FORMAT(Q,A)
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
        CALL WindowClose
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
                          InstallationDirectory(1:LEN_TRIM(InstallationDirectory)))              
      CALL WindowClose
      STOP

      END SUBROUTINE PolyFitterInitialise
!
!*****************************************************************************
!
      SUBROUTINE InitialiseVariables

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

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

      INTEGER*4         ChildWinHandler
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

      REAL, EXTERNAL :: WaveLengthOf, dSpacing2TwoTheta
      INTEGER iWidth, iHeight
      PARAMETER (iWidth = 300, iHeight = 1)
      INTEGER tData(1:iWidth,1:iHeight)
      INTEGER I, J
      INTEGER iRed, iGreen, iBlue, iRGBvalue
      REAL    UM, TH

! The initialisations should be split up into 'one off initialisations' (at the start up
! of DASH only) and 'whenever a new project file is opened'
      CALL WDialogSelect(IDD_SA_input3_2)
      DO I = 1, MVAR
        ModalFlag(I) = 0 ! 0 = not a torsion angle
      ENDDO
      ShowAgain = .TRUE.
      Counter   = 0
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
      VIEWEXE = ''
      MOGULEXE = ' '
      CALL GetPathToMercuryFromRegistry
      CALL GetPathToMogulFromRegistry
      DO I = 0, maxfrg
        izmoid(0,I) = 0
        izmbid(0,I) = 0
      ENDDO
      CALL Clear_SA ! Sets NumOf_SA_Runs to 0
      CALL Update_Solutions
      nPeaksFound = 0
      CALL WDialogSelect(IDD_SAW_Page5)
      CALL WDialogFieldState(IDB_Prog3, Disabled)
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
      LOG_HYDROGENS = .FALSE.
      T0 = 0.0
      RT = 0.02
      CALL Set_Wavelength(WaveLengthOf('Cu'))
! Now initialise the maximum resolution in the dialogue window
      DefaultMaxResolution = DASHDefaultMaxResolution
      CALL Update_TruncationLimits
      CALL WDialogSelect(IDD_SA_input3_2)
      ISeed1 = 315
      ISeed2 = 159
      CALL WDialogPutInteger(IDF_SA_RandomSeed1, ISeed1)
      CALL WDialogPutInteger(IDF_SA_RandomSeed2, ISeed2)
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_eps,0.03,'(F5.3)')
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogPutString(IDF_ViewExe,ViewExe)
      CALL WDialogPutString(IDF_ViewArg,ViewArg)
      CALL WDialogPutString(IDF_MogulExe, MogulExe)
      CALL WDialogPutCheckBoxLogical(IDF_AutoLocalOptimise, .TRUE.)
      SA_SimplexDampingFactor = 0.1
! Grey out the "Previous Results >" button in the DICVOL Wizard window
      CALL WDialogSelect(IDD_PW_Page8)
      CALL WDialogFieldState(IDB_PrevRes,Disabled)
! Grey out 'Remove background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOff)
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
      KolPGWindow       = Win_RGB(253,253,248)
      KolMain           = Win_RGB(20,20,150)
      KolObs            = Win_RGB(255,0,0)
      KolCal            = Win_RGB(0,0,255)
      KolDif            = Win_RGB(200,100,200)
      KolMTic           = Win_RGB(0,0,0)
      KolCTic           = Win_RGB(0,131,131)
      KolPanelVLite     = Win_RGB(245,245,245)
      KolPanelLite      = Win_RGB(235,235,235)
      KolPanelDark      = Win_RGB(210,210,210)
      KolPanelVDark     = Win_RGB(170,170,170)
      KolPanelOuter     = Win_RGB(190,190,190)
      KolRectSelect     = Win_RGB(150,150,5)
      KolLargeCrossHair = Win_RGB(150,150,5)
      KolPeakFit        = Win_RGB(0,131,131)
      KolPeakPos        = Win_RGB(50,50,200)
      KolBack           = Win_RGB(164,211,105)

      CALL ReadConfigurationFile
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
      CALL WBitMapCreate(bmIHANDLE,iWidth,iHeight)
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
      CALL WBitMapGetData(bmIHANDLE,tData)

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
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      LOGICAL, EXTERNAL :: SavePDB, SaveCSSR, SaveCCL, SaveCIF, SaveRES,  &
                           Get_ColourFlexibleTorsions, ConnectPointsObs,  &
                           PlotErrorBars, PlotBackground,                 &
                           PlotPeakFitDifferenceProfile,                  &
                           WDialogGetCheckBoxLogical,                     &
                           Get_HydrogenTreatment, Get_SavePRO, Get_OutputChi2vsMoves, &
                           Get_AutoLocalMinimisation, Get_DivideByEsd
      LOGICAL, EXTERNAL :: UseHydrogensDuringAuto, Get_ShowCumChiSqd, Get_AutoAlign
      REAL, EXTERNAL :: WavelengthOf
      CHARACTER*MaxPathLength tFileName
      CHARACTER*MaxPathLength DefaultWorkingDir
      INTEGER    RecNr
      INTEGER*4 tInteger
      REAL*4    tReal
      INTEGER RW, hFile

      RW = 0
      tFileName = 'D3.cfg'
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
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
      CALL FileWriteLogical(hFile, RecNr, PlotErrorBars())
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
      CALL WDialogSelect(IDD_PW_Page6)
  ! Number of iterations
      CALL WDialogGetInteger(IDF_NumOfIterations, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
  ! Window
      CALL WDialogGetInteger(IDF_WindowWidth, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
  ! Use Monte Carlo YES / NO
      CALL FileWriteLogical(hFile, RecNr, WDialogGetCheckBoxLogical(IDF_UseMCYN))
  ! Use spline smooth YES / NO
      CALL FileWriteLogical(hFile, RecNr, WDialogGetCheckBoxLogical(IDF_UseMCYN))
! Save default wavelength
      CALL FileWriteReal(hFile, RecNr, WavelengthOf('Cu'))
! Save default maximum resolution
      CALL FileWriteReal(hFile, RecNr, DASHDefaultMaxResolution)
! Save the viewer
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_ViewExe, ViewExe)
      CALL FileWriteString(hFile, RecNr, ViewExe)
! and the viewer arguments
      CALL WDialogGetString(IDF_ViewArg, ViewArg)
      CALL FileWriteString(hFile, RecNr, ViewArg)
! Save hydrogen treatment 1 = ignore, 2 = absorb, 3 = explicit
      CALL FileWriteLogical(hFile,RecNr, Get_HydrogenTreatment())
! Colour flexible torsions (in Z-matrix viewer) YES / NO
      CALL FileWriteLogical(hFile,RecNr,Get_ColourFlexibleTorsions())
! Save YES / NO which molecular file formats are to be written out when a best solution is found
      CALL FileWriteLogical(hFile, RecNr, SavePDB())  ! 1. .pdb  ?
      CALL FileWriteLogical(hFile, RecNr, SaveCSSR()) ! 2. .cssr ?
      CALL FileWriteLogical(hFile, RecNr, SaveCCL())  ! 3. .ccl  ?
      CALL FileWriteLogical(hFile, RecNr, SaveCIF())  ! 4. .cif  ?
      CALL FileWriteLogical(hFile, RecNr, SaveRES())  ! 5. .res  ?
! Save YES / NO if .pro file is to be written out when a best solution is found
      CALL FileWriteLogical(hFile, RecNr, Get_SavePRO())
! Auto local minimisation at the end of every run in multirun YES / NO
      CALL FileWriteLogical(hFile, RecNr, Get_AutoLocalMinimisation())
! Save output profile chi**2 versus moves plot YES / NO
      CALL FileWriteLogical(hFile, RecNr, Get_OutputChi2vsMoves())
! Save the damping factor for the local minimisation
      CALL FileWriteReal(hFile,RecNr,SA_SimplexDampingFactor)
! Save the seeds for the random number generator
      CALL WDialogSelect(IDD_SA_input3_2)
      CALL WDialogGetInteger(IDF_SA_RandomSeed1, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL WDialogGetInteger(IDF_SA_RandomSeed2, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL WDialogGetInteger(IDF_SA_MaxRepeats, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL WDialogGetReal(IDF_SA_ChiTest, tReal)
      CALL FileWriteReal(hFile, RecNr, tReal)
      CALL WDialogGetReal(IDF_MaxMoves1, tReal)
      CALL FileWriteReal(hFile, RecNr, tReal)
      CALL WDialogGetInteger(IDF_MaxMoves2, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
      CALL WDialogSelect(IDD_SAW_Page5)
! Atom labels for SA solutions overlay. Two options: 
! 1. "Element symbol + solution number"
! 2. "Original atom labels"
      CALL WDialogGetRadioButton(IDF_UseSolutionNr, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
! Atom colours for SA solutions overlay. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL WDialogGetRadioButton(IDF_ColourBySolution, tInteger)
      CALL FileWriteInteger(hFile, RecNr, tInteger)
! Following is new in DASH 2.1
! Use hydrogens for auto local minimise
      CALL FileWriteLogical(hFile, RecNr, UseHydrogensDuringAuto())
! Plot cumulative chi-squared      
      CALL FileWriteLogical(hFile, RecNr, Get_ShowCumChiSqd())
! Divide difference by ESDs      
      CALL FileWriteLogical(hFile, RecNr, Get_DivideByEsd())
! Auto align      
      CALL FileWriteLogical(hFile, RecNr, Get_AutoAlign())
! Save the mogul path
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_MogulExe, MogulExe)
      CALL FileWriteString(hFile, RecNr, MogulExe)
! Use Mogul
      CALL FileWriteLogical(hFile, RecNr, UseMogul)

  999 CLOSE(hFile)

      END SUBROUTINE WriteConfigurationFile
!
!*****************************************************************************
!
      SUBROUTINE ReadConfigurationFile

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      REAL, EXTERNAL :: WavelengthOf
      REAL, EXTERNAL :: dSpacing2TwoTheta
      INTEGER, EXTERNAL :: GetBFIOError
      CHARACTER*MaxPathLength tFileName
      INTEGER   RecNr, RW
      INTEGER   hFile
      CHARACTER*MaxPathLength tString
      CHARACTER*MaxPathLength MainVersionStr
      CHARACTER*MaxPathLength SubVersionStr
      INTEGER*4 tInteger
      LOGICAL*4 tLogical
      REAL*4    tReal
      LOGICAL   FExists
      INTEGER   tLen, MainVersionLen, SubVersionLen

      RW = 1
      tFileName = 'D3.cfg'
      INQUIRE(FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//tFileName,EXIST=FExists)
      IF (.NOT. FExists) RETURN
      hFile = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=hFile,FILE=InstallationDirectory(1:LEN_TRIM(InstallationDirectory))//tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
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
      CALL FileReadLogical(hFile,RecNr,UseConfigFile)
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
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
! Show error bars YES / NO
      CALL FileReadLogical(hFile,RecNr,tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_ErrorBar_Check,tLogical)
! Show background YES / NO
      CALL FileReadLogical(hFile,RecNr,tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_background_check,tLogical)
! Connect data points with lines YES / NO
      CALL FileReadLogical(hFile,RecNr,tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_ConnectObsPoints,tLogical)
! Plot peak fit difference YES / NO
      CALL FileReadLogical(hFile,RecNr,tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_PlotPeakFitDif,tLogical)
! Read the default working directory
      CALL FileReadString(hFile,RecNr,tString)
! Read defaults for background subtraction
      CALL WDialogSelect(IDD_PW_Page6)
      CALL FileReadInteger(hFile,RecNr,tInteger)      ! Number of iterations
      CALL WDialogPutInteger(IDF_NumOfIterations,tInteger)
      CALL FileReadInteger(hFile,RecNr,tInteger)      ! Window
      CALL WDialogPutInteger(IDF_WindowWidth,tInteger)
      CALL FileReadLogical(hFile,RecNr,tLogical)      ! Use Monte Carlo YES / NO
      CALL WDialogPutCheckBoxLogical(IDF_UseMCYN,tLogical)
      CALL FileReadLogical(hFile,RecNr,tLogical)      ! Use spline smooth YES / NO
! Read default wavelength
      CALL FileReadReal(hFile,RecNr,tReal)
      CALL Set_Wavelength(tReal)
! Read default maximum resolution
      CALL FileReadReal(hFile,RecNr,DefaultMaxResolution)
! Now initialise the maximum resolution in the dialogue window
      CALL Update_TruncationLimits
! Read the viewer
      CALL WDialogSelect(IDD_Configuration)
      CALL FileReadString(hFile,RecNr,ViewExe)
      CALL WDialogPutString(IDF_ViewExe,ViewExe)
! and the viewer arguments
      CALL FileReadString(hFile,RecNr,ViewArg)
      CALL WDialogPutString(IDF_ViewArg,ViewArg)
! Read hydrogen treatment. This was a LOGICAL for versions below DASH 2.2
      IF ( (MainVersionStr .EQ. "1") .OR.    &
          ((MainVersionStr .EQ. "2") .AND. ((SubVersionStr .EQ. "0") .OR. (SubVersionStr .EQ. "1") .OR. (SubVersionStr .EQ. "2"))) ) THEN
        CALL FileReadLogical(hFile, RecNr, LOG_HYDROGENS)
        IF (LOG_HYDROGENS) THEN
          CALL Set_HydrogenTreatment(3) ! Explicit
        ELSE
          CALL Set_HydrogenTreatment(1) ! Ignore
        ENDIF
      ELSE
        CALL FileReadInteger(hFile, RecNr, tInteger)
        CALL Set_HydrogenTreatment(tInteger)
        LOG_HYDROGENS = (tInteger .EQ. 3)
      ENDIF
! Colour flexible torsions (in Z-matrix viewer) YES / NO
      CALL FileReadLogical(hFile,RecNr,tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_ColFlexTors,tLogical)
! Read YES / NO which molecular file formats are to be written out when a best solution is found
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 1. .pdb  ?
      CALL WDialogPutCheckBoxLogical(IDF_OutputPDB, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 2. .cssr ?
      CALL WDialogPutCheckBoxLogical(IDF_OutputCSSR, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 3. .ccl  ?
      CALL WDialogPutCheckBoxLogical(IDF_OutputCCL, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 4. .cif  ?
      CALL WDialogPutCheckBoxLogical(IDF_OutputCIF, tLogical)
      CALL FileReadLogical(hFile, RecNr, tLogical)   ! 5. .res  ?
      CALL WDialogPutCheckBoxLogical(IDF_OutputRES, tLogical)
! Read YES / NO if .pro file is to be written out when a best solution is found
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_SavePRO(tLogical)
! Auto local minimisation at the end of every run in multirun YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_AutoLocalOptimise, tLogical)
      IF (LOG_HYDROGENS) THEN
! If hydrogens are used during SA, force use of hydrogens during autominimise
        CALL WDialogFieldState(IDF_UseHydrogensAuto, Disabled)
      ELSE
        CALL WDialogFieldStateLogical(IDF_UseHydrogensAuto, tLogical)
      ENDIF
! Read output profile chi**2 versus moves plot YES / NO
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL WDialogPutCheckBoxLogical(IDF_OutputChi2vsMoves, tLogical)
! Read the damping factor for the local minimisation
      CALL FileReadReal(hFile,RecNr,SA_SimplexDampingFactor)
! Read the seeds for the random number generator
      CALL WDialogSelect(IDD_SA_input3_2)
      CALL FileReadInteger(hFile, RecNr, tInteger)
      CALL WDialogPutInteger(IDF_SA_RandomSeed1, tInteger)
      CALL FileReadInteger(hFile, RecNr, tInteger)
      CALL WDialogPutInteger(IDF_SA_RandomSeed2, tInteger)
      CALL FileReadInteger(hFile, RecNr, tInteger)
      CALL WDialogPutInteger(IDF_SA_MaxRepeats, tInteger)
      CALL FileReadReal(hFile, RecNr, tReal)
      CALL WDialogPutReal(IDF_SA_ChiTest, tReal)
      CALL FileReadReal(hFile, RecNr, tReal)
      CALL WDialogPutReal(IDF_MaxMoves1, tReal)
      CALL FileReadInteger(hFile, RecNr, tInteger)
      CALL WDialogPutInteger(IDF_MaxMoves2, tInteger)
      CALL WDialogSelect(IDD_SAW_Page5)
! Atom labels for SA solutions overlay. Two options: 
! 1. "Element symbol + solution number"
! 2. "Original atom labels"
      CALL FileReadInteger(hFile,RecNr,tInteger)
      SELECT CASE (tInteger)
        CASE (1)
          CALL WDialogPutRadioButton(IDF_UseSolutionNr)
        CASE (2)
          CALL WDialogPutRadioButton(IDF_UseOriginal)
      END SELECT
! Atom colours for SA solutions overlay. Two options: 
! 1. "By solution number"
! 2. "By element"
      CALL FileReadInteger(hFile,RecNr,tInteger)
      SELECT CASE (tInteger)
        CASE (1)
          CALL WDialogPutRadioButton(IDF_ColourBySolution)
        CASE (2)
          CALL WDialogPutRadioButton(IDF_ColourByElement)
      END SELECT
! Following is new in DASH 2.1
! Use hydrogens for auto local minimise
      CALL FileReadLogical(hFile,RecNr,tLogical)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogPutCheckBoxLogical(IDF_UseHydrogensAuto, tLogical)
! Plot cumulative chi-squared 
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      CALL WDialogPutCheckBoxLogical(IDF_ShowCumChiSqd, tLogical)
! Following is new in DASH 2.2
! Divide difference by ESDs
      CALL FileReadLogical(hFile,RecNr,tLogical)
      IF (GetBFIOError() .NE. 0) THEN
        CLOSE(hFile)
        RETURN
      ENDIF
      CALL WDialogPutCheckBoxLogical(IDF_DivDiffByEsd, tLogical)
! Auto align      
      CALL FileReadLogical(hFile, RecNr, tLogical)
      CALL Set_AutoAlign(tLogical)
! Read the Mogul path
      CALL WDialogSelect(IDD_Configuration)
      CALL FileReadString(hFile,RecNr,MogulExe)
      CALL WDialogPutString(IDF_MogulExe,MogulExe)
! Use Mogul
      CALL FileReadLogical(hFile, RecNr, tLogical)
      UseMogul = tLogical
      CLOSE(hFile)
      RETURN
  999 CALL DebugErrorMessage('Error while opening config file')
      CLOSE(hFile)

      END SUBROUTINE ReadConfigurationFile
!
!*****************************************************************************
! 
