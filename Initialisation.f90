!
!*****************************************************************************
!
      SUBROUTINE Init_StdOut()
 
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DFLIB

      IMPLICIT NONE

      INTEGER :: IFlags, Ilen, Instlen, Idashlen
      CHARACTER(LEN=MaxPathLength) :: Dirname, DashDir, InstDirLc, DashDirLc, DirNameLc
      LOGICAL Confirm ! Function

      Idashlen = GETENVQQ("DASH_DIR",DashDir)
      Instlen = LEN_TRIM(INSTDIR)
      InstDirLc = InstDir
      DashDirLc = DashDir
      CALL ILowerCase(DashDirLc)
      CALL ILowerCase(InstDirLc)
! JvdS @ Rewrite using GOTOs
      LOOP_DIRECTORY_SELECT : DO WHILE (.TRUE.)
        IFlags = DirChange + DirCreate
        Dirname = ' '
        CALL WSelectDir(IFlags,Dirname,"Select working directory for DASH...")
        IF (LEN_TRIM(Dirname) .EQ. 0) THEN
          CALL WindowClose()
          STOP
        END IF
        DirNameLc = DirName
        CALL ILowerCase(DirNameLc)
        Ilen = LEN_TRIM(DirNameLc)
        IF ( (DirNameLc(1:Ilen) .EQ. DashDirLc(1:LEN_TRIM(DashDirLc))) .OR.  &
             (DirNameLc(1:Ilen) .EQ. InstDirLc(1:LEN_TRIM(InstDirLc))) ) THEN
          IF (.NOT. Confirm("Are you sure you wish to start Dash in"//CHAR(13)//"the installation directory "//&
            CHAR(13)//DirNameLc(1:Ilen)//" ?")) CYCLE LOOP_DIRECTORY_SELECT
        END IF
! Open the file
        OPEN(UNIT = 6, FILE = 'dash.out', STATUS = 'UNKNOWN', ERR = 110)
        RETURN
 110    CONTINUE
        CALL WMessageBox(OKCancel,ExclamationIcon,CommonOk, &
          "DASH problem: Could not open temporary files"//&
          CHAR(13)//"in the directory "//DirName(4:Ilen)//CHAR(13)//&
          "Please pick an alternative directory for your DASH run",&
          "File-open failure")
        IF (WInfoDialog(4) .NE. CommonOK) THEN
          CALL WindowClose()
          STOP
        END IF
      END DO LOOP_DIRECTORY_SELECT

      END SUBROUTINE Init_StdOut
!
!*****************************************************************************
!
!C>> JCC Rather than continually load/unload the various widgets, upload them all only once
!C>> This way, the state can be memorised from session to session
      SUBROUTINE PolyFitter_UploadDialogues

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

!C>> SA bitmap
      INTEGER it, Ibmhandle
      COMMON / BMPHAN / Ibmhandle

      CALL WDialogLoad(IDD_Structural_Information)
      CALL WDialogLoad(IDD_SA_Action1)
      CALL WDialogLoad(IDD_SA_Action2)
      CALL WDialogLoad(IDD_Plot_Option_Dialog)
!      CALL WDialogLoad(IDD_About_Polyfitter)
      CALL WDialogLoad(IDD_Pawley_Status)
      CALL WDialogLoad(IDD_Peak_Positions)
      CALL WDialogLoad(IDD_Index_Preparation)
! Set the colours of the grid manually
      CALL WDialogLoad(IDD_Plot_Option_Dialog)
! JvdS Starting to add SA to Wizard
!      CALL WDialogLoad(IDD_SA_input1)
      CALL WDialogLoad(IDD_SAW_Page1)
      CALL WDialogLoad(IDD_SA_input2)
      CALL WDialogLoad(IDD_SA_input3)
      CALL WDialogLoad(IDD_Crystal_Symmetry)
      CALL WDialogLoad(IDD_Data_Properties)
      CALL WDialogLoad(IDD_Sigma_info)
      CALL WDialogLoad(IDD_Gamma_info)
      CALL WDialogLoad(IDD_HPSL_info)
      CALL WDialogLoad(IDD_HMSL_info)
      CALL WDialogLoad(IDD_Polyfitter_Wizard_01)
      CALL WDialogLoad(IDD_PW_Page1)
      CALL WDialogLoad(IDD_PW_Page2)
      CALL WDialogLoad(IDD_PW_Page3)
      CALL WDialogLoad(IDD_PW_Page4)
      CALL WDialogLoad(IDD_PW_Page5)
      CALL WDialogLoad(IDD_PW_Page6)
      CALL WDialogLoad(IDD_SA_Completed)
! ep      CALL WDialogLoad(IDD_SA_Multi_Completed) 
      CALL WDialogLoad(IDD_SA_Multi_completed_ep)
      CALL WDialogLoad(IDD_License_Dialog)
      CALL WDialogLoad(ID_Background_Fit)
      CALL WDialogLoad(IDD_Pawley_ErrorLog)
! Upload sa bitmap into memory
      ibmhandle = 0
      CALL WDialogSelect(IDD_SA_Action1)
      CALL IGrSelect(3,IDF_minchisq_picture)
      CALL WBitmapGet(ibmhandle,0)
      it = InfoError(1)
      RETURN

      END SUBROUTINE PolyFitter_UploadDialogues
!
!*****************************************************************************
!
      SUBROUTINE PolyFitterInitialise()

      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      CHARACTER(LEN=128) lintem
      INTEGER I, II, nl, OpenFail, PolyFitter_OpenSpaceGroupSymbols
!O      DATA LPosSG/1,1,3,38,73,108,349,430,455,462,489,531/

      DoSaRedraw = .FALSE.
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
!>> JCC Init the viewing etc
      CALL PolyFitter_EnableExternal
! Get the space group symbols ...
      OpenFail = PolyFitter_OpenSpaceGroupSymbols()
      IF (OpenFail .NE. 0) GOTO 999 ! fail gracefully!
      i=0
 10   lintem=' '
      READ(110,1100,END=100) nl,lintem
 1100 FORMAT(Q,A)
      IF (lintem(1:1) .EQ. '-') GOTO 100
      IF (nl .LT. 70) THEN
        DO ii=nl+1,70
          lintem(ii:ii)=' '
        END DO
      END IF
      i=i+1
      SGNumStr(i) = lintem(4:13)
      SGHMaStr(i) = lintem(15:26)
!U      SGHalStr(i) = lintem(29:46)
      SGShmStr(i) = lintem(47:70)
      GOTO 10
 100  IF (I .NE. MaxSPGR) THEN
        CALL ErrorMessage('Number of space groups in space group file has changed.')
        GOTO 999
      ENDIF
      CLOSE(110)
! Initialise crystal system to Triclinic, LatBrav = 1
      CALL SetCrystalSystem(1)
      NumberSGTable = 1
! Initialise the space group menus in the main window and the wizard.
      CALL SetSpaceGroupMenu
      RETURN
 999  CONTINUE
! Failure, so exit gracefully
      CALL WindowClose()
      STOP

      END SUBROUTINE PolyFitterInitialise
!
!*****************************************************************************
!
      SUBROUTINE InitialiseVariables

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PLTINI/ XPG1,XPG2,YPG1,YPG2
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)

      INCLUDE 'statlog.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'
      INCLUDE 'DialogPosCmn.inc'

      REAL    WaveLengthOf ! Function

      DashRawFile = ' '
      DashHcvFile = ' '
      DashPikFile = ' '
      DashTicFile = ' '
!O      IDCurrent_Cursor_Mode = ID_Default_Mode
      IDCurrent_Cursor_Mode = ID_Peak_Fitting_Mode
      DataSetChange = 0
      NumInternalDSC = -1
      ZeroPoint = 0.0
      CALL UpdateWavelength(WaveLengthOf('Cu'))
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogGetReal(IDF_MaxResolution,tReal)
      CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tReal))
      SLIMVALUE = 1.0
      SCALFAC   = 0.01
      BACKREF   = .TRUE.

      JRadOption = 1 ! Initialise to X-ray lab data

      IXPos_IDD_Pawley_Status = 0.1  * XBSWidth
      IYPos_IDD_Pawley_Status = 0.06 * XBSHeight
      IXPos_IDD_SA_Input = 0.1  * XBSWidth
      IYPos_IDD_SA_Input = 0.06 * XBSHeight
      IXPos_IDD_Wizard = 0.1  * XBSWidth
      IYPos_IDD_Wizard = 0.06 * XBSHeight

      FromPawleyFit = .FALSE. 

      NUMOBSTIC = 0
      NTIC = 0
      LBIN = 1

      MARKER_SIZE = 0.35
      CHAR_SIZE = 1.0

      XPG1 = 0.12
      XPG2 = 0.95
      YPG1 = 0.12
      YPG2 = 0.93

      KolNumPGWindow=220
      KolNumMain=221
      KolNumObs=222
      KolNumCal=223
      KolNumDif=224
      KolNumMTic=225
      KolNumCTic=226
      KolNumPanelVLite=227
      KolNumPanelLite=228
      KolNumPanelDark=229
      KolNumPanelVDark = 230
      KolNumPanelOuter = 231
      KolNumRectSelect = 232
      KolNumLargeCrossHair = 233
      KolNumPeakFit=234
      KolNumPeakPos=235
      KolNumBack=236

      KolDefPGWindow=Win_RGB(253,253,248)
      KolDefMain=Win_RGB(20,20,150)
      KolDefObs=Win_RGB(161,0,0)
      KolDefCal=Win_RGB(10,70,10)
      KolDefDif=Win_RGB(200,100,200)
      KolDefMTic=Win_RGB(191,0,0)
      KolDefCTic=Win_RGB(0,131,131)
      KolDefPanelVLite=Win_RGB(245,245,245)
      KolDefPanelLite=Win_RGB(235,235,235)
      KolDefPanelDark=Win_RGB(210,210,210)
      KolDefPanelVDark=Win_RGB(170,170,170)
      KolDefPanelOuter=Win_RGB(190,190,190)
!      KolDefPanelVLite=Win_RGB(250,250,250)
!      KolDefPanelLite=Win_RGB(245,245,245)
!      KolDefPanelDark=Win_RGB(230,230,230)
!      KolDefPanelVDark=Win_RGB(220,220,220)
!      KolDefPanelOuter=Win_RGB(225,225,225)
      KolDefRectSelect=Win_RGB(150,150,5)
      KolDefLargeCrossHair=Win_RGB(150,150,5)
      KolDefPeakFit=Win_RGB(20,20,240)
      KolDefPeakPos=Win_RGB(50,50,200)
      KolDefBack=Win_RGB(164,211,105)

      KolPGWindow=KolDefPGWindow
      KolMain=KolDefMain
      KolObs=KolDefObs
      KolCal=KolDefCal
      KolDif=KolDefDif
      KolMTic=KolDefMTic
      KolCTic=KolDefCTic
      KolPanelVLite=KolDefPanelVLite
      KolPanelLite=KolDefPanelLite
      KolPanelDark=KolDefPanelDark
      KolPanelVDark=KolDefPanelVDark
      KolPanelOuter=KolDefPanelOuter
      KolRectSelect=KolDefRectSelect
      KolLargeCrossHair=KolDefLargeCrossHair
      KolPeakFit=KolDefPeakFit
      KolPeakPos=KolDefPeakPos
      KolBack=KolDefBack

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

      END SUBROUTINE InitialiseVariables
!
!*****************************************************************************
!
!C>> Handle file opening. Exit with a message to say what is wrong if all attempts fail
! JvdS What does it return? Filehandle and 0 otherwise?
      INTEGER FUNCTION PolyFitter_OpenSpaceGroupSymbols

      USE WINTERACTER
      USE VARIABLES
      USE dflib ! Windows environment variable handling: for GETENVQQ
      USE dfport

      INTEGER       errstat, lval, dlen
      CHARACTER*255 DashDir, Command

      PolyFitter_OpenSpaceGroupSymbols = 0
!   Try the default installation directory first
      OPEN(110,file=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//SPACEGROUPS,status='old', err = 10)
      RETURN
 10   CONTINUE
!   Fail so look in current working directory
      OPEN(110,file=SPACEGROUPS,status='old', err = 20, iostat = errstat)
      dlen = GETCWD(INSTDIR)
      RETURN
 20   CONTINUE
!   Failed to open in the current working directory: try getting the environment variable DASH_DIR
      lval = GETENVQQ("DASH_DIR",DashDir)
      IF ((lval .LE. LEN(DashDir)) .AND. (lval .GT. 0)) THEN
!   Environment variable is set
        Open(110,file=DashDir(1:LEN_TRIM(DashDir))//DIRSPACER//SPACEGROUPS,status='old', err = 30)
        INSTDIR = DASHDIR
        RETURN
 30     CONTINUE
! If DASH_DIR is set, then use a different message
        Call WMessageBox(OKOnly, ExclamationIcon, CommonOk, &
          "Sorry, DASH is not installed correctly: Could not find the file"//CHAR(13)//CHAR(13)&
          //SPACEGROUPS//CHAR(13)//CHAR(13)//"in the default installation directory "//CHAR(13)&
          //CHAR(13)//INSTDIR(1:LEN_TRIM(INSTDIR))&
          //CHAR(13)//CHAR(13)//"in your current working directory, or in the directory "//CHAR(13)//CHAR(13)&
          //DashDir(1:LEN_TRIM(DashDir)),"Installation failure")              
          PolyFitter_OpenSpaceGroupSymbols = errstat
        RETURN
      ENDIF
! Try looking at the command path itself and deriving the path from that
      CALL GetArg(0,Command)
      dlen = LEN_TRIM(Command)
      DO WHILE (Command(dlen:dlen) .NE. DIRSPACER)
        dlen = dlen - 1
      END DO
! JvdS What happens if no DIRSPACER is present?
      dlen = dlen - 1
      OPEN(110,File=Command(1:dlen)//DIRSPACER//SPACEGROUPS,status='old', err = 40)
      INSTDIR = Command(1:dlen)
      RETURN
 40   CONTINUE
! If we get here, all attempts failed to open the file so fail gracefully
      CALL WMessageBox(OKOnly, ExclamationIcon, CommonOk, &
        "Sorry, DASH is not installed correctly: Could not find the file"//CHAR(13)//CHAR(13)&
        //SPACEGROUPS//CHAR(13)//CHAR(13)//"in the default installation directory "//CHAR(13)&
        //CHAR(13)//INSTDIR(1:LEN_TRIM(INSTDIR))&
        //CHAR(13)//CHAR(13)//"or in your current working directory", "Installation failure")
      PolyFitter_OpenSpaceGroupSymbols = errstat
      RETURN

      END FUNCTION PolyFitter_OpenSpaceGroupSymbols
!
!*****************************************************************************
! 
      SUBROUTINE PolyFitter_EnableExternal()

      USE WINTERACTER
      USE VARIABLES
      USE DFLIB ! Windows environment variable handling: for GETENVQQ

      INTEGER       lval
      CHARACTER*255 DashDir
      CHARACTER*255 line
      CHARACTER*3   KeyChar
      INTEGER       nl, dlen

      ViewOn     = .FALSE.
      ViewAct    = .FALSE.
      AutoUpdate = .FALSE.
      ConvOn     = .FALSE.
      lval = GETENVQQ("DASH_DIR",DashDir)
      IF ((lval .LE. LEN(DashDir)) .AND. (lval .GT. 0)) THEN
        CONVEXE = DashDir(1:LEN_TRIM(DashDir))//DIRSPACER//'zmconv.exe'
        OPEN(121, FILE=DashDir(1:LEN_TRIM(DashDir))//DIRSPACER//CONFIG, STATUS='OLD', ERR = 10)
        GOTO 20
 10     OPEN(121, FILE=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//CONFIG, STATUS='OLD', ERR = 30)
 20     CONTINUE
      ELSE
        OPEN(121, FILE=INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//CONFIG, STATUS='OLD', ERR = 22)
        GOTO 24
 22     CONTINUE
        CALL Getarg(0,line)
        dlen = LEN_TRIM(line)
        DO WHILE (line(dlen:dlen) .NE. DIRSPACER)
          dlen = dlen - 1
        END DO
        dlen = dlen - 1
        OPEN(121, FILE=line(1:dlen)//DIRSPACER//CONFIG, STATUS='OLD', ERR = 30)
        INSTDIR = line(1:dlen)
 24     CONTINUE
        CONVEXE = INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'zmconv.exe'
      END IF
! Read it
      DO WHILE ( .TRUE. )
 25     READ(121,'(a)',END=30,ERR=30) line
        nl=LEN_TRIM(line)
        CALL ILowerCase(line(:nl))
        CALL INextString(line,keychar)
        SELECT CASE (KeyChar)
          CASE ('vie')
            VIEWEXE = line(IlocateChar(line):nl)
          CASE ('con')
            CONVEXE = line(IlocateChar(line):nl)
          CASE ('arg')
            VIEWARG = line(IlocateChar(line):nl) ! Arguments for the viewer
          CASE ('rel')
            AUTOUPDATE = .TRUE.
        END SELECT
      END DO            
 30   CONTINUE
      INQUIRE(FILE=VIEWEXE(1:LEN_TRIM(VIEWEXE)),EXIST=ViewOn)
      INQUIRE(FILE=CONVEXE(1:LEN_TRIM(CONVEXE)),EXIST=ConvOn)

      END SUBROUTINE PolyFitter_EnableExternal
!
!*****************************************************************************
! 
