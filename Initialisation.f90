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

      INTEGER it

      CALL WDialogLoad(IDD_Structural_Information)
      CALL WDialogLoad(IDD_SA_Action1)
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
! ep      CALL WDialogLoad(IDD_SA_Multi_Completed) 
      CALL WDialogLoad(IDD_SA_Multi_completed_ep)
      CALL WDialogLoad(IDD_License_Dialog)
      CALL WDialogLoad(IDD_Background_Fit)
      CALL WDialogLoad(IDD_Pawley_ErrorLog)
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
        GOTO 999
      ENDIF
      CLOSE(110)
! Initialise space group to P 1
      NumberSGTable = 1
! Initialise crystal system to Triclinic, LatBrav = 1
      CALL UserSetCrystalSystem(1)
      RETURN
 999  CONTINUE
! Failure, so exit gracefully
      CALL WindowClose()
      STOP

      END SUBROUTINE PolyFitterInitialise
!
!*****************************************************************************
!
      SUBROUTINE SaveConfigurationFile

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'Poly_Colours.inc'

      CHARACTER*MaxPathLength tFileName
      INTEGER   I
      INTEGER   RecNr
      INTEGER   ISEED
      CHARACTER*MaxPathLength DefaultWorkingDir
      CHARACTER*256 TempString ! Must be multiple of 4
      INTEGER*4 I4(64)
      EQUIVALENCE (I4,TempString)

      tFileName = 'D3.cfg'
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=10,FILE=tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      RecNr = 1
      WRITE(10,REC=RecNr,ERR=999) UseConfigFile
      RecNr = RecNr + 1
      IF (.NOT. UseConfigFile) GOTO 999
! Save all colours
      WRITE(10,REC=RecNr,ERR=999) KolNumPGWindow
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPGWindow%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPGWindow%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPGWindow%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumMain
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMain%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMain%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMain%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumObs
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolObs%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolObs%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolObs%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumCal
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCal%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCal%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCal%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumDif
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolDif%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolDif%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolDif%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumMTic
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMTic%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMTic%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolMTic%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumCTic
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCTic%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCTic%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolCTic%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPanelVLite
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVLite%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVLite%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVLite%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPanelLite
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelLite%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelLite%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelLite%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPanelDark
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelDark%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelDark%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelDark%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPanelVDark
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVDark%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVDark%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelVDark%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPanelOuter
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelOuter%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelOuter%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPanelOuter%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumRectSelect
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolRectSelect%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolRectSelect%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolRectSelect%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumLargeCrossHair
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolLargeCrossHair%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolLargeCrossHair%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolLargeCrossHair%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPeakFit
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakFit%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakFit%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakFit%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumPeakPos
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakPos%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakPos%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolPeakPos%IBlue
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolNumBack
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolBack%IRed
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolBack%IGreen
      RecNr = RecNr + 1
      WRITE(10,REC=RecNr,ERR=999) KolBack%IBlue
      RecNr = RecNr + 1
! Save the default working directory
      DefaultWorkingDir = 'D:\cvsDASH\dash\Debug'
      TempString = DefaultWorkingDir
      DO I = 1, 64
        WRITE(10,REC=RecNr,ERR=999) I4(I)
        RecNr = RecNr + 1
      ENDDO
! Save defaults for background subtraction
 ! Number of iterations
      WRITE(10,REC=RecNr,ERR=999) 20
      RecNr = RecNr + 1
 ! Window
      WRITE(10,REC=RecNr,ERR=999) 100
      RecNr = RecNr + 1
 ! Use Monte Carlo YES /NO
      WRITE(10,REC=RecNr,ERR=999) .TRUE.
      RecNr = RecNr + 1
 ! Use spline smooth
      WRITE(10,REC=RecNr,ERR=999) .TRUE.
      RecNr = RecNr + 1
! Save the seeds for the random number generator
      CALL WDialogGetInteger(IDF_SA_RandomSeed1,ISEED)
      WRITE(10,REC=RecNr,ERR=999) ISEED
      RecNr = RecNr + 1
      CALL WDialogGetInteger(IDF_SA_RandomSeed2,ISEED)
      WRITE(10,REC=RecNr,ERR=999) ISEED
      RecNr = RecNr + 1
      CALL WDialogGetInteger(IDF_SA_RandomSeed3,ISEED)
      WRITE(10,REC=RecNr,ERR=999) ISEED
      RecNr = RecNr + 1
! Save use hydrogens YES / NO

! Save default wavelength

! Save default maximum resolution

! Save YES /NO which molecular file formats are to be written out when a best solution is found
! 1. .pdb ?
      WRITE(10,REC=RecNr,ERR=999) SavePDB
      RecNr = RecNr + 1
! 2. .cssr ?
      WRITE(10,REC=RecNr,ERR=999) SaveCSSR
      RecNr = RecNr + 1
! 3. .ccl ?
      WRITE(10,REC=RecNr,ERR=999) SaveCCL
      RecNr = RecNr + 1
! 4. .res ?
      WRITE(10,REC=RecNr,ERR=999) SaveRES
      RecNr = RecNr + 1
! Auto local minimisation at the end of every run in multirun YES / NO
      WRITE(10,REC=RecNr,ERR=999) AutoLocalMinimisation
      RecNr = RecNr + 1


  999 CLOSE(10)
      RETURN

      END SUBROUTINE SaveConfigurationFile
!
!*****************************************************************************
!
      SUBROUTINE InitialiseVariables

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'Poly_Colours.inc'
      INCLUDE 'DialogPosCmn.inc'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PLTINI/ XPG1,XPG2,YPG1,YPG2
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2

      REAL    WaveLengthOf ! Function

      DashRawFile = ' '
      DashHcvFile = ' '
      DashPikFile = ' '
      DashTicFile = ' '
      SavePDB  = .TRUE.
      SaveCSSR = .TRUE.
      SaveCCL  = .TRUE.
      SaveRES  = .TRUE.
      AutoLocalMinimisation = .TRUE.
      UseConfigFile = .TRUE.
!O      IDCurrent_Cursor_Mode = ID_Default_Mode
      IDCurrent_Cursor_Mode = ID_Peak_Fitting_Mode
      DataSetChange = 0
      NumInternalDSC = -1
      ZeroPoint = 0.0
      CALL UpdateWavelength(WaveLengthOf('Cu'))
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogGetReal(IDF_MaxResolution,tReal)
      CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tReal))
      CALL WDialogSelect(IDD_SA_input3)
      ISeed1 = 314
      ISeed2 = 159
      ISeed3 = 265
      CALL WDialogPutInteger(IDF_SA_RandomSeed1,ISeed1)
      CALL WDialogPutInteger(IDF_SA_RandomSeed2,ISeed2)
      CALL WDialogPutInteger(IDF_SA_RandomSeed3,ISeed3)
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
!O      KolObs            = Win_RGB(161,0,0)
      KolObs            = Win_RGB(255,0,0)
!O      KolCal            = Win_RGB(10,70,10)
      KolCal            = Win_RGB(0,0,255)
      KolDif            = Win_RGB(200,100,200)
      KolMTic           = Win_RGB(191,0,0)
      KolCTic           = Win_RGB(0,131,131)
      KolPanelVLite     = Win_RGB(245,245,245)
      KolPanelLite      = Win_RGB(235,235,235)
      KolPanelDark      = Win_RGB(210,210,210)
      KolPanelVDark     = Win_RGB(170,170,170)
      KolPanelOuter     = Win_RGB(190,190,190)
      KolRectSelect     = Win_RGB(150,150,5)
      KolLargeCrossHair = Win_RGB(150,150,5)
      KolPeakFit        = Win_RGB(20,20,240)
      KolPeakPos        = Win_RGB(50,50,200)
      KolBack           = Win_RGB(164,211,105)
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
