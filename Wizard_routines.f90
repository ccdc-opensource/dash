!
!*****************************************************************************
!
      SUBROUTINE StartWizard

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'DialogPosCmn.inc'

      CALL SetWizardState(-1)
      CALL SelectMode(ID_Peak_Fitting_Mode)
      CALL SetModeMenuState(0,-1,-1)
      CALL ToggleMenus(0)
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,IDNEXT,Modeless)

      END SUBROUTINE StartWizard
!
!*****************************************************************************
!
      SUBROUTINE EndWizard

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmn.inc'

      IXPos_IDD_Wizard = WInfoDialog(6)
      IYPos_IDD_Wizard = WInfoDialog(7)
      CALL WDialogHide()
      CALL ToggleMenus(1)
      IF (.NOT. NoData) CALL SetModeMenuState(1,0,0)

      CALL PushActiveWindowID
      CALL Upload_CrystalSystem
      CALL Upload_Cell_Constants()
      CALL Upload_Range()
      CALL PopActiveWindowID

      CALL Generate_TicMarks
      CALL SetWizardState(1)

      END SUBROUTINE EndWizard
!
!*****************************************************************************
!
      SUBROUTINE DealWithMainWizardWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      INTEGER :: IPW_Option
      LOGICAL FnUnitCellOK ! Function
      CHARACTER*MaxPathLength tString

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_PW0_Skip, IDCANCEL) ! The 'Close' button
              CALL EndWizard
            CASE (IDNEXT)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
! We're off the main page and on to new pages depending on the option.
              CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
                SELECT CASE (IPW_Option)
                  CASE (1) ! View data / determine peaks positions
                    CALL WDialogSelect(IDD_PW_Page3)
                    CALL WDialogGetString(IDF_PWa_DataFileName_String,tString)
! If no filename provided => grey out buttons 'Next' and 'Finish'
                    IF (LEN_TRIM(tString) .EQ. 0) THEN
                      CALL WDialogFieldState(IDFINISH,Disabled)
                      CALL WDialogFieldState(IDNEXT,Disabled)
                    ELSE
                      CALL WDialogFieldState(IDFINISH,Enabled)
                      CALL WDialogFieldState(IDNEXT,Enabled)
                    ENDIF
                    CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
                  CASE (2) ! Preparation for Pawley refinement
                    CALL WDialogSelect(IDD_PW_Page1)
! If the cell is OK, the Next> button should be enabled
                    IF (FnUnitCellOK()) THEN
                      CALL WDialogFieldState(IDNEXT,Enabled)
                    ELSE
                      CALL WDialogFieldState(IDNEXT,Disabled)
                    END IF
                    CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
                  CASE (3) ! Simulated annealing structure solution
                    CALL SA_MAIN()
                END SELECT
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithMainWizardWindow 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithMainWizardWindow')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithMainWizardWindow
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyFinish_1
!
! This subroutine deals with the Finish button of the first series of wizard windows:
!
! Load file -> ( diffraction setup -> ) truncate start/end -> { excluded regions -> } Background
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL WizardApplyDiffractionFileInput
      CALL WizardApplyProfileRange
      CALL WizardApplyBackground
      CALL EndWizard

      END SUBROUTINE WizardApplyFinish_1
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyDiffractionFileInput

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

      REAL              XPF_Range
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                                   &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

! Not too pretty, but safe
      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER I, J, JJ, IST
      REAL XADD, YOADD, VADD

      NOBS = BackupNOBS
      XOBS = BackupXOBS
      YOBS = BackupYOBS
      EOBS = BackupEOBS
      XPMIN = XOBS(1)
      XPMAX = XOBS(1)
      YPMIN = YOBS(1)
      YPMAX = YOBS(1)
      DO I = 1, NOBS
        XPMIN = MIN(XOBS(I),XPMIN)
        XPMAX = MAX(XOBS(I),XPMAX)
        YPMIN = MIN(YOBS(I),YPMIN)
        YPMAX = MAX(YOBS(I),YPMAX)
      ENDDO
      OriginalNOBS = NOBS
      EndNOBS = OriginalNOBS
      NBIN = (NOBS/LBIN)
      DO I = 1, NBIN
        IST = (I-1)*LBIN
        XADD  = 0.0
        YOADD = 0.0
        VADD  = 0.0
        DO J = 1, LBIN
          JJ = J + IST
          XADD  = XADD+XOBS(JJ)
          YOADD = YOADD+YOBS(JJ)
          VADD  = VADD+EOBS(JJ)**2
        ENDDO
        XBIN(I)  = XADD/FLOAT(LBIN)
        YOBIN(I) = YOADD/FLOAT(LBIN)
        YCBIN(I) = YOBIN(I)
        EBIN(I)  = SQRT(VADD)/FLOAT(LBIN)
! JvdS Assume no knowledge on background
        YBBIN(I) = 0.0
      ENDDO
      XPGMIN = XPMIN
      XPGMAX = XPMAX
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      CALL UPLOAD_RANGE()
      IPMIN = 1
      IPMAX = NBIN
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX

      END SUBROUTINE WizardApplyDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionFileInput

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

! Not too pretty, but safe
      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      CHARACTER(MaxPathLength) CTEMP
      INTEGER ISTAT
      INTEGER DiffractionFileBrowse ! Function
      INTEGER DiffractionFileOpen ! Function

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyFinish_1
            CASE (IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDNEXT)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page4)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (ID_PWa_DF_Open)
              CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
              ISTAT = DiffractionFileOpen(CTEMP)
              IF (ISTAT .EQ. 1) THEN
                CALL WDialogFieldState(IDNEXT,Enabled)
                CALL WDialogFieldState(IDFINISH,Enabled)
              ELSE
                CALL WDialogFieldState(IDNEXT,Disabled)
                CALL WDialogFieldState(IDFINISH,Disabled)
              ENDIF
            CASE (IDBBROWSE)
              ISTAT = DiffractionFileBrowse()
! Don't change if the user pressed 'Cancel' (ISTAT = 2)
              IF      (ISTAT .EQ. 1) THEN
                CALL WDialogFieldState(IDNEXT,Enabled)
                CALL WDialogFieldState(IDFINISH,Enabled)
                BackupNOBS = NOBS
                BackupXOBS = XOBS
                BackupYOBS = YOBS
                BackupEOBS = EOBS
              ELSE IF (ISTAT .EQ. 0) THEN
                CALL WDialogFieldState(IDNEXT,Disabled)
                CALL WDialogFieldState(IDFINISH,Disabled)
              ENDIF
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionSetup
!
! Effectively, the user is asked to provide / confirm the wavelength
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      REAL Temp
      INTEGER IRadSelection

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page4)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyFinish_1
            CASE (IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page3)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDNEXT)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page5)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_PW_LabX_Source,IDF_PW_SynX_Source,IDF_PW_CWN_Source,IDF_PW_TOF_source)
              CALL WDialogGetRadioButton(IDF_PW_LabX_Source,JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
            CASE (IDF_PW_wavelength1)
              CALL WDialogGetReal(IDF_PW_wavelength1,Temp)
              CALL UpdateWavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
          END SELECT                
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionSetup
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyProfileRange

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER ISTATE
      REAL    tReal

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogGetCheckBox(IDF_TruncateStartYN,ISTATE)
      IF (ISTATE .EQ. 1) THEN
        CALL WDialogGetReal(IDF_Min2Theta,tReal)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tReal = 0.0
      ENDIF
      CALL TruncateDataStart(tReal)
      CALL WDialogGetCheckBox(IDF_TruncateEndYN,ISTATE)
      IF (ISTATE .EQ. 1) THEN
        CALL WDialogGetReal(IDF_Max2Theta,tReal)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tReal = 90.0
      ENDIF
      CALL TruncateData(tReal)
      CALL PopActiveWindowID

      END SUBROUTINE WizardApplyProfileRange
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowProfileRange

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      INTEGER ISTATE
      REAL tReal
      REAL TwoTheta2dSpacing ! Function
      REAL dSpacing2TwoTheta ! Function

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyFinish_1
            CASE (IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page4)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDNEXT)
              CALL WizardApplyProfileRange
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page6)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDB_ConvertT2R) ! Convert max. 2 theta to max. resolution
              CALL WDialogGetReal(IDF_Max2Theta,tReal)
              CALL WDialogPutReal(IDF_MaxResolution,TwoTheta2dSpacing(tReal))
            CASE (IDB_ConvertR2T) ! Convert max. resolution to max. 2 theta
              CALL WDialogGetReal(IDF_MaxResolution,tReal)
              CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tReal))
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowProfileRange 1')
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_TruncateStartYN)
! If set to 'TRUE', ungrey value field and vice versa
              CALL WDialogGetCheckBox(IDF_TruncateStartYN,ISTATE)
              IF (ISTATE .EQ. 1) THEN
                CALL WDialogFieldState(IDF_Min2Theta,Enabled)
              ELSE
                CALL WDialogFieldState(IDF_Min2Theta,Disabled)
              ENDIF
            CASE (IDF_TruncateEndYN)
! If set to 'TRUE', ungrey value fields and vice versa
              CALL WDialogGetCheckBox(IDF_TruncateEndYN,ISTATE)
              IF (ISTATE .EQ. 1) THEN
                CALL WDialogFieldState(IDF_Max2Theta,Enabled)
                CALL WDialogFieldState(IDF_MaxResolution,Enabled)
              ELSE
                CALL WDialogFieldState(IDF_Max2Theta,Disabled)
                CALL WDialogFieldState(IDF_MaxResolution,Disabled)
              ENDIF
            CASE (IDF_Max2Theta)
! When entering a maximum value for 2 theta, update maximum value for the resolution
              CALL WDialogGetReal(IDF_Max2Theta,tReal)
              CALL WDialogPutReal(IDF_MaxResolution,TwoTheta2dSpacing(tReal))
            CASE (IDF_MaxResolution)
! When entering a maximum value for the resolution, update maximum value for 2 theta
              CALL WDialogGetReal(IDF_MaxResolution,tReal)
              CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tReal))
          END SELECT
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowProfileRange')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowProfileRange
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyBackground

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      INTEGER ISTATE, tInt1, tInt2
      LOGICAL tUseMC, tUseSpline

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page6)
      CALL WDialogGetCheckBox(IDF_UseMCYN,ISTATE)
      tUseMC = (ISTATE .EQ. 1)
      CALL WDialogGetCheckBox(IDF_UseSplineYN,ISTATE)
      tUseSpline = (ISTATE .EQ. 1)
      CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
      CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
      CALL SubtractBackground(tInt1,tInt2,tUseMC,tUseSpline)
      CALL Profile_Plot(IPTYPE)
      CALL PopActiveWindowID

      END SUBROUTINE WizardApplyBackground
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowBackground

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      INTEGER ISTATE, tInt1, tInt2
      LOGICAL tUseMC, tUseSpline

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page6)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyFinish_1
            CASE (IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page5)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDF_Preview)
              CALL WDialogGetCheckBox(IDF_UseMCYN,ISTATE)
              tUseMC = (ISTATE .EQ. 1)
              CALL WDialogGetCheckBox(IDF_UseSplineYN,ISTATE)
              tUseSpline = (ISTATE .EQ. 1)
              CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
              CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
              CALL CalculateBackground(tInt1,tInt2,tUseMC,tUseSpline)
              CALL Profile_Plot(IPTYPE)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowBackground
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowUnitCellParameters

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page1)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDNEXT)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page2)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowUnitCellParameters 1')
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Space_Group_Menu)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              NumPawleyRef = 0
            CASE (IDF_Crystal_System_Menu)
              CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
              CALL Upload_CrystalSystem
              CALL Generate_TicMarks
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,CellPar(1))
              CALL UpdateCell
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,CellPar(2))
              CALL UpdateCell
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,CellPar(3))
              CALL UpdateCell
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell
          END SELECT                
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowUnitCellParameters')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowUnitCellParameters
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionSetup2

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER IRadSelection
      CHARACTER(LEN=MaxPathLength) CTEMP
      REAL    Temp
      INTEGER ISTAT
      INTEGER DiffractionFileBrowse ! Function
      INTEGER DiffractionFileOpen ! Function

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page2)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page1)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDFINISH, IDCANCEL)
              CALL EndWizard
            CASE (ID_PW_DF_Open)
              CALL WDialogGetString(IDF_PW_DataFileName_String,CTEMP)
              ISTAT = DiffractionFileOpen(CTEMP)
            CASE (ID_PW_DF_Browse)
              ISTAT = DiffractionFileBrowse()
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionSetup 1')
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_PW_LabX_Source,IDF_PW_SynX_Source,IDF_PW_CWN_Source,IDF_PW_TOF_source)
              CALL WDialogGetRadioButton(IDF_PW_LabX_Source,JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
            CASE (IDF_PW_wavelength1)
              CALL WDialogGetReal(IDF_PW_wavelength1,Temp)
              CALL UpdateWavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
          END SELECT                
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionSetup')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionSetup2
!
!*****************************************************************************
!
