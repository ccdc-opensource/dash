!
!*****************************************************************************
!
      SUBROUTINE StartWizard

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      CALL SetModeMenuState(1,0)
      CALL SelectMode(ID_Peak_Fitting_Mode)
      CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)

      END SUBROUTINE StartWizard
!
!*****************************************************************************
!
      SUBROUTINE WizardWindowHide

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

! Things go all wrong if some intelligent part of DASH decides that the current Wizard window should be
! hidden while the main DASH window has been minimised. Therefore, just enter an infinite loop...
      CALL WindowSelect(0)
      DO WHILE (WinfoWindow(WindowState) .EQ. WinMinimised)
        CALL PeekEvent
        CALL WindowSelect(0)
        CALL IOsWait(50) ! wait half a sec
      ENDDO
      IF (CurrentWizardWindow .EQ. 0) RETURN
      CALL WDialogSelect(CurrentWizardWindow)
      IF (WInfoDialog(6) .GT. 0) THEN
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
      ENDIF
      CALL WDialogHide()
      CurrentWizardWindow = 0

      END SUBROUTINE WizardWindowHide
!
!*****************************************************************************
!
      SUBROUTINE WizardWindowShow(TheDialogID)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheDialogID

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard
 
      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

! Hide any visible Wizard window
      CALL WizardWindowHide
      CALL WDialogSelect(TheDialogID)
      CurrentWizardWindow = TheDialogID
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,IDNEXT,Modeless)
      CALL SetWizardState(-1)

      END SUBROUTINE WizardWindowShow
!
!*****************************************************************************
!
      SUBROUTINE EndWizardCommon

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement

      CALL WizardWindowHide
      IF (WeCanDoAPawleyRefinement()) THEN
        CALL SetModeMenuState(1,1)
      ELSE
        CALL SetModeMenuState(1,-1)
      ENDIF
      CALL SelectMode(ID_Peak_Fitting_Mode)
      CALL SetWizardState(1)

      END SUBROUTINE EndWizardCommon
!
!*****************************************************************************
!
      SUBROUTINE EndWizard

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL EndWizardCommon
! Legacy code from first release. Should not be necessary any more?
! The wizard windows can only be closed by DASH, and when it closes the
! windows that need this code, it updates all these variables automatically?
      CALL Upload_CrystalSystem
      CALL Upload_Cell_Constants()
      CALL Upload_Range()
      CALL Generate_TicMarks

      END SUBROUTINE EndWizard
!
!*****************************************************************************
!
      SUBROUTINE EndWizardPastPawley
!
! After Pawley refinement (i.e., at the moment we enter the window where a .sdi file can be loaded)
! several things in DASH change state. Therefore, if we leave one of 
! those windows, they must be shown/ungreyed again.
! Effectively, this subroutine is supposed to be the inverse of ShowWizardWindowZmatrices
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement
   
      CALL EndWizardCommon
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogPutRadioButton(IDF_PW_Option3)
      PastPawley = .FALSE.
! Ungrey 'Delete peak fit ranges' button on toolbar
      IF (NumPeakFitRange .GT. 0) CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOn)
! Ungrey 'Clear cell parameters' button on toolbar
      CALL WMenuSetState(ID_Delabc,ItemEnabled,WintOn)
! Ungrey 'Remove background' button on toolbar
      IF (.NOT. NoData) CALL WMenuSetState(ID_Remove_Background,ItemEnabled,WintOn)
! Ungrey 'Load diffraction pattern' button on toolbar
      CALL WMenuSetState(ID_import_xye_file,ItemEnabled,WintOn)
! Ungrey 'Load DASH Pawley file' button on toolbar
      CALL WMenuSetState(ID_import_dpj_file,ItemEnabled,WintOn)
! Make unit cell etc. under 'View' no longer read only 
      CALL Upload_Cell_Constants
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogFieldState(IDF_Space_Group_Menu,Enabled)
      CALL WDialogFieldState(IDF_Crystal_System_Menu,Enabled)
      CALL WDialogFieldState(IDF_ZeroPoint,Enabled)
      CALL WDialogFieldState(IDAPPLY,Enabled)
      CALL WDialogFieldState(IDB_Delabc,Enabled)
      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogFieldState(IDAPPLY,Enabled)
      IF (JRadOption .EQ. 1) CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
      CALL WDialogFieldState(IDF_wavelength1,Enabled)
      CALL WDialogFieldState(IDF_LabX_Source,Enabled)
      CALL WDialogFieldState(IDF_SynX_Source,Enabled)
      CALL WDialogFieldState(IDF_CWN_Source,Disabled)
      CALL WDialogFieldState(IDF_TOF_source,Disabled)
! @@ ?
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogFieldState(ID_Index_Output,DialogReadOnly)
      IPTYPE = 1
      CALL Profile_Plot

      END SUBROUTINE EndWizardPastPawley
!
!*****************************************************************************
!
      SUBROUTINE DealWithMainWizardWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER :: IPW_Option
      CHARACTER*MaxPathLength tString

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardCommon
            CASE (IDNEXT)
! We're off the main page and on to new pages depending on the option.
              CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
              SELECT CASE (IPW_Option)
                CASE (1) ! View data / determine peaks positions
                  CALL WDialogSelect(IDD_PW_Page3)
                  CALL WDialogGetString(IDF_PWa_DataFileName_String,tString)
! If no filename provided => grey out 'Next >' button
                  CALL WDialogFieldStateLogical(IDNEXT,LEN_TRIM(tString) .NE. 0)
                  CALL WizardWindowShow(IDD_PW_Page3)
                CASE (2) ! Preparation for Pawley refinement
                  CALL WDialogSelect(IDD_PW_Page2)
! If we have loaded a powder pattern, the Next > button should be enabled
                  CALL WDialogFieldStateLogical(IDNEXT,.NOT. NoData)
                  CALL WizardWindowShow(IDD_PW_Page2)
                CASE (3) ! Simulated annealing structure solution
                  CALL ShowWizardWindowZmatrices
                CASE (4) ! Analyse solutions
                  CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
                  CALL WDialogShow(-1,-1,0,Modeless)
              END SELECT
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithMainWizardWindow
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyDiffractionFileInput

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
   
      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), YBAK(MOBS),  EOBS(MOBS)
      
! Not too pretty, but safe
      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER I

      NOBS = BackupNOBS
      DO I = 1, NOBS
        XOBS(I) = BackupXOBS(I)
        YOBS(I) = BackupYOBS(I)
        EOBS(I) = BackupEOBS(I)
      ENDDO
! JvdS Assume no knowledge on background
      CALL Clear_BackGround
      CALL Rebin_Profile

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

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      CHARACTER(MaxPathLength) CTEMP
      INTEGER ISTAT
      INTEGER, EXTERNAL :: DiffractionFileBrowse, DiffractionFileOpen

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT)
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page4)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDB_Bin)
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page3a)
            CASE (ID_PWa_DF_Open)
              CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
              ISTAT = DiffractionFileOpen(CTEMP)
              CALL WDialogFieldStateLogical(IDNEXT,ISTAT .EQ. 1)
            CASE (IDBBROWSE)
              ISTAT = DiffractionFileBrowse()
! Don't change if the user pressed 'Cancel' (ISTAT = 2)
              IF      (ISTAT .EQ. 1) THEN
                CALL WDialogFieldState(IDNEXT,Enabled)
              ELSE IF (ISTAT .EQ. 0) THEN
                CALL WDialogFieldState(IDNEXT,Disabled)
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowRebin

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3a)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page3)
            CASE (IDNEXT)
! Get value from window
              IF (WDialogGetCheckBoxLogical(IDF_BinData)) THEN
                CALL WDialogGetInteger(IDF_LBIN,LBIN)
              ELSE
                LBIN = 1
              ENDIF
              CALL Rebin_Profile
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page4)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDAPPLY)
! Get value from window
              IF (WDialogGetCheckBoxLogical(IDF_BinData)) THEN
                CALL WDialogGetInteger(IDF_LBIN,LBIN)
              ELSE
                LBIN = 1
              ENDIF
              CALL Rebin_Profile
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_BinData)
! If set to 'TRUE', ungrey value field and vice versa
              CALL WDialogFieldStateLogical(IDF_LBIN,WDialogGetCheckBoxLogical(IDF_BinData))
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowRebin
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

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL Temp
      INTEGER IRadSelection
      REAL, EXTERNAL :: TwoTheta2dSpacing
      LOGICAL, EXTERNAL :: FnWavelengthOK

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page4)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page3)
            CASE (IDNEXT)
              IF (.NOT. FnWavelengthOK()) THEN
                CALL ErrorMessage('Invalid wavelength.')
              ELSE
                CALL WDialogGetReal(IDF_wavelength1,Temp)
                CALL Set_Wavelength(Temp)
                CALL WizardWindowShow(IDD_PW_Page5)
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_LabX_Source,IDF_SynX_Source,IDF_CWN_Source,IDF_TOF_source)
              CALL WDialogGetRadioButton(IDF_LabX_Source,JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
            CASE (IDF_wavelength1)
              CALL WDialogGetReal(IDF_wavelength1,Temp)
              CALL Set_Wavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
          END SELECT                
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

      REAL    tMin, tMax
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, FnPatternOK
      REAL, EXTERNAL :: TwoTheta2dSpacing

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      IF (WDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
        CALL WDialogGetReal(IDF_Min2Theta,tMin)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tMin = 0.0
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
        CALL WDialogGetReal(IDF_Max2Theta,tMax)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tMax = 90.0
      ENDIF
      CALL TruncateData(tMin,tMax)
! Now check if we have reasonable data left. If not, don't allow pressing 'Next >'
      IF (FnPatternOK()) THEN
! Update the values in the min/max fields to reflect what has actually happened
! (e.g. in case min was .GT. max)
        IF (WDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
          CALL WDialogPutReal(IDF_Min2Theta,tMin)
        ENDIF
        IF (WDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
          CALL WDialogPutReal(IDF_Max2Theta,tMax)
          CALL WDialogPutReal(IDF_MaxResolution,TwoTheta2dSpacing(tMax))
        ENDIF
      ENDIF
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

      INCLUDE 'PARAMS.INC'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      REAL tReal, tMin, tMax
      REAL, EXTERNAL :: TwoTheta2dSpacing, dSpacing2TwoTheta
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, FnPatternOK
      INTEGER tFieldState

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page4)
            CASE (IDNEXT)
              CALL WizardApplyProfileRange
! Check if we have reasonable data left. If not, don't allow pressing 'Next >'
              IF (FnPatternOK()) THEN
                CALL WizardWindowShow(IDD_PW_Page6)
              ELSE
                CALL ErrorMessage('Invalid profile range.')
                tMin = 0.0
                tMax = 90.0
                CALL TruncateData(tMin,tMax)
              ENDIF
              CALL Profile_Plot
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
! Check if we have reasonable data left. If not, don't allow the truncation
              IF (.NOT. FnPatternOK()) THEN
                CALL ErrorMessage('Invalid profile range.')
                tMin = 0.0
                tMax = 90.0
                CALL TruncateData(tMin,tMax)
              ENDIF
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_TruncateStartYN)
! If set to 'TRUE', ungrey value field and vice versa
              CALL WDialogFieldStateLogical(IDF_Min2Theta,WDialogGetCheckBoxLogical(IDF_TruncateStartYN))
            CASE (IDF_TruncateEndYN)
! If set to 'TRUE', ungrey value fields and vice versa
              IF (WDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
              ENDIF
              CALL WDialogFieldState(IDF_Max2Theta,tFieldState)
              CALL WDialogFieldState(IDF_MaxResolution,tFieldState)
              CALL WDialogFieldState(IDB_Convert,tFieldState)
            CASE (IDF_Min2Theta)
              CALL WDialogGetReal(IDF_Min2Theta,tMin)
              IF (tMin .LT. BackupXOBS(1)) tMin = BackupXOBS(1)
              CALL WDialogGetReal(IDF_Max2Theta,tMax)
              IF (tMin .GT. tMax) tMin = tMax
              CALL WDialogPutReal(IDF_Min2Theta,tMin)
            CASE (IDF_Max2Theta)
! When entering a maximum value for 2 theta, update maximum value for the resolution
              CALL WDialogGetReal(IDF_Max2Theta,tMax)
              IF (tMax .GT. BackupXOBS(BackupNOBS)) tMax = BackupXOBS(BackupNOBS)
              CALL WDialogGetReal(IDF_Min2Theta,tMin)
              IF (tMax .LT. tMin) tMax = tMin
              CALL WDialogPutReal(IDF_Max2Theta,tMax)
              CALL WDialogPutReal(IDF_MaxResolution,TwoTheta2dSpacing(tMax))
            CASE (IDF_MaxResolution)
! When entering a maximum value for the resolution, update maximum value for 2 theta
              CALL WDialogGetReal(IDF_MaxResolution,tReal)
              tMax = dSpacing2TwoTheta(tReal)
              IF (tMax .GT. BackupXOBS(BackupNOBS)) tMax = BackupXOBS(BackupNOBS)
              CALL WDialogGetReal(IDF_Min2Theta,tMin)
              IF (tMax .LT. tMin) tMax = tMin
              CALL WDialogPutReal(IDF_Max2Theta,tMax)
              CALL WDialogPutReal(IDF_MaxResolution,TwoTheta2dSpacing(tMax))
          END SELECT
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

      INCLUDE 'lattice.inc'

      INTEGER tInt1, tInt2
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page6)
      IF (WDialogGetCheckBoxLogical(IDF_SubtractBackground)) THEN
        CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
        CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
        CALL SubtractBackground(tInt1,tInt2,WDialogGetCheckBoxLogical(IDF_UseMCYN))
      ELSE
        CALL Clear_BackGround
        BACKREF = .TRUE.
      ENDIF
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

      INTEGER tInt1, tInt2, tFieldState
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page6)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardApplyProfileRange
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page5)
            CASE (IDNEXT)
              CALL WizardApplyProfileRange
              CALL WizardApplyBackground
              CALL Profile_Plot
              CALL CheckIfWeCanIndex
              CALL WizardWindowShow(IDD_PW_Page7)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDF_Preview)
              CALL WizardApplyProfileRange
              IF (WDialogGetCheckBoxLogical(IDF_SubtractBackground)) THEN
                CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
                CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
                CALL CalculateBackground(tInt1,tInt2,WDialogGetCheckBoxLogical(IDF_UseMCYN))
              ELSE
                CALL Clear_BackGround
              ENDIF
! Force display of background
              CALL WDialogSelect(IDD_Plot_Option_Dialog)
              CALL WDialogPutCheckBoxLogical(IDF_background_check,.TRUE.)
              CALL Profile_Plot
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
              CALL WizardApplyBackground
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_SubtractBackground)
! If set to 'TRUE', ungrey value field and vice versa
              IF (WDialogGetCheckBoxLogical(IDF_SubtractBackground)) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
                CALL WizardApplyProfileRange
                CALL Clear_BackGround
                CALL Profile_Plot
              ENDIF
              CALL WDialogFieldState(IDF_LABEL7,tFieldState)
              CALL WDialogFieldState(IDF_NumOfIterations,tFieldState)
              CALL WDialogFieldState(IDF_LABEL8,tFieldState)
              CALL WDialogFieldState(IDF_WindowWidth,tFieldState)
              CALL WDialogFieldState(IDF_UseMCYN,tFieldState)
              CALL WDialogFieldState(IDF_Preview,tFieldState)
              CALL WDialogFieldState(IDAPPLY,tFieldState)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowBackground
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowIndexing1

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      INTEGER IndexOption
      LOGICAL, EXTERNAL :: FnUnitCellOK

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page7)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page6)
            CASE (IDNEXT)
              CALL WDialogGetRadioButton(IDF_RADIO3,IndexOption) ! 'Index now' or 'Enter known cell'
              SELECT CASE (IndexOption)
                CASE (1) ! Index pattern now
                  CALL WDialogSelect(IDD_PW_Page8)
! If this is synchrotron data, then set the default error in the peak positions to 0.02 rather than 0.03.
! This decreases the number of solutions and increases the speed of the search.
                  IF (JRadOption .EQ. 2) THEN
                    CALL WDialogPutReal(IDF_eps,0.02,'(F5.3)')
                  ELSE
                    CALL WDialogPutReal(IDF_eps,0.03,'(F5.3)')
                  ENDIF
                  CALL WizardWindowShow(IDD_PW_Page8)
                CASE (2) ! Enter known unit cell parameters
                  CALL WDialogSelect(IDD_PW_Page1)
! If the cell is OK, the Next> button should be enabled
                  CALL WDialogFieldStateLogical(IDNEXT,FnUnitCellOK())
                  CALL WizardWindowShow(IDD_PW_Page1)
              END SELECT
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
          END SELECT
        CASE (FieldChanged)
          CALL CheckIfWeCanIndex
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowIndexing1
!
!*****************************************************************************
!
      SUBROUTINE DealWithDICVOLRunning

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_DICVOLRunning)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBSTOP, IDCANCEL)
              DICVOL_Error = cDICVOL_ErrorInterrupted
            CASE (IDBPause)
              CALL WMessageBox(OKOnly,ExclamationIcon,CommonOK,'Press OK to continue','Pause')
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDICVOLRunning
!
!*****************************************************************************
!
      SUBROUTINE DICVOL_FinishedCrystalSystem(TheCrystalSystem)
!
! Called when DICVOL has finished searching a crystal system. Updates the
! "DICVOL Running..." dialogue.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem 

      CHARACTER*20 tString
      CHARACTER*20, EXTERNAL :: Integer2String

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_DICVOLRunning)
      IF (IAND(TheCrystalSystem,cCubic       ) .EQ. cCubic       ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(1))
        CALL WDialogPutString(IDF_LabProgCubic,tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem,cTetragonal  ) .EQ. cTetragonal  ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(2))
        CALL WDialogPutString(IDF_LabProgTetragonal,tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem,cHexagonal   ) .EQ. cHexagonal   ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(3))
        CALL WDialogPutString(IDF_LabProgHexagonal,tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem,cOrthorhombic) .EQ. cOrthorhombic) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(4))
        CALL WDialogPutString(IDF_LabProgOrthorhombic,tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem,cMonoclinic  ) .EQ. cMonoclinic  ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(5))
        CALL WDialogPutString(IDF_LabProgMonoclinic,tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem,cTriclinic   ) .EQ. cTriclinic   ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(6))
        CALL WDialogPutString(IDF_LabProgTriclinic,tString(1:LEN_TRIM(tString)))
      ENDIF
      CALL PeekEvent
      CALL PopActiveWindowID

      END SUBROUTINE DICVOL_FinishedCrystalSystem
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowIndexing2

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      REAL Rvpar(2), Rdens, Rmolwt, Rexpzp
      LOGICAL system(6)
      INTEGER I, Iord
      INTEGER IHANDLE
      REAL    Epsilon
      REAL    MaxLen
      LOGICAL, EXTERNAL :: FnUnitCellOK, Confirm, WDialogGetCheckBoxLogical
      REAL,    EXTERNAL :: TwoTheta2dSpacing
      REAL    MaxSinBeta
      REAL    tBeta
      INTEGER NumDoF, ilen
      CHARACTER*2 nStr

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page8)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDBACK)
              CALL CheckIfWeCanIndex
              CALL WizardWindowShow(IDD_PW_Page7)
            CASE (IDNEXT)
!              CALL EstimateZeroPointError
              CALL WDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
              CALL WDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
              CALL WDialogGetReal(IDF_Indexing_Maxa, amax)
              Bmax = amax
              Cmax = amax
              CALL WDialogGetReal       (IDF_Indexing_MinAng,      Bemin)
              CALL WDialogGetReal       (IDF_Indexing_MaxAng,      Bemax)
              CALL WDialogGetReal       (IDF_Indexing_Density,     Rdens)
              CALL WDialogGetReal       (IDF_Indexing_MolWt,       Rmolwt)
              CALL WDialogGetReal       (IDF_ZeroPoint,            Rexpzp)
              system(1) = WDialogGetCheckBoxLogical(IDF_Indexing_Cubic)
              system(2) = WDialogGetCheckBoxLogical(IDF_Indexing_Tetra)
              system(3) = WDialogGetCheckBoxLogical(IDF_Indexing_Hexa)
              system(4) = WDialogGetCheckBoxLogical(IDF_Indexing_Ortho)
              system(5) = WDialogGetCheckBoxLogical(IDF_Indexing_Monoclinic)
              system(6) = WDialogGetCheckBoxLogical(IDF_Indexing_Triclinic)
              CALL WDialogGetReal       (IDF_eps,                  Epsilon)
              CALL WDialogGetReal       (IDF_Indexing_Fom,         fom)
              CALL WDialogGetReal       (IDF_Indexing_ScaleFactor, DV_ScaleFactor)
! Number of degrees of freedom, we don't even count the zero point
              NumDoF = 0
              IF (system(1)) NumDof = MAX(NumDoF,1)
              IF (system(2)) NumDof = MAX(NumDoF,2)
              IF (system(3)) NumDof = MAX(NumDoF,2)
              IF (system(4)) NumDof = MAX(NumDoF,3)
              IF (system(5)) NumDof = MAX(NumDoF,4)
              IF (system(6)) NumDof = MAX(NumDoF,6)
! Check if any crystal system checked at all
              IF (NumDoF .EQ. 0) THEN
                CALL ErrorMessage('Please check at least one crystal system.')
                GOTO 999
              ENDIF
! Check if the number of observed lines is consistent with the crystal systems
              IF (NTPeak .LT. NumDoF) THEN
                CALL ErrorMessage('The number of observed lines is less than the number of degrees of freedom.')
                GOTO 999
              ENDIF
! Warn the user if we have less observed lines than twice the number of degrees of freedom including the zero point
              IF ((2*(NumDoF+1)) .GT. NTPeak) THEN
                IF (.NOT. Confirm('The number of observed lines is less than twice the number of degrees of freedom,'//CHAR(13)//&
                                  'do you wish to continue anyway?')) THEN
                  GOTO 999
                ENDIF
              ENDIF
              IF (DV_ScaleFactor .LT. 0.1) DV_ScaleFactor = 0.1
              IF (DV_ScaleFactor .GT. 5.0) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
              IF (system(5)) THEN
                IF (Bemin .GT. Bemax) THEN
                  tBeta = Bemin
                  Bemin = Bemax
                  Bemax = tBeta
                ENDIF
                IF ((Bemin .LT. 45.0) .OR. (Bemax .GT. 150.0)) THEN
                  CALL ErrorMessage('The range of the angle beta does not make sense.')
                  GOTO 999
                ELSE
! Correct maximum cell length for the angle beta
 ! If 90.0 is inside the range, then that's the maximum
                  IF ((Bemin .LT. 90.0) .AND. (Bemax .GT. 90.0)) THEN
                    MaxSinBeta = 1.0 ! Beta = 90.0
                  ELSE         
                    MaxSinBeta = MAX(SIN(Bemin),SIN(Bemax))
                  ENDIF
                ENDIF
              ELSE
                MaxSinBeta = 1.0 ! Beta = 90.0
              ENDIF
! Add in very quick check: is the d-spacing belonging to the first peak greater
! than the maximum cell length requested? If so, tell the user he/she is a moron.
              MaxLen = MaxSinBeta*amax
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
              IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
                CALL ErrorMessage('The maximum cell axis length is shorter than required for indexing the first peak.')
                GOTO 999
              ENDIF
              n = NTPeak
              wave2 = (ALambda / 2) * DV_ScaleFactor
              epst = (Epsilon + 0.015) * DV_ScaleFactor
              DO I = 1, n
                epsil(I) = Epsilon * DV_ScaleFactor
              ENDDO
              DO I = 1, NTPeak
                IOrd = IOrdTem(I)
                d(I) = AllPkPosVal(IOrd) - Rexpzp
              ENDDO
              CALL WCursorShape(CurHourGlass)
              NumOfDICVOLSolutions = 0
              CALL WDialogSelect(IDD_DICVOLRunning)
              IF (system(1)) THEN
                CALL WDialogFieldState(IDF_LabCubic            ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgCubic        ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabCubic            ,Disabled)
                CALL WDialogFieldState(IDF_LabProgCubic        ,Disabled)
              ENDIF
              IF (system(2)) THEN
                CALL WDialogFieldState(IDF_LabTetragonal       ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgTetragonal   ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabTetragonal       ,Disabled)
                CALL WDialogFieldState(IDF_LabProgTetragonal   ,Disabled)
              ENDIF
              IF (system(3)) THEN
                CALL WDialogFieldState(IDF_LabHexagonal        ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgHexagonal    ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabHexagonal        ,Disabled)
                CALL WDialogFieldState(IDF_LabProgHexagonal    ,Disabled)
              ENDIF
              IF (system(4)) THEN
                CALL WDialogFieldState(IDF_LabOrthorhombic     ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgOrthorhombic ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabOrthorhombic     ,Disabled)
                CALL WDialogFieldState(IDF_LabProgOrthorhombic ,Disabled)
              ENDIF
              IF (system(5)) THEN
                CALL WDialogFieldState(IDF_LabMonoclinic       ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgMonoclinic   ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabMonoclinic       ,Disabled)
                CALL WDialogFieldState(IDF_LabProgMonoclinic   ,Disabled)
              ENDIF
              IF (system(6)) THEN
                CALL WDialogFieldState(IDF_LabTriclinic        ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgTriclinic    ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabTriclinic        ,Disabled)
                CALL WDialogFieldState(IDF_LabProgTriclinic    ,Disabled)
              ENDIF
              CALL WDialogPutString(IDF_LabProgCubic,' ')
              CALL WDialogPutString(IDF_LabProgTetragonal,' ')
              CALL WDialogPutString(IDF_LabProgHexagonal,' ')
              CALL WDialogPutString(IDF_LabProgOrthorhombic,' ')
              CALL WDialogPutString(IDF_LabProgMonoclinic,' ')
              CALL WDialogPutString(IDF_LabProgTriclinic,' ')
              CALL WDialogShow(-1,-1,0,Modeless)
              CALL WCursorShape(CurHourGlass)
              CALL DICVOL91(system(1),system(2),system(3),system(4),system(5),system(6),Rvpar(1),Rvpar(2),Rmolwt,Rdens,Rdens/50.0)
              CALL WDialogSelect(IDD_DICVOLRunning)
              CALL WDialogHide
              CALL WCursorShape(CurCrossHair)
! Pop up a window showing the DICVOL output file in a text editor
              CALL WindowOpenChild(IHANDLE)
              CALL WEditFile('DICVOL.OUT',Modeless,0,FileMustExist+ViewOnly+NoToolBar,4)
! If 'ViewOnly' is specified:
! 1. The file can be accessed while it is displayed.
! 2. There is no 'Save as...' option in the menu.
! If the output file is viewed without 'ViewOnly', the file cannot be accessed, which means that
! DICVOL returns with an error message which means that there are no solutions.
! Hence, this way, DICVOL can be run several times in succession and the results can be compared
! on screen. To save one of the output files (that all have the same name),
! the user must select all text and copy and paste it to an other editor window.
              CALL SetChildWinAutoClose(IHANDLE)
              IF (NumOfDICVOLSolutions .EQ. 0) THEN
                CALL ErrorMessage('No solutions were found.')
                GOTO 999
              ENDIF  
              IF (DICVOL_Error .EQ. cDICVOL_TooManySolutions) CALL WarningMessage('More than 30 solutions found, please check your data.')
! If only a single solution, and no valid cell available, import that solution by default
              IF ((NumOfDICVOLSolutions.EQ.1) .AND. (.NOT. FnUnitCellOK())) THEN
! Import the unit cell parameters into DASH
                CellPar(1) = DICVOLSolutions(1)%a
                CellPar(2) = DICVOLSolutions(1)%b
                CellPar(3) = DICVOLSolutions(1)%c
                CellPar(4) = DICVOLSolutions(1)%alpha
                CellPar(5) = DICVOLSolutions(1)%beta
                CellPar(6) = DICVOLSolutions(1)%gamma
                LatBrav = DICVOLSolutions(1)%CrystalSystem
                CALL Upload_CrystalSystem
                CALL UpdateCell()
              ENDIF
! Pop up the next Wizard window showing the solutions, so that the user can choose one to be imported into DASH
              CALL WDialogSelect(IDD_PW_Page9)
! Clear all fields in the grid
              CALL WDialogClearField(IDF_DV_Summary_0)
              WRITE(nStr,'(I2)') n
              CALL StrClean(nStr,ilen)
              CALL WGridLabelColumn(IDF_DV_Summary_0,10,'M('//nStr(1:ilen)//')')
              CALL WGridLabelColumn(IDF_DV_Summary_0,11,'F('//nStr(1:ilen)//')')
! Set the number of rows in the grid to the number of solutions.
              CALL WGridRows(IDF_DV_Summary_0,NumOfDICVOLSolutions)
              DO I = 1, NumOfDICVOLSolutions
                CALL WGridPutCellString(IDF_DV_Summary_0, 2,I,CrystalSystemString(DICVOLSolutions(I)%CrystalSystem))
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 3,I,DICVOLSolutions(I)%a,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 4,I,DICVOLSolutions(I)%b,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 5,I,DICVOLSolutions(I)%c,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 6,I,DICVOLSolutions(I)%alpha,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 7,I,DICVOLSolutions(I)%beta,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 8,I,DICVOLSolutions(I)%gamma,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 9,I,DICVOLSolutions(I)%Volume,'(F9.2)')
                IF (DICVOLSolutions(I)%M .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,10,I,DICVOLSolutions(I)%M,'(F7.1)')
                IF (DICVOLSolutions(I)%F .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,11,I,DICVOLSolutions(I)%F,'(F7.1)')
              ENDDO
              CALL WizardWindowShow(IDD_PW_Page9)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_ZeroPoint)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
          END SELECT
      END SELECT
  999 CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowIndexing2
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDICVOLResults

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'lattice.inc'

      INTEGER irow, istatus

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page9)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page8)
            CASE (IDNEXT)
              CALL WizardWindowShow(IDD_PW_Page1)
          END SELECT
          CALL PopActiveWindowID
          RETURN
      END SELECT
      DO irow = 1, NumOfDICVOLSolutions
        CALL WGridGetCellCheckBox(IDF_DV_Summary_0,1,irow,istatus)
        IF (istatus .EQ. 1) THEN
! Import the unit cell parameters into DASH
          CellPar(1) = DICVOLSolutions(irow)%a
          CellPar(2) = DICVOLSolutions(irow)%b
          CellPar(3) = DICVOLSolutions(irow)%c
          CellPar(4) = DICVOLSolutions(irow)%alpha
          CellPar(5) = DICVOLSolutions(irow)%beta
          CellPar(6) = DICVOLSolutions(irow)%gamma
          LatBrav = DICVOLSolutions(irow)%CrystalSystem
          CALL WGridPutCellCheckBox(IDF_DV_Summary_0,1,irow,0)
          CALL Upload_CrystalSystem
          CALL UpdateCell()
        ENDIF
      ENDDO               
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDICVOLResults
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionSetup2
!
! The above 9 windows have taken me months to program.
! This window bypasses all of them, because Elna wants that.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

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
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT)
              CALL WizardWindowShow(IDD_PW_Page1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (ID_PW_DF_Open)
              CALL WDialogGetString(IDF_PW_DataFileName_String,CTEMP)
              ISTAT = DiffractionFileOpen(CTEMP)
              CALL WDialogFieldStateLogical(IDNEXT,ISTAT .EQ. 1)
            CASE (IDBBROWSE)
              ISTAT = DiffractionFileBrowse()
! Don't change if the user pressed 'Cancel' (ISTAT = 2)
              IF      (ISTAT .EQ. 1) THEN
                CALL WDialogFieldState(IDNEXT,Enabled)
              ELSE IF (ISTAT .EQ. 0) THEN
                CALL WDialogFieldState(IDNEXT,Disabled)
              ENDIF
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_LabX_Source,IDF_SynX_Source,IDF_CWN_Source,IDF_TOF_source)
              CALL WDialogGetRadioButton(IDF_LabX_Source,JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
            CASE (IDF_wavelength1)
              CALL WDialogGetReal(IDF_wavelength1,Temp)
              CALL Set_Wavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionSetup2
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowUnitCellParameters

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER IOption
      LOGICAL, EXTERNAL :: Confirm


      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page1)
      SpaceGroupDetermination = .FALSE. 
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
            CASE (IDBACK)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
! There were three ways to get here:
!   1. choose option 2 in main Wizard window
!   2. choose 'Enter known cell' in wizard window Indexing I
!   3. after DICVOL, after choosing 'Index now' in wizard window Indexing I
! Did we get here from the main wizard window?
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogGetRadioButton(IDF_PW_Option1,IOption)
              IF (IOption .EQ. 2) THEN
                CALL WizardWindowShow(IDD_PW_Page2)
              ELSE
! Did we get here from 'Enter known cell' in wizard window Indexing I?
                CALL WDialogSelect(IDD_PW_Page7)
                CALL WDialogGetRadioButton(IDF_RADIO3,IOption) ! 'Index now' or 'Enter known cell'
                IF (IOption .EQ. 1) THEN
                  CALL WizardWindowShow(IDD_PW_Page9)
                ELSE
                  CALL WizardWindowShow(IDD_PW_Page7)
                ENDIF
              ENDIF
            CASE (IDNEXT)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
              CALL CheckUnitCellConsistency
              IF (NumberSGTable .EQ. LPosSG(LatBrav)) CALL WarningMessage('Space-group symmetry has not been reset.')
              CALL WizardWindowShow(IDD_PW_Page10)
            CASE (IDAPPLY)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
              CALL CheckUnitCellConsistency
            CASE (IDB_Delabc)
              CALL Clear_UnitCell_WithConfirmation
! Space Group Program Interface
            CASE (IDF_SGDet) 
               NumberSGTable = LPosSG(LatBrav) ! Most general space group of crystal system chosen
               CALL SetSpaceGroupMenu 
               CALL Generate_TicMarks
               CALL Download_SpaceGroup(IDD_PW_Page1) 
               SpaceGroupDetermination = .TRUE.
               CALL WizardWindowShow(IDD_PW_Page10)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,CellPar(1))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,CellPar(2))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,CellPar(3))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_Crystal_System_Menu)
              CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
              CALL Upload_CrystalSystem
              CALL Generate_TicMarks
            CASE (IDF_Space_Group_Menu)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Generate_TicMarks
            CASE (IDF_ZeroPoint)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Generate_TicMarks
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowUnitCellParameters
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowPawley1

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page10)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page1)
            CASE (IDNEXT)
              CALL ShowPawleyFitWindow
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDF_ClearPeakFitRanges)
              IF (Confirm('Do you wish to delete all peak fit ranges?')) CALL Clear_PeakFitRanges
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowPawley1
!
!*****************************************************************************
!
