!
!*****************************************************************************
!
      SUBROUTINE StartWizard

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'DialogPosCmn.inc'

      CALL SetWizardState(-1)
      CALL ToggleMenus(0)
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)

      END SUBROUTINE StartWizard
!
!*****************************************************************************
!
      SUBROUTINE EndWizard

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'DialogPosCmn.inc'

      IXPos_IDD_Wizard = WInfoDialog(6)
      IYPos_IDD_Wizard = WInfoDialog(7)
      CALL WDialogHide()
      CALL ToggleMenus(1)
      IF (.NOT. NoData) CALL SetModeMenuState(1,0,0)
      IF (.NOT. NoData) CALL Upload_Wizard_Information()
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

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
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
      RETURN

      END SUBROUTINE DealWithMainWizardWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionFileInput

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      CHARACTER(LEN=MaxPathLength) :: CTEMP

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page3)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyProfileRange
! Subtract the background
              CALL WizardApplyBackground
              CALL EndWizard
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
              CALL Diffraction_File_Open(CTEMP)
!@ Should test for success here
              CALL WDialogFieldState(IDNEXT,Enabled)
            CASE (ID_PWa_DF_Browse)
              CALL Diffraction_File_Browse
!@ Should test for success here
              CALL WDialogFieldState(IDNEXT,Enabled)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithWizardWindowDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionSetup

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
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDFINISH)
              CALL WizardApplyProfileRange
! Subtract the background
              CALL WizardApplyBackground
              CALL EndWizard
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
              CALL SetSourceDataState(JRadOption)
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
      RETURN

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
              CALL WizardApplyProfileRange
! Subtract the background
              CALL WizardApplyBackground
              CALL EndWizard
            CASE (IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page4)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDNEXT)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_PW_Page6)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
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
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID
      RETURN

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
              CALL WizardApplyProfileRange
! Subtract the background
              CALL WizardApplyBackground
              CALL EndWizard
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
            CASE (IDAPPLY)
              CALL WizardApplyBackground
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionFileInput 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithWizardWindowDiffractionFileInput')
      END SELECT
      CALL PopActiveWindowID
      RETURN

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
              CALL Update_Space_Group(IDD_PW_Page1)
              NumPawleyRef = 0
            CASE (IDF_Crystal_System_Menu)
              CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
              CALL UserSetCrystalSystem(LatBrav)
              CALL SetSpaceGroupMenu
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
      RETURN

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
      CHARACTER(LEN=MaxPathLength) :: CTEMP
      REAL    Temp

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
              CALL Diffraction_File_Open(CTEMP)
            CASE (ID_PW_DF_Browse)
              CALL Diffraction_File_Browse
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithWizardWindowDiffractionSetup 1')
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_PW_LabX_Source,IDF_PW_SynX_Source,IDF_PW_CWN_Source,IDF_PW_TOF_source)
              CALL WDialogGetRadioButton(IDF_PW_LabX_Source,JRadOption)
              CALL SetSourceDataState(JRadOption)
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
      RETURN

      END SUBROUTINE DealWithWizardWindowDiffractionSetup2
!
!*****************************************************************************
!
      SUBROUTINE Upload_Wizard_Information()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      CALL PushActiveWindowID
      CALL SetCrystalSystem(LatBrav)
      CALL SetSpaceGroupMenu
      CALL Upload_Cell_Constants()
      CALL Upload_Range()
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Wizard_Information      
!
!*****************************************************************************
!
! Subroutine to set the state of the global variable JRadOption to either synchrotron or lab data.
! This is updated in the main dialogue window and in the wizard.

      SUBROUTINE SetSourceDataState(tIRadOption)

      USE WINTERACTER
      USE DRUID_HEADER 

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: tIRadOption
      
      INCLUDE 'GLBVAR.INC' ! Contains JRadOption

      JRadOption = tIRadOption
      CALL PushActiveWindowID
      SELECT CASE (JRadOption)
        CASE (1) ! Lab X-ray
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_LabX_Source)
        CASE (2,3) ! Synchrotron X-ray & CW neutron  
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
          END IF
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
          END IF
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_CWN_Source)
          END IF
        CASE (4) ! TOF neutron
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Disabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_PW_TOF_source)
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Disabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_PW_TOF_source)
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Disabled)
          CALL WDialogFieldState(IDF_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_flight_path,Enabled)
          CALL WDialogFieldState(IDF_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_TOF_source)
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE SetSourceDataState
!
!*****************************************************************************
!
