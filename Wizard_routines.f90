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
!U      SUBROUTINE PolyFitter_Wizard
!U!   Open the first wizard child window
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U      USE VARIABLES
!U
!U      IMPLICIT NONE                           
!U!
!U!   Variable declarations
!U!
!U!O      LOGICAL :: NODATA
!U      LOGICAL :: SKIP = .FALSE.
!U      INTEGER :: IPW_Option
!U
!U      INCLUDE 'statlog.inc'
!U      INCLUDE 'DialogPosCmn.inc'
!U      INCLUDE 'Lattice.inc'
!U
!U      INTEGER :: IRadSelection
!U      REAL Temp
!U      CHARACTER(LEN=256) :: CTEMP
!U
!U      INCLUDE 'GLBVAR.INC' ! Contains JRadOption which replaces SourceState and IRadOption
!U      INTEGER IDummy
!U
!U      LOGICAL FnUnitCellOK ! Function
!U
!U      SKIP = .FALSE.
!U!.. Set up some defaults
!U 8    CONTINUE
!U 1    CONTINUE
!U      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
!U      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
!U
!U! JvdS Wizard is now dealt with per window
!U      RETURN
!U
!U
!U! Now check on which button was pressed ...
!U      DO WHILE (.NOT. SKIP)
!U        IXPos_IDD_Wizard = WInfoDialog(6)
!U        IYPos_IDD_Wizard = WInfoDialog(7)
!U        CALL GetEvent
!U        SELECT CASE (EventInfo%WIN)
!U          CASE (0)
!U! Process main window messages
!U            CALL process_mainwindow_message
!U            CYCLE
!U        END SELECT
!U        SELECT CASE (EventType)
!U          CASE (PushButton)
!U            SELECT CASE (EventInfo%VALUE1)
!U              CASE (IDNEXT)
!U                IXPos_IDD_Wizard = WInfoDialog(6)
!U                IYPos_IDD_Wizard = WInfoDialog(7)
!U                CALL WDialogHide()
!U! We're off the main page and on to new pages depending on the option.
!U                CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
!U                SELECT CASE (IPW_Option)
!U                  CASE(1) ! view data/ determine peak positions
!U                    GOTO 300
!U                  CASE(2) ! Pawley
!U                    GOTO 100
!U                  CASE(3) ! Simulated Annealing
!U                    GOTO 500
!U                  CASE(4) ! Read Druid pawley file
!U                    GOTO 600
!U                END SELECT
!U              CASE (IDF_PW0_Skip,IDCANCEL)
!U                SKIP = .TRUE.
!U                CALL WDialogHide()
!U                CALL ToggleMenus(1)
!U                RETURN
!U            END SELECT
!U        END SELECT
!U      END DO
!U      CALL ToggleMenus(1)
!U      RETURN
!U!.. Deal here with page 1 (cases 2 and 3)
!U 100  SKIP = .FALSE.
!U      CALL WDialogSelect(IDD_PW_Page1)
!U! If the cell is OK, the Next> button should be enabled
!U      IF (FnUnitCellOK()) THEN
!U! Enable the wizard next button
!U        CALL WDialogFieldState(IDNEXT,Enabled)
!U      ELSE
!U! Disable the wizard next button
!U        CALL WDialogFieldState(IDNEXT,Disabled)
!U      END IF
!U      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
!U      DO WHILE (.NOT. SKIP)
!U        IXPos_IDD_Wizard = WInfoDialog(6)
!U        IYPos_IDD_Wizard = WInfoDialog(7)
!U        CALL GetEvent
!U        SELECT CASE (EventInfo%WIN)
!U          CASE (0)
!U! Process main window messages
!U            CALL process_mainwindow_message
!U            CYCLE
!U        END SELECT
!U        SELECT CASE (EventType)
!U          CASE (PushButton)
!U            SELECT CASE (EventInfo%VALUE1)
!U              CASE(IDBACK)
!U                CALL WDialogHide()
!U                GOTO 1
!U              CASE(IDNEXT)
!U! We're off the second page and on to the last page
!U                CALL WDialogHide()
!U!C>>                          CALL WDialogSelect(IDD_PW_Page1)
!U                GOTO 200
!U              CASE(IDCLOSE,IDCANCEL)
!U                SKIP = .TRUE.
!U                CALL WDialogSelect(IDD_PW_Page1)
!U                CALL WDialogHide()
!U                CALL ToggleMenus(1)
!U                RETURN
!U            END SELECT
!U          CASE (FieldChanged)
!U            SELECT CASE (EventInfo%VALUE1)
!U              CASE (IDF_Space_Group_Menu)
!U                CALL Update_Space_Group(IDD_PW_Page1, IDummy, IDummy)
!U                NumPawleyRef = 0
!U              CASE (IDF_Crystal_System_Menu)
!U                CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
!U                CALL SetCrystalSystem(LatBrav)
!U                CALL SetSpaceGroupMenu(LatBrav)
!U                CALL Generate_TicMarks
!U              CASE (IDF_a_latt)
!U                CALL WDialogGetReal(IDF_a_latt,CellPar(1))
!U                CALL UpdateCell(IDD_PW_Page1)
!U              CASE (IDF_b_latt)
!U                CALL WDialogGetReal(IDF_b_latt,CellPar(2))
!U                CALL UpdateCell(IDD_PW_Page1)
!U              CASE (IDF_c_latt)
!U                CALL WDialogGetReal(IDF_c_latt,CellPar(3))
!U                CALL UpdateCell(IDD_PW_Page1)
!U              CASE (IDF_alp_latt)
!U                CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
!U                CALL UpdateCell(IDD_PW_Page1)
!U              CASE (IDF_bet_latt)
!U                CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
!U                CALL UpdateCell(IDD_PW_Page1)
!U              CASE (IDF_gam_latt)
!U                CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
!U                CALL UpdateCell(IDD_PW_Page1)
!U            END SELECT                
!U        END SELECT
!U      END DO
!U!.. Deal here with page 2 of Pawley, i.e. Basic Diffraction Information
!U! Now called: Diffraction Setup
!U 200 SKIP = .FALSE.
!U      CALL WDialogSelect(IDD_PW_Page2)
!U! JvdS @ following line should not be necessary
!U      CALL SetSourceDataState(JRadOption)
!U      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
!U      DO WHILE(.NOT. SKIP)
!U        IXPos_IDD_Wizard = WInfoDialog(6)
!U        IYPos_IDD_Wizard = WInfoDialog(7)
!U        CALL GetEvent
!U        SELECT CASE (EventInfo%WIN)
!U          CASE (0)
!U! Process main window messages
!U            CALL process_mainwindow_message
!U            CYCLE
!U        END SELECT
!U! JvdS @ Next line is in a weird place again
!U        CALL WDialogGetRadioButton(IDF_PW_LabX_Source,JRadOption)
!U        CALL SetSourceDataState(JRadOption)
!U        SELECT CASE (EventType)
!U          CASE(FieldChanged)
!U            SELECT CASE (EventInfo%VALUE1)
!U              CASE (IDF_PW_wavelength1)
!U                CALL WDialogGetReal(IDF_PW_wavelength1,Temp)
!U                CALL UpdateWavelength(Temp)
!U!C>> JCC Add in function  
!U              CASE (IDF_Wavelength_Menu)
!U                CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
!U                CALL SetWavelengthToSelection(IRadSelection)
!U            END SELECT
!U          CASE (PushButton)
!U            SELECT CASE (EventInfo%VALUE1)
!U              CASE (ID_PW_DF_Open)
!U!C>> JCC Add callback for Open button
!U                CALL WDialogGetString(IDF_PW_DataFileName_String,CTEMP)
!U                CALL Diffraction_File_Open(CTEMP)
!U!C>> JCC Have to reselect after the file open
!U                CALL WDialogSelect(IDD_PW_Page2)
!U              CASE (ID_PW_DF_Browse)
!U                CALL Diffraction_File_Browse
!U!C>> JCC Have to reselect after the file open
!U                CALL WDialogSelect(IDD_PW_Page2)
!U              CASE (IDBACK)
!U                CALL WDialogHide()
!U                GOTO 100
!U              CASE (IDFINISH)
!U                SKIP = .TRUE.
!U                CALL WDialogHide()
!U                CALL ToggleMenus(1)
!U                RETURN
!U              CASE (IDCANCEL)
!U                SKIP = .TRUE.
!U                CALL WDialogHide()
!U                CALL ToggleMenus(1)
!U                RETURN
!U            END SELECT
!U        END SELECT
!U      END DO
!U!   view data/ determine peak positions
!U!.. Deal here with page 3 - only visited for options 1 & 2
!U 300  SKIP = .FALSE.
!U      CALL WDialogSelect(IDD_PW_Page3)
!U      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
!U      DO WHILE(.NOT. SKIP)
!U        IXPos_IDD_Wizard = WInfoDialog(6)
!U        IYPos_IDD_Wizard = WInfoDialog(7)
!U        CALL GetEvent
!U        SELECT CASE (EventInfo%WIN)
!U          CASE (0)
!U! Process main window messages
!U            CALL process_mainwindow_message
!U            CYCLE
!U        END SELECT
!U        SELECT CASE (EventType)
!U          CASE (PushButton)
!U            SELECT CASE (EventInfo%VALUE1)
!U!C>> JCC Add callback for Open button
!U              CASE(ID_PWa_DF_Open)
!U                CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
!U                CALL Diffraction_File_Open(CTEMP)
!U!C>> JCC Need to reselect dialogue after the action
!U                CALL WDialogSelect(IDD_PW_Page3)
!U              CASE(ID_PWa_DF_Browse)
!U                CALL Diffraction_File_Browse
!U!C>> JCC Need to reselect dialogue after the action
!U                CALL WDialogSelect(IDD_PW_Page3)
!U              CASE(IDBACK)
!U                CALL WDialogHide()
!U                GOTO 1
!U!C>>                  CASE(IDF_PW3_Skip)
!U!C>>                    SKIP = .TRUE.
!U                CALL WDialogHide()
!U! JvdS It's kind of dirty that 'Cancel' and 'Finish' amount to the same thing.
!U              CASE(IDFINISH,IDCANCEL)
!U                SKIP = .TRUE.
!U                CALL WDialogHide()
!U                CALL ToggleMenus(1)
!U                RETURN
!U            END SELECT
!U        END SELECT
!U      END DO
!U!.. Deal with the simulated annealing ...
!U 500  SKIP = .FALSE.
!U      IXPos_IDD_SA_Input = IXPos_IDD_Wizard
!U      IYPos_IDD_SA_Input = IYPos_IDD_Wizard
!U      CALL SA_MAIN()
!U      GOTO 8
!U 600  IXPos_IDD_SA_Input = WInfoDialog(6)
!U      IYPos_IDD_SA_Input = WInfoDialog(7)
!U      CALL SA_MAIN()
!U      CALL ToggleMenus(1)
!U      RETURN
!U
!U      END SUBROUTINE PolyFitter_Wizard
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
            CASE (IDFINISH, IDCANCEL)
              CALL EndWizard
            CASE (IDBACK)
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE(ID_PWa_DF_Open)
              CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
              CALL Diffraction_File_Open(CTEMP)
            CASE(ID_PWa_DF_Browse)
              CALL Diffraction_File_Browse
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
      SUBROUTINE DealWithWizardWindowUnitCellParameters

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER :: IDummy

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
              CALL SetCrystalSystem(LatBrav)
              CALL SetSpaceGroupMenu
              CALL Generate_TicMarks
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,CellPar(1))
              CALL UpdateCell(IDD_PW_Page1)
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,CellPar(2))
              CALL UpdateCell(IDD_PW_Page1)
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,CellPar(3))
              CALL UpdateCell(IDD_PW_Page1)
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell(IDD_PW_Page1)
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell(IDD_PW_Page1)
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell(IDD_PW_Page1)
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
      SUBROUTINE DealWithWizardWindowDiffractionSetup

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

      END SUBROUTINE DealWithWizardWindowDiffractionSetup
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
      NumBrSG = LPosSG(LatBrav+1) - LPosSG(LatBrav)
      DO ISG = 1, NumBrSG
        JSG = LPosSG(LatBrav)+ISG-1
        SGHMaBrStr(ISG)( 1:12) = SGNumStr(JSG)(1:12)
        SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO
      ISPosSG=1+NumberSGTable-LposSG(LatBrav)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
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
        CASE(1) ! Lab X-ray
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
        CASE(2,3) ! Synchrotron X-ray & CW neutron  
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
        CASE(4) ! TOF neutron
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
