      SUBROUTINE PolyFitter_Wizard(NoData)
!   Open the first wizard child windowCheck_Wizard_Crystal_Symmetry
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE                           
!
!   Variable declarations
!
      LOGICAL :: NODATA
      LOGICAL :: SKIP = .FALSE.
      INTEGER :: IPW_Option
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmnf90.inc'
!
!C>> JCC Now these declarations are in an include
      INCLUDE 'Lattice.inc'

      INTEGER :: IRadSelection
      REAL Temp
      CHARACTER(LEN=256) :: CTEMP

      INCLUDE 'GLBVAR.INC' ! Contains JRadOption which replaces SourceState and IRadOption
      INTEGER IDummy

      INTEGER :: KPosSG,Isp,ISGShow(530)
      LOGICAL FnUnitCellOK ! Function

      SKIP = .FALSE.
!.. Set up some defaults
 8    CONTINUE
      CALL ToggleMenus(0)
 1    CONTINUE
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
! Now check on which button was pressed ...
      DO WHILE (.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL GetEvent
        SELECT CASE (EventInfo%WIN)
          CASE (0)
! Process main window messages
            CALL process_mainwindow_message
            CYCLE
        END SELECT
        SELECT CASE (EventType)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDNEXT)
                IXPos_IDD_Wizard = WInfoDialog(6)
                IYPos_IDD_Wizard = WInfoDialog(7)
                CALL WDialogHide()
! We're off the main page and on to new pages depending on the option.
                CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
                SELECT CASE (IPW_Option)
                  CASE(1) ! view data/ determine peak positions
                    GOTO 300
                  CASE(2) ! Pawley
                    GOTO 100
                  CASE(3) ! Simulated Annealing
                    GOTO 500
                  CASE(4) ! Read Druid pawley file
                    GOTO 600
                END SELECT
              CASE (IDF_PW0_Skip,IDCANCEL)
                SKIP = .TRUE.
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
            END SELECT
        END SELECT
      END DO
      CALL ToggleMenus(1)
      RETURN
!.. Deal here with page 1 (cases 2 and 3)
 100  SKIP = .FALSE.
      CALL WDialogSelect(IDD_PW_Page1)
! If the cell is OK, the Next> button should be enabled
      IF (FnUnitCellOK()) THEN
! Enable the wizard next button
        CALL WDialogFieldState(IDNEXT,Enabled)
      ELSE
! Disable the wizard next button
        CALL WDialogFieldState(IDNEXT,Disabled)
      END IF
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE (.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL GetEvent
        SELECT CASE (EventInfo%WIN)
          CASE (0)
! Process main window messages
            CALL process_mainwindow_message
            CYCLE
          CASE (IDD_Plot_Option_Dialog)
            CALL DealWithPlotOptionsWindow
            CYCLE
          CASE (IDD_Data_Properties)
            CALL DealWithDiffractionSetupPane
            CYCLE
          CASE (IDD_Structural_Information)
            CALL DealWithStructuralInformation
            CYCLE
        END SELECT
        SELECT CASE (EventType)
          CASE (Expose,Resize)
            CALL Redraw()
            CALL WDialogSelect(IDD_PW_Page1)
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
              CASE(IDBACK)
                CALL WDialogHide()
                GOTO 1
              CASE(IDNEXT)
! We're off the second page and on to the last page
                CALL WDialogHide()
!C>>                          CALL WDialogSelect(IDD_PW_Page1)
                GOTO 200
! JvdS I think that 'IDF_PW1_Skip' should have been something like 'CommonClose'
              CASE(IDF_PW1_Skip,IDCANCEL)
                SKIP = .TRUE.
                CALL WDialogSelect(IDD_PW_Page1)
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
            END SELECT
          CASE (FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_PW_Space_Group_Menu)
                CALL Update_Space_Group(IDD_PW_Page1, IDummy, IDummy)
              CASE (IDF_PW_Crystal_System_Menu)
                CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,LatBrav)
                CALL Set_Crystal_Symmetry(LatBrav)
                CALL Set_Space_Group(IDD_PW_Page1)
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
        END SELECT
      END DO
!.. Deal here with page 2 of Pawley, i.e. Basic Diffraction Information
! Now called: Diffraction Setup
 200 SKIP = .FALSE.
      CALL WDialogSelect(IDD_PW_Page2)
! JvdS @ following line should not be necessary
      CALL SetSourceDataState(JRadOption)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL GetEvent
        SELECT CASE (EventInfo%WIN)
          CASE (0)
! Process main window messages
            CALL process_mainwindow_message
            CYCLE
        END SELECT
! JvdS @ Next line is in a weird place again
        CALL WDialogGetRadioButton(IDF_PW_LabX_Source,JRadOption)
        CALL SetSourceDataState(JRadOption)
        SELECT CASE (EventType)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE(FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_PW_wavelength1)
                CALL WDialogGetReal(IDF_PW_wavelength1,Temp)
                CALL UpdateWavelength(Temp)
!C>> JCC Add in function  
              CASE (IDF_PW_Wavelength_Menu)
                CALL WDialogGetMenu(IDF_PW_Wavelength_Menu,IRadSelection)
                CALL SetWavelengthToSelection(IRadSelection)
            END SELECT
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
              CASE (ID_PW_DF_Open)
!C>> JCC Add callback for Open button
                CALL WDialogGetString(IDF_PW_DataFileName_String,CTEMP)
                CALL Diffraction_File_Open(CTEMP,NoData)
!C>> JCC Have to reselect after the file open
                CALL WDialogSelect(IDD_PW_Page2)
              CASE (ID_PW_DF_Browse)
                CALL Diffraction_File_Browse(NoData)
!C>> JCC Have to reselect after the file open
                CALL WDialogSelect(IDD_PW_Page2)
              CASE (IDBACK)
                CALL WDialogHide()
                GOTO 100
              CASE (IDFINISH)
                SKIP = .TRUE.
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
              CASE (IDCANCEL)
                SKIP = .TRUE.
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
            END SELECT
        END SELECT
      END DO
!   view data/ determine peak positions
!.. Deal here with page 3 - only visited for options 1 & 2
 300  SKIP = .FALSE.
      CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL GetEvent
        SELECT CASE (EventInfo%WIN)
          CASE (0)
! Process main window messages
            CALL process_mainwindow_message
            CYCLE
        END SELECT
        SELECT CASE (EventType)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE (PushButton)
            SELECT CASE (EventInfo%VALUE1)
!C>> JCC Add callback for Open button
              CASE(ID_PWa_DF_Open)
                CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
                CALL Diffraction_File_Open(CTEMP,NoData)
!C>> JCC Need to reselect dialogue after the action
                CALL WDialogSelect(IDD_PW_Page3)
              CASE(ID_PWa_DF_Browse)
                CALL Diffraction_File_Browse(NoData)
!C>> JCC Need to reselect dialogue after the action
                CALL WDialogSelect(IDD_PW_Page3)
              CASE(IDBACK)
                CALL WDialogHide()
                GOTO 1
!C>>                  CASE(IDF_PW3_Skip)
!C>>                    SKIP = .TRUE.
                CALL WDialogHide()
! JvdS It's kind of dirty that 'Cancel' and 'Finish' amount to the same thing.
              CASE(IDF_PW3_Finish,IDCANCEL)
                SKIP = .TRUE.
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
            END SELECT
        END SELECT
      END DO
!.. Deal with the simulated annealing ...
 500  SKIP = .FALSE.
      IXPos_IDD_SA_Input = IXPos_IDD_Wizard
      IYPos_IDD_SA_Input = IYPos_IDD_Wizard
      CALL SA_MAIN()
      GOTO 8
 600  IXPos_IDD_SA_Input = WInfoDialog(6)
      IYPos_IDD_SA_Input = WInfoDialog(7)
      CALL SA_MAIN()
      CALL ToggleMenus(1)
      RETURN

      END SUBROUTINE PolyFitter_Wizard
!
!*****************************************************************************
!
      SUBROUTINE Default_Crystal_Symmetry()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page1)
!O      CALL WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
      CALL WDialogPutOption(IDF_PW_Crystal_System_Menu,LatBrav)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
!O      CALL WDialogPutMenu(IDF_Crystal_System_Menu,   CS_Options,NCS_Options,LatBrav)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      DO ISG = 1, NumSG
        JSG = LPosSG(LatBrav) + ISG - 1
        SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
        SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO      
      IPosSG = 1
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
      CALL PopActiveWindowID

      END SUBROUTINE Default_Crystal_Symmetry
!
!*****************************************************************************
!
      SUBROUTINE Upload_Wizard_Information()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

! JvdS @ I don't think the following two lines are necessary
      CALL WDialogSelect(IDD_Structural_Information)
      CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Crystal_Symmetry)
      CALL Upload_Crystal_Symmetry()
! JvdS @ I don't think the following two lines are necessary
      CALL WDialogSelect(IDD_Structural_Information)
      CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Data_Properties)
      CALL Upload_Range()

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
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Enabled)
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
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Disabled)
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
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Disabled)
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
