      SUBROUTINE PolyFitter_Wizard(NoData)
!   Open the first wizard child windowCheck_Wizard_Crystal_Symmetry
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE                           
!
!   Type declarations
!
      TYPE(WIN_MESSAGE) :: MESSAGE
!
!   Variable declarations
!
      LOGICAL :: NODATA
      LOGICAL :: SKIP    = .FALSE.
      LOGICAL :: GOTCELL(6),GOTALLCELL, BackFrom2
!C>> No effect so removed      LOGICAL :: GOTWVLN
      LOGICAL :: SA_Option_Requested
      INTEGER :: I,ITYPE,IDNUMBER,IPW_Option
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmnf90.inc'
!
!C>> JCC Now these declarations are in an include
      INCLUDE 'Lattice.inc'
!
      INTEGER :: II,IRadSelection
      REAL Temp
      CHARACTER(LEN=20) :: Dummy
      CHARACTER(LEN=256) :: CTEMP
      LOGICAL FExists
      INTEGER ILCT

!C>> JCC static state of radiation window. This data may ultimately need to
!C>> be in a common

      INTEGER :: SourceState, IDummy
      LOGICAL :: SingleSource
      DATA SourceState / 0 / ! The default value: Xray lab data
      DATA SingleSource / .TRUE. /
      SAVE SourceState
      SAVE SingleSource
!
      INTEGER :: KPosSG,Isp,ISGShow(530)
      LOGICAL process_mainwindow_message
      LOGICAL Quit
!
      SKIP = .FALSE.
!.. Set up some defaults
 8    DO I = 1, 6
        gotcell(i) = .FALSE.
      END DO
      BackFrom2 = .FALSE.
      CALL ToggleMenus(0)
 1    CONTINUE
!       CALL WDialogHide()
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
! Now check on which button was pressed ...
      DO WHILE (.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL WMessage(ITYPE,MESSAGE)
! Process main window messages
        IF (MESSAGE%WIN .EQ. 0) THEN
          QUIT = process_mainwindow_message(ITYPE,MESSAGE)
          CYCLE
        END IF
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE (PushButton)
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
              CASE(IDNEXT)
                IXPos_IDD_Wizard = WInfoDialog(6)
                IYPos_IDD_Wizard = WInfoDialog(7)
                CALL WDialogHide()
! We're off the main page and on to new pages depending on the option.
                CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
                SA_Option_Requested = .FALSE.
                SELECT CASE (IPW_Option)
                  CASE(1) ! view data/ determine peak positions
                    GOTO 300
                  CASE(2) ! Pawley
                    GOTO 100
                  CASE(3) ! Simulated Annealing
                    SA_Option_Requested = .TRUE. 
                    GOTO 500
! JvdS ? there are only 3 options
                  CASE(4) ! Read Druid pawley file
                    GOTO 600
                END SELECT
              CASE(IDF_PW0_Skip,IDCANCEL)
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
! PolyFitter_Wizard_Check_Status() sets CellOK
      CALL PolyFitter_Wizard_Check_Status()
! CellOK is a global variable in statlog.inc
      IF (CellOK) THEN
! Enable the wizard next button
! JvdS Was:      CALL WDialogFieldState(IDF_PW1_Next,Enabled)
        CALL WDialogFieldState(IDNEXT,Enabled)
      ELSE
! Disable the wizard next button
! JvdS Was:      CALL WDialogFieldState(IDF_PW1_Next,Disabled)
        CALL WDialogFieldState(IDNEXT,Disabled)
      END IF
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      BackFrom2 = .FALSE.
      DO WHILE (.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
!         call PolyFitter_Wizard_Check_Status(NoData)
!         call Check_Wizard_Crystal_Symmetry()
        CALL WMessage(ITYPE,MESSAGE)
! Process main window messages
        IF (MESSAGE%WIN .EQ. 0) THEN
          QUIT = process_mainwindow_message(ITYPE,MESSAGE)
          CYCLE
        END IF
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
            CALL WDialogSelect(IDD_PW_Page1)
          CASE (PushButton)
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
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
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
              CASE (IDF_PW_Space_Group_Menu)
                CALL Update_Space_Group(IDD_PW_Page1, IDummy, IDummy)
              CASE (IDF_PW_Crystal_System_Menu)
                CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
                LatBrav = IOption
                CALL Set_Crystal_Symmetry(LatBrav)
                CALL Set_Space_Group(IDD_PW_Page1)
              CASE (IDF_PW_a_latt)
                CALL WDialogGetReal(IDF_PW_a_latt,CellPar(1))
                GOTCELL(1) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
              CASE (IDF_PW_b_latt)
                CALL WDialogGetReal(IDF_PW_b_latt,CellPar(2))
                GOTCELL(2) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
              CASE (IDF_PW_c_latt)
                CALL WDialogGetReal(IDF_PW_c_latt,CellPar(3))
                GOTCELL(3) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
              CASE (IDF_PW_alp_latt)
                CALL WDialogGetReal(IDF_PW_alp_latt,CellPar(4))
                GOTCELL(4) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
              CASE (IDF_PW_bet_latt)
                CALL WDialogGetReal(IDF_PW_bet_latt,CellPar(5))
                GOTCELL(5) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
              CASE (IDF_PW_gam_latt)
                CALL WDialogGetReal(IDF_PW_gam_latt,CellPar(6))
                GOTCELL(6) = .TRUE.
                CALL UpdateCell(IDD_PW_Page1)
            END SELECT                
        END SELECT
      END DO
!.. Deal here with page 2 of Pawley, i.e. Basic Diffraction Information
 200 SKIP = .FALSE.
!      CALL WDialogHide()
      CALL WDialogSelect(IDD_PW_Page2)
      IF (SourceState .EQ. 0) THEN
        CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
!C>> JCC           CALL WDialogPutRadioButton(IDF_PW_single_wavelength)
        SourceState = 1
      END IF
      CALL SetSourceDataState(SourceState)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL WMessage(ITYPE,MESSAGE)
        CALL WDialogGetRadioButton(IDF_PW_LabX_Source,IRadOption)
! Attempting to write to the main view windows
!C>> JCC          CALL WDialogLoad(IDD_Data_Properties)
!             CALL WDialogHide()
!             CALL WDialogSelect(IDD_Data_Properties)
!             CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,SemiModeless)

!C>> JCC Now use a subroutine to set the state

! Process main window messages
        IF (MESSAGE%WIN .EQ. 0) THEN
! JvdS QUIT is set, but is it handled?
          QUIT = process_mainwindow_message(ITYPE,MESSAGE)
          CYCLE
        END IF
        CALL SetSourceDataState(IRadOption)
        SourceState = IRadOption
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE(FieldChanged)
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
              CASE (IDF_PW_wavelength1)
                CALL WDialogGetReal(IDF_PW_wavelength1,Temp)
!C>> No effect so removed                    GOTWVLN=.TRUE.
                CALL UpdateWavelength(Temp)
!C>> JCC Add in function  
              CASE (IDF_PW_Wavelength_Menu)
                CALL WDialogGetMenu(IDF_PW_Wavelength_Menu,IRadSelection,Dummy)
                CALL SetWavelengthToSelection(IRadSelection)
            END SELECT
          CASE (PushButton)
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
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
                BackFrom2 = .TRUE.
                CALL WDialogHide()
                GOTO 100
              CASE (IDFINISH)
                SKIP = .TRUE.
                CALL WDialogHide()
                CALL ToggleMenus(1)
                RETURN
!C>> JCC No longer present
!                 CASE(IDF_PW2_Finish)
!                    SKIP = .TRUE.
!                             CALL WDialogHide()
!                    RETURN
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
!C     CALL WDialogHide()
      CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT. SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        CALL WMessage(ITYPE,MESSAGE)
! Process main window messages
        IF (MESSAGE%WIN .EQ. 0) THEN
          QUIT = process_mainwindow_message(ITYPE,MESSAGE)
          CYCLE
        END IF
        SELECT CASE (ITYPE)
          CASE (Expose,Resize)
            CALL Redraw()
          CASE (PushButton)
            IDNumber = MESSAGE%VALUE1
            SELECT CASE (IDNumber)
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
!      return
 600  IXPos_IDD_SA_Input = WInfoDialog(6)
      IYPos_IDD_SA_Input = WInfoDialog(7)
      CALL SA_MAIN()
      CALL ToggleMenus(1)
      RETURN

      END SUBROUTINE PolyFitter_Wizard
!
!*****************************************************************************
!
      SUBROUTINE PolyFitter_Wizard_Check_Status()

      USE WINTERACTER
      USE DRUID_HEADER    

      IMPLICIT NONE
!
      TYPE(WIN_MESSAGE) :: MESSAGE
!
      LOGICAL :: SKIP = .FALSE.
      INTEGER :: I,ITYPE,IDNUMBER,IPW_Option
      REAL    :: CELLPAR,ZEROPOINT,ALAMBDA
      REAL    :: a,b,c,d2r,calp,cbet,cgam,arg,vcell
! JvdS Wasn't this supposed to be in an include?
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 
      INCLUDE 'statlog.inc'
!
! JvdS CELLOK is a global variable in statlog.inc
! Maybe it should be a function? It is very simple.
! The way it has been implemented at the moment, it effectively is a function.
! What do we need the 'GotCell(1:6)' for? It isn't used to check if the cell is OK.
!
      CellOK = .TRUE.
      DO I = 1, 6
        CellOK = CellOK .AND. (CellPar(I) .GT. 0.0)
      END DO
      IF (.NOT. CellOK) RETURN
! Calculate the unit cell volume - if sensible upload lattice constants
! d2r converts degrees to radians
      d2r = ATAN(1.0)/45.0
      a = cellpar(1)
      b = cellpar(2)
      c = cellpar(3)
      calp = COS(d2r*cellpar(4))
      cbet = COS(d2r*cellpar(5))
      cgam = COS(d2r*cellpar(6))
      arg = 1.0 + 2.0*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
      VCELL = a*b*c*SQRT(MAX(0.0,arg))
!C JCC add checks
      CellOK = CellOK .AND. (VCELL .GT. 10.0) 
      RETURN

      END SUBROUTINE PolyFitter_Wizard_Check_Status
!

!      subroutine Check_Wizard_Crystal_Symmetry()
!
!
!      use Winteracter
!      use Druid_Header
!       INCLUDE 'Lattice.inc'
!       INTEGER OldLatBrav,ICurSel
!

!       ICurSel = WInfoDialog(CurrentDialog)
!      Call WDialogSelect(IDD_PW_Page1)
!      Call WDialogGetReal(IDF_PW_a_latt,CellPar(1))
!      Call WDialogGetReal(IDF_PW_b_latt,CellPar(2))      
!      Call WDialogGetReal(IDF_PW_c_latt,CellPar(3))      
!      Call WDialogGetReal(IDF_PW_alp_latt,CellPar(4))      
!      Call WDialogGetReal(IDF_PW_bet_latt,CellPar(5))      
!      Call WDialogGetReal(IDF_PW_gam_latt,CellPar(6))

!C>> JCC Have one routine to handle this now

!       OldLatBrav = LatBrav
!90     Call Check_Lattice_Type
!       IF (OldLatBrav .NE. LatBrav) THEN
!           CALL Set_Crystal_Symmetry(LatBrav)
!       END IF
!
!       IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)       
!      endsubroutine Check_Wizard_Crystal_Symmetry
!
!*****************************************************************************
!
      SUBROUTINE Default_Crystal_Symmetry()
!
      USE WINTERACTER
      USE DRUID_HEADER
!
      INCLUDE 'Lattice.inc'
!
      INCLUDE 'statlog.inc'
      INTEGER ICurSel

      ICurSel = WInfoDialog(CurrentDialog)
!C>> JCC No longer needed     Listbrav=1
      Ioption = 1
!C>> JCC Was
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! now
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
      CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
      DO ISG = 1, NumSG
        JSG = LPosSG(IOption) + ISG - 1
        SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
        SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO      
      IPosSG = 1
      NumberSGTable = IPosSG
!C>> JCC Was
!      Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,ListBrav)
! now
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
      IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE Default_Crystal_Symmetry
!
!C>> JCC No longer used
!
!      subroutine Set_Wizard_Crystal_Symmetry()
!
!
!      use Winteracter
!      use druid_header
!
!
!      include 'statlog.inc'
!C>> JCC Lattice declarations now in a common include file
!      include 'Lattice.inc'
!
!C>> JCC LatBrav is now correct: no need for Listbrav
! 50  Listbrav=Latbrav+1
! 50 continue
!C>> JCC Changed from
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! to
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
!      Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      ISG=1
!      IPosSG=LPosSG(IOption)
!      NumberSGTable=IPosSG
!      Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,ISG)
!
!      endsubroutine Set_Wizard_Crystal_Symmetry
!
!
!C>> JCC No longer used
!      subroutine Set_Wizard_Space_Group()
!
!      USE WINTERACTER
!      USE druid_header
!      IMPLICIT NONE
!
!   Variable declarations
!
!
!C>> JCC Lattice/Cell declarations in an INCLUDE file now
!       include 'Lattice.inc'
!
!
!      include 'statlog.inc'
!
!      Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!      ISPosSG=1
!      NumBrSG=LPosSG(IOption+1)-LPosSG(IOption)
!     write(76,*) ' lpossg ',LPosSG(IOption),LPosSG(IOption+1)
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      IPosSG=LPosSG(IOption)
!      Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
!      Call WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
!
!      end subroutine Set_Wizard_Space_Group
!
!*****************************************************************************
!
      SUBROUTINE Upload_Wizard_Information()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CALL WDialogSelect(IDD_Structural_Information)
      CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Crystal_Symmetry)
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL Upload_Crystal_Symmetry()
      CALL WDialogSelect(IDD_Structural_Information)
      CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Data_Properties)
      CALL WDialogSelect(IDD_Data_Properties)
! JvdS Commented out the following line
!      CALL Upload_Data_Properties()
      CALL Upload_Range()

      END SUBROUTINE Upload_Wizard_Information      
!
!*****************************************************************************
!
!C>> JCC Subroutines to set the state of the page 2 wizard to either synchrotron or lab data 
!C>> JCC Subroutine to set the state of the page 2 wizard to either synchrotron or lab data 

      SUBROUTINE SetSourceDataState(IRadOption)

      USE WINTERACTER
      USE DRUID_HEADER 

      IMPLICIT NONE

      INTEGER IRadOption, JRadOption
      COMMON /RadOption/ JRadOption

      JRadOption = IRadOption
      SELECT CASE (IRadOption)
        CASE(1)
! Lab X-ray
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Enabled)
!C>>              CALL WDialogFieldState(IDF_PW_single_wavelength,Enabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
!C>>              CALL WDialogGetRadioButton(IDF_PW_single_wavelength,INWOption)
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Enabled)
!C>>              CALL WDialogFieldState(IDF_PW_two_wavelength,Enabled)
!C>>              If (INWOption.eq.1) then
!C>>                CALL WDialogFieldState(IDF_PW_wavelength2,Disabled)
!C>> JCC              Else
!C>>                CALL WDialogFieldState(IDF_PW_wavelength2,Enabled)
!C>>              End If
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
! Also update the main popup
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Enabled)
!C>>              CALL WDialogFieldState(IDF_PW_single_wavelength,Enabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
!C>>              CALL WDialogGetRadioButton(IDF_PW_single_wavelength,INWOption)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
!C>>              CALL WDialogFieldState(IDF_PW_two_wavelength,Enabled)
!C>>              If (INWOption.eq.1) then
!C>>                CALL WDialogFieldState(IDF_PW_wavelength2,Disabled)
!C>> JCC              Else
!C>>                CALL WDialogFieldState(IDF_PW_wavelength2,Enabled)
!C>>              End If
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_LabX_Source)
        CASE(2,3)
! Synchrotron X-ray & CW neutron
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Disabled)
!C>>              CALL WDialogFieldState(IDF_PW_single_wavelength,Enabled)
!C>>              CALL WDialogFieldState(IDF_PW_two_wavelength,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
!C>> JCC              CALL WDialogFieldState(IDF_PW_wavelength2,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          IF (IradOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
          END IF
! Synchrotron X-ray & CW neutron
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          IF (IradOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_CWN_Source)
          END IF
        CASE(4)
! TOF neutron
          CALL WDialogFieldState(IDF_PW_CW_group,Disabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_PW_Wavelength_Menu,Disabled)
!C>>              CALL WDialogFieldState(IDF_PW_single_wavelength,Disabled)
!C>>              CALL WDialogFieldState(IDF_PW_two_wavelength,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Disabled)
!C>> JCC              CALL WDialogFieldState(IDF_PW_wavelength2,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_PW_TOF_source)
! TOF neutron
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
      RETURN

      END SUBROUTINE SetSourceDataState
