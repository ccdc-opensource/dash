      SUBROUTINE Main_Field_Changed_Routines(IDNumber,JDNumber)
!
      USE WINTERACTER
      USE DRUID_HEADER

!C>> Not used here 
!      LOGICAL :: NODATA
      LOGICAL :: SKIP    = .FALSE.
      LOGICAL :: GOTCELL(6), GOTALLCELL, BackFrom2
!C>> JCC Does nothing so removed      LOGICAL :: GOTWVLN
      INTEGER :: I, ITYPE, IDNUMBER, IPW_Option
      INCLUDE 'statlog.inc'
!
      PARAMETER (MOBS=15000)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
!
      INTEGER IPTYPE
      COMMON /PLTYPE/ IPTYPE
!
!C>> JCC: lattice definitions now in an include file
      INCLUDE 'Lattice.inc'

!C>> JCC A few useful declarations
      CHARACTER(LEN=20) :: Dummy
      INTEGER IRadSelection, IDummy
!
!      INTEGER :: KPosSG,Isp,ISGShow(530)
!
      SELECT CASE (IDNumber)
        CASE (IDF_wavelength1,IDD_Data_Properties)
!C>> JCC Moved to a subroutine
          CALL DownloadWavelength(IDD_Data_Properties)
!C>> JCC Check the cell before trying to regenerate tickmarks with the new wavelength
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks
        CASE (IDF_Indexing_Lambda,IDD_Index_Preparation)
          CALL DownloadWavelength(IDD_Index_Preparation)
          call PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks   
        CASE (IDD_Crystal_Symmetry) ! Deal with the changing pane from symmetry
!C>> JCC Moved this to a subroutine.
          CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
          CALL UpdateCell(IDD_Crystal_Symmetry)
          CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
!C>> JCC Check the cell before trying to generate tickmarks
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks             
        CASE (IDF_Space_Group_Menu)  
!C>> JCC Moved this to a subroutine.
          CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
!C>> JCC Check the cell before trying to generate tickmarks
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks
        CASE (IDF_Crystal_System_Menu)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL Set_Space_Group(IDD_Crystal_Symmetry)
!C>> JCC If the system is reset then regenerate the tic marks
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks
        CASE (IDF_a_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_a_latt,CellPar(1))
          GOTCELL(1) = .TRUE.
          CALL UpdateCell(IDD_Crystal_Symmetry)
        CASE (IDF_b_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_b_latt,CellPar(2))
          GOTCELL(2) = .TRUE.
          call UpdateCell(IDD_Crystal_Symmetry)
        CASE (IDF_c_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_c_latt,CellPar(3))
          GOTCELL(3) = .TRUE.
          call UpdateCell(IDD_Crystal_Symmetry)
        CASE (IDF_alp_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
          GOTCELL(4) = .TRUE.
          call UpdateCell(IDD_Crystal_Symmetry)
        CASE (IDF_bet_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
          GOTCELL(5) = .TRUE.
          call UpdateCell(IDD_Crystal_Symmetry)
        CASE (IDF_gam_latt)
          CALL WDialogSelect(IDD_Crystal_Symmetry)
          CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
          GOTCELL(6) = .TRUE.
          CALL UpdateCell(IDD_Crystal_Symmetry)               
        CASE (IDF_LabX_Source,IDF_SynX_Source,IDF_CWN_Source,IDF_TOF_source)
          CALL WDialogSelect(IDD_Data_Properties)
!          CALL Get_Main_IRadOption()
          CALL Set_Main_IRadOption()
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks           
!C>> JCC Handle a few other events 
        CASE (IDF_Wavelength_Menu) ! Wavelength menu selection
          CALL WDialogSelect(IDD_Data_Properties)
          CALL Set_Main_IRadOption()  
          CALL PolyFitter_Wizard_Check_Status()
          IF (CellOk) CALL Generate_TicMarks           
      END SELECT
      SELECT CASE (JDNumber)
!         CASE (IDF_binning)
! Change the binning
!           CALL WDialogSelect(IDD_Plot_Option_Dialog)
!           CALL WDialogGetInteger(IDF_Binning,LBin)
!           CALL Rebin_Profile()
! Now replot
!           CALL Profile_Plot(IPTYPE)
!C>> JCC Added
        CASE (IDF_Wavelength_Menu) ! tab has changed from the wavelength
          CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection,Dummy)
          CALL SetWavelengthToSelection(IRadSelection)                
      END SELECT

      END SUBROUTINE Main_Field_Changed_Routines
!
!*****************************************************************************
!
      SUBROUTINE UpdateCell(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'statlog.inc'
      INTEGER ICurSel

! JvdS The current dialogue window is saved here. Where is it restored?
      IcurSel = WInfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_PW_Page1)
! PolyFitter_Wizard_Check_Status() sets CellOK
      CALL PolyFitter_Wizard_Check_Status()
! CellOK is a global variable in statlog.inc
      IF (CellOK) THEN
! Enable the wizard next button
! JvdS Was:      CALL WDialogFieldState(IDF_PW1_Next,Enabled)
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL Upload_Cell_Constants()
        CALL Check_Crystal_Symmetry()
        CALL Set_Space_Group(IUploadFrom)
        CALL Generate_TicMarks
      ELSE
! Disable the wizard next button
! JvdS Was:      CALL WDialogFieldState(IDF_PW1_Next,Disabled)
        CALL WDialogFieldState(IDNEXT,Disabled)
      END IF

      END SUBROUTINE UpdateCell
!
!*****************************************************************************
!
      SUBROUTINE Set_Space_Group(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

!C>> JCC Now Cell/Lattice in an include file
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'
      INTEGER IUploadFrom
      INTEGER ICurSg,ICurSel

      ICurSel = WInfoDialog(CurrentDialog)
      IF (IUpLoadFrom .EQ. IDD_Crystal_Symmetry) THEN
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!        Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!        Call WDialogGetMenu(IDF_Space_Group_Menu,ICurSg)
      ELSE
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!        Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!        Call WDialogGetMenu(IDF_PW_Space_Group_Menu,ICurSg)
      END IF
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
! No value given
      IF (Ioption .EQ. -999) IOption = 1
11    NumBrSG = LPosSG(IOption+1) - LPosSG(IOption)
!      IActSg = ICurSG + LPosSG(IOption)
! Only update if the current setting is not of the correct lattice type
      IF ((IOption .EQ. 1) .OR. &
          (IPosSg .GE. LPosSg(IOption) .AND. IPosSg .LT. LPosSg(IOption) + NumBrSg) ) THEN
! Selection of same lattice so retain current space group
!C>> Reset to whatever was previously selected
        IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
        RETURN
      END IF
      ISPosSG = 1
      DO ISG = 1, NumBrSG
        JSG = LPosSG(IOption) + ISG - 1
        SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
        SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO
      IPosSG = LPosSG(IOption)
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
!C>> JCC Set in the wizard too
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
!C>> Reset to whatever was previously selected
      IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE Set_Space_Group
!
!*****************************************************************************
!
!C>> Sets the space group selected
      SUBROUTINE Update_Space_Group(IUploadFrom, ILat, ISgnum)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'
      INTEGER IUploadFrom
      INTEGER ICurSel

      ICurSel = WInfoDialog(CurrentDialog)
      IF (IUploadFrom .EQ. IDD_Crystal_Symmetry) THEN
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
        CALL WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
      ELSE IF (IUploadFrom .EQ. IDD_PW_Page1) THEN
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
        CALL WDialogGetMenu(IDF_PW_Space_Group_Menu,ISPosSG)
      ELSE
        IOption = ILat
        ISPosSG = ISgnum - LPosSG(IOption) + 1
      END IF
!.. Trap 0 for unknown and make it P 1
      NumBrSG = MAX(1,(LPosSG(IOption+1)-LPosSG(IOption)))
      DO ISG = 1, NumBrSG
       JSG = LPosSG(IOption) + ISG - 1
       SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
       SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO
      IPosSG = LPosSG(IOption) + ISPosSg - 1
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
! Set in the wizard too
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
! Finally set the number of pawley refinements to zero
      NumPawleyRef = 0
      IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE Update_Space_Group
!
!*****************************************************************************
!
!      subroutine Check_Main_Crystal_Symmetry()
!
!
!C>> JCC This is redundant now
!      use Winteracter
!      use Druid_Header
!
!      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
! Set the values
!      Call WDialogSelect(IDD_Crystal_Symmetry)
!      Call WDialogGetReal(IDF_a_latt,CellPar(1))
!      Call WDialogGetReal(IDF_b_latt,CellPar(2))      
!      Call WDialogGetReal(IDF_c_latt,CellPar(3))      
!      Call WDialogGetReal(IDF_alp_latt,CellPar(4))      
!      Call WDialogGetReal(IDF_bet_latt,CellPar(5))      
!      Call WDialogGetReal(IDF_gam_latt,CellPar(6))

!C>> Just a dummy call
!       Call Check_Crystal_Symmetry
!
!      end subroutine Check_Main_Crystal_Symmetry
!
!*****************************************************************************
!
!C>> JCC Added this in as a single function
      SUBROUTINE  Check_Lattice_Type

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'
      LOGICAL ABC_Same,AB_Same,AC_Same,BC_Same,Ang_Same,Alp_90,Bet_90,Gam_90,Gam_120

!C>> JCC save static local copies of the previous cell. Only update things if the cell changes on call to
!C>> this routine

      REAL LastCellPar(6)
      DATA LastCellPar / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      SAVE LastCellPar
      INTEGER I
      REAL, PARAMETER :: SmallVal = 1.0E-6
!C>> JCC Added this: initialise to one
      DATA LatBrav / 1 / ! The default setting
!
      DO I = 1 ,6
        IF (CellPar(I) .LE. 0.0) RETURN
      END DO
! C>> JCC run check on last Cell
      IF (   ABS( CellPar(1) - LastCellPar(1) ) .LE. SmallVal  &
       .AND. ABS( CellPar(2) - LastCellPar(2) ) .LE. SmallVal  &   
       .AND. ABS( CellPar(3) - LastCellPar(3) ) .LE. SmallVal  &
       .AND. ABS( CellPar(4) - LastCellPar(4) ) .LE. SmallVal  &
       .AND. ABS( CellPar(5) - LastCellPar(5) ) .LE. SmallVal  &
       .AND. ABS( CellPar(6) - LastCellPar(6) ) .LE. SmallVal ) RETURN
      DO I = 1, 6
        LastCellPar(I) = CellPar(I)
      END DO      
      AB_Same  = ABS(CellPar(2)-CellPar(1)) .LE. SmallVal 
      BC_Same  = ABS(CellPar(3)-CellPar(2)) .LE. SmallVal 
      AC_Same  = ABS(CellPar(3)-CellPar(1)) .LE. SmallVal 
      ABC_Same = AB_Same .AND. BC_Same 
      Alp_90   = (ABS(CellPar(4)-90.0) .LE. SmallVal)
      Bet_90   = (ABS(CellPar(5)-90.0) .LE. SmallVal)
      Gam_90   = (ABS(CellPar(6)-90.0) .LE. SmallVal)
      Gam_120  = (ABS(CellPar(6)-120.0) .LE. SmallVal)
      Ang_Same = ABS(CellPar(6)-CellPar(5)) .LE. SmallVal &
           .AND. ABS(CellPar(5)-CellPar(4)) .LE. SmallVal
!
! Unk=1, Tri=2,Mon_a=3,Mon_b=4,Mon_c=5,Ort=6,Tet=7,Tri=8,Rho=9,Hex=10,Cub=11

!C>> JCC The next code has changed a bit: The LatBrav indices are now set to
!C>> the correct values, and some code for handling equal axes lengths but
!C>> non-equal angles has been added
      IF (ABC_Same .AND. Ang_Same) THEN
        IF (Alp_90) THEN
          LatBrav = 11 ! Cubic
          RETURN
        ELSE
          LatBrav = 9 ! Rhombohedral
          RETURN
        END IF
      END IF
      IF (AB_Same) THEN
        IF (Ang_Same .AND. Alp_90) THEN
          LatBrav = 7 ! Tetragonal
          RETURN
        ELSE IF (Alp_90 .AND. Bet_90 .AND. Gam_120) THEN
          LatBrav = 10 ! Hexagonal
          RETURN
        END IF
      END IF
      IF (Ang_Same .AND. Alp_90) THEN
        LatBrav = 6 ! Orthorhombic
        RETURN
      END IF
      IF      (      Alp_90 .AND.       Bet_90 .AND. .NOT. Gam_90) THEN
        LatBrav = 5 ! Monoclinic c
        RETURN
      ELSE IF (      Alp_90 .AND. .NOT. Bet_90 .AND.       Gam_90) THEN
        LatBrav = 4 ! Monoclinic b
        RETURN
      ELSE IF (.NOT. Alp_90 .AND.       Bet_90 .AND.       Gam_90) THEN
        LatBrav = 3 ! Monoclinic a
        RETURN
      END IF
      LatBrav = 2 ! Triclinic
      RETURN

      END SUBROUTINE Check_Lattice_Type
!
!
!      subroutine Get_Main_IRadOption()
!
!      USE WINTERACTER
!      USE DRUID_HEADER
!
!         CALL WDialogGetRadioButton(IDF_LabX_Source,IRadOption)
!          SELECT CASE (IRadOption)
!            CASE(1)
! Lab X-ray
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Enabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!              CALL WDialogGetRadioButton(IDF_single_wavelength,INWOption)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Enabled)
!C>>              If (INWOption.eq.1) then
!C>>                CALL WDialogFieldState(IDF_wavelength2,Disabled)
!C>>              Else
!C>>                CALL WDialogFieldState(IDF_wavelength2,Enabled)
!C>>              End If
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!            CASE(2,3)
! Synchrotron X-ray & CW neutron
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!>>              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!            CASE(4)
! TOF neutron
!              CALL WDialogFieldState(IDF_CW_group,Disabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Disabled)
!>>              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Enabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Enabled)
!              CALL WDialogFieldState(IDF_flight_path,Enabled)
!              CALL WDialogFieldState(IDF_2theta_label,Enabled)
!              CALL WDialogFieldState(IDF_2theta0,Enabled)
!          END SELECT
!      end subroutine Get_Main_IRadOption
!
!*****************************************************************************
!
      SUBROUTINE Set_Main_IRadOption()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER IRadOption

      CALL WDialogGetRadioButton(IDF_LabX_Source,IRadOption)
      CALL SetSourceDataState(IRadOption)
!          SELECT CASE (IRadOption)
!            CASE(1)
! Lab X-ray
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Enabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!              CALL WDialogGetRadioButton(IDF_single_wavelength,INWOption)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Enabled)
!>>              If (INWOption.eq.1) then
!>>                CALL WDialogFieldState(IDF_wavelength2,Disabled)
!>>              Else
!>>                CALL WDialogFieldState(IDF_wavelength2,Enabled)
!>>              End If
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!            CASE(2,3)
! Synchrotron X-ray & CW neutron
!              CALL WDialogFieldState(IDF_CW_group,Enabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Enabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Enabled)
!>>              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Disabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
!              CALL WDialogFieldState(IDF_flight_path,Disabled)
!              CALL WDialogFieldState(IDF_2theta_label,Disabled)
!              CALL WDialogFieldState(IDF_2theta0,Disabled)
!            CASE(4)
! TOF neutron
!              CALL WDialogFieldState(IDF_CW_group,Disabled)
!              CALL WDialogFieldState(IDF_radiation_label,Disabled)
!              CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
!              CALL WDialogFieldState(IDF_single_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_two_wavelength,Disabled)
!              CALL WDialogFieldState(IDF_wavelength1,Disabled)
!>>JCC              CALL WDialogFieldState(IDF_wavelength2,Disabled)
!              CALL WDialogFieldState(IDF_TOF_group,Enabled)
!              CALL WDialogFieldState(IDF_Flight_Path_Label,Enabled)
!              CALL WDialogFieldState(IDF_flight_path,Enabled)
!              CALL WDialogFieldState(IDF_2theta_label,Enabled)
!              CALL WDialogFieldState(IDF_2theta0,Enabled)
!          END SELECT
      END SUBROUTINE Set_Main_IRadOption
!
!*****************************************************************************
!
!C>> Sequence of subroutines that handle the downloading of each field in turn
      SUBROUTINE DownLoadWavelength(From)

      USE WINTERACTER
      USE DRUID_HEADER

      REAL Temp
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'
      INTEGER ICurrentSel
      INTEGER From

      ICurrentSel = WinfoDialog(CurrentDialog)
      IF (From .EQ. IDD_Data_Properties) THEN
        CALL WDialogSelect(IDD_Data_Properties)
        CALL WDialogGetReal(IDF_wavelength1,Temp)
      ELSE IF (From .EQ. IDD_Index_Preparation) THEN
        CALL WDialogSelect(IDD_Index_Preparation)
        CALL WDialogGetReal(IDF_Indexing_Lambda,Temp)
      END IF
      CALL UpdateWavelength(Temp)
      IF (ICurrentSel .NE. 0) CALL WDialogSelect(ICurrentSel)

      END SUBROUTINE DownLoadWavelength
!
!*****************************************************************************
!
      SUBROUTINE SetWavelengthToSelection(Iselection)

      INCLUDE 'Lattice.inc'

      IF ((Iselection .GT. 1) .AND. (Iselection .LE. (NRad_Types + 1))) THEN
        CALL UpdateWavelength(RadWaveLengths(Iselection - 1))
      ENDIF

      END SUBROUTINE SetWavelengthToSelection
!
!*****************************************************************************
!
      SUBROUTINE UpdateWavelength(TheWaveLength)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      REAL    TheWaveLength
      INTEGER ICurrentSel
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'

      IF ((TheWaveLength .GT. 0.1) .AND. (TheWaveLength .LT. 20.0)) THEN
        alambda = TheWaveLength
        WVLNOK = .TRUE.
      ENDIF
      ICurrentSel = WinfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogPutReal(IDF_wavelength1,alambda,'(f10.5)')
      CALL WDialogSelect(IDD_PW_Page2)
      CALL WDialogPutReal(IDF_PW_wavelength1,alambda,'(f10.5)')
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_Indexing_Lambda,alambda,'(f10.5)')
      CALL WDialogSelect(ICurrentSel)

      END SUBROUTINE UpdateWavelength
!
!*****************************************************************************
!
!C>> JCC Not used anymore
!C>> JCC Sets the popup to contain the defaults for the selected lattice 
!      subroutine Set_Main_Crystal_Symmetry()
!
!
!      use Winteracter
!      use druid_header
!
!C>> JCC Lattice declarations now in an include file 
!       INCLUDE 'Lattice.inc'
!
!      Call WDialogSelect(IDD_Crystal_Symmetry)
!C>> JCC Not needed any more
!Was
! 50  Listbrav=Latbrav+1
! now
! 50   continue
!C>> JCC Was
!      Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! Now
!       Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
!      Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      ISG=1
!      IPosSG=LPosSG(IOption)
!      Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,ISG)
!
!      endsubroutine Set_Main_Crystal_Symmetry
!
!*****************************************************************************
!
!C>> ID Lattice from space group number
      INTEGER FUNCTION LatticeNumber(ISpaceGroup,IDashSg)

      INCLUDE 'Lattice.inc'
      INTEGER,PARAMETER ::  TricLim =   2
      INTEGER,PARAMETER ::  MonoLim =  15
      INTEGER,PARAMETER ::  OrthLim =  74
      INTEGER,PARAMETER ::  TetrLim = 142
      INTEGER,PARAMETER ::  TrigLim = 167
      INTEGER,PARAMETER ::  HexaLim = 194

      IF      (ISpaceGroup .LE. TricLim) THEN
        LatticeNumber = 2
      ELSE IF (ISpaceGroup .LE. MonoLim) THEN
! More work required: a, b or c?
        LatticeNumber = 3
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('a')
              LatticeNumber = 3
            CASE ('b')
              LatticeNumber = 4
            CASE ('c')
              LatticeNumber = 5
            CASE (':')
              GOTO 1
          END SELECT
        END DO
1       CONTINUE
      ELSE IF (ISpaceGroup .LE. OrthLim) THEN
        LatticeNumber = 6
      ELSE IF (ISpaceGroup .LE. TetrLim) THEN
        LatticeNumber = 7
      ELSE IF (ISpaceGroup .LE. TrigLim) THEN
! More work required - trigonal or rhombohedral
        LatticeNumber = 8
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('H')
              LatticeNumber = 8
            CASE ('R')
              LatticeNumber = 9
            CASE (':')
              GOTO 2
          END SELECT
        END DO
2       CONTINUE
      ELSE IF (ISpaceGroup .LE. HexaLim) THEN
        LatticeNumber = 10
      ENDIF
      RETURN

      END FUNCTION LatticeNumber
!
!*****************************************************************************
!
      SUBROUTINE SetSAFileName(filename)

      USE WINTERACTER
      USE DRUID_HEADER

      CHARACTER*(*) filename
      INTEGER       ICurSel

      ICurSel = WInfoDialog(CurrentDialog)
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL WDialogPutString(IDF_SA_Project_Name,filename)
      IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE SetSAFileName
!
!*****************************************************************************
!
      SUBROUTINE SetWizardState(State)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN) :: State

      IF (State .EQ. 1) THEN
        CALL WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOn)
      ELSE
        CALL WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOff)
      END IF
      RETURN

      END SUBROUTINE SetWizardState
!
!*****************************************************************************
!
!>> JCC Subroutine for controlling the configuration of the menus and tool buttons in DASH
      SUBROUTINE SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
!>> If PeakOn is positive then peak fitting will be enabled
!>> If PawleyOn is positive then Pawley fitting will be enabled
!>> Is SolutionOn is positive solving will be enabled
!>> If PeakOn is negative then peak fitting will be disabled
!>> If PawleyOn is negative then Pawley fitting will be disabled
!>> Is SolutionOn is negative solving will be disabled
!>> If PeakOn is zero then the peak fitting state is left as is
!>> If PawleyOn is zero then the Pawley fitting state is left as is
!>> Is SolutionOn is zero then the solving state is left as is

      USE WINTERACTER
      USE druid_header

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: PeakOn, PawleyOn, SolutionOn

      IF (PeakOn .GT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOn)
      ELSE IF (PeakOn .LT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOff)
      END IF
      IF (PawleyOn .GT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
      ELSE IF (PawleyOn .LT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOff)
      END IF
      IF (SolutionOn .GT. 0) THEN
        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOn)
      ELSE IF (SolutionOn .LT. 0) THEN
        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOff)
      END IF
      RETURN

      END SUBROUTINE SetModeMenuState
!
!*****************************************************************************
!
!>> JCC Subroutine for controlling the selection of the mode menu and tool buttons in DASH
      SUBROUTINE SelectModeMenuState(Selection)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: Selection

      CALL WMenuSetState(ID_Peak_Fitting_Mode,      ItemChecked,WintOff)
      CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemChecked,WintOff)
      CALL WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOff)
      IF      (Selection .EQ. ID_Peak_Fitting_Mode)       THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode,      ItemChecked,WintOn)
      ELSE IF (Selection .EQ. ID_Pawley_Refinement_Mode)  THEN 
        CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemChecked,WintOn)
      ELSE IF (Selection .EQ. ID_Structure_Solution_Mode) THEN 
        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOn)
      END IF
      RETURN

      END SUBROUTINE SelectModeMenuState


