      subroutine Main_Field_Changed_Routines(IDNumber,JDNumber)
!
     USE WINTERACTER
      USE druid_header
!
!   Variable declarations
!
!C>> Not used here 
!      LOGICAL :: NODATA
      LOGICAL           :: SKIP    = .FALSE.
      LOGICAL :: GOTCELL(6),GOTALLCELL, BackFrom2
!C>> JCC Does nothing so removed      LOGICAL :: GOTWVLN
      INTEGER           :: I,ITYPE,IDNUMBER,IPW_Option
      include 'statlog.inc'
!
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
!	write(76,*) ' IDNumber in Main Field Changed is ',IDNUMBER,JDNUMBER
!
      SELECT CASE (IDNumber)
        CASE(IDF_wavelength1,IDD_Data_Properties)
!C>> JCC Moved to a subroutine
             CALL DownloadWavelength(IDD_Data_Properties)
!C>> JCC Check the cell before trying to regenerate tickmarks with the new wavelength
			 call PolyFitter_Wizard_Check_Status()
			 if (CellOk) call Generate_TicMarks
        CASE(IDF_Indexing_Lambda,IDD_Index_Preparation)
             CALL DownloadWavelength(IDD_Index_Preparation)
			 call PolyFitter_Wizard_Check_Status()
		     if (CellOk) call Generate_TicMarks	
	    CASE(IDD_Crystal_Symmetry) ! Deal with the changing pane from symmetry
!C>> JCC Moved this to a subroutine.
			 CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
		     CALL UpdateCell(IDD_Crystal_Symmetry)
			 CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
!C>> JCC Check the cell before trying to generate tickmarks
			 call PolyFitter_Wizard_Check_Status()
			 if (CellOk) call Generate_TicMarks			
        CASE(IDF_Space_Group_Menu)  
!C>> JCC Moved this to a subroutine.
			 CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
!C>> JCC Check the cell before trying to generate tickmarks
			 call PolyFitter_Wizard_Check_Status()
			 if (CellOk) call Generate_TicMarks
        CASE (IDF_Crystal_System_Menu)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             call Set_Space_Group(IDD_Crystal_Symmetry)
!C>> JCC If the system is reset then regenerate the tic marks
			 call PolyFitter_Wizard_Check_Status()
			 if (CellOk) call Generate_TicMarks
        CASE(IDF_a_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_a_latt,CellPar(1))
             GOTCELL(1)=.TRUE.
             call UpdateCell(IDD_Crystal_Symmetry)
        CASE(IDF_b_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_b_latt,CellPar(2))
             GOTCELL(2)=.TRUE.
             call UpdateCell(IDD_Crystal_Symmetry)
        CASE(IDF_c_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_c_latt,CellPar(3))
             GOTCELL(3)=.TRUE.
			 call UpdateCell(IDD_Crystal_Symmetry)
        CASE(IDF_alp_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
             GOTCELL(4)=.TRUE.
			 call UpdateCell(IDD_Crystal_Symmetry)
        CASE(IDF_bet_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
             GOTCELL(5)=.TRUE.
             call UpdateCell(IDD_Crystal_Symmetry)
        CASE(IDF_gam_latt)
             CALL WDialogSelect(IDD_Crystal_Symmetry)
             CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
             GOTCELL(6)=.TRUE.
             call UpdateCell(IDD_Crystal_Symmetry)		     
        CASE(IDF_LabX_Source,IDF_SynX_Source,IDF_CWN_Source,IDF_TOF_source)
             CALL WDialogSelect(IDD_Data_Properties)
!             CALL Get_Main_IRadOption()
		   CALL Set_Main_IRadOption()
		   call PolyFitter_Wizard_Check_Status()
		   if (CellOk) call Generate_TicMarks		
!C>> JCC Handle a few other events 
		CASE(IDF_Wavelength_Menu) ! Wavelength menu selection
		   CALL WDialogSelect(IDD_Data_Properties)
		   CALL Set_Main_IRadOption()  
		   call PolyFitter_Wizard_Check_Status()
		   if (CellOk) call Generate_TicMarks		
		END SELECT
!
      SELECT CASE (JDNumber)
!         CASE(IDF_binning)
! Change the binning
!           CALL WDialogSelect(IDD_Plot_Option_Dialog)
!           CALL WDialogGetInteger(IDF_Binning,LBin)
!           CALL Rebin_Profile()
! Now replot
!           CALL Profile_Plot(IPTYPE)
!C>> JCC Added
         CASE(IDF_Wavelength_Menu) ! tab has changed from the wavelength
		   CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection,Dummy)
		   CALL SetWavelengthToSelection(IRadSelection)			   
      END SELECT

      end subroutine Main_Field_Changed_Routines
!
!
!
      subroutine UpdateCell(IUploadFrom)
	  use winteracter
	  use druid_header
      include 'statlog.inc'
	  integer ICurSel
	  IcurSel = WInfoDialog(CurrentDialog)
	  CALL WDialogSelect(IDD_PW_Page1)
	  call PolyFitter_Wizard_Check_Status()

      If (CellOK) then
				CALL WDialogFieldState(IDF_PW1_Next,Enabled)
	  			Call Upload_Cell_Constants()
                call Check_Crystal_Symmetry()
                call Set_Space_Group(IUploadFrom)
				call Generate_TicMarks

! Enable the wizard next button


	  Else
				CALL WDialogFieldState(IDF_PW1_Next,Disabled)
      End If
	  end subroutine UpdateCell
!
!


!
!
      subroutine Set_Space_Group(IUploadFrom)
!
      USE WINTERACTER
      USE druid_header
      IMPLICIT NONE
!
!   Variable declarations
!
!C>> JCC Now Cell/Lattice in an include file
	  include 'Lattice.inc'
      include 'statlog.inc'
	  INTEGER IUploadFrom

!
	  INTEGER ICurSg,ICurSel
	  ICurSel = WInfoDialog(CurrentDialog)
	  IF (IUpLoadFrom .EQ. IDD_Crystal_Symmetry) THEN
		Call WDialogSelect(IDD_Crystal_Symmetry)
		Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!       Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!		Call WDialogGetMenu(IDF_Space_Group_Menu,ICurSg)
	  ELSE
		Call WDialogSelect(IDD_PW_Page1)
		Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!       Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!		Call WDialogGetMenu(IDF_PW_Space_Group_Menu,ICurSg)
	  END IF

!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)

! No value given
      IF (Ioption .EQ. -999) IOption = 1

11      NumBrSG=LPosSG(IOption+1)-LPosSG(IOption)
!	    IActSg = ICurSG + LPosSG(IOption)
! Only update if the current setting is not of the correct lattice type
	  IF (IOption .EQ. 1 .OR. &
	      ( IPosSg .GE. LPosSg(IOption) .AND. IPosSg .LT. LPosSg(IOption) + NumBrSg) ) THEN
! Selection of same lattice so retain current space group
!C>> Reset to whatever was previously selected
	      IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
		  RETURN
	  END IF


      ISPosSG=1
!	write(76,*) ' lpossg ',LPosSG(IOption),LPosSG(IOption+1)
      Do ISG=1,NumBrSG
         JSG=LPosSG(IOption)+ISG-1
         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
      End Do
      IPosSG=LPosSG(IOption)
      NumberSGTable=IPosSG
!	write(76,*) ' IPosSG, ISPosSG is ',IPosSG,ISPosSG,NumberSGTable

      Call WDialogSelect(IDD_Crystal_Symmetry)
	  Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      Call WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
!C>> JCC Set in the wizard too
      Call WDialogSelect(IDD_PW_Page1)
	  Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
	  Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      Call WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
!C>> Reset to whatever was previously selected
	  IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
      end subroutine Set_Space_Group
!
!
!

!C>> Sets the space group selected
  subroutine Update_Space_Group(IUploadFrom, ILat, ISgnum)
  USE WINTERACTER
  USE druid_header
  include 'statlog.inc'
  include 'Lattice.inc'
  INTEGER IUploadFrom
  INTEGER ICurSel

  ICurSel = WInfoDialog(CurrentDialog)

  IF (IUploadFrom.EQ.IDD_Crystal_Symmetry) THEN
	Call WDialogSelect(IDD_Crystal_Symmetry)
	Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
	Call WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
  ELSE IF (IUploadFrom.EQ.IDD_PW_Page1) THEN
    Call WDialogSelect(IDD_PW_Page1)
	Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
	Call WDialogGetMenu(IDF_PW_Space_Group_Menu,ISPosSG)
  ELSE
    IOption = ILat
    ISPosSG = ISgnum - LPosSG(IOption) + 1
  END IF

!.. Trap 0 for unknown and make it P 1
  NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
  Do ISG=1,NumBrSG
     JSG=LPosSG(IOption)+ISG-1
     SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
     SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
  End Do
  IPosSG=LPosSG(IOption)+ISPosSg-1
  NumberSGTable=IPosSG
!  write(76,*) ' IPosSG, ISPosSG is ',IPosSG,ISPosSG,NumberSGTable

  Call WDialogSelect(IDD_Crystal_Symmetry)
  Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
  Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
  Call WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
! Set in the wizard too
  Call WDialogSelect(IDD_PW_Page1)
  Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
  Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
  Call WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)

! Finally set the number of pawley refinements to zero

  NumPawleyRef = 0

  IF (ICurSel .NE. 0) Call WDialogSelect(ICurSel)

  end subroutine Update_Space_Group


!
!
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
!	  Call Check_Crystal_Symmetry
!
!      end subroutine Check_Main_Crystal_Symmetry


!C>> JCC Added this in as a single function
      subroutine  Check_Lattice_Type

      use Winteracter
      use Druid_Header
!
	  include 'Lattice.inc'
      Logical ABC_Same,AB_Same,AC_Same,BC_Same,Ang_Same,Alp_90,Bet_90,Gam_90,Gam_120
	  Logical Continue_Check
!


!C>> JCC save static local copies of the previous cell. Only update things if the cell changes on call to
!C>> this routine

	  character*40 LastSetting
	  real LastCellPar(6)
	  Data LastCellPar / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
	  save LastCellPar

!C>> JCC Added this: initialise to one
	  DATA LatBrav / 1 / ! The default setting
!
	do i=1,6
	  if (CellPar(i).le.0.) goto 50
	end do

    SmallVal=1.e-6
! C>> JCC run check on last Cell
	if (   Abs( CellPar(1) - LastCellPar(1) ) .le. SmallVal  &
	 .AND. Abs( CellPar(2) - LastCellPar(2) ) .le. SmallVal  &	 
	 .AND. Abs( CellPar(3) - LastCellPar(3) ) .le. SmallVal  &
	 .AND. Abs( CellPar(4) - LastCellPar(4) ) .le. SmallVal  &
	 .AND. Abs( CellPar(5) - LastCellPar(5) ) .le. SmallVal  &
     .AND. Abs( CellPar(6) - LastCellPar(6) ) .le. SmallVal ) goto 50
 
  
!
	do i = 1,6
	   LastCellPar(i) = CellPar(i)
	end do	

      AB_Same=Abs(CellPar(2)-CellPar(1)).le.SmallVal 
      BC_Same=Abs(CellPar(3)-CellPar(2)).le.SmallVal 
      AC_Same=Abs(CellPar(3)-CellPar(1)).le.SmallVal 
      ABC_Same=AB_Same.and.BC_Same 
      Alp_90=(Abs(CellPar(4)-90.0).le.SmallVal)
      Bet_90=(Abs(CellPar(5)-90.0).le.SmallVal)
      Gam_90=(Abs(CellPar(6)-90.0).le.SmallVal)
      Gam_120=(Abs(CellPar(6)-120.0).le.SmallVal)
      Ang_Same=Abs(CellPar(6)-CellPar(5)).le.SmallVal &
         .and. Abs(CellPar(5)-CellPar(4)).le.SmallVal
!
! Unk=1, Tri=2,Mon_a=3,Mon_b=4,Mon_c=5,Ort=6,Tet=7,Tri=8,Rho=9,Hex=10,Cub=11

!C>> JCC The next code has changed a bit: The LatBrav indicies are now set to
!C>> the correct values, and some code for handling equal axis lengths but
!C>> non-equal angles has been added
      LatBrav=1 ! Unknown
!
	  Continue_Check = .true.
      If (ABC_Same) Then
        If (Ang_Same) Then
          If (Alp_90) Then
            LatBrav=11 ! Cubic
			Continue_Check = .false.
          Else
            LatBrav=9 ! Rhombohedral
			Continue_Check = .false.
          End If
        End If
      End If


	  If (Continue_Check) Then
        If (AB_Same) Then
		 If (Ang_Same .AND. Alp_90) Then
            LatBrav=7 ! Tetragonal
			Continue_Check = .false.
          Else If (Alp_90 .AND. Bet_90 .AND. Gam_120) Then
            LatBrav=10 ! Hexagonal
			Continue_Check = .false.
          End If
		End IF
	  End If

	  If (Continue_Check) Then
          If(Ang_Same) Then
			If (Alp_90) Then
				LatBrav=6! Orthorhombic
				Continue_Check = .false.
			Endif
		  End If
	  End If

      If (Continue_Check) Then
          If (Alp_90 .and. Bet_90.and. .not.Gam_90) Then
            LatBrav=5 ! monoclinc c
			Continue_Check = .false.
          Else If (Alp_90 .and. .not.Bet_90.and. Gam_90) Then
            LatBrav=4 ! monoclinic b
			Continue_Check = .false.
          Else If (.not.Alp_90 .and. Bet_90.and. Gam_90) Then
            LatBrav=3 ! monoclinic a
			Continue_Check = .false.
          End If
      End If

	  If (Continue_Check) LatBrav = 2 ! Triclinic

 50   return


	  end subroutine Check_Lattice_Type
!
!
!      subroutine Get_Main_IRadOption()
!
!      USE WINTERACTER
!      USE druid_header
!
!	write(76,*) ' Get_Main_IRadOption ',IDF_LabX_Source,IRadOption
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

    subroutine Set_Main_IRadOption()
!
      USE WINTERACTER
      USE druid_header
!
!	write(76,*) ' Set_Main_IRadOption ',IDF_LabX_Source,IRadOption
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
      end subroutine Set_Main_IRadOption



!C>> Sequence of subroutines that handle the downloading of each field in turn
	  subroutine DownLoadWavelength(From)
	  USE WINTERACTER
      USE druid_header
	  REAL Temp
	  include 'statlog.inc'
	  include 'Lattice.inc'
	  Integer ICurrentSel
	  Integer From

	  ICurrentSel = WinfoDialog(CurrentDialog)
      IF (From .EQ. IDD_Data_Properties) THEN
		Call WDialogSelect(IDD_Data_Properties)
		Call WDialogGetReal(IDF_wavelength1,Temp)
		Call UpdateWavelength(Temp)
	  ELSE IF (From .EQ. IDD_Index_Preparation) THEN
		Call WDialogSelect(IDD_Index_Preparation)
		Call WDialogGetReal(IDF_Indexing_Lambda,Temp)
		Call UpdateWavelength(Temp)
	  END IF
	  IF (ICurrentSel .NE. 0) Call WDialogSelect(ICurrentSel)
	  end subroutine DownLoadWavelength

      subroutine SetWavelengthToSelection(Iselection)
	  include 'Lattice.inc'
	  IF (Iselection.GT.1 .AND. Iselection .LE. NRad_Types + 1) THEN
		  CALL UpdateWavelength(RadWaveLengths(Iselection - 1))
	  ENDIF
	  end subroutine SetWavelengthToSelection

      subroutine UpdateWavelength(Temp)
      USE WINTERACTER
      USE druid_header
	  REAL Temp
	  LOGICAL Ok
	  INTEGER ICurrentSel
      include 'statlog.inc'
	  include 'Lattice.inc'

 
      Ok= (Temp.gt.0.1.and.Temp.lt.20.)
	  if (Ok) then
		alambda = Temp
		wvlnok = .TRUE.
	  endif
	  ICurrentSel = WinfoDialog(CurrentDialog)
	  CALL WDialogSelect(IDD_Data_Properties)
	  CALL WDialogPutReal(IDF_wavelength1,alambda,'(f10.5)')
	  CALL WDialogSelect(IDD_PW_Page2)
	  CALL WDialogPutReal(IDF_PW_wavelength1,alambda,'(f10.5)')
   	  CALL WDialogSelect(IDD_Index_Preparation)
	  CALL WDialogPutReal(IDF_Indexing_Lambda,alambda,'(f10.5)')

	  CALL WDialogSelect(ICurrentSel)

      end subroutine UpdateWavelength



!C>> JCC Not used anymore
!C>> JCC Sets the popup to contain the defaults for the selected lattice 
!      subroutine Set_Main_Crystal_Symmetry()
!
!
!      use Winteracter
!      use druid_header
!
!C>> JCC Lattice declarations now in an include file 
!	  INCLUDE 'Lattice.inc'
!
!      Call WDialogSelect(IDD_Crystal_Symmetry)
!C>> JCC Not needed any more
!Was
! 50	Listbrav=Latbrav+1
! now
! 50   continue
!	write(76,*) ' LatBrav is ',LatBrav
!C>> JCC Was
!      Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! Now
!	  Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
!	 Write(76,*) 'Main crystal system menu option set at',ioption
!      Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!	 Write(76,*) 'Main crystal system menu option ',ioption
!      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      ISG=1
!      IPosSG=LPosSG(IOption)
!      Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,ISG)
!C>> JCC	 Write(76,*) 'Main SG  menu option set at',listbrav
!
!      endsubroutine Set_Main_Crystal_Symmetry
!




!C>> ID Lattice from space group number

  INTEGER FUNCTION LatticeNumber(ISpaceGroup,IDashSg)
  INCLUDE 'Lattice.inc'
  INTEGER,PARAMETER ::  TricLim = 2
  INTEGER,PARAMETER ::  MonoLim = 15
  INTEGER,PARAMETER ::  OrthLim = 74
  INTEGER,PARAMETER ::  TetrLim  = 142
  INTEGER,PARAMETER ::  TrigLim = 167
  INTEGER,PARAMETER ::  HexaLim  = 194

  IF      (ISpaceGroup .LE. TricLim) THEN
	LatticeNumber  = 2
  ELSE IF (ISpaceGroup .LE. MonoLim) THEN
! More work required: a, b or c?
	LatticeNumber = 3
	do i = len_trim(SGNumStr(IDashSg)),1,-1
		SELECT CASE(SGNumStr(IDashSg)(i:i)) 
			CASE ('a')
				LatticeNumber = 3
			CASE ('b')
				LatticeNumber = 4
			CASE ('c')
				LatticeNumber = 5
			CASE (':')
				goto 1
		END SELECT
	end do
1   continue
  ELSE IF (ISpaceGroup .LE. OrthLim) THEN
	LatticeNumber = 6
  ELSE IF (ISpaceGroup .LE. TetrLim) THEN
	LatticeNumber = 7
  ELSE IF (ISpaceGroup .LE. TrigLim) THEN
! More work required - trigonal or rhombohedral
	LatticeNumber = 8
	do i = len_trim(SGNumStr(IDashSg)),1,-1
		SELECT CASE(SGNumStr(IDashSg)(i:i)) 
			CASE ('H')
				LatticeNumber = 8
			CASE ('R')
				LatticeNumber = 9
			CASE (':')
				goto 2
		END SELECT
	end do
2   continue
  ELSE IF (ISpaceGroup .LE. HexaLim) THEN
	LatticeNumber =  10
  ELSE
  ENDIF
  RETURN
  END FUNCTION LatticeNumber


  subroutine SetSAFileName(filename)
  use Winteracter
  use Druid_Header
  character*(*) filename
  integer ICurSel

  ICurSel = WInfoDialog(CurrentDialog)
  Call WDialogSelect(IDD_SA_input1)
  Call WDialogPutString(IDF_SA_Project_Name,filename)
  IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)
  end subroutine SetSAFileName

  subroutine SetWizardState(State)
  USE WINTERACTER
  USE druid_header
  INTEGER, INTENT (IN) :: State
  IF (State .EQ. 1) THEN
    call WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOn)
  ELSE
    call WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOff)
  END IF
  end subroutine SetWizardState

!>> JCC Subroutine for controlling the configuration of the menus and tool buttons in DASH
  subroutine SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
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

	call WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOn)

  ELSE IF (PeakOn .LT. 0) THEN
    call WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOff)
  END IF

  IF (PawleyOn .GT. 0) THEN
	call WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
  ELSE IF (PawleyOn .LT. 0) THEN
	call WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOff)
  END IF

  IF (SolutionOn .GT. 0) THEN
	call WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOn)
  ELSE IF (SolutionOn .LT. 0) THEN
	call WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOff)
  END IF

  return
  end subroutine SetModeMenuState

!>> JCC Subroutine for controlling the selection of the mode menu and tool buttons in DASH
  subroutine SelectModeMenuState(Selection)
  USE WINTERACTER
  USE druid_header
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: Selection

  call WMenuSetState(ID_Peak_Fitting_Mode,ItemChecked,WintOff)
  call WMenuSetState(ID_Pawley_Refinement_Mode,ItemChecked,WintOff)
  call WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOff)

  IF      (Selection .EQ. ID_Peak_Fitting_Mode)       THEN
		call WMenuSetState(ID_Peak_Fitting_Mode,ItemChecked,WintOn)
  ELSE IF (Selection .EQ. ID_Pawley_Refinement_Mode)  THEN 
		call WMenuSetState(ID_Pawley_Refinement_Mode,ItemChecked,WintOn)
  ELSE IF (Selection .EQ. ID_Structure_Solution_Mode) THEN 
		call WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOn)
  END IF

  return
  end subroutine SelectModeMenuState


