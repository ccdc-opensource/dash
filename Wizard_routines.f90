    subroutine PolyFitter_Wizard(NoData)
!   Open the first wizard child windowCheck_Wizard_Crystal_Symmetry
      USE WINTERACTER
      USE druid_header
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
      include 'statlog.inc'
      INCLUDE 'DialogPosCmnf90.inc'
!
!C>> JCC Now these declarations are in an include
	  INCLUDE 'Lattice.inc'
!
      INTEGER :: II,IRadSelection
	  REAL Temp
	  CHARACTER(LEN=20) :: Dummy
	  CHARACTER(LEN=255) :: CTEMP
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
 8	do i=1,6
	  gotcell(i)=.false.
	end do
	BackFrom2=.false.

	CALL ToggleMenus(0)
!
 1    continue
!C>> JCC      CALL WDialogLoad(IDD_Polyfitter_Wizard_01)
!	  CALL WDialogHide()
      CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)

! Now check on which button was pressed ...
      DO WHILE(.NOT.SKIP)
        IXPos_IDD_Wizard   = WInfoDialog(6)
        IYPos_IDD_Wizard   = WInfoDialog(7)
        IWidth_IDD_Wizard  = WInfoDialog(8)
        IHeight_IDD_Wizard = WInfoDialog(9)
          CALL WMessage(ITYPE,MESSAGE)
! Process main window messages
		  IF (MESSAGE%WIN .EQ. 0) THEN
				QUIT = process_mainwindow_message(ITYPE,MESSAGE)
				CYCLE
		  END IF
!	write(76,*) ' Wizard loop 1',itype,Message%Value1,Message%Value2,skip
          CALL WDialogGetRadioButton(IDF_PW_Option1,IPW_Option)
!	write(76,*) ' Wizard Option ',IPW_Option
          SELECT CASE (ITYPE)
              CASE (Expose,Resize)
                  CALL Redraw()
              CASE (PushButton)
                IDNumber=Message%Value1
!	write(76,*) ' IDNumber in Wizard Loop 1 is ',IDNUMBER
                SELECT CASE (IDNumber)
                  CASE(IDF_PW0_Next)
!C>> JCC                   CALL WDialogUnload()
					CALL WDialogHide()
! We're off the main page and on to new pages depending on the option.
                    SA_Option_Requested=.false.
                    SELECT CASE (IPW_Option)
                      CASE(1) ! view data/ determine peak positions
                        GOTO 300
                      CASE(2) ! Pawley
                        GOTO 100
                      CASE(3) ! Simulated Annealing
                        SA_Option_Requested=.true. 
                        GOTO 500
                      CASE(4) ! Read Druid pawley file
                        GOTO 600
                    END SELECT
                  CASE(IDF_PW0_Skip, IDCANCEL)
                    SKIP=.TRUE.
!C>> JCC                   CALL WDialogUnload()
					CALL WDialogHide()
					CALL ToggleMenus(1)
                    RETURN
                END SELECT
          END SELECT
      END DO
	  CALL ToggleMenus(1)
      RETURN
!
!.. Deal here with page 1 (cases 2 and 3)
 100  SKIP=.false.
!C>> JCC      CALL WDialogLoad(IDD_PW_Page1)
!	  CALL WDialogHide()
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)

!C>> JCC None of this is necessary anymore since the widgets are never unloaded
!c      If(BackFrom2) then
!C        do ii=1,6
!c          GotCell(ii)=GotCell(ii) .and. CellPar(ii).gt.0.
!C        end do 
!C        if (GotCell(1)) Call WDialogPutReal(IDF_PW_a_latt,CellPar(1),'(F10.5)')
!C        if (GotCell(2)) Call WDialogPutReal(IDF_PW_b_latt,CellPar(2),'(F10.5)')      
!C        if (GotCell(3)) Call WDialogPutReal(IDF_PW_c_latt,CellPar(3),'(F10.5)')      
!C        if (GotCell(4)) Call WDialogPutReal(IDF_PW_alp_latt,CellPar(4),'(F10.3)')      
!C        if (GotCell(5)) Call WDialogPutReal(IDF_PW_bet_latt,CellPar(5),'(F10.3)')      
!C        if (GotCell(6)) Call WDialogPutReal(IDF_PW_gam_latt,CellPar(6),'(F10.3)')
!C      else
!C	   do i=1,6
!C	     GotCell(i)=.false.
!C	   end do
!C        CALL Default_Crystal_Symmetry()
!C      end if
      BackFrom2=.false.
      DO WHILE(.NOT.SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        IWidth_IDD_Wizard = WInfoDialog(8)
        IHeight_IDD_Wizard = WInfoDialog(9)
!         call PolyFitter_Wizard_Check_Status(NoData)
!         call Check_Wizard_Crystal_Symmetry()
          CALL WMessage(ITYPE,MESSAGE)

! Process main window messages
		  IF (MESSAGE%WIN .EQ. 0) THEN
				QUIT = process_mainwindow_message(ITYPE,MESSAGE)
				CYCLE
		  END IF

!	write(76,*) ' Wizard loop 2',itype,Message%Value1,Message%Value2
          SELECT CASE (ITYPE)
              CASE (Expose,Resize)
                  CALL Redraw()
!C>> JCC
				  CALL WDialogSelect(IDD_PW_Page1)
              CASE (PushButton)
                IDNumber=Message%Value1
                SELECT CASE (IDNumber)
                  CASE(IDF_PW1_Back)
!C>> JCC                    CALL WDialogUnload()
					call WDialogHide()
                    GOTO 1
                  CASE(IDF_PW1_Next)
!	write(76,*) ' CellPar ',(cellpar(i),i=1,6)
!C>> JCC                    CALL WDialogUnload()
! We're off the second page and on to the last page
!C>> JCC
					call WDialogHide()
!C>>					CALL WDialogSelect(IDD_PW_Page1)
                    Goto 200
                  CASE(IDF_PW1_Skip)
                    SKIP=.TRUE.
!C>> JCC                     CALL WDialogUnload()
					CALL WDialogSelect(IDD_PW_Page1)
					CALL WDialogHide()
					CALL ToggleMenus(1)
                    RETURN
                END SELECT
              CASE(FieldChanged)
                IDNumber=Message%Value1
                SELECT CASE (IDNumber)
                  CASE (IDF_PW_Space_Group_Menu)
					Call Update_Space_Group(IDD_PW_Page1, IDummy, IDummy)
                  CASE (IDF_PW_Crystal_System_Menu)
					Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
					LatBrav = IOption
					Call Set_Crystal_Symmetry(LatBrav)
                    Call Set_Space_Group(IDD_PW_Page1)
                  CASE(IDF_PW_a_latt)
                    Call WDialogGetReal(IDF_PW_a_latt,CellPar(1))
                    GOTCELL(1)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                  CASE(IDF_PW_b_latt)
                    CALL WDialogGetReal(IDF_PW_b_latt,CellPar(2))
                    GOTCELL(2)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                  CASE(IDF_PW_c_latt)
                    CALL WDialogGetReal(IDF_PW_c_latt,CellPar(3))
                    GOTCELL(3)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                  CASE(IDF_PW_alp_latt)
                    CALL WDialogGetReal(IDF_PW_alp_latt,CellPar(4))
                    GOTCELL(4)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                  CASE(IDF_PW_bet_latt)
                    CALL WDialogGetReal(IDF_PW_bet_latt,CellPar(5))
                    GOTCELL(5)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                  CASE(IDF_PW_gam_latt)
                    CALL WDialogGetReal(IDF_PW_gam_latt,CellPar(6))
                    GOTCELL(6)=.TRUE.
					Call UpdateCell(IDD_PW_Page1)
                END SELECT                
          END SELECT
      END DO
!
!.. Deal here with page 2
 200 SKIP=.false.
!C>> JCC     CALL WDialogLoad(IDD_PW_Page2)
!	 CALL WDialogHide()

     CALL WDialogSelect(IDD_PW_Page2)
!
	 IF (SourceState.EQ.0) THEN
		 CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
!C>> JCC		 CALL WDialogPutRadioButton(IDF_PW_single_wavelength)
		 SourceState = 1
	 END IF
	 CALL SetSourceDataState(SourceState)
     CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT.SKIP)
        IXPos_IDD_Wizard   = WInfoDialog(6)
        IYPos_IDD_Wizard   = WInfoDialog(7)
        IWidth_IDD_Wizard  = WInfoDialog(8)
        IHeight_IDD_Wizard = WInfoDialog(9)
          CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Wizard loop 3',itype,Message%Value1,Message%Value2
          CALL WDialogGetRadioButton(IDF_PW_LabX_Source,IRadOption)
! Attempting to write to the main view windows
!C>> JCC          CALL WDialogLoad(IDD_Data_Properties)
!		  CALL WDialogHide()
!		  CALL WDialogSelect(IDD_Data_Properties)
!		  CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,SemiModeless)

!C>> JCC Now use a subroutine to set the state

! Process main window messages
		  IF (MESSAGE%WIN .EQ. 0) THEN
				QUIT = process_mainwindow_message(ITYPE,MESSAGE)
				CYCLE
		  END IF

		  CALL SetSourceDataState(IRadOption)
		  SourceState = IRadOption
          SELECT CASE (ITYPE)
              CASE (Expose,Resize)
                  CALL Redraw()
              CASE(FieldChanged)
                IDNumber=Message%Value1
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
                IDNumber=Message%Value1
                SELECT CASE (IDNumber)
                  CASE(ID_PW_DF_Open)
!C>> JCC Add callback for Open button
			          CALL WDialogGetString(IDF_PW_DataFileName_String,CTEMP)
					  CALL XYEPRO_file_Read(CTEMP,NoData)
!C>> JCC Have to reselect after the file open
				    CALL WDialogSelect(IDD_PW_Page2)
                  CASE(ID_PW_DF_Browse)
                    call XYEPRO_file_Open(NoData)
!C>> JCC Have to reselect after the file open
				    CALL WDialogSelect(IDD_PW_Page2)
                  CASE(IDF_PW2_Back)
!C>> JCC                    CALL WDialogUnload()
                    BackFrom2=.true.
					CALL WDialogHide()
                    GOTO 100
                  CASE(IDF_PW2_Skip)
                    SKIP=.TRUE.
!C>> JCC                    CALL WDialogUnload()
					CALL WDialogHide()
					CALL ToggleMenus(1)
                    RETURN
!C>> JCC No longer present
!                 CASE(IDF_PW2_Finish)
!                    SKIP=.TRUE.
!C>> JCC                    CALL WDialogUnload()
!					CALL WDialogHide()
!                    RETURN
                END SELECT
          END SELECT
      END DO
!
!.. Deal here with page 3 - only visited for options 1 & 2
 300 SKIP=.false.
!C>> JCC    CALL WDialogLoad(IDD_PW_Page3)
!C	 CALL WDialogHide()
     CALL WDialogSelect(IDD_PW_Page3)
     CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      DO WHILE(.NOT.SKIP)
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
        IWidth_IDD_Wizard = WInfoDialog(8)
        IHeight_IDD_Wizard = WInfoDialog(9)
          CALL WMessage(ITYPE,MESSAGE)
! Process main window messages
		  IF (MESSAGE%WIN .EQ. 0) THEN
				QUIT = process_mainwindow_message(ITYPE,MESSAGE)
				CYCLE
		  END IF

!	write(76,*) ' Wizard loop 4',itype,Message%Value1,Message%Value2
          SELECT CASE (ITYPE)
		
              CASE (Expose,Resize)
                  CALL Redraw()
              CASE (PushButton)
                IDNumber=Message%Value1
                SELECT CASE (IDNumber)
!C>> JCC Add callback for Open button
				  CASE(ID_PWa_DF_Open)
			          CALL WDialogGetString(IDF_PWa_DataFileName_String,CTEMP)
					  CALL XYEPRO_file_Read(CTEMP,NoData)
!C>> JCC Need to reselect dialogue after the action
					CALL WDialogSelect(IDD_PW_Page3)
                  CASE(ID_PWa_DF_Browse)
                    call XYEPRO_file_Open(NoData)
!C>> JCC Need to reselect dialogue after the action
					CALL WDialogSelect(IDD_PW_Page3)
                  CASE(IDF_PW3_Back)
!C>>JCC                    CALL WDialogUnload()
					CALL WDialogHide()
                    GOTO 1
!C>>                  CASE(IDF_PW3_Skip)
!C>>                    SKIP=.TRUE.
!C>>JCC                    CALL WDialogUnload()
					CALL WDialogHide()
                  CASE(IDF_PW3_Finish)
                    SKIP=.TRUE.
!C>>JCC                    CALL WDialogUnload()
					CALL WDialogHide()
					CALL ToggleMenus(1)
                    RETURN
                END SELECT
          END SELECT
      END DO
!
!.. Deal with the simulated annealing ...
 500  SKIP=.false.
        IXPos_IDD_SA_Input = IXPos_IDD_Wizard
        IYPos_IDD_SA_Input = IYPos_IDD_Wizard
        IWidth_IDD_SA_Input = IWidth_IDD_Wizard
        IHeight_IDD_SA_Input = IHeight_IDD_Wizard
      call SA_MAIN()
!	 write(56,*) ' Out of SA_Main in the wizard routines '
      goto 8
!      return
 600    IXPos_IDD_SA_Input = WInfoDialog(6)
        IYPos_IDD_SA_Input = WInfoDialog(7)
        IWidth_IDD_SA_Input = WInfoDialog(8)
        IHeight_IDD_SA_Input = WInfoDialog(9)
      call SA_MAIN()
!
      CALL ToggleMenus(1)
      RETURN

!
     end subroutine PolyFitter_Wizard
!
!
     subroutine PolyFitter_Wizard_Check_Status()
!
!   Open the first wizard child window
      USE WINTERACTER
      USE druid_header
      IMPLICIT NONE
!
!   Type declarations
!
      TYPE(WIN_MESSAGE) :: MESSAGE
!
!   Variable declarations
!
      LOGICAL           :: SKIP    = .FALSE.
      INTEGER           :: I,ITYPE,IDNUMBER,IPW_Option
      REAL :: CELLPAR,ZEROPOINT,ALAMBDA
      REAL :: a,b,c,d2r,calp,cbet,cgam,arg,vcell
      COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA 
      include 'statlog.inc'
!
	cellok = .true.
	do i = 1,6
		cellok = cellok .and. cellpar(i) .gt. 0.
	end do
	if (.not.cellok) return
! Calculate the unit cell volume - if sensible upload lattice constants
	d2r=atan(1.)/45.
     a=cellpar(1)
	b=cellpar(2)
	c=cellpar(3)
     calp=cos(d2r*cellpar(4))
     cbet=cos(d2r*cellpar(5))
     cgam=cos(d2r*cellpar(6))
	arg=1.+2.*calp*cbet*cgam-(calp**2+cbet**2+cgam**2)
    VCELL=a*b*c*sqrt(max(0.,arg))
!C JCC add checks
    cellok= cellok .AND. (VCELL.gt.10.) 
!
	return
!
     end subroutine PolyFitter_Wizard_Check_Status

!
!
!

!      subroutine Check_Wizard_Crystal_Symmetry()
!
!
!      use Winteracter
!      use Druid_Header
!	  INCLUDE 'Lattice.inc'
!	  INTEGER OldLatBrav,ICurSel
!

!	  ICurSel = WInfoDialog(CurrentDialog)
!      Call WDialogSelect(IDD_PW_Page1)
!      Call WDialogGetReal(IDF_PW_a_latt,CellPar(1))
!      Call WDialogGetReal(IDF_PW_b_latt,CellPar(2))      
!      Call WDialogGetReal(IDF_PW_c_latt,CellPar(3))      
!      Call WDialogGetReal(IDF_PW_alp_latt,CellPar(4))      
!      Call WDialogGetReal(IDF_PW_bet_latt,CellPar(5))      
!      Call WDialogGetReal(IDF_PW_gam_latt,CellPar(6))

!C>> JCC Have one routine to handle this now

!	  OldLatBrav = LatBrav
!90	  Call Check_Lattice_Type
!	  IF (OldLatBrav .NE. LatBrav) THEN
!		CALL Set_Crystal_Symmetry(LatBrav)
!	  END IF
!
!	  IF (ICurSel .NE. 0) CALL WDialogSelect(ICurSel)	  
!      endsubroutine Check_Wizard_Crystal_Symmetry
!
!
!
!
      subroutine Default_Crystal_Symmetry()
!
!
      use Winteracter
      use druid_header
!
!
	  include 'Lattice.inc'
!
      include 'statlog.inc'
	  INTEGER ICurSel
!

	  ICurSel = WInfoDialog(CurrentDialog)


!C>> JCC No longer needed	Listbrav=1
 	  Ioption=1
!
!C>> JCC Was
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! now

	  Call WDialogSelect(IDD_PW_Page1)
      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
      Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
	  Call WDialogSelect(IDD_Crystal_Symmetry)
      Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)

      Do ISG=1,NumSG
         JSG=LPosSG(IOption)+ISG-1
         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
      End Do      
      IPosSG=1
      NumberSGTable=IPosSG
!C>> JCC Was
!      Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,ListBrav)
! now
	 Call WDialogSelect(IDD_PW_Page1)
	 Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
	 Call WDialogSelect(IDD_Crystal_Symmetry)
	 Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,LatBrav)
	 IF (ICurSel .NE. 0) Call WDialogSelect(ICurSel)

      endsubroutine Default_Crystal_Symmetry
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
! 50	Listbrav=Latbrav+1
! 50 continue
!	write(76,*) ' LatBrav is ',LatBrav
!C>> JCC Changed from
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! to
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
!	 Write(76,*) 'Wizard crystal system menu option set at',ioption
!      Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!	 Write(76,*) 'Wizard crystal system menu option ',ioption
!      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      ISG=1
!      IPosSG=LPosSG(IOption)
!      NumberSGTable=IPosSG
!	write(76,*) ' IPosSG, ISPosSG is ',IPosSG,ISPosSG,NumberSGTable
!      Call WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumSG,ISG)
!	 Write(76,*) 'Wizard SG  menu option set at',listbrav
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
!	  include 'Lattice.inc'
!
!
!      include 'statlog.inc'
!
!      Call WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!      ISPosSG=1
!      NumBrSG=LPosSG(IOption+1)-LPosSG(IOption)
!	write(76,*) ' lpossg ',LPosSG(IOption),LPosSG(IOption+1)
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
!
      subroutine Upload_Wizard_Information()
!
!
      use Winteracter
      use druid_header
!
!C>> JCC            CALL WDialogLoad(IDD_Structural_Information)
!			CALL WDialogHide()
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Crystal_Symmetry)
            CALL WDialogSelect(IDD_Crystal_Symmetry)
            CALL Upload_Crystal_Symmetry()
!            call WDialogUnload()
!
!C>> JCC             CALL WDialogLoad(IDD_Structural_Information)
!			CALL WDialogHide()
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Data_Properties)
            CALL WDialogSelect(IDD_Data_Properties)
            CALL Upload_Data_Properties()
            CALL Upload_Range()
!            call WDialogUnload()
!
      end subroutine Upload_Wizard_Information      


!C>> JCC Subroutines to set the state of the page 2 wizard to either synchrotron or lab data 
!C>> JCC Subroutine to set the state of the page 2 wizard to either synchrotron or lab data 

	  subroutine SetSourceDataState(IRadOption)

	  use Winteracter
	  use druid_header 

	  integer IRadOption, JRadOption
	  common /RadOption/ JRadOption
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
              If (IradOption.eq.2) then
                CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
              Else
                CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
              End If
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
              If (IradOption.eq.2) then
                CALL WDialogPutRadioButton(IDF_SynX_Source)
              Else
                CALL WDialogPutRadioButton(IDF_CWN_Source)
              End If
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

