!
!
  SUBROUTINE SA_MAIN()
!
  USE WINTERACTER
  USE DRUID_HEADER
  USE VARIABLES
!
! Define some parameters to match those in the resource file
!
!
! Declare window-type and message variables
!
  TYPE(WIN_STYLE)    WINDOW
  TYPE(WIN_MESSAGE)  MESSAGE
  character*80 SDIFile
  character*80 pikfile,ticfile,hcvfile
  real         rpos
  integer      ipos,istart,iend,npar
  integer      IFlags

  include 'IZMCheck.inc'
  integer IDFZMCheck(CheckSize),IZMNumber(CheckSize)

  integer      IDBZMBrowse(CheckSize),IDFZMpars(CheckSize),IDFZMFile(CheckSize)
  integer      II
  include 'DialogPosCmnF90.inc'
  include 'statlog.inc'
!
!
      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,CSH,FSTAR,XP,FOPT
      common /sacmn/ XOPT(NMAX),CSH(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT
!
      INTEGER  NACP(NMAX), NS, NT, NFCNEV, IER, ISEED1, ISEED2
      INTEGER MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAXLOG,RESTART,QUIT,MAKET0
!
	  LOGICAL :: NODATA = .FALSE.
      character*132 line
      character*80  sa_file
      logical   log_inf_file,log_nvar,log_bounds,log_reduce
      logical log_eps,log_ns,log_nt,log_neps,log_maxevl,log_iprint
      logical log_iseed1,log_iseed2,log_T0,log_target_value
      logical log_frag_file
      double precision cen,sig
      logical gaussb
      character*80  inf_file,zm_file
      double precision T,T0,rt,eps,target_value
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

	  double precision prevub, prevlb ! For saving the previous range
	  common /pvalues/ prevub(mvar), prevlb(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
!
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
!
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
     al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
     occ(maxatm,maxfrg)
      integer nfrag,lfrag
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),&
            czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
!
      COMMON /PLTYPE/ IPTYPE
      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150),&
      TF(150),KTF(150),SITE(150),KSITE(150),&
      ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
!
!C>> JCC Added in declarations
! The implementation has changed - this is now a function
	  INTEGER Read_One_Zm
	  INTEGER zmread,ilenf
	  INTEGER NextEnabled
	  LOGICAL FExists
	  LOGICAL LimsChanged
	  DATA LimsChanged / .FALSE. /
	  SAVE LimsChanged
	  LOGICAL ZmStateChanged
	  LOGICAL CheckWhat
	  LOGICAL NoZmatrix

  IHANDLE = 0
!
!  Load up our SA control dialogs
!
!C>> JCC   CALL WDialogLoad(IDD_SA_input1)
!C>> JCC   CALL WDialogLoad(IDD_SA_input2)
!C>> JCC   CALL WDialogLoad(IDD_SA_input3)
  CALL WMessageEnable(FieldChanged,1)
!
!
!.. If FromPawleyFit read in the HCV, PIK and TIC files from POLYP
    If (FromPawleyFit) then
     pikfile=DashPikFile
     ticfile=DashTicFile
     hcvfile=DashHcvFile
     CALL GETPIK(pikfile,len_trim(pikfile),IER)
     CALL GET_LOGREF(ticfile,len_trim(ticfile),IER)
     CALL GETHCV(hcvfile,len_trim(hcvfile),IER)
    End If
  CALL SA_Defaults()
!
  IDFZMCheck(1)=IDF_ZM_file_check1
  IDFZMCheck(2)=IDF_ZM_file_check2
  IDFZMCheck(3)=IDF_ZM_file_check3
  IDFZMCheck(4)=IDF_ZM_file_check4
  IDFZMCheck(5)=IDF_ZM_file_check5
  IDFZMFile(1)=IDF_ZMatrix_file1
  IDFZMFile(2)=IDF_ZMatrix_file2
  IDFZMFile(3)=IDF_ZMatrix_file3
  IDFZMFile(4)=IDF_ZMatrix_file4
  IDFZMFile(5)=IDF_ZMatrix_file5
  IDBZMBrowse(1)=IDB_ZMatrix_Browse1
  IDBZMBrowse(2)=IDB_ZMatrix_Browse2
  IDBZMBrowse(3)=IDB_ZMatrix_Browse3
  IDBZMBrowse(4)=IDB_ZMatrix_Browse4
  IDBZMBrowse(5)=IDB_ZMatrix_Browse5
  IDFZMpars(1)=IDF_ZM_pars1
  IDFZMpars(2)=IDF_ZM_pars2
  IDFZMpars(3)=IDF_ZM_pars3
  IDFZMpars(4)=IDF_ZM_pars4
  IDFZMpars(5)=IDF_ZM_pars5

!>> JCC 
!>> This following section of code makes no sense, and causes array bounds to exceed
!>>
!  DO ii=1,5
!    IZMNumber(IDBZMBrowse(ii))=ii
!  End Do
!>>
!>> So I've changed it to what I think it should be ..., i.e. a copy of the flags used

  DO ii=1,5
    IZMNumber(ii)=IDBZMBrowse(ii)
  End Do

!
! Message loop for the first SA window
!
    CALL WDialogSelect(IDD_SA_input1)
!C>> JCC I've moved this lot to a separate subroutine. We want to 
!C>> Remember the files/data etc, so only call this on first entry

	Call ClearZmatrices(CheckSize,IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse,IZMCheck)
	NoZmatrix = .TRUE.
 222 Continue
    CALL WDialogSelect(IDD_SA_input1)
    If (FromPawleyFit) then
!C>> JCC Added
		NoData = .FALSE.
        Call WDialogClearField(IDF_SA_Project_Name)
        Call WDialogFieldState(IDF_SA_Project_Name,Disabled)
        Call WDialogFieldState(IDB_SA_Project_Browse,Disabled)
		Call WDialogFieldState(IDB_SA_Project_Open,Disabled)
        Call WDialogFieldState(IDF_SA_project_name_label,Disabled)
    Else
        Call WDialogFieldState(IDF_SA_Project_Name,Enabled)
        Call WDialogFieldState(IDB_SA_Project_Browse,Enabled)
		Call WDialogFieldState(IDB_SA_Project_Open,Enabled)
        Call WDialogFieldState(IDF_SA_project_name_label,Enabled)
    End If
    IF (ConvOn) THEN
		Call WDialogFieldState(IDF_SA_Project_Import,Enabled)
	ELSE
		Call WDialogFieldState(IDF_SA_Project_Import,Disabled)
	END IF

! Let's check the z-matrix check boxes
    Do II=1,5
      call WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
    End Do
!
!
     ZmStateChanged = .TRUE.
     CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,SemiModeless)
  DO                                 ! Loop until user terminates

! Let's check the z-matrix check boxes
    IF (ZmStateChanged) THEN
     Do II=1,5
      call WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
     End Do

     Do II=1,5
      If (IZMCheck(II).eq.Checked .OR. gotzmfile(II) ) then
        Call WDialogFieldState(IDFZMFile(II),Enabled)
        Call WDialogFieldState(IDBZMBrowse(II),Enabled)
        Call WDialogFieldState(IDFZMPars(II),Enabled)
		NextEnabled = II+1
      Else
        Call WDialogFieldState(IDFZMFile(II),Disabled)
        Call WDialogFieldState(IDBZMBrowse(II),Disabled)
        Call WDialogFieldState(IDFZMPars(II),Disabled)
        if (ii.lt.5) then
         do jj=ii+1,5
          Call WDialogFieldState(IDFZMCheck(jj),Disabled)
          Call WDialogPutCheckBox(IDFZMCheck(jj),Unchecked)
          Call WDialogFieldState(IDFZMFile(jj),Disabled)
          Call WDialogFieldState(IDBZMBrowse(jj),Disabled)
          Call WDialogFieldState(IDFZMPars(jj),Disabled)
         end do
        end if
      End If
     End Do

	 If (NextEnabled .LT. 5) then
		Call WDialogPutCheckBox(IDFZMCheck(NextEnabled),Unchecked)
	 	Call WDialogPutCheckBox(IDFZMCheck(NextEnabled),Unchecked)
	 End If
	 ZmStateChanged = .FALSE.
	END IF

 	IF (NoZmatrix) THEN
	    Call WDialogSelect(IDD_SA_input1)
		Call WDialogFieldState(IDB_SA1_next,Disabled)
	ELSE
		Call WDialogSelect(IDD_SA_input1)
		Call WDialogFieldState(IDB_SA1_next,Enabled)
	END IF
! Start the message loop
    IXPos_IDD_SA_Input = WInfoDialog(6)
    IYPos_IDD_SA_Input = WInfoDialog(7)
    IWidth_IDD_SA_Input = WInfoDialog(8)
    IHeight_IDD_SA_Input = WInfoDialog(9)
    CALL WMessage(ITYPE, MESSAGE)

 10 CONTINUE

! Enable or disable the "Next" button
 	IF (NoZmatrix) THEN
	    Call WDialogSelect(IDD_SA_input1)
		Call WDialogFieldState(IDB_SA1_next,Disabled)
	ELSE
		Call WDialogSelect(IDD_SA_input1)
		Call WDialogFieldState(IDB_SA1_next,Enabled)
	END IF

    SELECT CASE (ITYPE)
!.. Interact with the main window and look at the Pawley refinement...
      CASE (MouseButDown)
         CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
      CASE (KeyDown)
         CALL Check_KeyDown(MESSAGE)
	  CASE (Expose, Resize)
	    IF (MESSAGE%WIN .EQ. 0) THEN
!		   WRITE (6,*) MESSAGE%GX, MESSAGE%GY, MESSAGE%VALUE1, MESSAGE%VALUE2
           CALL Redraw()
		END IF
      CASE (PushButton)
         SELECT CASE (Message%Value1)
!C>> JCC Add in new 'clear' button
			 CASE (IDF_clear_zmatrix)
			   Call ClearZmatrices(CheckSize,IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse,IZMCheck)
			   NoZmatrix = .TRUE.
			   ZmStateChanged = .TRUE.					   
             CASE (IDF_SA1_cancel)
! Go back to the Pawley refinement or the initial wizard
			   CALL WDialogSelect(IDD_SA_input1)
               Call WDialogHide()
			   IPType = 2
               Return
            CASE (IDB_SA1_next)
! Go to the next stage of the SA input
			   CALL WDialogSelect(IDD_SA_input1)
               Call WDialogHide()
               Call SA_Parameter_Set(CheckSize,IZMCheck)
               Goto 444
            CASE (IDB_SA_Project_Browse)
! Look for a .SDI file which will contain the following format
! PIK <file1>.pik
! HCV <file2>.hcv
! TIC <file3>.tic
             If (.not.FromPawleyFit) then
               CALL WDialogSelect(IDD_SA_input1)
               SDIFile = ' '
               CALL WDialogPutString(IDF_SA_Project_Name,SDIFile)
               IFlags=PromptOn+DirChange+AppendExt
               CALL WSelectFile('Diffraction information file (*.sdi)|*.sdi|', &
                       IFlags,SDIFile,'Load diffraction information file')
			   ilenf = LEN_TRIM(SDIFile)
			   IF (ilenf .GT. 0) THEN
			     INQUIRE(FILE=SDIFile(1:Ilenf),EXIST=FExists)
			     IF (.NOT.FExists) THEN
				  Call WMessageBox(OkOnly,ExclamationIcon,CommonOk,&
				  "The file "//SDIFile(1:Ilenf)//" does not exist!",&
				  "No such file")
				  ilenf = 0 ! Dont read it if it doesnt exist
			     ENDIF
			   END IF
			   IF ( ilenf .NE.0 ) THEN
					NoData = .TRUE.
					CALL OpenHCVPIKTIC(SDIFile,0,NoData)
					IF (NODATA) THEN
						CALL WMessageBox(OKOnly,ExclamationIcon, CommonOk,&
						"Could not read the pawley file "//SDIFile(:ilenf)//&
						CHAR(13)//"successfully."//CHAR(13)//&
						"If you have moved this project to a new directory you may "//CHAR(13)//&
						"need to edit the file names in the pawley file","Failed to read project")
!					ELSE
!						CALL WDialogPutString(IDF_SA_Project_Name,SDIFile)
					END IF
			   END IF
             End If
!>> JCC Open
			CASE (IDB_SA_Project_Open)
			   CALL WDialogSelect(IDD_SA_input1)
               SDIFile = ' '
               CALL WDialogGetString(IDF_SA_Project_Name,SDIFile)
			   ilenf = LEN_TRIM(SDIFile)
			   INQUIRE(FILE=SDIFile(1:Ilenf),EXIST=FExists)
			   IF (.NOT.FExists) THEN
				Call WMessageBox(OkOnly,ExclamationIcon,CommonOk,&
				"The file "//SDIFile(1:Ilenf)//" does not exist!",&
				"No such file")
				ilenf = 0 ! Dont read it if it doesnt exist
			   ENDIF
			   IF ( ilenf .NE.0 ) THEN
					NoData = .TRUE.
					CALL OpenHCVPIKTIC(SDIFile,0,NoData)
					IF (NODATA) THEN
						CALL WMessageBox(OKOnly,ExclamationIcon, CommonOk,&
						"Could not read the pawley file "//SDIFile(:ilenf)//&
						CHAR(13)//"successfully","Failed to read project")
					END IF
			   END IF

			CASE (IDB_SA_Project_Import)
!>> JCC Import .. convert a mol/pdb/mol2 file into a zmatrix

				CALL ImportZmatrix

            CASE (IDB_ZMatrix_Browse1,IDB_ZMatrix_Browse2,IDB_ZMatrix_Browse3,&
                  IDB_ZMatrix_Browse4,IDB_ZMatrix_Browse5)
			  ZmStateChanged = .TRUE.
!>> JCC This doesnt work Im afraid: Need to loop through and find the message
!              ifrg=IZMNumber(MESSAGE%VALUE1)
			  ifrg = 1
			  do while (ifrg .LT. 5 .AND. IZMNumber(ifrg) .NE. MESSAGE%VALUE1)
				ifrg = ifrg + 1
			  enddo

              gotzmfile(ifrg)=.false.
              frag_file(ifrg) = ' '
              IFlags=PromptOn+DirChange+AppendExt
              CALL WSelectFile('z-matrix file (*.zmatrix)|*.zmatrix|', &
                   IFlags,frag_file(ifrg),'Load z-matrix file')
!C>> JCC Need to check here to see if the user hit cancel
! So I added a check here
			  if (frag_file(ifrg) .NE. ' ') then
               lfrag(ifrg)=len_trim(frag_file(ifrg))
 
			   zmread = Read_One_ZM(ifrg)
               If ( zmread .EQ. 0) then ! successful read
			    CALL WDialogSelect(IDD_SA_input1)
                CALL WDialogPutString(IDFZMFile(ifrg),frag_file(ifrg))
! done within Read_One_ZM >>              gotzmfile(ifrg)=.true.
!>> Now update the front end widget
				Do II=1,5
					call WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
				End Do
			    CALL UpdateZmatrixSelection(CheckSize, IZMCheck, IDFZMPars)
! Set the next dialogue on
                If (ifrg.lt.5) then
                  Call WDialogFieldState(IDFZMCheck(ifrg+1),Enabled)
				  Call WDialogFieldState(IDFZMCheck(ifrg+1),Enabled)
                End If
!>> JCC traps for zmatrix reading
				NoZmatrix = .FALSE.
			   Else 
				call FileErrorPopup(frag_file(ifrg),zmread)
			   End If ! If the read on the zmatrix was ok
		      End If  ! If the user selected a file
!>> JCC Also act on selection of check box
		END SELECT
	 CASE (FieldChanged)
		SELECT CASE(Message%Value1)
          CASE (IDF_ZM_file_check1,IDF_ZM_file_check2,IDF_ZM_file_check3,&
                IDF_ZM_file_check4,IDF_ZM_file_check5)
! Update the selection
			Do II=1,5
				call WDialogGetCheckBox(IDFZMCheck(ii),IZMCheck(ii))
			End Do
			CALL UpdateZmatrixSelection(CheckSize, IZMCheck, IDFZMPars)
!
			ZmStateChanged = .TRUE. 	 
         End Select
     End Select
  End Do
!
!.. We are now on window number 2
 444 Call WDialogSelect(IDD_SA_input2)
     CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)
   DO                                 ! Loop until user terminates
     IXPos_IDD_SA_Input = WInfoDialog(6)
     IYPos_IDD_SA_Input = WInfoDialog(7)
     IWidth_IDD_SA_Input = WInfoDialog(8)
     IHeight_IDD_SA_Input = WInfoDialog(9)
    CALL WMessage(ITYPE, MESSAGE)
!       write(76,*) ' In 2nd SA window with itype & value1 ',ITYPE,MESSAGE%VALUE1
!
    SELECT CASE (ITYPE)
!.. Interact with the main window and look at the Pawley refinement...
	  CASE (Expose, Resize)
	    IF (MESSAGE%WIN .EQ. 0) THEN
			CALL Redraw()
		END IF
      CASE (MouseButDown)
            CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
      CASE (KeyDown)
             CALL Check_KeyDown(MESSAGE)
!
      CASE (PushButton)
         SELECT CASE (Message%Value1)
             CASE (IDF_SA2_cancel)
! Go back to the Pawley refinement or the initial wizard
               Call WDialogHide()
               IPType=2
               Return
             CASE (IDB_SA2_back)
! Go back to the 1st window
!>> JCC Check if the limits have changed and warn about it 

			   IF (LimsChanged) THEN
				    Call WMessageBox(OKCancel,ExclamationIcon, CommonOk,&
						"Note: Going back will erase the edits made to the current parameters",&
						"Overwrite changes?")
					IF (WInfoDialog(4) .EQ. 1) THEN
						LimsChanged = .FALSE.
					END IF
			   END IF
					
			   IF (.NOT.LimsChanged ) THEN
					Call WDialogHide()
					Goto 222
			   END IF

            CASE (IDB_SA2_next)
! Go to the next stage of the SA input
               Call WDialogHide() 
!C>> JCC               Call SA_Parameter_Update(CheckSize,IZMCheck)
               Goto 777
         END SELECT
!
      CASE (FieldChanged)
        SELECT CASE (Message%Value1)
           CASE (IDF_parameter_grid)
             CALL WGridPos(MESSAGE%X,IFCol,IFRow)
             Select Case (IFCol)
                Case(1)
!.. parameter
                 Call WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
				 IF (ICHK .EQ. Checked) THEN
                   Call WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                   xtem=max(sngl(lb(IFRow)),xtem)
				   IF ( ABS(xtem - x(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                   X(IFRow)=dble(min(sngl(ub(IFRow)),xtem))
				   CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,sngl(x(IFRow)),'(F12.5)')
				 END IF
                Case(2)
!.. lower bound
                  Call WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
				  IF (ICHK .EQ. Checked) THEN
                    Call WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem=min(sngl(ub(IFRow)),xtem)
				    IF ( ABS(xtem - lb(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
				    lb(IFRow)=dble(xtem)
				    prevlb(IFRow) = lb(IFRow)
					CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,sngl(lb(IFRow)),'(F12.5)')
					xtem=max(lb(IFRow),x(IFRow))
					X(IFRow)=dble(xtem)
					CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,sngl(x(IFRow)),'(F12.5)')
				  END IF
                Case(3)
!.. upper bound
!>> JCC Check the bounding - only update if parameter is set to vary
                  Call WGridGetCellCheckBox(IDF_parameter_grid,5,IFRow,ICHK)
				  IF (ICHK .EQ. Checked) THEN
                    Call WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem=max(sngl(lb(IFRow)),xtem)
                    IF ( ABS(xtem - ub(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    ub(IFRow)=dble(xtem)
				    prevub(IFRow) = ub(IFRow)
					CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,sngl(ub(IFRow)),'(F12.5)')
					xtem=min(ub(IFRow),x(IFRow))
					X(IFRow)=dble(xtem)
					CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,sngl(x(IFRow)),'(F12.5)')
				  END IF
                Case(4, 5)
!.. fix or vary
                  Call WGridGetCellCheckBox(IDF_parameter_grid,IFCol,IFRow,ICHK)
				  IF ( (IFCol .EQ. 4 .AND. ICHK .EQ. Checked) .OR. &
					   (IFCol .EQ. 5 .AND. ICHK .EQ. UnChecked) ) THEN

!					JCHK=1-ICHK
					Call WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,Checked)
					Call WGridPutCellCheckBox(IDF_parameter_grid,5,IFRow,UnChecked)
					Call WGridGetCellReal(IDF_parameter_grid,1,IFRow,xtem)
					lb(IFRow)=dble(xtem-1.e-5)
					ub(IFRow)=dble(xtem+1.e-5)
				   
					CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,sngl(lb(IFRow)),'(F12.5)')
					CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,sngl(ub(IFRow)),'(F12.5)')
					CALL WGridStateCell(IDF_parameter_grid,1,IFRow,DialogReadOnly)
					CALL WGridStateCell(IDF_parameter_grid,2,IFRow,DialogReadOnly)
					CALL WGridStateCell(IDF_parameter_grid,3,IFRow,DialogReadOnly)

		 			LimsChanged = .TRUE.
				  ELSE
!	                JCHK=1-ICHK
					Call WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,UnChecked)
					Call WGridPutCellCheckBox(IDF_parameter_grid,5,IFRow,Checked)
					lb(IFRow)=prevlb(IFRow)
					ub(IFRow)=prevub(IFRow)

					CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,sngl(lb(IFRow)),'(F12.5)')
					CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,sngl(ub(IFRow)),'(F12.5)')
					CALL WGridStateCell(IDF_parameter_grid,1,IFRow,Enabled)
					CALL WGridStateCell(IDF_parameter_grid,2,IFRow,Enabled)
					CALL WGridStateCell(IDF_parameter_grid,3,IFRow,Enabled)

					LimsChanged = .TRUE.				    
				  END IF
!                Case(5)
!.. vary
!                  Call WGridGetCellCheckBox(IDF_parameter_grid,IFCol,IFRow,ICHK)

!>> JCC Check the status here. This ensures that only messages that yield this grid position
!>> are processed if the parameter is actually checked.
!				  IF (ICHK .EQ. Checked) THEN
!	                  JCHK=1-ICHK
!					  Call WGridPutCellCheckBox(IDF_parameter_grid,4,IFRow,JCHK)
!					  lb(IFRow)=prevlb(IFRow)
!					  ub(IFRow)=prevub(IFRow)  
!					  CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,sngl(lb(IFRow)),'(F12.5)')
!					  CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,sngl(ub(IFRow)),'(F12.5)')
!					  CALL WGridStateCell(IDF_parameter_grid,1,IFRow,Enabled)
!					  CALL WGridStateCell(IDF_parameter_grid,2,IFRow,Enabled)
!					  CALL WGridStateCell(IDF_parameter_grid,3,IFRow,Enabled)

!					  LimsChanged = .TRUE.
!				  END IF
             End Select ! IFCol
        END SELECT ! Message%Value1 Field Changed Options
    END SELECT  ! ITYPE
  END DO

!.. We are now on window number 3
 777 Call WDialogSelect(IDD_SA_input3)
     CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)
     ISeed1=314
     ISeed2=159
     ISeed3=265
     CALL WDialogPutInteger(IDF_SA_RandomSeed1,ISeed1)
     CALL WDialogPutInteger(IDF_SA_RandomSeed2,ISeed2)
     CALL WDialogPutInteger(IDF_SA_RandomSeed3,ISeed3)
     T0=0.0
     RPOS=T0
     CALL WDialogPutReal(IDF_SA_T0,RPOS,'(f7.2)')
     IPOS=1000-NINT(RPOS)
     CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
     RT=0.02
     RPOS=RT
     CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(f6.3)')
     IPOS=501-NINT(1000.*RPOS)
     CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
             JPOS=20
             CALL WDialogPutInteger(IDF_SA_NS,JPOS)
             IPOS=101-JPOS
             NS=JPOS
             CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
             JPOS=25
             CALL WDialogPutInteger(IDF_SA_NT,JPOS)
             IPOS=101-JPOS
             NT=JPOS
             CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
             NMoves=NT*NS*NVAR
             CALL WDialogPutInteger(IDF_SA_Moves,NMoves)
   DO                                 ! Loop until user terminates
     IXPos_IDD_SA_Input = WInfoDialog(6)
     IYPos_IDD_SA_Input = WInfoDialog(7)
     IWidth_IDD_SA_Input = WInfoDialog(8)
     IHeight_IDD_SA_Input = WInfoDialog(9)
    CALL WMessage(ITYPE, MESSAGE)
!       write(76,*) ' In 3rd SA window with itype & value1 ',ITYPE,MESSAGE%VALUE1
!
    SELECT CASE (ITYPE)
	  CASE (Expose, Resize)
		 IF (MESSAGE%WIN .EQ. 0)  THEN
			CALL ReDraw()
		 END IF
!.. Interact with the main window and look at the Pawley refinement...
      CASE (MouseButDown)
         CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
      CASE (KeyDown)
         CALL Check_KeyDown(MESSAGE)
!
      CASE (PushButton)
         SELECT CASE (Message%Value1)
             CASE (IDF_SA3_cancel)
! Go back to the Pawley refinement or the initial wizard
               IPType=2
               Call WDialogHide()
               Return
             CASE (IDB_SA3_back)
! Go back to the 2nd window
               Call WDialogHide()
               Goto 444
            CASE (IDB_SA3_finish)
! We've finished the SA input
               Call WDialogHide()
               Goto 888
         END SELECT
      CASE (FieldChanged)
        Select Case (Message%Value1)
         CASE(IDF_SA_T0_trackbar)
           If (MESSAGE%VALUE2 .EQ. IDF_SA_T0_trackbar) THEN
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetTrackBar(IDF_SA_T0_trackbar,IPOS)
             RPOS=1000-IPOS
             T0=RPOS
             CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
           End If
         CASE (IDF_SA_T0) 
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetReal(IDF_SA_T0,RPOS)
             T0=RPOS
             IPOS=1000-NINT(RPOS)
             CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
         CASE (IDF_SA_Tredrate_trackbar)
           IF (MESSAGE%VALUE2 .EQ. IDF_SA_Tredrate_trackbar) THEN
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetTrackBar(IDF_SA_Tredrate_trackbar,IPOS)
             RPOS=0.001*(501.-float(IPOS))
             RT=RPOS
             CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(F6.3)')
           END IF
         CASE (IDF_SA_Tredrate) 
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetReal(IDF_SA_Tredrate,RPOS)
             RT=RPOS
             IPOS=501-NINT(1000.*RPOS)
             CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
         CASE (IDF_SA_NS_trackbar)
           IF (MESSAGE%VALUE2 .EQ. IDF_SA_NS_trackbar) THEN
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetTrackBar(IDF_SA_NS_trackbar,IPOS)
             JPOS=101-IPOS
             CALL WDialogPutInteger(IDF_SA_NS,JPOS)
             NS=JPOS
             KPOS=NS*NT*NVAR
             CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
           END IF
         CASE (IDF_SA_NS) 
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetInteger(IDF_SA_NS,JPOS)
             IPOS=101-JPOS
             NS=JPOS
             CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
             KPOS=NS*NT*NVAR
             CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
         CASE (IDF_SA_NT_trackbar)
           IF (MESSAGE%VALUE2 .EQ. IDF_SA_NT_trackbar) THEN
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetTrackBar(IDF_SA_NT_trackbar,IPOS)
             JPOS=101-IPOS
             CALL WDialogPutInteger(IDF_SA_NT,JPOS)
             NT=JPOS
             KPOS=NS*NT*NVAR
             CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
           END IF
         CASE (IDF_SA_NT) 
             CALL WDialogSelect(IDD_SA_input3)
             CALL WDialogGetInteger(IDF_SA_NT,JPOS)
             IPOS=101-JPOS
             NT=JPOS
             CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
             KPOS=NS*NT*NVAR
             CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
         CASE (IDF_SA_RandomSeed1) 
            CALL WDialogSelect(IDD_SA_input3)
            CALL WDialogGetInteger(IDF_SA_RandomSeed1,ISeed1)
         CASE (IDF_SA_RandomSeed2) 
            CALL WDialogSelect(IDD_SA_input3)
            CALL WDialogGetInteger(IDF_SA_RandomSeed2,ISeed2)
         CASE (IDF_SA_RandomSeed3) 
            CALL WDialogSelect(IDD_SA_input3)
            CALL WDialogGetInteger(IDF_SA_RandomSeed3,ISeed3)
       End Select
     End Select
   END DO ! End of Message loop
!... We've finished the three SA input pages
 888 continue
!    do i=1,20
!	   write(56,*) ' bounds ',sngl(lb(i)),sngl(ub(i))
!	end do
	call MakRHm()
	call CalCosArx()
	Call BeginSA(imyexit)
!	write(56,*) imyexit
     jmyexit=imyexit
     imyexit=0
     Select Case (JMyExit)
       Case(1)
         Goto 222
       Case(2)
         Goto 444
       Case(3)
         Goto 777
     End Select
  END SUBROUTINE SA_MAIN
!
     SUBROUTINE OPENHCVPIKTIC(SDIFile,Mode,NoData)
!
!
!C>> JCC Cell/Lattice declarations now in an include file

	 USE VARIABLES
  	  INCLUDE 'Lattice.inc'
      COMMON /PRCHISQ/ PAWLEYCHISQ,RWPOBS,RWPEXP
      include 'statlog.inc'
!
  character(LEN = MaxPathLength) ::  SDIFile,dslfile,line,subline
  character*12 KeyChar

!C>> JCC Declaration

  integer Load_TIC_File
  integer Iptype, i
  integer ihcver,iticer,ipiker,iloger,idsl, isst, ised, iactsgnum
  COMMON /PLTYPE/ IPTYPE
  LOGICAL gotdslfile

!C>> JCC Set to success in all cases
  ihcver = 0
  iloger = 0
  iticer = 1
  ipiker = 0
  idsl = 0
! Now open all the appropriate PIK, TIC and HCV files
!
     open(11,file=SDIFile(:len_trim(SDIFile)),status='old',err=999)

	 CALL sa_SetOutputFiles(SDIFile)

	  PikExists = .false.
	  RawExists = .false.
	  HcvExists = .false.
	  TicExists = .false.

 10   line=' '
      read(11,1100,end=100) line
 1100 format(a)
      nl=len_trim(line)
      call ILowerCase(line(:nl))
      call INextString(line,keychar)
      Select Case (KeyChar(1:3))
        Case ('pik')
!C>> JCC Cant use this, since file paths can have spaces in under windows
!          call INextString(line,pikfile)
	      call ILocateString(line,isst,ised)
	      write(DashPikFile,*) line(isst:nl)
          PikExists = .true.

        Case ('tic')
!C>> JCC Cant use this, since file paths can have spaces in under windows
!         call INextString(line,ticfile)
	      call ILocateString(line,isst,ised)
	      write(DashTicFile,*) line(isst:nl)
          TicExists=.true.

        Case ('hcv')
!C>> JCC Cant use this, since file paths can have spaces in under windows
!          call INextString(line,hcvfile)
	      call ILocateString(line,isst,ised)
	      write(DashHcvFile,*) line(isst:nl)
		  HcvExists=.true.

!C>> JCC Additional file: 'dsl'. The selection file.
!C>> This lists the peak selection data, shape parameters and experimental 
!C>> data such as wavelength.
	    Case ('dsl')
	      call ILocateString(line,isst,ised)
	      write(dslfile,*) line(isst:nl)
		  gotdslfile=.true.
        Case ('cel')
          do i=1,6
            call INextReal(line,cellpar(i))
          end do
!          write(56,*) (cellpar(i),i=1,6)
		  Call Upload_Lattice_Only()
        Case ('spa')
          call INextInteger(line,NumberSGTable)
!C>> JCC Need to set space group infor in the menus
! Get the lattice number
		  NumSG = NumberSGTable

! 
          call INextString(line,subline)
!		  call INextInteger(line,IActSGNum)

! Chop out ":" char if present
		  do i = 1,len_trim(subline)
			if ( subline(i:i) .EQ. ':' ) then
				subline(i:i) = ' '
				exit
			end if
		  end do
		  call INextInteger(subline,IActSGNum)

! Set the lattice numbers
		  LatBrav = LatticeNumber(IActSGNum,NumSG)
		  Call Set_Crystal_Symmetry(LatBrav)
! Last but not least set the space group
 		  Call Update_Space_Group(-1,LatBrav,NumberSGTable)
          Call FillSymmetry()
		  
        Case ('paw')
          call INextReal(line,PawleyChiSq)

		Case ('raw')
	      call ILocateString(line,isst,ised)
	      write(DashRawFile,*) line(isst:nl)
		  HcvExists=.true.	
      End Select
      goto 10 
!
 100  continue
	  IF (GotDSLFile) CALL GETDSL(dslfile,len_trim(dslfile),idsl)

      CALL Load_DashDataFiles(mode,NoData)
!C>>  enable the buttons,
      IF (.not. NoData) THEN
	    IF (idsl .EQ. 0) THEN
		  CALL SetModeMenuState(1,1,1)
		ELSE
		  CALL SetModeMenuState(1,-1,1)
		END IF
	  ELSE
	    
	  END IF
!C>>  update the file name of the project in the SA pop up
	  CALL SetSAFileName(SDIFile(:len_trim(SDIFile)))
!
!
 999 END
!
!
      subroutine Load_DashDataFiles(mode,NoData)
	  use variables
	  use winteracter
	  implicit none
	  integer mode
	  logical NoData
	  logical FExists
	  integer klen
	  integer Iptype,Istat
	  COMMON /PLTYPE/ IPTYPE
	  character*3 ext3

      include 'statlog.inc'

	  INTEGER Load_Tic_File, Load_Pro_File, Load_Diffraction_File ! Functions

	  integer ipiker, iloger, iticer, ihcver

      IF (Mode .EQ. 0) THEN

         IF (PikExists) THEN
           CALL GETPIK(DashPikFile,len_trim(DashPikFile),ipiker)
           IF (ipiker.EQ.0) THEN
				CALL INF_UPLOAD()
		   ELSE
				PikExists = .False.
		   END IF
	     END IF
!

! Load the TIC file
         klen=len_trim(DashTicFile)
         IF (TicExists) THEN
		    CALL GET_LOGREF(DashTicFile,klen,iloger)
		    iticer = Load_Tic_File(klen,DashTicFile)
			IF (iticer .EQ. 0) TicExists = .FALSE.
	     END IF

         IF (HcvExists) THEN
		    CALL GETHCV(DashHcvFile,len_trim(DashHcvFile),ihcver)
			IF (Ihcver .EQ. 1) THEN
				HcvExists = .FALSE.
			ELSE 
				CALL INF_UPLOAD()
			END IF
	     END IF

!C>> JCC Last thing - reload the profile. Previously this was done in Load_TIC_File but 
!C>> I moved it, since i wanted to check that all the data read in ok before calling it

	     IF (TicExists  .AND. PikExists .AND. HcvExists ) THEN
!C>> JCC before, this just didnt plot anything, even though in theory we should be able
!C>> to observe the full profile. Firstly have to synchronize the common blocks though
		    CALL Synchronize_Data()
		    NumPawleyRef = 0 ! We dont have the info for refinement so treat as if none has been done
		    Iptype = 2
		    CALL Profile_Plot(Iptype)	
		    NoData = .false.
	     ENDIF
      ELSE
	     IF (RawExists) THEN
		    klen = len_trim(DashRawFile)
			INQUIRE(FILE=DashRawFile(1:klen),EXIST=FExists)
			IF (FExists) THEN
				ext3=DashRawFile( max(klen-2,1):max(klen,1) )
				CALL ILowerCase(ext3)
				if (ext3.eq.'pro') then
!>> JCC Trap the return status from this
				  ISTAT = Load_PRO_File(klen,DashRawFile,NoData)
				else
!>> JCC Trap the return status from this
				  ISTAT = Load_Diffraction_File(klen,DashRawFile,NoData)
				end if
			 END IF
		 END IF
	  END IF
	  END subroutine Load_DashDataFiles
!
      subroutine SA_Parameter_Set(CheckSize, IZMCheck)
!
!
  USE WINTERACTER
  USE DRUID_HEADER
!C>> JCC Add in checking: only use z-matrices that the user has selected!
  INTEGER CheckSize
  INTEGER IZMCheck(CheckSize)
!
      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,CSH,FSTAR,XP,FOPT
      common /sacmn/ XOPT(NMAX),CSH(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT
!
      INTEGER  NACP(NMAX), NS, NT, NFCNEV, IER, ISEED1, ISEED2
      INTEGER MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAXLOG,RESTART,QUIT,MAKET0
!
      character*132 line
      character*80  sa_file
      logical   log_inf_file,log_nvar,log_bounds,log_reduce
      logical log_eps,log_ns,log_nt,log_neps,log_maxevl,log_iprint
      logical log_iseed1,log_iseed2,log_T0,log_target_value
      logical log_frag_file
      double precision cen,sig
      logical gaussb
      character*80  inf_file,zm_file
      double precision T,T0,rt,eps,target_value
      common /inffil/ lfinf,lfzm,inf_file,zm_file
!
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
!>> JCC Handle via the PDB standard
	  double precision f2cpdb
	  common /pdbcat/ f2cpdb(3,3)
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
     al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
     occ(maxatm,maxfrg)
!
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

	  double precision prevub, prevlb ! For saving the previous range
	  common /pvalues/ prevub(mvar), prevlb(mvar)

      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
!
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
!
      character*36 parlabel(mvar)
!
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),&
            czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
	  logical EnableOnward
!
       COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
       double precision dcel(6)
!
!	write(56,*) ' In SA_parameter_set '

        do i=1,6
          dcel(i)=dble(cellpar(i))
        end do
        call frac2cart(f2cmat,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
        call frac2pdb(f2cpdb,dcel(1),dcel(2),dcel(3),dcel(4),dcel(5),dcel(6))
!
     call CREATE_FOB()
!      nvar=izmtot
      kk=0
!C>> JCC Run through all possible fragments
      do ifrg=1,CheckSize
!C>> Only include those that are now checked
	   if (IZMCheck(ifrg).EQ.Checked) THEN
!	write(56,*) ' In SA_parameter_set ',ifrg,izmpar(ifrg)
        do ii=1,izmpar(ifrg)
          kk=kk+1
          x(kk)=xzmpar(ii,ifrg)
          parlabel(kk)=czmpar(ii,ifrg)
!	write(56,*) ' In SA_parameter_set ',ifrg,ii,kzmpar(ii,ifrg)
          select case(kzmpar(ii,ifrg))
            case(1)
!.. position
              lb(kk)=0.0
              ub(kk)=1.0
              vm(kk)=0.1
            case(2)
!.. quaternion
              lb(kk)=-1.0
              ub(kk)=1.0
              vm(kk)=0.1
            case(3)
!.. torsion
!C>> JCC - need to factor in the sign of the wee beasty, otherwise the front end gets in a paddy!
!              lb(kk)=0.0
!              ub(kk)=360.0
			  if      (x(kk) .LT. 0. .AND. x(kk) .GT. -180.0) then
				lb(kk) =  -180.0
				ub(kk) =   180.0
			  else if (x(kk) .GT. 0. .AND. x(kk) .LT. 360.0) then
				lb(kk) =  0.0
				ub(kk) =  360.0
			  else 
			    lb(kk) = x(kk) - 180.0
				ub(kk) = x(kk) + 180.0
			  end if		    
              vm(kk)=10.0
            case(4)
!.. angle
              lb(kk)=x(kk)-10.0
              ub(kk)=x(kk)+10.0
              vm(kk)=1.0
            case(5)
!.. bond
              lb(kk)=0.9*x(kk)
              ub(kk)=x(kk)/0.9
              vm(kk)=0.1*(ub(kk)-lb(kk))
          end select
!	write(56,*) ' In SA_parameter_set ',&
!     sngl(x(kk)),sngl(lb(kk)),sngl(ub(kk)),sngl(vm(kk))
        end do
!C>> JCC End of check on selection
	   end if
      end do
      nvar=kk
!	write(56,*) ' In SA_parameter_set ',nvar
!
!.. Now fill the grid
      Call WDialogSelect(IDD_SA_input2)
      Call WGridRows(IDF_parameter_grid,nvar)
      Do i=1,nvar
         CALL WGridLabelRow(IDF_parameter_grid,i,parlabel(i))
         CALL WGridPutCellReal(IDF_parameter_grid,1,i,sngl(x(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,2,i,sngl(lb(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,3,i,sngl(ub(i)),'(F12.5)')
         CALL WGridPutCellCheckBox(IDF_parameter_grid,4,i,Unchecked)
         CALL WGridPutCellCheckBox(IDF_parameter_grid,5,i,Checked)
		 CALL WGridStateCell(IDF_parameter_grid,1,i,Enabled)
		 CALL WGridStateCell(IDF_parameter_grid,2,i,Enabled)
		 CALL WGridStateCell(IDF_parameter_grid,3,i,Enabled)
		 prevub(i) = ub(i)
		 prevlb(i) = lb(i)
      End Do
!      Call WDialogHide()      
!
      end subroutine SA_Parameter_Set
!
!
!
!
!
!
      subroutine SA_Parameter_Update(CheckSize,IZMCheck)
!
!
  USE WINTERACTER
  USE DRUID_HEADER
	  INTEGER CheckSize
	  INTEGER IZMCheck(CheckSize)
!
      PARAMETER (NMAX = 100, MXEPS = 10)
      DOUBLE PRECISION XOPT,CSH,FSTAR,XP,FOPT
      common /sacmn/ XOPT(NMAX),CSH(NMAX),FSTAR(MXEPS),XP(NMAX),FOPT
!
      INTEGER  NACP(NMAX), NS, NT, NFCNEV, IER, ISEED1, ISEED2
      INTEGER MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAXLOG,RESTART,QUIT,MAKET0
!
      character*132 line
      character*80  sa_file
      logical   log_inf_file,log_nvar,log_bounds,log_reduce
      logical log_eps,log_ns,log_nt,log_neps,log_maxevl,log_iprint
      logical log_iseed1,log_iseed2,log_T0,log_target_value
      logical log_frag_file
      double precision cen,sig
      logical gaussb
      character*80  inf_file,zm_file
      double precision T,T0,rt,eps,target_value
      common /inffil/ lfinf,lfzm,inf_file,zm_file
!
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
     al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
     occ(maxatm,maxfrg)
!
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      parameter (mvar=100)
      common /gaubou/ cen(mvar),sig(mvar)
      common /gaulog/ gaussb(mvar)
      character*80  torfile
      logical ltorfil
      common /torfcm/ torfile(mvar)
      common /torlog/ ltorfil(mvar)
      common /jitter/ rjittr
      double precision x,lb,ub,vm,xpreset
      common /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)
      common /presetr/ xpreset(mvar)
      logical log_preset
      common /presetl/ log_preset
!
      common /saparl/ T0,rt,eps,target_value
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      common /shadl/ log_shad(mvar)
      common /shadi/ kshad(mvar)
!
      character*36 parlabel(mvar)
!
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),&
            czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
!
       COMMON /CELLREF/ CELLPAR(6),ZEROPOINT,ALAMBDA
       double precision dcel(6)
!
!	write(56,*) ' In SA_parameter_update '
      kk=0
!C>> JCC Only use those that are checked
      do ifrg=1,CheckSize
	   if (IZMCheck(ifrg) .EQ. Checked) then
!	write(56,*) ' In SA_parameter_update ',ifrg,izmpar(ifrg)
        do ii=1,izmpar(ifrg)
          kk=kk+1
!C!>> Leave these alone - save the edits
!          x(kk)=xzmpar(ii,ifrg)
!          parlabel(kk)=czmpar(ii,ifrg)
!	write(56,*) ' In SA_parameter_update ',ifrg,ii,kzmpar(ii,ifrg)
!	write(56,*) ' In SA_parameter_update ',&
!     sngl(x(kk)),sngl(lb(kk)),sngl(ub(kk)),sngl(vm(kk))
        end do
!C>> JCC Check
	   endif
      end do
      nvar=kk
!	write(56,*) ' In SA_parameter_update ',nvar
!
!.. Now fill the grid
      Call WDialogSelect(IDD_SA_input2)
      Call WGridRows(IDF_parameter_grid,nvar)
      Do i=1,nvar
         CALL WGridLabelRow(IDF_parameter_grid,i,parlabel(i))
         CALL WGridPutCellReal(IDF_parameter_grid,1,i,sngl(x(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,2,i,sngl(lb(i)),'(F12.5)')
         CALL WGridPutCellReal(IDF_parameter_grid,3,i,sngl(ub(i)),'(F12.5)')
!         CALL WGridPutCellCheckBox(IDF_parameter_grid,4,i,Unchecked)
!         CALL WGridPutCellCheckBox(IDF_parameter_grid,5,i,Checked)
      End Do
!      Call WDialogHide()      
!
      end subroutine SA_Parameter_Update



	  !C>> JCC This subroutine handles the various types of staus error that can arise 
!C>> during a reading of a file and produces a suitable message to say what went wrong.
	  Subroutine FileErrorPopup(FileName, ErrorStatus)
	  USE Winteracter
	  INCLUDE 'iosdef.for'
	  integer ErrorStatus
	  CHARACTER*(*) FileName
	  INTEGER lenstr
	  lenstr = len_trim(FileName)
	
	  Select Case(ErrorStatus)
	   Case (FOR$IOS_FILNOTFOU) 
		Call WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
		                 "The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" does not exist", &
						 "No such file")
		return

	   Case (FOR$IOS_OPEFAI)
	    Call WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
		                 "The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//" could not be opened", &
						 "Cant open file")
		return

	   Case  (FOR$IOS_PERACCFIL)
	  	Call WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
		                 "You do not have permission to access the file "//FileName(1:lenstr), &
						 "Permission denied")
		return

	  End Select
! Catch all error statement
	  Call WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
		                 "The file "//CHAR(13)//FileName(1:lenstr)//CHAR(13)//"was not read successfully", &
						 "Bad File")
	  End Subroutine FileErrorPopup


	  subroutine ClearZmatrices(CheckSize,IDFZMFile,IDFZMPars,IDFZMCheck,IDBZMBrowse,IZMCheck)
	  use winteracter
	  use druid_header
	  INTEGER CheckSize
	  INTEGER IDFZMFile(CheckSize), IDFZMPars(CheckSize)
	  INTEGER IDFZMCheck(CheckSize), IDBZMBrowse(CheckSize)
	  INTEGER IZMCheck(CheckSize)
	  parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
     al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
     occ(maxatm,maxfrg)
      integer nfrag,lfrag
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),&
            czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
!
! Blow away the selected z-matrices
	  CALL WDialogSelect(IDD_SA_input1)


	  Do II=1,5
		izmpar(ii)=0
		natoms(ii)=0
		if (gotzmfile(ii)) THEN
			CALL WDialogClearField(IDFZMFile(ii))
			CALL WDialogClearField(IDFZMPars(ii))
			call WDialogPutCheckBox(IDFZMCheck(ii),Unchecked)
			Call WDialogClearField(IDFZMFile(II))
			Call WDialogClearField(IDFZMPars(II))
		endif
		Call WDialogFieldState(IDFZMFile(II),Disabled)
		Call WDialogFieldState(IDFZMCheck(II),Disabled)
		Call WDialogFieldState(IDBZMBrowse(II),Disabled)
		Call WDialogFieldState(IDFZMPars(II),Disabled)
	  End Do
	  do ii=1,maxfrg
		gotzmfile(ii)=.false.
	  end do
	  Call WDialogFieldState(IDFZMFile(1),Enabled)
	  call WDialogPutCheckBox(IDFZMCheck(1),checked)
	  Call WDialogFieldState(IDFZMCheck(1),ReadOnly)
	  Call WDialogFieldState(IDBZMBrowse(1),Enabled)
	  Call WDialogFieldState(IDFZMPars(1),ReadOnly)
	  Call WDialogClearField(IDF_ZM_allpars)

	  Call UpdateZmatrixSelection(CheckSize, IZMCheck, IDFZMpars)

	  Call WDialogSelect(IDD_SA_input2)
	  ! Clear the grid too
	  Call WDialogClearField(IDF_parameter_grid)
	  Call WDialogSelect(IDD_SA_input1)

	  end subroutine ClearZmatrices



	  subroutine UpdateZmatrixSelection(CheckSize, IZMCheck, IDFZMpars)
	  use winteracter
	  use druid_header
	  INTEGER CheckSize
	  INTEGER IZMcheck(CheckSize)
	  INTEGER IDFZMpars(CheckSize)
      parameter (maxatm=100)
      parameter (maxfrg=20)
      double precision a,b,c,al,be,ga
      double precision tiso,occ
      double precision blen,alph,bet,f2cmat
      character*3 asym
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
     ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomr/ blen(maxatm,maxfrg),alph(maxatm,maxfrg),&
     bet(maxatm,maxfrg),f2cmat(3,3)
      common /zmcomc/ asym(maxatm,maxfrg)
      common /zmcomo/ a(maxfrg),b(maxfrg),c(maxfrg),&
     al(maxfrg),be(maxfrg),ga(maxfrg),tiso(maxatm,maxfrg),&
     occ(maxatm,maxfrg)
      integer nfrag,lfrag
      common /frgcom/ nfrag,lfrag(maxfrg)
      character*80 frag_file
      common /frgcha/ frag_file(maxfrg)
      character*36 czmpar
      common /zmnpar/ izmtot,izmpar(maxfrg),&
            czmpar(30,maxfrg),kzmpar(30,maxfrg),xzmpar(30,maxfrg)
      logical gotzmfile
      common /zmlgot/ gotzmfile(maxfrg)
      COMMON /POSNS/NATOM,XATO(3,150),KX(3,150),AMULT(150),&
      TF(150),KTF(150),SITE(150),KSITE(150),&
      ISGEN(3,150),SDX(3,150),SDTF(150),SDSITE(150),KOM17
	  nfrag=0
      izmtot=0
      ntatm=0

!C>> JCC Changes here - account for unchecked zmatrices which have been read in but are unselected
      do ii=1,5
       if (gotzmfile(ii) .AND. IZMCheck(ii) .EQ. Checked) then
         nfrag=nfrag+1
         ntatm=ntatm+natoms(ii)
         izmtot=izmtot+izmpar(ii)
         call WDialogPutInteger(IDFZMpars(ii),izmpar(ii))
                   
	    end if
       end do
       natom=ntatm             
       call WDialogPutInteger(IDF_ZM_allpars,izmtot)
	   
	   return
	   end subroutine UpdateZmatrixSelection



	   subroutine ImportZmatrix
 
       USE WINTERACTER
	   USE DRUID_HEADER
	   USE VARIABLES

       TYPE(WIN_MESSAGE)  MESSAGE
	   INTEGER IFlags, Isel, Ilen, Istat, Nzm
	   CHARACTER(LEN=80) :: FilterStr, F
	   CHARACTER(LEN=512) :: Zmfiles
	   CHARACTER(LEN=5)  :: fmt     
	   CHARACTER(LEN=512) :: Info = 'You can import molecules from mol2,mol or pdb files into DASH.'//CHAR(13)//&
							    	'When you click on Ok, you will be prompted for a file in one'//CHAR(13)//&
									'of these formats. DASH will create separate zmatrix files for '//CHAR(13)//&
									'each chemical residue present in the first entry in the file. '//CHAR(13)//&
									'In multiple entry files the first entry will be read only.'


	   CALL WMessageBox(OkCancel, InformationIcon, CommonOk, Info, "Create Z-matrix")
       IF (WinfoDialog(4) .EQ. 1) THEN
				IFlags = LoadDialog + DirChange + AppendExt
				FilterStr = "pdb files|*.pdb|Mol2 files|*.mol2;*.ml2|mdl mol files|*.mol; *.mdl; *.sdi|"
				Isel = 1
				Fname = ' '
				CALL WSelectFile(FilterStr, IFLAGS, FNAME, &
				"Select a file for conversion",Isel)
				Ilen = len_trim(fname)
				IF (Ilen .EQ. 0) RETURN
				if (Isel .EQ. 1) THEN
					fmt = '-pdb'
				else if (Isel .EQ. 2) THEN
					fmt = '-mol2'
				else if (Isel .EQ. 3) THEN
				    fmt = '-mol'
				end if 
				! Run silently, 
				CALL IOSDeleteFile('MakeZmatrix.log')
				Istat = InfoError(1) ! Clear any errors 
				Istart = 1
				do I = 1,Ilen
					IF (FNAME(I:I) .EQ. DIRSPACER) Istart = I + 1
				end do

				CALL IOSCommand(CONVEXE(1:len_trim(CONVEXE))//' '//fmt(1:len_trim(fmt))//' "'//FNAME(Istart:Ilen)//'"',3)
				! Check return status
				OPEN(UNIT=145, FILE='MakeZmatrix.log',STATUS='OLD',IOSTAT = ISTAT)
				IF (InfoError(1) .EQ. ErrOSCommand .OR. ISTAT .NE. 0) THEN
					! An error occurred: get the return status
						! IECODE = InfoError(3)
						! 
					CALL WMessageBox(OkOnly, ExclamationICon, CommonOk, &
					"Sorry, could not create z-matrices",&
					"Generation failed")
					! Prompt with files created
				ELSE ! All Ok: Need to read in the file names
					Ilen = 1
					DO WHILE (Ilen .LT. 512)
						Nzm = Nzm + 1
						READ (145,'(a)',ERR=20,END=20) F
						ZmFiles(Ilen:512) =CHAR(13)//F(1:len_trim(F))
						Ilen = len_trim(ZmFiles)
					END DO
	20              CONTINUE
					CALL WMessageBox(OkOnly, InformationICon, CommonOk, &
					"Generated the following zmatrices successfully:"//CHAR(13)//&
					ZmFiles(1:Ilen)//CHAR(13)//CHAR(13)//&
					"You can load them by clicking on the zmatrix browse buttons"//CHAR(13)//&
					"in the SA setup window",&
					"Generation Successful")
					

					CLOSE(145)
				END IF
	   END IF
	   end subroutine ImportZmatrix