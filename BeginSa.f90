      subroutine BeginSa(imyexit)
!
      USE WINTERACTER
      USE DRUID_HEADER
!>> JCC 
	  USE VARIABLES

	  LOGICAL DoSaRedraw
	  COMMON /SARDRW/ DoSaRedraw

	  LOGICAL process_mainwindow_message
!
! Declare window-type and message variables
!
  TYPE(WIN_STYLE)    WINDOW
  TYPE(WIN_MESSAGE)  MESSAGE
!
  include 'DialogPosCmnF90.inc'
!
      parameter (mvar=100)
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
      integer ihandle
      common /winwifd/ ihandle(20)
!C>> Function defn
	  integer CheckOverwriteSaOutput
	  integer test

	  test = CheckOverwriteSaOutput()
	  if (test .EQ. 0) then
	    imyexit = 3
		return
      end if
!
!C>> JCC      CALL WDialogLoad(IDD_SA_Action1)
 !C>> JCC     CALL WDialogLoad(IDD_SA_Action2)
      Call WDialogSelect(IDD_SA_Action1)
!
!      Call WDialogFieldState(IDF_Pause_Annealing,Disabled)
!      Call WDialogFieldState(IDF_SA_Stop_Button,Disabled)
!      Call WDialogFieldState(IDF_Viewer,Disabled)
      Call WDialogFieldState(IDF_Pause_Annealing,Enabled)
!      Call WDialogFieldState(IDF_SA_Stop_Button,Enabled)
!C>> JCC Added check on viewer
      IF (ViewOn) THEN
		Call WDialogFieldState(IDF_Viewer,Enabled)
	  ELSE
		Call WDialogFieldState(IDF_Viewer,Disabled)
	  END IF
	
!
      CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)

      IXPos_IDD_SA_Input = WInfoDialog(6)
      IYPos_IDD_SA_Input = WInfoDialog(7)
      IWidth_IDD_SA_Input = WInfoDialog(8)
      IHeight_IDD_SA_Input = WInfoDialog(9)

      DoSaRedraw = .TRUE.
	  CALL SimulatedAnnealing(imyexit)

! After completion, save the list of solutions
      CALL SaveMultiRun_LogData

      DoSaRedraw = .FALSE.

!>> JCC Automatic end so pop up a message to say what happened
!	  Call WDialogSelect(IDD_SA_Completed)
      Ierrflag = InfoError(1)

	  CALL WindowSelect(0)
	  Ierrflag = InfoError(1)

!>> Wait for the user to raise the window. Under NT the "WindowRaise call" 
!>> Does not seem to work annoyingly, so once complete, wait for the user to
!>> raise the window

	  DO WHILE ( WinfoWindow(WindowState) .EQ. 0)
		CALL IOsWait(50) ! wait half a sec
	  END DO
	  

      Call WDialogSelect(IDD_SA_Action1)
	  CALL WDialogHide()

	  CALL ToggleMenus(0)


      Call WDialogSelect(IDD_SA_Multi_Completed)
	  CALL WDialogShow(-1,-1,0,Modeless)
	  DO
			CALL WMessage(ITYPE, MESSAGE)
			IF (MESSAGE%WIN .EQ. 0) THEN
				Quit = process_mainwindow_message(ITYPE, MESSAGE)
			ELSE
			    SELECT CASE (ITYPE)
				  CASE (PushButton)
!					IF ( IDF_SA_Complete_Ok .EQ. MESSAGE%VALUE1) THEN
					IF ( IDOK .EQ. MESSAGE%VALUE1) THEN
						CALL WDialogHide()
						EXIT
					END IF
			    END SELECT
		    END IF
	  END DO
	  CALL ToggleMenus(1)

	  Ierrflag =  InfoError(1)
	  DoSaRedraw = .FALSE.

      END

!>> JCC Check the files before we trash them  
    integer function CheckOverwriteSaOutput()

	USE WINTERACTER
	USE DRUID_HEADER
	character*85 new_fname
    character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen
	integer Iflags,Idummy
	character*80 filehead
	logical extcssr,extpdb,extccl
!
	INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
	IF (.NOT.extcssr) THEN
	  DO I = 1,100
	    CALL AppendNumToFileName(I,cssr_file,new_fname)
		INQUIRE(FILE=new_fname(1:len_trim(new_fname)),EXIST=extcssr) 
		IF (extcssr) EXIT
	  END DO
	END IF
	INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
	IF (.NOT.extpdb) THEN
	  DO I = 1,100
	    CALL AppendNumToFileName(I,pdb_file,new_fname)
		INQUIRE(FILE=new_fname(1:len_trim(new_fname)),EXIST=extpdb) 
		IF (extpdb) EXIT
	  END DO
	END IF
	INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)
	IF (.NOT.extccl) THEN
	  DO I = 1,100
	    CALL AppendNumToFileName(I,ccl_file,new_fname)
		INQUIRE(FILE=new_fname(1:len_trim(new_fname)),EXIST=extccl) 
		IF (extccl) EXIT
	  END DO
	END IF

	CheckOverwriteSaOutput = 1
	DO WHILE (extcssr .OR. extpdb .OR. extccl)

			  CALL WMessageBox(YesNoCancel, QuestionIcon, CommonYes, &
			  "Do you wish to overwrite existing files? "//CHAR(13) &
			  //"(Hit No to enter a new filename)", &
			  "Overwrite Output Files?")

        If (WInfoDialog(4).eq.1) THEN ! Yes - Overwrite
			RETURN 
		ELSE IF (WinfoDialog(4).eq.2) THEN ! No - so enter a new file name
			Iflags = SaveDialog+NonExPath+DirChange+AppendExt
			Idummy = 1
			filehead = ' '
			CALL WSelectFile('ccl files|*.ccl|cssr files|*.cssr|pdb files|*.pdb|',&
						     Iflags, &
							 filehead,&
							 'Choose SA output file name',&
							 Idummy)
			IF (filehead .NE. ' ') THEN
				CALL sa_SetOutputFiles(filehead)
			END IF
		ELSE ! Cancel
				CheckOverwriteSaOutput = 0
				RETURN
		END IF

		INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
		INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
		INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)

! Read in a new file name

	END DO
	RETURN 
	END FUNCTION CheckOverwriteSaOutput
!
!
!
	SUBROUTINE FillSymmetry()

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),&
      ICDN(26,9),IERR,IO10,SDREAD
      common/iounit/lpt,iti,ito,iplo,luni,iout
      integer matsz
      character*6 xxx
      character*10 fname
!
	INCLUDE 'Lattice.inc'
	character(len=45) FILTER
      parameter (msymmin=10)
      character*20 symline
	common /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
!
! 
      open(42,file='polys.ccl',status='unknown')
      write(42,4210) 
 4210 format('N Determining the space group ')
      if (IPosSG.ge.1) then
        call DecodeSGSymbol(SGShmStr(IPosSg))
        if (nsymmin.gt.0) then
          do isym=1,nsymmin
            write(42,4235) symline(isym)
 4235       format('S ',a)
          end do
        end if
      end if
	  close(42)
!
      fname='polys'
      xxx='SPGMAK'

      call FORSYM(xxx,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(lpt)

	return
	END SUBROUTINE FillSymmetry