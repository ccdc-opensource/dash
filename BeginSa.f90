!
!*****************************************************************************
!
      SUBROUTINE BeginSa(imyexit)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'

      PARAMETER (mvar=100)
      COMMON /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2

      INTEGER CheckOverwriteSaOutput ! Function
      INTEGER test                   ! Function

      test = CheckOverwriteSaOutput()
      IF (test .EQ. 0) THEN
        imyexit = 3
        RETURN
      END IF
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogFieldState(IDF_Pause_Annealing,Enabled)
! Check on viewer
      IF (ViewOn) THEN
        CALL WDialogFieldState(IDF_Viewer,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_Viewer,Disabled)
      END IF
      CALL WDialogShow(IXPos_IDD_SA_Input,IYPos_IDD_SA_Input,0,Modeless)
      DoSaRedraw = .TRUE.
      CALL SimulatedAnnealing(imyexit)
! After completion, save the list of solutions
      CALL SaveMultiRun_LogData
      DoSaRedraw = .FALSE.
! Automatic end so pop up a message to say what happened
!       Call WDialogSelect(IDD_SA_Completed)
      Ierrflag = InfoError(1)
      CALL WindowSelect(0)
      Ierrflag = InfoError(1)
! Wait for the user to raise the window. Under NT the "WindowRaise call" 
! Does not seem to work annoyingly, so once complete, wait for the user to
! raise the window
      DO WHILE (WinfoWindow(WindowState) .EQ. 0)
        CALL IOsWait(50) ! wait half a sec
      END DO
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WDialogHide()
      CALL ToggleMenus(0)

!ep SASummary presents a grid summarising results of the Simulated
!   Annealing runs.  
      CALL SaSummary()

!ep      Call WDialogSelect(IDD_SA_Multi_Completed)
!ep     CALL WDialogShow(-1,-1,0,Modeless)
!ep     DO
!ep               CALL WMessage(ITYPE, MESSAGE)
!ep               IF (MESSAGE%WIN .EQ. 0) THEN
!ep                     Quit = process_mainwindow_message(ITYPE, MESSAGE)
!ep               ELSE
!ep                   SELECT CASE (ITYPE)
!ep                       CASE (PushButton)
!jcc                          IF ( IDF_SA_Complete_Ok .EQ. MESSAGE%VALUE1) THEN
!ep                           IF ( IDOK .EQ. MESSAGE%VALUE1) THEN
!ep                                 CALL WDialogHide()
!ep                                 EXIT
!ep                           END IF
!ep                   END SELECT
!ep             END IF
!ep     END DO

      CALL ToggleMenus(1)
      Ierrflag =  InfoError(1)
      DoSaRedraw = .FALSE.

      END SUBROUTINE BeginSa
!
!*****************************************************************************
!
!>> JCC Check the files before we trash them  
      INTEGER FUNCTION CheckOverwriteSaOutput()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*85 new_fname

      CHARACTER*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,pro_file   
      COMMON /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file,pro_file
      INTEGER logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen
      COMMON /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen,pro_flen

      INTEGER I, Iflags, Idummy
      CHARACTER*80 filehead
!ep added extpro
      LOGICAL extcssr,extpdb,extccl, extpro
!
      INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
      IF (.NOT. extcssr) THEN
        DO I = 1, 100
          CALL AppendNumToFileName(I,cssr_file,new_fname)
            INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extcssr) 
            IF (extcssr) EXIT
        END DO
      END IF
      INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
      IF (.NOT. extpdb) THEN
        DO I = 1, 100
          CALL AppendNumToFileName(I,pdb_file,new_fname)
            INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extpdb) 
            IF (extpdb) EXIT
        END DO
      END IF
      INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)
      IF (.NOT. extccl) THEN
        DO I = 1, 100
          CALL AppendNumToFileName(I,ccl_file,new_fname)
            INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extccl) 
            IF (extccl) EXIT
        END DO
      END IF
!     ep added.  Pro_file contains the powder diffraction data and fit....
      INQUIRE(FILE=pro_file(1:pro_flen),  EXIST=extpro)
      IF (.NOT.extpro) THEN
        DO I = 1,100
          CALL AppendNumToFileName(I,pro_file,new_fname)
            INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extpro) 
            IF (extpro) EXIT
        END DO
      END IF
      CheckOverwriteSaOutput = 1
      DO WHILE (extcssr .OR. extpdb .OR. extccl .OR. extpro)
        CALL WMessageBox(YesNoCancel, QuestionIcon, CommonYes, &
                    "Do you wish to overwrite existing files? "//CHAR(13) &
                    //"(Hit No to enter a new filename)", &
                    "Overwrite Output Files?")
        IF (WInfoDialog(4) .EQ. 1) THEN ! Yes - Overwrite
          RETURN 
        ELSE IF (WinfoDialog(4) .EQ. 2) THEN ! No - so enter a new file name
          Iflags = SaveDialog+NonExPath+DirChange+AppendExt
          Idummy = 1
          filehead = ' '
!ep appended
          CALL WSelectFile('ccl files|*.ccl|cssr files|*.cssr|pdb files|*.pdb|pro files|*.pro|',&
                           Iflags, &
                           filehead,&
                           'Choose SA output file name')
          IF (filehead .NE. ' ') CALL sa_SetOutputFiles(filehead)
        ELSE ! Cancel
          CheckOverwriteSaOutput = 0
          RETURN
        END IF
        INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
        INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
        INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)
        INQUIRE(FILE=pro_file(1:pro_flen),  EXIST=extpro)
! Read in a new file name
      END DO
      RETURN 

      END FUNCTION CheckOverwriteSaOutput
!
!*****************************************************************************
!
      SUBROUTINE FillSymmetry()

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.
      LOGICAL SDREAD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),&
      ICDN(26,9),IERR,IO10,SDREAD
      common/iounit/lpt,iti,ito,iplo,luni,iout
      CHARACTER*6 xxx
      CHARACTER*10 fname

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      PARAMETER (msymmin=10)
      CHARACTER*20 symline
      COMMON /symgencmn/ nsymmin,symmin(4,4,msymmin),symline(msymmin)
 
      OPEN(42,file='polys.ccl',status='unknown')
      WRITE(42,4210) 
 4210 FORMAT('N Determining the space group ')
      IF (NumberSGTable .GE. 1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin .GT. 0) THEN
          DO isym = 1, nsymmin
            WRITE(42,4235) symline(isym)
 4235       FORMAT('S ',a)
          END DO
        END IF
      END IF
      CLOSE(42)
      fname='polys'
      xxx='SPGMAK'
      CALL FORSYM(xxx,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)
      RETURN

      END SUBROUTINE FillSymmetry
!
!*****************************************************************************
!
