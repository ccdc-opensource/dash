!
!*****************************************************************************
!
      SUBROUTINE BeginSa

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      INTEGER, EXTERNAL :: CheckOverwriteSaOutput
      LOGICAL, EXTERNAL :: Get_UseHydrogens
      REAL    T1
      REAL    SA_Duration ! The time the SA took, in seconds
      CHARACTER*10 SA_DurationStr
      INTEGER Ierrflag
      INTEGER I

      IF (CheckOverwriteSaOutput() .EQ. 0) THEN
        CALL WizardWindowShow(IDD_SA_input3)
        RETURN
      ENDIF
      LOG_HYDROGENS = Get_UseHydrogens()
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Disabled)
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WizardWindowShow(IDD_SA_Action1)
      DoSaRedraw = .TRUE.
      T1 = SECNDS(0.0)
      CALL PDB_SymmRecords()
      CALL Init_MultiRun()
! Grey out start next button if not multirun
      IF (RESTART) THEN
        CALL WDialogFieldState(IDF_StartNext,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_StartNext,Disabled)
      ENDIF
      IPTYPE = 2
      CALL SimulatedAnnealing
      SA_Duration = SECNDS(T1)
      WRITE(SA_DurationStr,'(F10.1)') SA_Duration
  !    CALL DebugErrorMessage('The SA took '//SA_DurationStr(1:LEN_TRIM(SA_DurationStr))//' seconds.')
! After completion, save the list of solutions
      CALL SaveMultiRun_LogData
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Enabled)
      DoSaRedraw = .FALSE.
      Ierrflag = InfoError(1)
      CALL WindowSelect(0)
      Ierrflag = InfoError(1)
! Wait for the user to raise the window. Under NT the "WindowRaise call" 
! Does not seem to work annoyingly, so once complete, wait for the user to
! raise the window
      DO WHILE (WinfoWindow(WindowState) .EQ. 0)
        CALL IOsWait(50) ! wait half a sec
      ENDDO
      CALL WizardWindowHide
!ep SASummary presents a grid summarising results of the Simulated
!   Annealing runs.  
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
! Initialise all overlay checkboxes to 'Checked'
      DO I = 1, SA_Run_Number
        CALL WGridPutCellCheckBox(IDF_SA_summary,3,I,Checked)
      ENDDO
      CALL WDialogShow(-1,-1,0,Modeless)
      Ierrflag =  InfoError(1)
      DoSaRedraw = .FALSE.

      END SUBROUTINE BeginSa
!
!*****************************************************************************
!
! JCC Check the files before we trash them  
      INTEGER FUNCTION CheckOverwriteSaOutput()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*85 new_fname

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file, bin_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file, bin_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen, bin_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen, bin_flen

      INTEGER I, Iflags
      CHARACTER*80 filehead, tDirName
!ep added extpro
      LOGICAL extcssr, extpdb, extccl, extpro

      INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
      IF (extcssr) GOTO 10
      DO I = 1, 30
        CALL AppendNumToFileName(I,cssr_file,new_fname)
        INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extcssr) 
        IF (extcssr) GOTO 10
      ENDDO
      INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
      IF (extpdb) GOTO 10
      DO I = 1, 30
        CALL AppendNumToFileName(I,pdb_file,new_fname)
        INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extpdb) 
        IF (extpdb) GOTO 10
      ENDDO
      INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)
      IF (extccl) GOTO 10
      DO I = 1, 30
        CALL AppendNumToFileName(I,ccl_file,new_fname)
        INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extccl) 
        IF (extccl) GOTO 10
      ENDDO
!     ep added.  Pro_file contains the powder diffraction data and fit....
      INQUIRE(FILE=pro_file(1:pro_flen),  EXIST=extpro)
      IF (extpro) GOTO 10
      DO I = 1, 30
        CALL AppendNumToFileName(I,pro_file,new_fname)
        INQUIRE(FILE=new_fname(1:LEN_TRIM(new_fname)),EXIST=extpro) 
        IF (extpro) GOTO 10
      ENDDO
   10 CheckOverwriteSaOutput = 1
      DO WHILE (extcssr .OR. extpdb .OR. extccl .OR. extpro)
        CALL SplitPath(pdb_file,tDirName,filehead)
        CALL WMessageBox(YesNoCancel, QuestionIcon, CommonYes, &
                    "Do you wish to overwrite existing files?"//CHAR(13)//CHAR(13)// &
                    "Current base for filenames: "//filehead(1:LEN_TRIM(filehead)-4)//CHAR(13)//CHAR(13)// &
                    "(Hit No to enter a new filename)", &
                    "Overwrite Output Files?")
        IF (WInfoDialog(4) .EQ. 1) THEN ! Yes - Overwrite
          RETURN 
        ELSEIF (WinfoDialog(4) .EQ. 2) THEN ! No - so enter a new file name
          Iflags = SaveDialog + NonExPath + DirChange + AppendExt
          filehead = ' '
!ep appended
          CALL WSelectFile('ccl files|*.ccl|cssr files|*.cssr|pdb files|*.pdb|pro files|*.pro|',&
                           Iflags,filehead,'Choose SA output file name')
          IF (filehead .NE. ' ') CALL sa_SetOutputFiles(filehead)
        ELSE ! Cancel
          CheckOverwriteSaOutput = 0
          RETURN
        ENDIF
        INQUIRE(FILE=cssr_file(1:cssr_flen),EXIST=extcssr) 
        INQUIRE(FILE=pdb_file(1:pdb_flen),  EXIST=extpdb)
        INQUIRE(FILE=ccl_file(1:ccl_flen),  EXIST=extccl)
        INQUIRE(FILE=pro_file(1:pro_flen),  EXIST=extpro)
! Read in a new file name
      ENDDO

      END FUNCTION CheckOverwriteSaOutput
!
!*****************************************************************************
!
      SUBROUTINE FillSymmetry()

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.
      LOGICAL SDREAD
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),ICDN(26,9),IERR,IO10,SDREAD
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
          ENDDO
        ENDIF
      ENDIF
      CLOSE(42)
      fname='polys'
      xxx='SPGMAK'
      CALL FORSYM(xxx,fname)
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END SUBROUTINE FillSymmetry
!
!*****************************************************************************
!
