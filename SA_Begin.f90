!
!*****************************************************************************
!
      SUBROUTINE BeginSa

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      REAL                    chi_sqd
      INTEGER                                           it_count
      REAL                                                        y_max
      INTEGER                                                            MaxIterationSoFar
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), it_count, y_max, MaxIterationSoFar

      INTEGER, EXTERNAL :: CheckOverwriteSaOutput
      LOGICAL, EXTERNAL :: Get_UseHydrogens
      REAL    T1
      REAL    SA_Duration ! The time the SA took, in seconds
      CHARACTER*10 SA_DurationStr
      INTEGER Ierrflag
      INTEGER I, RangeOption
      CHARACTER*2 RowLabelStr

      IF (CheckOverwriteSaOutput() .EQ. 0) THEN
        CALL WizardWindowShow(IDD_SA_input3)
        RETURN
      ENDIF
! Get 'Use Hydrogens' from the configuration window and disable that option (should not be 
! changed while the SA is running).
      LOG_HYDROGENS = Get_UseHydrogens()
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Disabled)
! Pop up the SA status window
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WizardWindowShow(IDD_SA_Action1)
!O      CALL WDialogSelect(IDD_Parameter_Status_2)
!O      CALL WDialogShow(-1,-1,0,Modeless)
      T1 = SECNDS(0.0)
      CALL PDB_SymmRecords()
      CALL Init_MultiRun()
! Grey out "start next" button if not multirun
      IF (RESTART) THEN
        CALL WDialogFieldState(IDF_StartNext,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_StartNext,Disabled)
      ENDIF
      IPTYPE = 2
! Clear Chi-sqd array between starting sets of SA Runs
      Chi_sqd = 0.0
      MaxIterationSoFar = 0

!T      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
!T      CALL WDialogShow(-1,-1,0,Modeless)

      CALL SimulatedAnnealing
      SA_Duration = SECNDS(T1)
      WRITE(SA_DurationStr,'(F10.1)') SA_Duration
      CALL DebugErrorMessage('The SA took '//SA_DurationStr(1:LEN_TRIM(SA_DurationStr))//' seconds.')
! After completion, save the list of solutions
      CALL SaveMultiRun_LogData
      CALL OutputChi2vsMoves
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Enabled)
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
        WRITE(RowLabelStr,'(I2)') I
        CALL WGridLabelRow(IDF_SA_summary,I,RowLabelStr)
      ENDDO
      CALL WDialogGetRadioButton(IDF_ShowRange,RangeOption)
      IF (RangeOption .EQ. 1) THEN ! "Show Selected"
        CALL WDialogFieldState(IDF_Limit1,Enabled)
        CALL WDialogFieldState(IDF_Limit2,Enabled)
      ELSE
        CALL WDialogFieldState(IDF_Limit1,Disabled)
        CALL WDialogFieldState(IDF_Limit2,Disabled)
      ENDIF
      CALL WDialogShow(-1,-1,0,Modeless)
      Ierrflag =  InfoError(1)

      END SUBROUTINE BeginSa
!
!*****************************************************************************
!
! JCC Check the files before we trash them  
      INTEGER FUNCTION CheckOverwriteSaOutput()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) new_fname

      CHARACTER(MaxPathLength) cssr_file, pdb_file, ccl_file, log_file, pro_file
      COMMON /outfilnam/       cssr_file, pdb_file, ccl_file, log_file, pro_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      INTEGER I, Iflags
      CHARACTER(MaxPathLength) filehead, tDirName
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
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
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
      SUBROUTINE PO_Init   

      USE PO_VAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)

      INTEGER         MAXK
      REAL                  FOB
      COMMON /FCSTOR/ MAXK, FOB(150,MFCSTO)

      INTEGER         NLGREF, iREFH
      LOGICAL                                  LOGREF
      COMMON /FCSPEC/ NLGREF, iREFH(3,MFCSPE), LOGREF(8,MFCSPE)

      INTEGER           iHMUL
      COMMON /SAREFLN3/ iHMUL(MSAREF)

      INTEGER         NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      CHARACTER*10 filnam_root
      COMMON /commun/ filnam_root
 
      CHARACTER*6  PNAME
      REAL phases(1:48), RefHT(1:3,1:48)
      REAL PrfDir(1:3), H(1:3), RefLen
      INTEGER i, ii, iR, isym
      REAL, EXTERNAL :: VCTMOD, SCLPRD

      IF (.NOT. PrefParExists) RETURN
      OPEN(42,file='polyx.ccl',status='unknown')
      WRITE(42,4210) 
 4210 FORMAT('N Handles preferred orientation')
      WRITE(42,4220) (CellPar(i),i=1,6)
 4220 FORMAT('C ',3F9.5,3F9.3)
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
      pname = 'EXTMAK'
      filnam_root = 'polyx'
      NINIT = 1
      CALL PREFIN(PNAME)
      CALL SYMOP
      CALL RECIP
      DO i = 1, 3
        PrfDir(i) = PrefPars(i)
      ENDDO
      CALL VectorNormalise(PrfDir)
      DO iR = 1, MAXK
        DO ii = 1, 3
          H(ii) = SNGL(iREFH(ii,iR))
        ENDDO
        RefLen = VCTMOD(1.0,H,2) ! Calculate length of reciprocal-space vector
        CALL SYMREF(H,RefHT,iHMUL(iR),phases)
        DO ii = 1, iHMUL(iR)
          PrefCsqa(ii,iR) = (SCLPRD(PrfDir,RefHT(1,ii),2)/RefLen)**2
        ENDDO
      ENDDO
      CALL CLOFIL(ICRYDA)
      CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END  SUBROUTINE PO_Init
!
!*****************************************************************************
!
