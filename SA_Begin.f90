!
!*****************************************************************************
!
      SUBROUTINE BeginSA

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL                    chi_sqd
      INTEGER                                           it_count
      REAL                                                        y_max
      INTEGER                                                            MaxIterationSoFar
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), it_count, y_max, MaxIterationSoFar

      LOGICAL         InSA
      COMMON /SADATA/ InSA

      INTEGER, EXTERNAL :: CheckOverwriteSaOutput
      LOGICAL, EXTERNAL :: Get_UseHydrogens
      REAL    T1
      REAL    SA_Duration ! The time the SA took, in seconds
      CHARACTER*10 SA_DurationStr

      IF (CheckOverwriteSaOutput() .EQ. 0) THEN
        CALL WizardWindowShow(IDD_SA_input3)
        RETURN
      ENDIF
! Get 'Use Hydrogens' from the configuration window and disable that option (should not be 
! changed while the SA is running).
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Disabled)
      LOG_HYDROGENS = Get_UseHydrogens()
! Pop up the SA status window
      CALL WDialogSelect(IDD_SA_Action1)
      CALL WizardWindowShow(IDD_SA_Action1)
!O      CALL WDialogSelect(IDD_Parameter_Status_2)
!O      CALL WDialogShow(-1,-1,0,Modeless)
      T1 = SECNDS(0.0)
      CALL PDB_SymmRecords
      CALL Init_MultiRun
! Grey out "start next" button if not multirun
      CALL WDialogFieldStateLogical(IDF_StartNext,RESTART)
      IPTYPE = 2
! Clear Chi-sqd array between starting sets of SA Runs
      Chi_sqd = 0.0
      MaxIterationSoFar = 0
      CALL WDialogFieldState(IDB_Summary,Disabled)
      InSA = .TRUE.
      CALL SimulatedAnnealing
      InSA = .FALSE.
      SA_Duration = SECNDS(T1)
      WRITE(SA_DurationStr,'(F10.1)') SA_Duration
      CALL DebugErrorMessage('The SA took '//SA_DurationStr(1:LEN_TRIM(SA_DurationStr))//' seconds.')
! After completion, save the list of solutions
      CALL SaveMultiRun_LogData
      CALL OutputChi2vsMoves
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Enabled)
!O      Ierrflag = InfoError(1)
!O      CALL WindowSelect(0)
!O! Wait for the user to raise the window. Under NT the "WindowRaise call" 
!O! Does not seem to work annoyingly, so once complete, wait for the user to
!O! raise the window
!O      DO WHILE (WinfoWindow(WindowState) .EQ. WinMinimised)
!O        CALL IOsWait(50) ! wait half a sec
!O      ENDDO
      CALL WizardWindowShow(IDD_SAW_Page5)

      END SUBROUTINE BeginSA
!
!*****************************************************************************
!
! JCC Check the files before we trash them  
      INTEGER FUNCTION CheckOverwriteSaOutput()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      INTEGER I, iFlags
      CHARACTER(MaxPathLength) filehead, tDirName, tFileName
      LOGICAL OutputExists
      CHARACTER(3) NumStr

! Try all possible output names, when match, set OutputExists to .TRUE.
    1 DO I = 1, MaxRun
        WRITE (NumStr,'(I3.3)') I
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.cssr'
        INQUIRE(FILE=tFileName,EXIST=OutputExists) 
        IF (OutputExists) GOTO 10
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.pdb'
        INQUIRE(FILE=tFileName,EXIST=OutputExists) 
        IF (OutputExists) GOTO 10
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.ccl'
        INQUIRE(FILE=tFileName,EXIST=OutputExists) 
        IF (OutputExists) GOTO 10
! ep added.  Pro_file contains the powder diffraction data and fit....
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.pro'
        INQUIRE(FILE=tFileName,EXIST=OutputExists) 
        IF (OutputExists) GOTO 10
      ENDDO
! When we are here, none of the filenames we generated clashed with an existing one
      CheckOverwriteSaOutput = 1
      RETURN
! When we are here, at least one of the filenames we generated clashed
! ask user if (s)he wants to overwrite
   10 CALL SplitPath(OutputFilesBaseName,tDirName,filehead)
      CALL WMessageBox(YesNoCancel, QuestionIcon, CommonYes, &
                    "Do you wish to overwrite existing files?"//CHAR(13)//CHAR(13)// &
                    "Current base for filenames: "//filehead(1:LEN_TRIM(filehead))//CHAR(13)//CHAR(13)// &
                    "(Hit No to enter a new filename)", &
                    "Overwrite Output Files?")
      IF (WInfoDialog(4) .EQ. 1) THEN ! Yes - Overwrite
        CheckOverwriteSaOutput = 1
        RETURN 
      ELSEIF (WinfoDialog(4) .EQ. 2) THEN ! No - so enter a new file name
        iFlags = SaveDialog + NonExPath + DirChange + AppendExt
        filehead = ''
! ep appended
        CALL WSelectFile('pdb files|*.pdb|ccl files|*.ccl|cssr files|*.cssr|pro files|*.pro|',&
                         iFlags,filehead,'Choose SA output file name')
        IF ((WinfoDialog(4) .NE. CommonOk) .OR. (LEN_TRIM(filehead) .EQ. 0)) THEN ! Cancel
          CheckOverwriteSaOutput = 0
          RETURN
        ELSE
          CALL sa_SetOutputFiles(filehead)
! New checks necessary: does the new name exist?
          GOTO 1
        ENDIF
      ELSE ! Cancel
        CheckOverwriteSaOutput = 0
        RETURN
      ENDIF

      END FUNCTION CheckOverwriteSaOutput
!
!*****************************************************************************
!
      SUBROUTINE FillSymmetry

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      CHARACTER*6 xxx
      CHARACTER*10 fname

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)
 
      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

   10 IBMBER = 0
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
      IF (IBMBER .NE. 0) THEN
! Set the crystal system
        LatBrav = GetCrystalSystem(NumberSGTable)
        NumberSGTable = 1 ! P1
        CALL Upload_CrystalSystem
        CALL ErrorMessage('Error while determining space group: space group reset.')
        GOTO 10
      ENDIF

      END SUBROUTINE FillSymmetry
!
!*****************************************************************************
!
      SUBROUTINE PO_Init   

      USE ATMVAR
      USE PO_VAR
      USE REFVAR

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

      INTEGER         NLGREF
      LOGICAL                 LOGREF
      COMMON /FCSPEC/ NLGREF, LOGREF(8,MFCSTO)

      INTEGER           iHMUL
      COMMON /SAREFLN3/ iHMUL(MFCSTO)

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
      RefLen = VCTMOD(1.0,PrfDir,2)
      PrfDir = PrfDir / RefLen
      DO iR = 1, NumOfRef
        DO ii = 1, 3
          H(ii) = SNGL(iHKL(ii,iR))
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

      END SUBROUTINE PO_Init
!
!*****************************************************************************
!
