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

      INTEGER              iMyExit, num_new_min
      COMMON / CMN000001 / iMyExit, num_new_min

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed

      LOGICAL         InSA
      COMMON /SADATA/ InSA

      INTEGER, EXTERNAL :: CheckOverwriteSaOutput, DateToday, DateDaysElapsed, TimeNowSeconds
      LOGICAL, EXTERNAL :: Get_UseHydrogens
      CHARACTER*20, EXTERNAL :: Integer2String
      CHARACTER*100 SA_DurationStr
      INTEGER StartDate, EndDate, DaysElapsed, DSLen
      INTEGER StartTime, EndTime, NItems
      INTEGER DiffSecs, nweeks, nhours, nminutes
      INTEGER OneDay ! The number of seconds in one day
      CHARACTER*20 tIntStr

      OneDay = 24 * 60 * 60
      IF (CheckOverwriteSaOutput() .EQ. 0) THEN
        CALL WizardWindowShow(IDD_SA_input3)
        RETURN
      ENDIF
      CALL Create_AtomicWeightings
      CALL FillSymmetry_2
      CALL GET_LOGREF
! Get 'Use Hydrogens' from the configuration window and disable that option (should not be 
! changed while the SA is running).
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogFieldState(IDF_UseHydrogens,Disabled)
      LOG_HYDROGENS = Get_UseHydrogens()
! Ungrey the "Save... chi sqrd progress"
      CALL WDialogSelect(IDD_OutputSolutions)
      CALL WDialogFieldState(IDF_GROUP2,Enabled)
      CALL WDialogFieldState(IDB_OutputChiSqd,Enabled)
      CALL WDialogFieldState(IDF_LABEL5,Enabled)
      CALL WDialogFieldState(IDF_LABEL3,Enabled)
! Pop up the SA status window
      CALL WizardWindowShow(IDD_SA_Action1)
!O      CALL WDialogSelect(IDD_Parameter_Status_2)
!O      CALL WDialogShow(-1,-1,0,Modeless)
      StartDate = DateToday()
      StartTime = TimeNowSeconds()
      CALL Init_MultiRun
! Grey out "start next" button if not multirun
      CALL WDialogFieldStateLogical(IDF_StartNext,RESTART)
      IPTYPE = 2
!C Clear Chi-sqd array between starting sets of SA Runs
      Chi_sqd = 0.0
      MaxIterationSoFar = 0
      InSA = .TRUE.
      CALL MakRHm
      CALL SimulatedAnnealing
      InSA = .FALSE.
      EndDate = DateToday()
      EndTime = TimeNowSeconds()
      DaysElapsed = DateDaysElapsed(StartDate, EndDate)
      SA_DurationStr = ""
      DSLen = LEN_TRIM(SA_DurationStr)
      NItems = 0
      DiffSecs = EndTime - StartTime
      IF (DaysElapsed .GT. 0) THEN
        IF (DiffSecs .LT. 0) THEN
          DaysElapsed = DaysElapsed - 1
          DiffSecs = DiffSecs + OneDay
        ENDIF
!C Divide number of days into weeks and days
        nweeks = DaysElapsed / 7
        IF (nweeks .NE. 0) THEN
          DaysElapsed = DaysElapsed - nweeks*7
          tIntStr = Integer2String(nweeks)
          SA_DurationStr = SA_DurationStr(1:DSLen)//tIntStr(1:LEN_TRIM(tIntStr))//" weeks"
          DSLen = LEN_TRIM(SA_DurationStr)
          IF (nweeks .EQ. 1) DSLen = DSLen - 1
          NItems = NItems + 1
        ENDIF
        IF (DaysElapsed .NE. 0) THEN
          IF (NItems .GT. 0) THEN
            SA_DurationStr = SA_DurationStr(1:DSLen)//", "
            DSLen = DSLen + 2
          ENDIF
          tIntStr = Integer2String(DaysElapsed)
          SA_DurationStr = SA_DurationStr(1:DSLen)//tIntStr(1:LEN_TRIM(tIntStr))//" days"
          DSLen = LEN_TRIM(SA_DurationStr)
          IF (DaysElapsed .EQ. 1) DSLen = DSLen - 1
          NItems = NItems + 1
        ENDIF
      ENDIF
!C Divide number of seconds into hours, minutes and seconds
        nhours = DiffSecs / 3600
        IF (nhours .NE. 0) THEN
          DiffSecs = DiffSecs - nhours*3600
          IF (NItems .GT. 0) THEN
            SA_DurationStr = SA_DurationStr(1:DSLen)//", "
            DSLen = DSLen + 2
          ENDIF
          tIntStr = Integer2String(nhours)
          SA_DurationStr = SA_DurationStr(1:DSLen)//tIntStr(1:LEN_TRIM(tIntStr))//" hours"
          DSLen = LEN_TRIM(SA_DurationStr)
          IF (nhours .EQ. 1) DSLen = DSLen - 1
          NItems = NItems + 1
        ENDIF
        nminutes = DiffSecs / 60
        IF (nminutes .NE. 0) THEN
          DiffSecs = DiffSecs - nminutes*60
          IF (NItems .GT. 0) THEN
            SA_DurationStr = SA_DurationStr(1:DSLen)//", "
            DSLen = DSLen + 2
          ENDIF
          tIntStr = Integer2String(nminutes)
          SA_DurationStr = SA_DurationStr(1:DSLen)//tIntStr(1:LEN_TRIM(tIntStr))//" minutes"
          DSLen = LEN_TRIM(SA_DurationStr)
          IF (nminutes .EQ. 1) DSLen = DSLen - 1
          NItems = NItems + 1
        ENDIF
        IF (DiffSecs .NE. 0) THEN
          IF (NItems .GT. 0) THEN
            SA_DurationStr = SA_DurationStr(1:DSLen)//", "
            DSLen = DSLen + 2
          ENDIF
          tIntStr = Integer2String(DiffSecs)
          SA_DurationStr = SA_DurationStr(1:DSLen)//tIntStr(1:LEN_TRIM(tIntStr))//" seconds"
          DSLen = LEN_TRIM(SA_DurationStr)
          IF (DiffSecs .EQ. 1) DSLen = DSLen - 1
          NItems = NItems + 1
        ENDIF
      CALL InfoMessage('The Simulated Annealing took '//SA_DurationStr(1:DSLen))
!C After completion, save the list of solutions
      CALL SaveMultiRun_LogData
      CALL OutputChi2vsMoves
      CALL SetModeMenuState(0,0)
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
      CALL WDialogSelect(IDD_Summary)
      CALL WDialogHide
      IF (iMyExit .EQ. 5) THEN
        CALL WizardWindowShow(IDD_SA_Modal_input2)
      ELSE
        CALL SelectMode(IDB_AnalyseSolutions)
        CALL WDialogSelect(IDD_SAW_Page5)
        CALL WDialogPutInteger(IDF_Limit1,1)
        CALL WDialogPutInteger(IDF_Limit2,NumOf_SA_Runs)
        CALL WizardWindowShow(IDD_SAW_Page5)
      ENDIF

      END SUBROUTINE BeginSA
!
!*****************************************************************************
!
! JCC Check the files before we trash them  
      INTEGER FUNCTION CheckOverwriteSaOutput()

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

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
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.cif'
        INQUIRE(FILE=tFileName,EXIST=OutputExists) 
        IF (OutputExists) GOTO 10
        tFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//NumStr//'.res'
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
        CALL WSelectFile('Output files (*.pdb,*.cssr,*.ccl,*.cif,*.res,*.pro)|*.pdb;*.cssr;*.ccl;*.cif;*.res;*.pro|', &
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
      SUBROUTINE FillSymmetry_2

      USE PO_VAR
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

! Covers the eventuality of the default space group option.
! We need to determine the number of symmetry operators etc.
      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER     msymmin
      PARAMETER ( msymmin = 10 )
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                           symline
      COMMON /symgencmn/ nsymmin, symmin(1:4,1:4,1:msymmin), symline(1:msymmin)

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      CHARACTER*10 filnam_root
      COMMON /commun/ filnam_root

      CHARACTER*6 PNAME
      INTEGER hFile, iSym
      INTEGER, EXTERNAL :: GetCrystalSystem
      REAL phases(1:48), RefHT(1:3,1:48)
      REAL PrfDir(1:3), H(1:3), RefLen
      INTEGER i, ii, iR
      REAL, EXTERNAL :: VCTMOD, SCLPRD

   10 IBMBER = 0
      hFile = 42
      OPEN(hFile,FILE='polys.ccl',status='unknown',ERR=999)
      WRITE(hFile,4210,ERR=999) 
 4210 FORMAT('N Determining the space group ')
      WRITE(hFile,4220,ERR=999) (CellPar(i),i=1,6)
 4220 FORMAT('C ',3F9.5,3F9.3)
      IF (NumberSGTable .GE. 1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin .GT. 0) THEN
          DO iSym = 1, nsymmin
            WRITE(hFile,4235,ERR=999) symline(iSym)
 4235       FORMAT('S ',A)
          ENDDO
        ENDIF
      ENDIF
      CLOSE(hFile)
      PNAME = 'SPGMAK'
      filnam_root = 'polys'
      NINIT = 1
      CALL PREFIN(PNAME)
      CALL SYMOP
      IF (IBMBER .EQ. 0) THEN
        CALL OPSYM(1)
        CALL RECIP
      ENDIF
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
      IF (PrefParExists) THEN
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
      ENDIF
      RETURN
  999 CALL ErrorMessage('Error writing temporary file for space group decoding.')
      CLOSE(hFile)

      END SUBROUTINE FillSymmetry_2
!
!*****************************************************************************
!
