!
!*****************************************************************************
!
! Initialise the multirun settings
!
!*****************************************************************************
!
      SUBROUTINE Init_MultiRun

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Input3)
      CALL WDialogGetInteger(IDF_SA_MaxRepeats,MaxRuns)
      CALL WDialogGetReal(IDF_MaxMoves1,MaxMoves1)
      IF (MaxMoves1 .LT.   0.001) MaxMoves1 =   0.001
      IF (MaxMoves1 .GT. 100.0  ) MaxMoves1 = 100.0
      CALL WDialogGetInteger(IDF_MaxMoves2,MaxMoves2)
      IF (MaxMoves2 .LT. 1) MaxMoves2 = 1
      IF (MaxMoves2 .GT. 8) MaxMoves2 = 8
      tMaxMoves = MaxMoves1 * (10**FLOAT(MaxMoves2))
      IF (tMaxMoves .LT. 10.0) tMaxMoves = 10.0
      IF (tMaxMoves .GT.  2.0E9) tMaxMoves = 2.0E9
      MaxMoves = NINT(tMaxMoves)
      CALL WDialogGetReal(IDF_SA_ChiTest,   ChiMult)
      RESTART = ((MaxRuns .GT. 1) .AND. (ChiMult .GT. 0.0))
      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      CALL WDialogClearField(IDF_SA_Summary)
      CALL PopActiveWindowID

      END SUBROUTINE Init_MultiRun
!
!*****************************************************************************
!
! Add a multi-run pdb file to the list of solutions in the GUI
!
!*****************************************************************************
!
      SUBROUTINE Log_SARun_Entry(IntensityChiSquared)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL,          INTENT (IN   ) :: IntensityChiSquared

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      REAL Grid_ProfileChi, Grid_IntensityChi
      INTEGER Grid_Overlay
      CHARACTER*MaxPathLength Grid_Buffer
      INTEGER I
      CHARACTER*(MaxPathLength) PdbFileName

      PdbFileName = OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.pdb'
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
      DO I = NumOf_SA_Runs-1, 1, -1
        CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_ProfileChi)
        IF (Grid_ProfileChi .GT. CHIPROBEST) THEN
          CALL WGridGetCellString  (IDF_SA_Summary,1,I,Grid_Buffer)
          CALL WGridGetCellCheckBox(IDF_SA_Summary,3,I,Grid_Overlay)
          CALL WGridGetCellReal    (IDF_SA_Summary,5,I,Grid_IntensityChi)
          CALL WGridPutCellString  (IDF_SA_Summary,1,I+1,Grid_Buffer) 
          CALL WGridPutCellCheckBox(IDF_SA_Summary,3,I+1,Grid_Overlay)
          CALL WGridPutCellReal    (IDF_SA_Summary,4,I+1,Grid_ProfileChi,'(F7.2)')
          CALL WGridPutCellReal    (IDF_SA_Summary,5,I+1,Grid_IntensityChi,'(F7.2)')
        ELSE
          EXIT
        ENDIF
      ENDDO
      CALL WGridPutCellString(IDF_SA_Summary,1,I+1,PdbFileName(1:LEN_TRIM(PdbFileName)))
      CALL WGridPutCellCheckBox(IDF_SA_summary,3,I+1,Checked)
      CALL WGridPutCellReal(IDF_SA_Summary,4,  I+1,CHIPROBEST,'(F7.2)')
      CALL WGridPutCellReal(IDF_SA_Summary,5,  I+1,IntensityChiSquared,'(F7.2)')
      CALL PopActiveWindowID

      END SUBROUTINE Log_SARun_Entry
!
!*****************************************************************************
!
      SUBROUTINE SaveMultiRun_LogData

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER I
      REAL Grid_ProfileChi, Grid_IntensityChi
      CHARACTER(MaxPathLength) Grid_Buffer

      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      OPEN(UNIT=101, FILE=OutputFilesBaseName(1:OFBN_Len)//'.log', status = 'unknown',ERR=999)
      WRITE(101,*,ERR=999) 'File name, Profile Chi Squared, Intensity Chi Squared'
      DO I = 1, NumOf_SA_Runs
        CALL WGridGetCellString(IDF_SA_Summary,1,I,Grid_Buffer)
        CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_ProfileChi)
        CALL WGridGetCellReal(IDF_SA_Summary,5,I,Grid_IntensityChi)
        WRITE(101,10,ERR=999) Grid_Buffer(1:LEN_TRIM(Grid_Buffer)),Grid_ProfileChi,Grid_IntensityChi
      ENDDO
 10   FORMAT(A,',',F10.4,',',F10.4)
      CLOSE (101)
      RETURN
 999  CALL ErrorMessage('Error writing log file.')
      CLOSE (101)
 
      END SUBROUTINE SaveMultiRun_LogData
!
!*****************************************************************************
!
