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
      CALL WDialogSelect(IDD_SAW_Page5)
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
      SUBROUTINE Log_SARun_Entry

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER I, iSol

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page5)
      DO iSol = 1, NumOf_SA_Runs-1
        CALL WGridGetCellCheckBox(IDF_SA_Summary,3,iSol,iSolTicked(iSol2Run(iSol)))
      ENDDO
! Add this solution to the list
      DO I = 1, nvar
        BestValuesDoF(I,Curr_SA_Run) = SNGL(XOPT(I))
      ENDDO
      IntensityChiSqd(Curr_SA_Run) = SNGL(-FOPT)
      ProfileChiSqd(Curr_SA_Run) = CHIPROBEST
      iSolTicked(Curr_SA_Run) = 1
! Now sort the list according to Profile chi sqd
      CALL SORT_REAL(ProfileChiSqd,iSol2Run,NumOf_SA_Runs)
      CALL Update_Solutions
      CALL PopActiveWindowID

      END SUBROUTINE Log_SARun_Entry
!
!*****************************************************************************
!
      SUBROUTINE SaveMultiRun_LogData

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER iSol, hFile

      CALL WDialogSelect(IDD_SAW_Page5)
      hFile = 101
      OPEN(UNIT=hFile, FILE=OutputFilesBaseName(1:OFBN_Len)//'.log', status = 'unknown',ERR=999)
      WRITE(hFile,*,ERR=999) 'Run number, Profile Chi Squared, Intensity Chi Squared'
      DO iSol = 1, NumOf_SA_Runs
        WRITE(hFile,10,ERR=999) iSol2Run(iSol),                  &
                                ProfileChiSqd(iSol2Run(iSol)),   &
                                IntensityChiSqd(iSol2Run(iSol))
      ENDDO
 10   FORMAT(I3.3,',',F10.4,',',F10.4)
      CLOSE (hFile)
      RETURN
 999  CALL ErrorMessage('Error writing log file.')
      CLOSE (hFile)
 
      END SUBROUTINE SaveMultiRun_LogData
!
!*****************************************************************************
!
