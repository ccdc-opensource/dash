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

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2

      !C All a bit dodgy: these are the only few variables that are read directly from the
      !C interface, and therefore need special treatment when in batch mode.

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) &
        RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Input3_2)
      CALL WDialogGetInteger(IDF_SA_MaxRepeats, MaxRuns)
      CALL WDialogGetReal(IDF_MaxMoves1, MaxMoves1)
      IF (MaxMoves1 .LT.   0.001) MaxMoves1 =   0.001
      IF (MaxMoves1 .GT. 100.0  ) MaxMoves1 = 100.0
      CALL WDialogGetInteger(IDF_MaxMoves2, MaxMoves2)
      IF (MaxMoves2 .LT. 1) MaxMoves2 = 1
      IF (MaxMoves2 .GT. 8) MaxMoves2 = 8
      tMaxMoves = MaxMoves1 * (10**FLOAT(MaxMoves2))
      IF (tMaxMoves .LT. 10.0) tMaxMoves = 10.0
      IF (tMaxMoves .GT.  2.0E9) tMaxMoves = 2.0E9
      MaxMoves = NINT(tMaxMoves)
      CALL WDialogGetReal(IDF_SA_ChiTest, ChiMult)
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
      USE ZMVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER I, iSol
      INTEGER iFrg
      INTEGER KK, KK1, KK2, KK3, JQ, JQS
      REAL QQSUM, QDEN
      REAL Duonion(0:1)

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Summary)
      DO iSol = 1, NumOf_SA_Runs-1
        CALL WGridGetCellCheckBox(IDF_SA_Summary, 3, iSol, iSolTicked(iSol2Run(iSol)))
      ENDDO
! Add this solution to the list
      DO I = 1, nvar
        BestValuesDoF(I,Curr_SA_Run) = XOPT(I)
      ENDDO
      !C Normalise quaternions and translations
      KK = 0
! Loop over all the fragments
      DO iFrg = 1, nFrag
        KK1 = KK + 1     ! x-translation
        KK2 = KK + 2     ! y-translation
        KK3 = KK + 3     ! z-translation
        BestValuesDoF(KK1,Curr_SA_Run) = BestValuesDoF(KK1,Curr_SA_Run) - INT(BestValuesDoF(KK1,Curr_SA_Run))  ! Position centre of mass inside unit cell
        BestValuesDoF(KK2,Curr_SA_Run) = BestValuesDoF(KK2,Curr_SA_Run) - INT(BestValuesDoF(KK2,Curr_SA_Run))
        BestValuesDoF(KK3,Curr_SA_Run) = BestValuesDoF(KK3,Curr_SA_Run) - INT(BestValuesDoF(KK3,Curr_SA_Run))
        KK = KK + 3
! If more than one atom then proceed
        IF (natoms(iFrg) .GT. 1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
          IF (UseQuaternions(iFrg)) THEN
            QQSUM = 0.0
            DO JQ = 0, 3
              JQS = 1 + JQ + KK
              QQSUM = QQSUM + BestValuesDoF(JQS,Curr_SA_Run)**2
            ENDDO
! QQSUM now holds the sum of the squares of the quaternions
            QDEN = 1.0 / SQRT(QQSUM)
            DO JQ = 0, 3
              JQS = 1 + JQ + KK
              BestValuesDoF(JQS,Curr_SA_Run) = QDEN * BestValuesDoF(JQS,Curr_SA_Run)
            ENDDO
            KK = KK + 4
          ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
            Duonion(0) = BestValuesDoF(KK+1, Curr_SA_Run)
            Duonion(1) = BestValuesDoF(KK+2, Curr_SA_Run)
            QDEN = 1.0 / SQRT(Duonion(0)**2 + Duonion(1)**2)
            BestValuesDoF(KK+1,Curr_SA_Run) = BestValuesDoF(KK+1,Curr_SA_Run) * QDEN 
            BestValuesDoF(KK+2,Curr_SA_Run) = BestValuesDoF(KK+2,Curr_SA_Run) * QDEN 
            KK = KK + 2
          ENDIF
        ENDIF
        DO I = 1, natoms(iFrg)
          IF (IOPTB(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
          ENDIF
          IF (IOPTA(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
          ENDIF
          IF (IOPTT(I,iFrg) .EQ. 1) THEN
            KK = KK + 1
          ENDIF
        ENDDO
      ENDDO
      IntensityChiSqd(Curr_SA_Run) = FOPT
      ProfileChiSqd(Curr_SA_Run) = CHIPROBEST
! Now sort the list according to intensity chi sqd
      CALL SORT_REAL(IntensityChiSqd, iSol2Run, NumOf_SA_Runs)
      CALL Update_Solutions
      CALL PopActiveWindowID

      END SUBROUTINE Log_SARun_Entry
!
!*****************************************************************************
!
      SUBROUTINE SaveMultiRun_LogData

      USE WINTERACTER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      INTEGER iSol, hFile

      IF ( in_batch ) &
        RETURN
      IF (OFBN_Len .EQ. 0) THEN
        CALL DebugErrorMessage("OFBN_Len .EQ. 0 when saving .log file.")
      ELSE
        hFile = 101
        OPEN(UNIT=hFile, FILE=OutputFilesBaseName(1:OFBN_Len)//'.log', status = 'unknown',ERR=999)
        WRITE(hFile,*,ERR=999) 'Run number, Profile Chi Squared, Intensity Chi Squared'
        DO iSol = 1, NumOf_SA_Runs
          WRITE(hFile,10,ERR=999) iSol2Run(iSol),                  &
                                  ProfileChiSqd(iSol2Run(iSol)),   &
                                  IntensityChiSqd(iSol2Run(iSol))
        ENDDO
 10     FORMAT(I3.3,',',F10.4,',',F10.4)
        CLOSE (hFile)
        RETURN
 999    CALL ErrorMessage('Error writing log file.')
        CLOSE (hFile)
      ENDIF
 
      END SUBROUTINE SaveMultiRun_LogData
!
!*****************************************************************************
!
