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
      SUBROUTINE Log_SARun_Entry(IntensityChiSquared)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL,          INTENT (IN   ) :: IntensityChiSquared

      INCLUDE 'PARAMS.INC'

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      CHARACTER(MaxPathLength) OutputFilesBaseName
      INTEGER                                       OFBN_Len
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OutputFilesBaseName, OFBN_Len, SA_RunNumberStr

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_4)
          
      REAL            BestValuesDoF
      COMMON /SOLCOM/ BestValuesDoF(1:mvar,1:MaxRun)

      REAL                XAtmCoords
      COMMON /PDBOVERLAP/ XAtmCoords(1:3,1:MaxAtm_4,1:MaxRun)

      REAL Grid_ProfileChi, Grid_IntensityChi
      INTEGER Grid_Overlay
      CHARACTER*MaxPathLength Grid_Buffer
      INTEGER I, J, iAtom
      REAL Curr_BestValuesDoF(1:mvar), Curr_XAtmCoords(1:3,1:MaxAtm_4)
      CHARACTER*2 RowLabelStr

      DO J = 1, nvar
        Curr_BestValuesDoF(J) = BestValuesDoF(J,Curr_SA_Run)
      ENDDO
      DO iAtom = 1, TotNumOfAtoms
        Curr_XAtmCoords(1,iAtom) = XAtmCoords(1,iAtom,Curr_SA_Run)
        Curr_XAtmCoords(2,iAtom) = XAtmCoords(2,iAtom,Curr_SA_Run)
        Curr_XAtmCoords(3,iAtom) = XAtmCoords(3,iAtom,Curr_SA_Run)
      ENDDO
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page5)
      CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
      DO I = 1, NumOf_SA_Runs
        WRITE(RowLabelStr,'(I2)') I
        CALL WGridLabelRow(IDF_SA_summary,I,RowLabelStr)
      ENDDO
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
          DO J = 1, nvar
            BestValuesDoF(J,I+1) = BestValuesDoF(J,I)
          ENDDO
          DO iAtom = 1, TotNumOfAtoms
            XAtmCoords(1,iAtom,I+1) = XAtmCoords(1,iAtom,I)
            XAtmCoords(2,iAtom,I+1) = XAtmCoords(2,iAtom,I)
            XAtmCoords(3,iAtom,I+1) = XAtmCoords(3,iAtom,I)
          ENDDO
        ELSE
          EXIT
        ENDIF
      ENDDO
      CALL WGridPutCellString(IDF_SA_Summary,1,I+1,SA_RunNumberStr)
      CALL WGridPutCellCheckBox(IDF_SA_summary,3,I+1,Checked)
      CALL WGridPutCellReal(IDF_SA_Summary,4,  I+1,CHIPROBEST,'(F7.2)')
      CALL WGridPutCellReal(IDF_SA_Summary,5,  I+1,IntensityChiSquared,'(F7.2)')
      DO J = 1, nvar
        BestValuesDoF(J,I+1) = Curr_BestValuesDoF(J)
      ENDDO
      DO iAtom = 1, TotNumOfAtoms
        XAtmCoords(1,iAtom,I+1) = Curr_XAtmCoords(1,iAtom)
        XAtmCoords(2,iAtom,I+1) = Curr_XAtmCoords(2,iAtom)
        XAtmCoords(3,iAtom,I+1) = Curr_XAtmCoords(3,iAtom)
      ENDDO
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

      CALL WDialogSelect(IDD_SAW_Page5)
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
