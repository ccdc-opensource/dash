!**********************************************************************
!
! Initialise the multirun settings
!
!**********************************************************************
!
      SUBROUTINE Init_MultiRun

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      CHARACTER*80 SA_Label
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
      CALL WDialogSelect(IDD_SA_Action1)
      IF (MaxRuns .LT. 10) THEN
        WRITE(SA_Label,'(A,I1,A,I1)') 'Simulated annealing run number ',1,' of ',MaxRuns
      ELSE IF (MaxRuns .LT. 100) THEN
        WRITE(SA_Label,'(A,I1,A,I2)') 'Simulated annealing run number ',1,' of ',MaxRuns
      ELSE
        WRITE(SA_Label,'(A,I1,A,I3)') 'Simulated annealing run number ',1,' of ',MaxRuns
      END IF
      CALL WDialogPutString(IDD_SA_RunLabel,SA_Label)
      CALL PopActiveWindowID

      END SUBROUTINE Init_MultiRun
!
!**********************************************************************
!
! Add a multi-run pdb file to the list of solutions in the GUI
!
!**********************************************************************
!
      SUBROUTINE Log_SARun_Entry(PdbFileName, ProfileChiSquared, IntensityChiSquared)

      USE WINTERACTER
      USE DRUID_HEADER

      CHARACTER*(*) PdbFileName
      REAL ProfileChiSquared
      REAL IntensityChiSquared

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      REAL Grid_ProfileChi, Grid_IntensityChi
      CHARACTER*255 Grid_Buffer
      CHARACTER*80 SA_Label
      CHARACTER*4 CNruns,CMruns

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      CALL WGridRows(IDF_SA_Summary, SA_Run_Number)
      DO I = SA_Run_Number - 1,1,-1
        CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_ProfileChi)
        IF (Grid_ProfileChi .GT. ProfileChiSquared) THEN
          CALL WGridGetCellString(IDF_SA_Summary,1,I,Grid_Buffer)
          CALL WGridGetCellReal(IDF_SA_Summary,5,I,Grid_IntensityChi)
          CALL WGridPutCellReal(IDF_SA_Summary,4,I+1,Grid_ProfileChi,'(F7.2)')
          CALL WGridPutCellReal(IDF_SA_Summary,5,I+1,Grid_IntensityChi,'(F7.2)')
          CALL WGridPutCellString(IDF_SA_Summary,1,I+1,Grid_Buffer)   
        ELSE
          EXIT
        ENDIF
      ENDDO
      CALL WGridPutCellString(IDF_SA_Summary,1,I+1,PdbFileName(1:LEN_TRIM(PdbFileName)))
      CALL WGridPutCellReal(IDF_SA_Summary,4,  I+1,ProfileChiSquared,'(F7.2)')
      CALL WGridPutCellReal(IDF_SA_Summary,5,  I+1,IntensityChiSquared,'(F7.2)')
      CALL WDialogSelect(IDD_SA_Action1)
      WRITE(CNruns,'(I4)') SA_Run_Number + 1
      WRITE(CMruns,'(I4)') MaxRuns
      CMruns = ADJUSTL(CMruns)
      CNruns = ADJUSTL(CNruns)
      WRITE(SA_Label,'(A,A,A,A)') 'Simulated annealing run number ',&
           CNRuns(1:LEN_TRIM(CNruns)),' of ',CMRuns(1:LEN_TRIM(CMRuns))
      CALL WDialogPutString(IDD_SA_RunLabel,SA_Label)
      Ierrlab = InfoError(1)
      CALL PopActiveWindowID

      END SUBROUTINE Log_SARun_Entry
!
!**********************************************************************
!
      SUBROUTINE SaveMultiRun_LogData

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) cssr_file, pdb_file, ccl_file, log_file, pro_file
      COMMON /outfilnam/       cssr_file, pdb_file, ccl_file, log_file, pro_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      INTEGER I
      REAL Grid_ProfileChi, Grid_IntensityChi
      CHARACTER(MaxPathLength) Grid_Buffer

      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      OPEN(unit=101, file=log_file(1:log_flen), status = 'unknown', err = 99)
      WRITE(101,*) 'File name,Profile Chi Squared,Intensity Chi Squared'
      DO I = 1, SA_Run_Number
        CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_ProfileChi)
        CALL WGridGetCellString(IDF_SA_Summary,1,I,Grid_Buffer)
        CALL WGridGetCellReal(IDF_SA_Summary,5,I,Grid_IntensityChi)
        WRITE(101,10) Grid_Buffer(1:LEN_TRIM(Grid_Buffer)),Grid_ProfileChi,Grid_IntensityChi
      ENDDO
 10   FORMAT(A,',',F10.4,',',F10.4)
      CLOSE (101)
 99   RETURN
 
      END SUBROUTINE SaveMultiRun_LogData
!
!**********************************************************************
!
