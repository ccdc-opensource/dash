!**********************************************************************
!
! Initialise the multirun settings
!
!**********************************************************************

    SUBROUTINE Init_MultiRun

	USE WINTERACTER
	USE DRUID_HEADER

	LOGICAL RESTART
	INTEGER SA_Run_Number
	COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
    CHARACTER*80 SA_Label

    INTEGER ICurSel

	ICurSel = WinfoDialog(CurrentDialog)

    CALL WDialogSelect(IDD_SA_Input3)

	SA_Run_Number = 0
	MinMoves = 15000
    CALL WDialogGetInteger(IDF_SA_MaxRepeats,MaxRuns)
    CALL WDialogGetInteger(IDF_SA_MaxMoves,  MaxMoves)
	CALL WDialogGetReal(IDF_SA_ChiTest,   ChiMult)

	RESTART = (MaxRuns .GT. 1 .AND. ChiMult .GT. 0.0)

!ep    CALL WDialogSelect(IDD_SA_Multi_Completed)
!    CALL WDialogClearField(IDF_SA_Solution_Grid)
	
	CALL WDialogSelect(IDD_SA_Multi_completed_ep)
    CALL WDialogClearField(IDF_SA_Summary)


    CALL WDialogSelect(IDD_SA_Action1)
	IF (MaxRuns .LT. 10) THEN
		WRITE(SA_Label,'(A,I1,A,I1)') 'Simulated annealing run number ',&
		1,' of ',MaxRuns
	ELSE IF (MaxRuns .LT. 100) THEN
		WRITE(SA_Label,'(A,I1,A,I2)') 'Simulated annealing run number ',&
		1,' of ',MaxRuns
	ELSE
		WRITE(SA_Label,'(A,I1,A,I3)') 'Simulated annealing run number ',&
		1,' of ',MaxRuns
	END IF
	CALL WDialogPutString(IDD_SA_RunLabel,SA_Label)

	IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)

	END SUBROUTINE Init_MultiRun

!**********************************************************************
!
! Add a multi-run pdb file to the list of solutions in the GUI
!
!**********************************************************************
	SUBROUTINE Log_SARun_Entry(PdbFileName,  ProfileChiSquared, IntensityChiSquared)

	USE WINTERACTER
	USE DRUID_HEADER
    CHARACTER*(*) PdbFileName
	REAL ProfileChiSquared
	REAL IntensityChiSquared
	INTEGER ICurSel

	LOGICAL RESTART
	INTEGER SA_Run_Number
	COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult

    REAL Grid_ProfileChi, Grid_IntensityChi
	CHARACTER*255 Grid_Buffer
    CHARACTER*80 SA_Label
	CHARACTER*4 CNruns,CMruns


	ICurSel = WinfoDialog(CurrentDialog)


!ep    CALL WDialogSelect(IDD_SA_Multi_Completed)
!ep    CALL WGridRows(IDF_SA_Solution_Grid, SA_Run_Number)
    
	CALL WDialogSelect(IDD_SA_Multi_completed_ep)
    CALL WGridRows(IDF_SA_Summary, SA_Run_Number)

    DO I = SA_Run_Number - 1,1,-1
!ep		CALL WGridGetCellReal(IDF_SA_Solution_Grid,3,I,Grid_ProfileChi)
!		IF (Grid_ProfileChi .GT. ProfileChiSquared) THEN
!			CALL WGridGetCellString(IDF_SA_Solution_Grid,1,I,Grid_Buffer)
!			CALL WGridGetCellReal(IDF_SA_Solution_Grid,2,I,Grid_IntensityChi)

!		    CALL WGridPutCellReal(IDF_SA_Solution_Grid,3,I+1,Grid_ProfileChi,'(f7.2)')
!		    CALL WGridPutCellReal(IDF_SA_Solution_Grid,2,I+1,Grid_IntensityChi,'(f7.2)')
!			CALL WGridPutCellString(IDF_SA_Solution_Grid,1,I+1,Grid_Buffer)


		CALL WGridGetCellReal(IDF_SA_Summary,3,I,Grid_ProfileChi)
		IF (Grid_ProfileChi .GT. ProfileChiSquared) THEN
			CALL WGridGetCellString(IDF_SA_Summary,1,I,Grid_Buffer)
	CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_IntensityChi)
			CALL WGridPutCellReal(IDF_SA_Summary,3,I+1,Grid_ProfileChi,'(f7.2)')
		    CALL WGridPutCellReal(IDF_SA_Summary,4,I+1,Grid_IntensityChi,'(f7.2)')
			CALL WGridPutCellString(IDF_SA_Summary,1,I+1,Grid_Buffer)	
		ELSE
		    EXIT
		ENDIF
	END DO

!ep	CALL WGridPutCellString(IDF_SA_Solution_Grid,1,I+1,PdbFileName(1:len_trim(PdbFileName)))
!	CALL WGridPutCellReal(IDF_SA_Solution_Grid,3,  I+1,ProfileChiSquared,'(f7.2)')
!	CALL WGridPutCellReal(IDF_SA_Solution_Grid,2,  I+1,IntensityChiSquared,'(f7.2)')

	CALL WGridPutCellString(IDF_SA_Summary,1,I+1,PdbFileName(1:len_trim(PdbFileName)))
	CALL WGridPutCellReal(IDF_SA_Summary,3,  I+1,ProfileChiSquared,'(f7.2)')
	CALL WGridPutCellReal(IDF_SA_Summary,4,  I+1,IntensityChiSquared,'(f7.2)')

    CALL WDialogSelect(IDD_SA_Action1)

    WRITE(CNruns,'(I4)') SA_Run_Number + 1
	WRITE(CMruns,'(I4)') MaxRuns

	CMruns = ADJUSTL(CMruns)
	CNruns = ADJUSTL(CNruns)
	
	WRITE(SA_Label,'(A,A,A,A)') 'Simulated annealing run number ',&
	CNRuns(1:len_trim(CNruns)),' of ',CMRuns(1:len_trim(CMRuns))

	CALL WDialogPutString(IDD_SA_RunLabel,SA_Label)
	
    Ierrlab = InfoError(1)

	IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)


	END SUBROUTINE Log_SARun_Entry


	SUBROUTINE SaveMultiRun_LogData
	USE WINTERACTER
	USE DRUID_HEADER
! ep appended
    character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file, pro_file
    common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file, pro_file
    common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen, pro_flen

	LOGICAL RESTART
	INTEGER SA_Run_Number, I
	COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult

    REAL Grid_ProfileChi, Grid_IntensityChi
	CHARACTER*255 Grid_Buffer

	ICurSel = WinfoDialog(CurrentDialog)

!ep    CALL WDialogSelect(IDD_SA_Multi_Completed)
	CALL WDialogSelect(IDD_SA_Multi_completed_ep)

	OPEN(unit=101, file=log_file(1:log_flen), status = 'unknown', err = 99)
	WRITE(101,*) 'File name,Profile Chi Squared,Intensity Chi Squared'
    DO I = 1, SA_Run_Number
!ep		CALL WGridGetCellReal(IDF_SA_Solution_Grid,3,I,Grid_ProfileChi)
!		CALL WGridGetCellString(IDF_SA_Solution_Grid,1,I,Grid_Buffer)
!		CALL WGridGetCellReal(IDF_SA_Solution_Grid,2,I,Grid_IntensityChi)
!		WRITE(101,10) Grid_Buffer(1:len_trim(Grid_Buffer)),Grid_ProfileChi,Grid_IntensityChi

		CALL WGridGetCellReal(IDF_SA_Summary,3,I,Grid_ProfileChi)
		CALL WGridGetCellString(IDF_SA_Summary,1,I,Grid_Buffer)
		CALL WGridGetCellReal(IDF_SA_Summary,4,I,Grid_IntensityChi)
		WRITE(101,10) Grid_Buffer(1:len_trim(Grid_Buffer)),Grid_ProfileChi,Grid_IntensityChi

	END DO

 10 FORMAT(A,',',F10.4,',',F10.4)
    CLOSE (101)
 99 RETURN
 
    END SUBROUTINE SaveMultiRun_LogData