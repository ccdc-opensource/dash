

! Routines for error checking the pawley fit
! Modes can be as follows
! Mode = 1 : Add an error into local array
! Mode = 2 : Reset
! Returns the number of errors that occurred

	INTEGER FUNCTION PawleyErrorLog(Mode)

	INTEGER Mode
	INTEGER Nerr

	SAVE Nerr
	DATA Nerr / 0 /

    PawleyErrorLog = Nerr

	IF (Mode .EQ. 1) THEN
		Nerr = Nerr + 1
	ELSE IF (Mode .EQ. 2) THEN
		Nerr = 0
	END IF
	RETURN
	END FUNCTION PawleyErrorLog



	SUBROUTINE PawleyWarning()
	USE WINTERACTER
	USE DRUID_HEADER
	INTEGER ICurSel,IRetVal
    INTEGER IHan

	ICurSel = WInfoDialog(CurrentDialog)

	CALL  WDialogSelect(IDD_Pawley_ErrorLog)
	CALL  WDialogShow(-1,-1,0,Modal)
	IretVal = WInfoDialog(ExitButton)


	IF (IRetVal .EQ. ID_Edit_PawleyLog) THEN
	    CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,&
									   'View pawley fit log file'),IHan)
		CALL WEditFile('polyp.lis', Modal)
		IRetVal =  InfoError(1)
	ENDIF 

	IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)
		
	END SUBROUTINE PawleyWarning
	
