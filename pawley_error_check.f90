!
!*****************************************************************************
!
      INTEGER FUNCTION PawleyErrorLog(Mode)
!
! Routine for error checking the pawley fit
! Modes can be as follows
! Mode = 1 : Add an error into local array
! Mode = 2 : Reset
!
! RETURNS : the number of errors that occurred
!
      INTEGER, INTENT (IN   ) :: Mode

      INTEGER Nerr

      SAVE Nerr
      DATA Nerr / 0 /

      PawleyErrorLog = Nerr
      SELECT CASE (Mode)
        CASE (1)
          Nerr = Nerr + 1
        CASE (2)
          Nerr = 0
      END SELECT

      END FUNCTION PawleyErrorLog
!
!*****************************************************************************
!
      SUBROUTINE PawleyWarning

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER IRetVal
      INTEGER IHan

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_ErrorLog)
      CALL WDialogShow(-1,-1,0,Modal)
      IretVal = WInfoDialog(ExitButton)
      IF (IRetVal .EQ. ID_Edit_PawleyLog) THEN
        CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,'View pawley fit log file'),IHan)
        CALL WEditFile('polyp.lis', Modal, 0, 0, SystemFixed)
        IRetVal = InfoError(1)
      ENDIF 
      CALL PopActiveWindowID
            
      END SUBROUTINE PawleyWarning
!
!*****************************************************************************
!
