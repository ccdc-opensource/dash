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

      INTEGER IRetVal, IHan
      CHARACTER*4 NextLine
      CHARACTER*5000 text

      NextLine = CHAR(13)//CHAR(10)
      text = 'The extracted intensities are ill conditioned due to a high degree of'//NextLine// &
             'overlap in the data set and so could be unreliable in certain ranges.'//NextLine// &
             'Full details of the problems are recorded in the fit list file, polyp.lis.'//NextLine// &
             'You may be able to fix the problem by decreasing the data range in'//NextLine// &
             '2-theta or by increasing the overlap criterion.'//NextLine//NextLine// &
             'To proceed, click Yes, to view the list file, click No.'

      CALL WMessageBox(YesNo, QuestionIcon, CommonOK, text(1:LEN_TRIM(text)), 'Errors detected during Pawley fit')

      IF ( WInfoDialog(ExitButtonCommon) .NE. CommonYes ) THEN
        CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,'View pawley fit log file'),IHan)
        CALL WEditFile('polyp.lis', Modal, 0, 0, SystemFixed)
        IRetVal = InfoError(1)
      ENDIF 
   !   CALL PopActiveWindowID
            
      END SUBROUTINE PawleyWarning
!
!*****************************************************************************
!
