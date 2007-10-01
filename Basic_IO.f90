

      SUBROUTINE ErrorMessage(TheMessage)
!
! Displays an error message
!
! INPUT   : TheMessage : The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN) :: TheMessage

      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Error")
      RETURN

      END SUBROUTINE ErrorMessage
!
!*****************************************************************************
!
      LOGICAL FUNCTION Confirm(TheQuestion)      
!
! Asks the user to confirm before execution of next action.
! Please note that 'Cancel', meaning that the user panicked, equates to 'No',
! therefore, harmful actions should take place after explicit 'Yes' only.
!
! INPUT   : TheQuestion : The question to be confirmed
!
! RETURNS : .TRUE.  if user pressed 'Yes'/'OK'
!           .FALSE. if user pressed 'No'/'Abort'/'Cancel'
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN) :: TheQuestion

      CALL WMessageBox(YesNo,QuestionIcon,CommonOK,TheQuestion(1:LEN_TRIM(TheQuestion)),'Confirm')
      Confirm = (WInfoDialog(ExitButtonCommon) .EQ. CommonYes)
      RETURN

      END FUNCTION Confirm
!
!*****************************************************************************
!
