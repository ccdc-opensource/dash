

      SUBROUTINE ErrorMessage(TheMessage)
!
! Displays an error message
!
! INPUT   : TheMessage : The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN   ) :: TheMessage

      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Error")
      RETURN

      END SUBROUTINE ErrorMessage
!
!*****************************************************************************
!
      SUBROUTINE DebugErrorMessage(TheMessage)
!
! Displays an error message that should be ignored in the release version of DASH.
! Before release, this subroutine should simply RETURN without doing anything.
!
! INPUT   : TheMessage : The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN   ) :: TheMessage

      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Debug Error")
      RETURN

      END SUBROUTINE DebugErrorMessage
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
      SUBROUTINE GetEvent
!
! Fetches the last event from Winteracter and places it in two global variables
! called EventType and EventInfo (both in VARIABLES)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

  10  CALL WMessage(EventType,EventInfo)
      SELECT CASE (EventInfo%WIN)
        CASE (0) ! Main window
          SELECT CASE (EventType)
            CASE (Expose,Resize)
              CALL Redraw()
              GOTO 10
            CASE (MenuSelect)
              CALL ProcessMenu
              GOTO 10
            CASE (CloseRequest)
              CALL WExit
              GOTO 10
            CASE (MouseButDown)
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                CALL Plot_Alter
              ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement - fitting this time
                CALL Move_CrossHair_Fit
              END IF
              GOTO 10
            CASE (KeyDown)
              CALL Check_KeyDown
              CALL Check_KeyDown_PeakFit_Inner
              GOTO 10
          END SELECT
          CASE (IDD_Plot_Option_Dialog)
            CALL DealWithPlotOptionsWindow
            GOTO 10
          CASE (IDD_Structural_Information)
            CALL DealWithStructuralInformation
            GOTO 10
          CASE (IDD_Data_Properties)
            CALL DealWithDiffractionSetupPane
            GOTO 10
          CASE (IDD_Peak_Positions)
            CALL DealWithPeakPositionsPane
            GOTO 10
          CASE (IDD_Index_Preparation)
            CALL DealWithIndexPreparation
            GOTO 10
          CASE (IDD_Crystal_Symmetry)
            CALL DealWithCrystalSymmetryPane
            GOTO 10
          CASE (IDD_Peak_Widths)
            ! Do nothing
            GOTO 10
          CASE (IDD_Polyfitter_Wizard_01)
            CALL DealWithMainWizardWindow
            GOTO 10
          CASE (IDD_PW_Page3)
            CALL DealWithWizardWindowDiffractionFileInput
            GOTO 10
          CASE (IDD_PW_Page4)
            CALL DealWithWizardWindowDiffractionSetup
            GOTO 10
          CASE (IDD_PW_Page5)
            CALL DealWithWizardWindowProfileRange
            GOTO 10
          CASE (IDD_PW_Page6)
            CALL DealWithWizardWindowBackground
            GOTO 10
          CASE (IDD_PW_Page1)
            CALL DealWithWizardWindowUnitCellParameters
            GOTO 10
          CASE (IDD_PW_Page2)
            CALL DealWithWizardWindowDiffractionSetup2
            GOTO 10
      END SELECT

      END SUBROUTINE GetEvent
!
!*****************************************************************************
!
      SUBROUTINE PeekEvent
!
! Reports if events are waiting to be handled. If not, program execution resumes.
! If an event is waiting, it is placed in two global variables
! called EventType and EventInfo (both in VARIABLES)
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

  10  CALL WMessagePeek(EventType,EventInfo)
      IF (EventType .NE. NoMessage) THEN
        SELECT CASE (EventInfo%WIN)
          CASE (0) ! Main window
            SELECT CASE (EventType)
              CASE (Expose,Resize)
                CALL Redraw()
                GOTO 10
              CASE (MenuSelect)
                CALL ProcessMenu
                GOTO 10
              CASE (CloseRequest)
                CALL WExit
                GOTO 10
              CASE (MouseButDown)
                IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                  CALL Plot_Alter
                ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement - fitting this time
                  CALL Move_CrossHair_Fit
                END IF
                GOTO 10
              CASE (KeyDown)
                CALL Check_KeyDown
                CALL Check_KeyDown_PeakFit_Inner
                GOTO 10
            END SELECT
          CASE (IDD_Plot_Option_Dialog)
            CALL DealWithPlotOptionsWindow
            GOTO 10
          CASE (IDD_Structural_Information)
            CALL DealWithStructuralInformation
            GOTO 10
          CASE (IDD_Data_Properties)
            CALL DealWithDiffractionSetupPane
            GOTO 10
          CASE (IDD_Peak_Positions)
            CALL DealWithPeakPositionsPane
            GOTO 10
          CASE (IDD_Index_Preparation)
            CALL DealWithIndexPreparation
            GOTO 10
          CASE (IDD_Crystal_Symmetry)
            CALL DealWithCrystalSymmetryPane
            GOTO 10
          CASE (IDD_Peak_Widths)
            ! Do nothing
            GOTO 10
          CASE (IDD_Polyfitter_Wizard_01)
            CALL DealWithMainWizardWindow
            GOTO 10
          CASE (IDD_PW_Page3)
            CALL DealWithWizardWindowDiffractionFileInput
            GOTO 10
          CASE (IDD_PW_Page4)
            CALL DealWithWizardWindowDiffractionSetup
            GOTO 10
          CASE (IDD_PW_Page5)
            CALL DealWithWizardWindowProfileRange
            GOTO 10
          CASE (IDD_PW_Page6)
            CALL DealWithWizardWindowBackground
            GOTO 10
          CASE (IDD_PW_Page1)
            CALL DealWithWizardWindowUnitCellParameters
            GOTO 10
          CASE (IDD_PW_Page2)
            CALL DealWithWizardWindowDiffractionSetup2
            GOTO 10
        END SELECT
      ENDIF

      END SUBROUTINE PeekEvent
!
!*****************************************************************************
!
      SUBROUTINE PushActiveWindowID
!
! In order for a Winteracter window to respond, it must be 'selected' first.
! Only a single Winteracter window can be 'selected' at a time.
! This routine pushes the ID of the currently selected window onto a small stack.
!
! NOTE: as with all stack mechanisms, each call to PushActiveWindowID must be
! matched with a call to PopActiveWindowID
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER MaxWinStack
      PARAMETER (MaxWinStack = 25)
  
      INTEGER WinStackPtr
      DATA    WinStackPtr / MaxWinStack /
      INTEGER WinStack
    
      COMMON /COMWS/ WinStackPtr, WinStack(1:MaxWinStack)

! Check if stack full
      IF (WinStackPtr .EQ. 0) RETURN
! If not, store current window ID
      WinStack(WinStackPtr) = WInfoDialog(CurrentDialog)
! Dec(StackPtr)
      WinStackPtr = WinStackPtr - 1
      RETURN

      END SUBROUTINE PushActiveWindowID
!
!*****************************************************************************
!
      SUBROUTINE PopActiveWindowID
!
! In order for a Winteracter window to respond, it must be 'selected' first.
! Only a single Winteracter window can be 'selected' at a time.
! This routine pops the ID of the previously selected window from a small stack.
!
! NOTE: as with all stack mechanisms, each call to PopActiveWindowID must
! match with a call to PushActiveWindowID
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER MaxWinStack
      PARAMETER (MaxWinStack = 25)

      INTEGER WinStackPtr
      DATA    WinStackPtr / MaxWinStack /
      INTEGER WinStack

      COMMON /COMWS/ WinStackPtr, WinStack(1:MaxWinStack)

! Check if stack empty
      IF (WinStackPtr .EQ. 25) THEN
        CALL ErrorMessage('Serious programming bug: WinStack empty.')
        RETURN
      ENDIF
! If not, Inc(StackPtr)
      WinStackPtr = WinStackPtr + 1
! Restore current window ID
!O      IF (WinStack(WinStackPtr) .NE. 0) CALL WDialogSelect(WinStack(WinStackPtr))
      CALL WDialogSelect(WinStack(WinStackPtr))
      RETURN

      END SUBROUTINE PopActiveWindowID
!
!*****************************************************************************
!
