!
!*****************************************************************************
!
      SUBROUTINE ErrorMessage(TheMessage)
!
! Displays an error message
!
! INPUT   : TheMessage = The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN   ) :: TheMessage

      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Error")

      END SUBROUTINE ErrorMessage
!
!*****************************************************************************
!
      SUBROUTINE DebugErrorMessage(TheMessage)
!
! Displays an error message that should be ignored in the release version of DASH.
! Before release, this subroutine should simply RETURN without doing anything.
!
! INPUT   : TheMessage = The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN   ) :: TheMessage

      !RETURN
      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Debug Error")

      END SUBROUTINE DebugErrorMessage
!
!*****************************************************************************
!
      SUBROUTINE WarningMessage(TheMessage)
!
! Displays a warning message
!
! INPUT   : TheMessage = The message to be displayed
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN   ) :: TheMessage

      CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Warning")

      END SUBROUTINE WarningMessage
!
!*****************************************************************************
!
      LOGICAL FUNCTION Confirm(TheQuestion)      
!
! Asks the user to confirm before execution of next action.
! Please note that 'Cancel', meaning that the user panicked, equates to 'No',
! therefore, harmful actions should take place after explicit 'Yes' only.
!
! INPUT   : TheQuestion = The question to be confirmed
!
! RETURNS : .TRUE.  if user pressed 'Yes'/'OK'
!           .FALSE. if user pressed 'No'/'Abort'/'Cancel'
!
      USE WINTERACTER
      
      CHARACTER*(*), INTENT(IN) :: TheQuestion

      CALL WMessageBox(YesNo,QuestionIcon,CommonOK,TheQuestion(1:LEN_TRIM(TheQuestion)),'Confirm')
      Confirm = (WInfoDialog(ExitButtonCommon) .EQ. CommonYes)

      END FUNCTION Confirm
!
!*****************************************************************************
!
      SUBROUTINE SetChildWinAutoClose(TheIHANDLE)

! For child windows that are an editor, the only event DASH has to handle itself
! (all other events for editor child windows are handled automatically by Winteracter)
! is the CloseRequest. By 'registering' a child window with GetEvent, by setting the
! ChildWinAutoClose variable for that child window to true, GetEvent will take care of that.

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheIHANDLE

      LOGICAL         ChildWinAutoClose
      COMMON /ChWAC/  ChildWinAutoClose(1:20)

      ChildWinAutoClose(TheIHANDLE) = .TRUE.

      END SUBROUTINE SetChildWinAutoClose
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

      LOGICAL MseBtnPressed
! The routines that act on the mousebutton presses are non-reentrant
! and the one should not be called when the other is active, so we must keep a flag if we are dealing
! with a mouse button press. 
      DATA MseBtnPressed / .FALSE. /
      COMMON /JvdS1/ MseBtnPressed

! For child windows that are an editor, the only event DASH has to handle itself
! (all other events for editor child windows are handled automatically by Winteracter)
! is the CloseRequest. By 'registering' a child window with GetEvent, by setting the
! ChildWinAutoClose variable for that child window to true, GetEvent will take care of that.
      LOGICAL         ChildWinAutoClose
      COMMON /ChWAC/  ChildWinAutoClose(1:20)

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
                IF (MseBtnPressed) GOTO 10
                MseBtnPressed = .TRUE.
                CALL Plot_Alter
                MseBtnPressed = .FALSE.
              ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement--fitting this time
                IF (MseBtnPressed) GOTO 10
                MseBtnPressed = .TRUE.
                CALL Move_CrossHair_Fit
                MseBtnPressed = .FALSE.
              END IF
              GOTO 10
            CASE (KeyDown)
              CALL Check_KeyDown
              CALL Check_KeyDown_PeakFit_Inner
              GOTO 10
          END SELECT
        CASE (1:20)
          IF ((EventType.EQ.CloseRequest) .AND. ChildWinAutoClose(EventInfo%WIN)) THEN
            CALL PushActiveWindowID
            CALL WindowCloseChild(EventInfo%WIN)
            ChildWinAutoClose(EventInfo%WIN) = .FALSE.
            CALL PopActiveWindowID
            GOTO 10
          ENDIF
        CASE (IDD_Plot_Option_Dialog)
          CALL DealWithPlotOptionsWindow
          GOTO 10
        CASE (IDD_Configuration)
          CALL DealWithConfiguration
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
        CASE (IDD_DV_Results)
          CALL DealWithDVResults
          GOTO 10
        CASE (IDD_Pawley_Status)
          CALL DealWithPawleyFitWindow
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

      LOGICAL MseBtnPressed
! The routines that act on the mousebutton presses are non-reentrant
! and the one should not be called when the other is active, so we must keep a flag if we are dealing
! with a mouse button press. 
      COMMON /JvdS1/ MseBtnPressed

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
                  IF (MseBtnPressed) GOTO 10
                  MseBtnPressed = .TRUE.
                  CALL Plot_Alter
                  MseBtnPressed = .FALSE.
                ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement - fitting this time
                  IF (MseBtnPressed) GOTO 10
                  MseBtnPressed = .TRUE.
                  CALL Move_CrossHair_Fit
                  MseBtnPressed = .FALSE.
                END IF
                GOTO 10
              CASE (KeyDown)
                CALL Check_KeyDown
                CALL Check_KeyDown_PeakFit_Inner
                GOTO 10
            END SELECT
          CASE (1:20)
            IF (EventType .EQ. CloseRequest) THEN
              CALL PushActiveWindowID
              CALL WindowCloseChild(EventInfo%WIN)
              CALL PopActiveWindowID
              GOTO 10
            ENDIF
          CASE (IDD_Plot_Option_Dialog)
            CALL DealWithPlotOptionsWindow
            GOTO 10
          CASE (IDD_Configuration)
            CALL DealWithConfiguration
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
          CASE (IDD_DV_Results)
            CALL DealWithDVResults
            GOTO 10
          CASE (IDD_Pawley_Status)
            CALL DealWithPawleyFitWindow
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

      END SUBROUTINE PopActiveWindowID
!
!*****************************************************************************
!
