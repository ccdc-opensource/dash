! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!U!*****************************************************************************
!U!
!U      SUBROUTINE DebugRoutine
!U
!U      IMPLICIT NONE
!U
!U      INCLUDE 'params.inc'
!U
!U      INTEGER         MATPNT
!U      REAL                         BLSQ
!U      COMMON /MATDAT/ MATPNT(MaxBVar+1), BLSQ(MaxBVar)
!U    
!U      INTEGER I
!U
!U      OPEN(UNIT=10,FILE='Debug.txt',ERR=999)
!U      DO I = 1, MaxBVar
!U        WRITE(10,'(F15.9)',ERR=999) BLSQ(I)
!U      ENDDO
!U      CLOSE(10)
!U      RETURN
!U  999 CALL DebugErrorMessage('Error while accessing debug file')
!U      CLOSE(10)
!U
!U      END SUBROUTINE DebugRoutine
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
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) THEN
        CALL AppendBatchLogFile("Error: "//TheMessage)
      ELSE
        CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Error")
      ENDIF

      END SUBROUTINE ErrorMessage
!
!*****************************************************************************
!
      SUBROUTINE DebugErrorMessage(TheMessage)
!
! Displays an error message that should be ignored in the release version of DASH.
!
! INPUT   : TheMessage = The message to be displayed
!
      USE WINTERACTER
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL            ShowAgain
      INTEGER                       Counter
      COMMON  / DBGMSG / ShowAgain, Counter

      LOGICAL, EXTERNAL :: Confirm

#ifdef ONTBUG
      IF ( in_batch ) THEN
        CALL AppendBatchLogFile("Debug error: "//TheMessage)
      ELSE
        IF (ShowAgain) THEN
          ShowAgain = Confirm('Debug error : '//TheMessage(1:LEN_TRIM(TheMessage))//CHAR(13)//'More Debug Error messages?')
        ENDIF
      ENDIF
#endif

      END SUBROUTINE DebugErrorMessage
!
!*****************************************************************************
!
      SUBROUTINE DebugShow(TheMessage)
!
! Displays a debug message in the lower window bar that should be ignored in the release version of DASH.
!
! INPUT   : TheMessage = The message to be displayed
!      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      LOGICAL            ShowAgain
      INTEGER                       Counter
      COMMON  / DBGMSG / ShowAgain, Counter

      CHARACTER*(20), EXTERNAL :: Integer2String
      CHARACTER*(255) tMessage
      INTEGER         tLen

#ifdef ONTBUG
      Counter = Counter + 1
! Update the status bar at the bottom of the screen.
      tMessage = TheMessage
      tLen = LEN_TRIM(tMessage)
      tMessage = tMessage(1:tLen)//" "//Integer2String(Counter)
      tLen = LEN_TRIM(tMessage)
      CALL WindowOutStatusBar(1, tMessage(1:tLen))
#endif

      END SUBROUTINE DebugShow
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
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) THEN
        CALL AppendBatchLogFile("Warning: "//TheMessage)
      ELSE
        CALL WMessageBox(OkOnly,ExclamationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Warning")
      ENDIF

      END SUBROUTINE WarningMessage
!
!*****************************************************************************
!
      SUBROUTINE InfoMessage(TheMessage)
!
! Displays an info message
!
! INPUT   : TheMessage = The message to be displayed
!
      USE WINTERACTER
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) THEN
        CALL AppendBatchLogFile("Info: "//TheMessage)
      ELSE
        CALL WMessageBox(OkOnly,InformationIcon,CommonOk,TheMessage(1:LEN_TRIM(TheMessage)),"Info")
      ENDIF

      END SUBROUTINE InfoMessage
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
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheQuestion

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) THEN
      ELSE
        CALL WMessageBox(YesNo, QuestionIcon, CommonOK, TheQuestion(1:LEN_TRIM(TheQuestion)), 'Confirm')
        Confirm = (WInfoDialog(ExitButtonCommon) .EQ. CommonYes)
      ENDIF

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

      LOGICAL           ChildWinAutoClose
      COMMON /ChWinAC/  ChildWinAutoClose(1:20)

      IF ((TheIHANDLE.LT.1) .OR. (TheIHANDLE.GT.20)) THEN
        CALL DebugErrorMessage('Invalid Child Window ID in SetChildWinAutoClose')
        RETURN
      ENDIF
      ChildWinAutoClose(TheIHANDLE) = .TRUE.

      END SUBROUTINE SetChildWinAutoClose
!
!*****************************************************************************
!
      SUBROUTINE RegisterChildWindow(TheIHANDLE, TheSubroutine)
!
! This routine 'registers' a child window with the GetEvent routines.
! For each child window, it stores the address of the subroutine
! that deals with that child window.
! The registration must be undone manually by a call to UnRegisterChildWindow.
!
#ifdef __G95__
      USE FOR_G95
#endif
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheIHANDLE
      EXTERNAL TheSubroutine

#ifdef __G95__
      TYPE(CallbackPointer) ChildWinHandler
#else
      INTEGER*8         ChildWinHandler
#endif
      LOGICAL                                  ChildWinHandlerSet
      COMMON /ChWinHan/ ChildWinHandler(1:20), ChildWinHandlerSet(1:20)

      IF ((TheIHANDLE.LT.1) .OR. (TheIHANDLE.GT.20)) THEN
        CALL DebugErrorMessage('Invalid Child Window ID in RegisterChildWindow')
        RETURN
      ENDIF
#ifdef __G95__
      ChildWinHandler(TheIHANDLE)%p => TheSubroutine
#else
      ChildWinHandler(TheIHANDLE)    = LOC(TheSubroutine)
#endif
      ChildWinHandlerSet(TheIHANDLE) = .TRUE.

      END SUBROUTINE RegisterChildWindow
!
!*****************************************************************************
!
      SUBROUTINE UnRegisterChildWindow(TheIHANDLE)
!
! This routine undoes the action of RegisterChildWindow
!
#ifdef __G95__
      USE FOR_G95
#endif
      IMPLICIT NONE

      INTEGER,  INTENT (IN   ) :: TheIHANDLE

#ifdef __G95__
      TYPE(CallbackPointer) ChildWinHandler
#else
      INTEGER*8         ChildWinHandler
#endif
      LOGICAL                                  ChildWinHandlerSet
      COMMON /ChWinHan/ ChildWinHandler(1:20), ChildWinHandlerSet(1:20)

      IF ((TheIHANDLE.LT.1) .OR. (TheIHANDLE.GT.20)) THEN
        CALL DebugErrorMessage('Invalid Child Window ID in UnRegisterChildWindow')
        RETURN
      ENDIF
      ChildWinHandlerSet(TheIHANDLE) = .FALSE.

      END SUBROUTINE UnRegisterChildWindow
!
!*****************************************************************************
!
      LOGICAL FUNCTION DealWithEvent()

      USE dash_gui_resources
      USE VARIABLES
#ifdef __G95__
      USE FOR_G95
#endif
      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

! The routines that act on the mousebutton presses are non-reentrant
! and the one should not be called when the other is active, so we must keep a flag if we are dealing
! with a mouse button press. 
      LOGICAL         MseBtnPressed, OldEventWaiting
      COMMON /Events/ MseBtnPressed, OldEventWaiting

! For child windows that are an editor, the only event DASH has to handle itself
! (all other events for editor child windows are handled automatically by Winteracter)
! is the CloseRequest. By 'registering' a child window with GetEvent, by setting the
! ChildWinAutoClose variable for that child window to true, GetEvent will take care of that.
      LOGICAL           ChildWinAutoClose
      COMMON /ChWinAC/  ChildWinAutoClose(1:20)

#ifdef __G95__
      TYPE(CallbackPointer) ChildWinHandler
#else
      INTEGER*8         ChildWinHandler
#endif
      LOGICAL                                  ChildWinHandlerSet
      COMMON /ChWinHan/ ChildWinHandler(1:20), ChildWinHandlerSet(1:20)

#ifdef __G95__
      PROCEDURE (CallbackFunc), POINTER :: Handler
      INTEGER dummy
#else
! 'p' is now a code pointer to the subroutine 'Handler'
      EXTERNAL Handler
      POINTER (p, Handler)
#endif

      SELECT CASE (EventInfo%WIN)
        CASE (0) ! Main window
          SELECT CASE (EventType)
            CASE (Expose,Resize)
              CALL Profile_Plot
              GOTO 10
            CASE (MenuSelect)
              CALL ProcessMenu
              GOTO 10
            CASE (CloseRequest)
              CALL WExit
              GOTO 10
            CASE (MouseMove)
              CALL UpdateMousePosition ! Should also be called when the graph is updated.
              CALL HighLightPFR
              CALL DisplayHKL
            CASE (MouseButDown)
              IF (NoData) GOTO 10
              IF (MseBtnPressed) GOTO 10
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                MseBtnPressed = .TRUE.
                CALL Plot_Alter
                MseBtnPressed = .FALSE.
              ELSE IF(EventInfo%VALUE1 .EQ. RightButton) THEN
! Get to work on the cross-hair movement--fitting this time
                IF (PastPawley) GOTO 10
! Peaks being fitted and refined automatically makes it impossible to 
! play around with indexing. Therefore: when in one of the indexing windows, ignore
! this routine.
                IF ((CurrentWizardWindow .EQ. IDD_PW_Page3) .OR.     &
                    (CurrentWizardWindow .EQ. IDD_PW_Page4) .OR.     &
                    (CurrentWizardWindow .EQ. IDD_PW_Page5) .OR.     &
                    (CurrentWizardWindow .EQ. IDD_PW_Page6)) GOTO 10
                MseBtnPressed = .TRUE.
                CALL Move_CrossHair_Fit
                MseBtnPressed = .FALSE.
              ENDIF
              GOTO 10
            CASE (KeyDown)
              IF (MseBtnPressed) GOTO 10
              CALL Check_KeyDown
! Peaks being fitted and refined automatically makes it impossible to 
! play around with indexing. Therefore: when in one of the indexing windows, ignore
! this routine.
              IF ((CurrentWizardWindow .EQ. IDD_PW_Page3) .OR.     &
                  (CurrentWizardWindow .EQ. IDD_PW_Page4) .OR.     &
                  (CurrentWizardWindow .EQ. IDD_PW_Page5) .OR.     &
                  (CurrentWizardWindow .EQ. IDD_PW_Page6)) GOTO 10
              CALL Check_KeyDown_PeakFit_Inner
              GOTO 10
          END SELECT
        CASE (1:20) ! One of the Child Windows
          IF (EventType.EQ.CloseRequest .AND. ChildWinAutoClose(EventInfo%WIN)) THEN
            CALL PushActiveWindowID
            CALL WindowCloseChild(EventInfo%WIN)
            ChildWinAutoClose(EventInfo%WIN) = .FALSE.
            CALL PopActiveWindowID
            GOTO 10
          ENDIF
          IF (ChildWinHandlerSet(EventInfo%WIN)) THEN
#ifdef __G95__
            ! Can't directly call a procedure pointed by an arraya element, as the
            ! brackets causing confusion. G95 reports this line as conflicts:
            dummy = EventInfo%WIN
	    !CALL ChildWinHandler(dummy)%p()
            Handler => ChildWinHandler(dummy)%p
	    !CALL Handler         
#else
        p = ChildWinHandler(EventInfo%WIN)
        CALL Handler
#endif

            GOTO 10
          ENDIF
        CASE (IDD_Background_Fit)
          CALL Background_Fit
          GOTO 10
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
        CASE (IDD_DV_Results)
          CALL DealWithDVResults
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
        CASE (IDD_PW_Page3a)
          CALL DealWithWizardWindowRebin
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
        CASE (IDD_PW_Page7)
          CALL DealWithWizardWindowIndexing1
          GOTO 10
        CASE (IDD_DICVOLRunning)
          CALL DealWithDICVOLRunning
          GOTO 10
        CASE (IDD_PW_Page8)
          CALL DealWithWizardWindowIndexing2
          GOTO 10
        CASE (IDD_PW_Page8b)
          CALL DealWithWizardWindowExtDICVOL
          GOTO 10
        CASE (IDD_PW_Page8c)
          CALL DealWithWizardWindowMcMaille
          GOTO 10
        CASE (IDD_PW_Page9)
          CALL DealWithWizardWindowDICVOLResults
          GOTO 10
        CASE (IDD_PW_Page1)
          CALL DealWithWizardWindowUnitCellParameters
          GOTO 10
        CASE (IDD_PW_Page10)
          CALL DealWithWizardWindowPawley1
          GOTO 10
        CASE (IDD_SX_Page1)
          CALL DealWithWizardWindowSingleCrystalData1
          GOTO 10
        CASE (IDD_SX_Page1a)
          CALL DealWithWizardWindowSingleCrystalData1a
          GOTO 10
        CASE (IDD_SX_Page2)
          CALL DealWithWizardWindowSingleCrystalData2
          GOTO 10
        CASE (IDD_Pawley_Status)
          CALL DealWithPawleyFitWindow
          GOTO 10
        CASE (IDD_SAW_Page1)
          CALL DealWithWizardWindowZmatrices
          GOTO 10
        CASE (IDD_zmEdit)
          CALL DealWithEditZMatrixWindow
          GOTO 10
        CASE (IDD_zmEditRotations)
          CALL DealWithEditZMatrixRotationsWindow
          GOTO 10
        CASE (IDD_SAW_Page2)
          CALL DealWithWizardWindowAdditionalSAParams
          GOTO 10
!O        CASE (IDD_SA_input2)
!O          CALL DealWithWizardWindowParameterBounds
!O          GOTO 10
        CASE (IDD_ModalDialog)
          CALL DealWithBiModalDialog()
          GOTO 10
        CASE (IDD_SA_Modal_input2)
          CALL DealWithWizardWindowParameterBounds
          GOTO 10
        CASE (IDD_SA_input3_2)
          CALL DealWithWizardWindowSASettings
          GOTO 10
        CASE (IDD_SA_input4)
          CALL DealWithWizardWindowSAOptions
          GOTO 10
        CASE (IDD_SA_input5)
          CALL DealWithWizardWindowWriteGrid
          GOTO 10
        CASE (IDD_SA_Action1)
          CALL DealWithSAStatusWindow
          GOTO 10
        CASE (IDD_SAW_Page5)
          CALL DealWithAnalyseSolutionsWindow
          GOTO 10
        CASE (IDD_SAW_Page6)
          CALL DealWithWizardRietveldRefinement
          GOTO 10
        CASE (IDD_OutputSolutions)
          CALL DealWithOutputSolutions
          GOTO 10
        CASE (IDD_Summary)
          CALL DealWithSaSummary
          GOTO 10
        CASE (IDD_Rietveld2)
          CALL DealWithWindowRietveld
          GOTO 10
        CASE (IDD_SAW_Page6a)
          CALL DealWithWizardChooseRietveldRefinementMethod
          GOTO 10
        CASE (IDD_RR_External)
          CALL DealWithWizardExtRR
          GOTO 10
        CASE (IDD_RR_PO_Dialog)
          CALL DealWithRR_PO_Settings
          GOTO 10
        CASE (IDD_SAW_Page7_TOPAS)
          CALL DealWithRR_TOPAS
          GOTO 10
        CASE (IDD_SAW_Page7_GSAS)
          CALL DealWithRR_GSAS
          GOTO 10
        CASE (IDD_SAW_Page7_RIETAN)
          CALL DealWithRR_RIETAN
          GOTO 10
        CASE (IDD_SA_method)
          CALL DealWithWizardWindowSAMethod
          GOTO 10
        CASE (IDD_SA_ByDbfFile)
          CALL DealWithWizardWindowLoadDBFFile
          GOTO 10
        CASE (IDD_SA_Dist_Rest)
          CALL DealWithWizardWindowSADistRestraints
          GOTO 10
      END SELECT
      DealWithEvent = .FALSE.
      RETURN
! If we are here, we could deal with this event.
   10 DealWithEvent = .TRUE.

      END FUNCTION DealWithEvent
!
!*****************************************************************************
!
      SUBROUTINE GetEvent
!
! Fetches the last event from Winteracter and places it in two global variables
! called EventType and EventInfo (both in VARIABLES)
!
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL         MseBtnPressed, OldEventWaiting
      COMMON /Events/ MseBtnPressed, OldEventWaiting

      LOGICAL, EXTERNAL :: DealWithEvent

! Wait for a message
  10  IF (OldEventWaiting) THEN
        OldEventWaiting = .FALSE.
      ELSE
        CALL WMessage(EventType,EventInfo)
      ENDIF
! Try to process it. If we processed it, just wait for the next message
      IF (DealWithEvent()) GOTO 10
! If we couldn't process it ourselves, exit the routine and let the calling routine handle it
! The way things have been programmed now, that shouldn't happen very often.
      OldEventWaiting = .FALSE.

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
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL         MseBtnPressed, OldEventWaiting
      COMMON /Events/ MseBtnPressed, OldEventWaiting

      LOGICAL, EXTERNAL :: DealWithEvent

  10  IF (OldEventWaiting) THEN
        OldEventWaiting = .FALSE.
      ELSE
        CALL WMessagePeek(EventType,EventInfo)
        IF (EventType .EQ. NoMessage) RETURN
      ENDIF
! Try to process it. If we processed it, just wait for the next message
      IF (DealWithEvent()) GOTO 10
! If we couldn't process it ourselves, exit the routine and let the calling routine handle it
! The way things have been programmed now, that shouldn't happen very often.
      OldEventWaiting = .FALSE.

      END SUBROUTINE PeekEvent
!
!*****************************************************************************
!
      LOGICAL FUNCTION IsEventWaiting()
!
! Reports if events are waiting to be handled. If not, program execution resumes.
! It tries to emulate _not_ removing the events from the queu by
! setting a flag, 'OldEventWaiting', which is used by GetEvent, PeekEvent and IsEventWaiting.
!
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL         MseBtnPressed, OldEventWaiting
      COMMON /Events/ MseBtnPressed, OldEventWaiting

      IF (OldEventWaiting) THEN
        IsEventWaiting = .TRUE.
      ELSE
        CALL WMessagePeek(EventType,EventInfo)
        OldEventWaiting = (EventType .NE. NoMessage)
        IsEventWaiting = OldEventWaiting
      ENDIF

      END FUNCTION IsEventWaiting
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
      IF (WinStackPtr .EQ. 0) THEN
        CALL DebugErrorMessage('WinStack full.')
        RETURN
      ENDIF
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
      INTEGER WinStack

      COMMON /COMWS/ WinStackPtr, WinStack(1:MaxWinStack)

! Check if stack empty
      IF (WinStackPtr .EQ. MaxWinStack) THEN
        CALL DebugErrorMessage('WinStack empty.')
        RETURN
      ENDIF
! If not, Inc(StackPtr)
      WinStackPtr = WinStackPtr + 1
! Restore current window ID
      IF ( WinStack(WinStackPtr) .GT. 0 ) &
        CALL SelectDASHDialog(WinStack(WinStackPtr))

      END SUBROUTINE PopActiveWindowID
!
!*****************************************************************************
!
      SUBROUTINE AppendBatchLogFile(TheMessage)

! Append the message to log file, return on any open/write error
! It is intended for batch mode only

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheMessage

      CHARACTER(MaxPathLength) BatchLogName
      COMMON /BATLOG/ BatchLogName

      INTEGER, PARAMETER :: chLogFile = 119

      IF (LEN_TRIM(BatchLogName) .LE. 0) &
        RETURN
      
      OPEN(UNIT=chLogFile,FILE=BatchLogName,STATUS='UNKNOWN', POSITION='APPEND', ERR=99)
      WRITE (chLogFile, '(A)', ERR=99) TRIM(TheMessage)
 99   CLOSE(chLogFile)

      RETURN

      END SUBROUTINE AppendBatchLogFile
!
!*****************************************************************************
!
      SUBROUTINE ClearBatchLogFile()

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) BatchLogName
      COMMON /BATLOG/ BatchLogName

      INTEGER, PARAMETER :: chLogFile = 119
      LOGICAL Exists

      IF (LEN_TRIM(BatchLogName) .LE. 0) &
        RETURN
      
      INQUIRE(FILE=BatchLogName, EXIST=exists)
      IF (.NOT. exists) &
        RETURN
      
      OPEN(UNIT=chLogFile,FILE=BatchLogName, STATUS='UNKNOWN', POSITION='REWIND', ERR=99)
      ENDFILE(chLogFile, ERR=99)
 99   CLOSE(chLogFile)

      RETURN
      END SUBROUTINE ClearBatchLogFile
!
!*****************************************************************************
!
