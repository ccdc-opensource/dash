!
!*******************************************************************************
!
      SUBROUTINE DealWithAnalyseSolutionsWindow
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

!ep    need the common block to identify the number rows in the grid          
      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

!     required to handle the profile graphs plotted in child windows
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      INTEGER I, iRow, iStatus, iLimit1, iLimit2, tInteger, iOption

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page5)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Closes all SA profile child windows which are still open when OK button clicked
              DO i = 1, MaxNumChildWin
                IF (SAUsedChildWindows(i).EQ.1) THEN
                  CALL WindowCloseChild(i)
                  SAUsedChildWindows(i) = 0
                  CALL UnRegisterChildWindow(i)
                ENDIF
              ENDDO
! Close Chi-sqd plot 
              CALL Close_Chisq_Plot
! Go back to the Pawley refinement or the initial Wizard
! If we got here from the main Wizard window (option four), return to that window.
! Return to SA input otherwise
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogGetRadioButton(IDF_PW_Option1,iOption)
              IF (iOption .EQ. 4) THEN
                CALL EndWizardPastPawley
                CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
                CALL WDialogPutRadioButton(IDF_PW_Option4)
                CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
              ELSE
                CALL WizardWindowShow(IDD_SA_input3)
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
              CALL WDialogSelect(IDD_SA_Action1)
              CALL WDialogFieldState(IDB_Summary,Enabled)
! Closes all SA profile child windows which are still open when OK button clicked
              DO i = 1, MaxNumChildWin
                IF (SAUsedChildWindows(i).EQ.1) THEN
                  CALL WindowCloseChild(i)
                  SAUsedChildWindows(i) = 0
                  CALL UnRegisterChildWindow(i)
                ENDIF
              ENDDO
! Close Chi-sqd plot 
              CALL Close_Chisq_Plot
              CALL PopActiveWindowID
              RETURN
            CASE (IDB_Select)
              CALL WDialogGetInteger(IDF_Limit1,iLimit1)
              CALL WDialogGetInteger(IDF_Limit2,iLimit2)
              IF (iLimit1 .GT. iLimit2) THEN
                tInteger = iLimit2
                iLimit2  = iLimit1
                iLimit1  = tInteger
              ENDIF
              IF (iLimit2 .GT. NumOf_SA_Runs) iLimit2 = NumOf_SA_Runs
              IF (iLimit1 .LT.             1) iLimit1 =             1
              DO iRow = 1, NumOf_SA_Runs
                IF ((iRow .GE. iLimit1) .AND. (iRow .LE. iLimit2)) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Checked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Unchecked)
                ENDIF
              ENDDO
            CASE (IDF_InvertSelection)
              DO iRow = 1, NumOf_SA_Runs
                CALL WGridGetCellCheckBox(IDF_SA_summary,3,iRow,istatus)
                IF (istatus .EQ. 1) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Unchecked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Checked)
                ENDIF
              ENDDO
            CASE (IDB_ShowOverlap)
              CALL SA_STRUCTURE_OUTPUT_OVERLAP(IDD_SAW_Page5)
          END SELECT
        CASE (FieldChanged)
      END SELECT
!ep allows you to view pdb file of SA Solutions, each clicked
!   check box in fresh mercury window
      DO iRow = 1, NumOf_SA_Runs
        CALL WGridGetCellCheckBox(IDF_SA_summary,2,iRow,istatus)
        IF (istatus .EQ. 1) THEN
! calls subroutine which opens Mercury window with .pdb file
          CALL SA_STRUCTURE_OUTPUT_PDB(iSol2Run(iRow))
          CALL ViewStructure('SA_best.pdb')
! calls subroutine which plots observed diffraction pattern with calculated pattern
          CALL organise_sa_result_data(iSol2Run(iRow))
          CALL WGridPutCellCheckBox(IDF_SA_Summary,2,iRow,Unchecked)
        ENDIF
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE DealWithAnalyseSolutionsWindow
!
!*******************************************************************************
!
      SUBROUTINE DealWithProfilePlot
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
!!April2002 Added Zoom functionality.  Still needs a bit of tidying....
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 
       
! Required to handle the profile graphs plotted in child windows
      INTEGER  SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)
      INTEGER Ihandle
!      LOGICAL MseBtnPressed, OldEventWaiting
! The routines that act on the mousebutton presses are non-reentrant
! and the one should not be called when the other is active, so we must keep a flag if we are dealing
! with a mouse button press. 
!      COMMON /Events/ MseBtnPressed, OldEventWaiting


      SELECT CASE (EventType)

        CASE (CloseRequest)
          CALL WindowCloseChild(EventInfo%win)
          SAUsedChildWindows(EventInfo%win) = 0
          CALL UnRegisterChildWindow(EventInfo%win)

        CASE (expose, resize)
          CALL plot_pro_file(EventInfo%win)

        CASE (MouseButDown)
!!           IF (MseBtnPressed) GOTO 10
           IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
!!              MseBtnPressed = .TRUE.
              CALL plot_pro_file(EventInfo%win)
              Ihandle = EventInfo%win
              CALL PlotZoom(Ihandle)
!!              MseBtnPressed = .FALSE.
           ENDIF
!!10         CONTINUE

        CASE (KeyDown) ! home key resets the plot to original axes
           IF (EventInfo%VALUE1 .eq. KeyHome) THEN 
             CALL ResetProfPlotAxes(EventInfo%win)
             CALL WindowClear()
             CALL plot_pro_file(EventInfo%win)
           ENDIF
      END SELECT

      END SUBROUTINE DealWithProfilePlot
!
!*******************************************************************************
!
      SUBROUTINE PlotZoom(Ihandle)
!
!  Enable button up and mouse movement events
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      REAL xgcurold, ygcurold

      REAL, DIMENSION (20):: Ymin
      REAL, DIMENSION (20):: Ymax
      REAL, DIMENSION (20):: Xmax
      REAL, DIMENSION (20):: Xmin
      COMMON /PROFPLOTAXES/ Ymin, Ymax, XMin, XMax

      INTEGER Ihandle
      CALL WindowSelect(Ihandle)
      CALL WMessageEnable(MouseMove, Enabled)
      CALL WMessageEnable(MouseButUp, Enabled)
! JCC Set the scale correctly. 
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)
      CALL IPgArea(0.1,0.1,0.9,0.9)
      CALL IPgUnits(xmin(ihandle),ymin(ihandle),xmax(ihandle),ymax(ihandle))
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      xgcurold = xgcur(1)
      ygcurold = ygcur(1)

!!      CALL IGrFillPattern(0,1,1)
!!      CALL IGrPlotMode('EOR')
!!      CALL IGrColourN(KolNumRectSelect)
!!      ! Draw new 
!!      CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
!!      CALL IGrPlotMode('Normal')
!!      CALL IGrColourN(InfoGrScreen(PrevColReq))
      DO WHILE (.TRUE.)
!Can't use PeekEvent since the following events aren't handled for ChildWindows
         CALL WMessagePeek(EventType, EventInfo)
         IF (EventType .ne. (-1)) THEN
           IF (EventInfo%WIN .GT. 0) THEN
           CALL WindowSelect(Ihandle)
           CALL IGrUnits(0.0,0.0,1.0,1.0)
           CALL IPgArea(0.1,0.1,0.9,0.9)
           CALL IPgUnits(xmin(ihandle),ymin(ihandle),xmax(ihandle),ymax(ihandle))
           CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xcur(2),ycur(2))

          SELECT CASE (EventType)
            CASE (MouseMove)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL IGrPlotMode('EOR')
              CALL IGrColourN(KolNumRectSelect)
              CALL IGrFillPattern(0,1,1)
              ! Remove old
              CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
              ! Draw new
              CALL IGrRectangle(xgcur(1),ygcur(1),xgcur(2),ygcur(2))
              xgcurold = xgcur(2)
              ygcurold = ygcur(2)
              CALL IGrPlotMode('Normal')
              CALL IGrColourN(InfoGrScreen(PrevColReq))
            CASE (MouseButUp)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL WMessageEnable(MouseMove, Disabled)
              CALL WMessageEnable(MouseButUp, Disabled)
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                CALL IGrColourN(KolNumRectSelect)
                CALL IGrPlotMode('EOR')
                CALL IGrFillPattern(0,1,1)
                ! Remove old
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
                CALL IGrPlotMode('Normal')
                CALL IGrColourN(InfoGrScreen(PrevColReq))
                IF (ABS(XCUR(2)-XCUR(1)).LT.0.003*(xmax(ihandle)-Xmin(ihandle))) RETURN
                IF (ABS(YCUR(2)-YCUR(1)).LT.0.003*(YMAX(ihandle)-YMIN(ihandle))) RETURN
                XMin(ihandle) = MIN(XCUR(1),XCUR(2))
                XMax(ihandle) = MAX(XCUR(1),XCUR(2))  
                YMin(ihandle) = MIN(YCUR(1),YCUR(2))
                YMax(ihandle) = MAX(YCUR(1),YCUR(2))
              ENDIF
              CALL WindowClear()
              CALL Plot_pro_file(ihandle)
              RETURN  
          END SELECT
        ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE PlotZoom
!************************************************************************************************

      SUBROUTINE ResetProfPLotAxes(Ihandle)
!
!  
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'



      INTEGER Ihandle
      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL, DIMENSION (20):: Ymin
      REAL, DIMENSION (20):: Ymax
      REAL, DIMENSION (20):: Xmax
      REAL, DIMENSION (20):: Xmin
      COMMON /PROFPLOTAXES/ Ymin, Ymax, XMin, XMax

      YMin(ihandle) = MINVAL(YOBIN(1:NBIN))
      YMax(ihandle) = MAXVAL(YOBIN(1:NBIN))
      Xmin(ihandle) = XBIN(1)
      Xmax(ihandle) = XBIN(NBIN)

      END SUBROUTINE ResetProfPlotAxes
