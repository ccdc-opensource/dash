!
!*******************************************************************************
!
      SUBROUTINE DealWithSaSummary
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

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

      INTEGER RangeOption, I, iRow, iStatus

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide()
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
            CASE (IDF_InvertSelection)
              DO iRow = 1, MaxRuns
                CALL WGridGetCellCheckBox(IDF_SA_summary,3,iRow,istatus)
                IF (istatus .EQ. 1) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Unchecked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iRow,Checked)
                ENDIF
              ENDDO
            CASE (IDB_ShowOverlap)
              CALL SA_STRUCTURE_OUTPUT_OVERLAP
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_ShowRange, IDF_ShowTicked)
              CALL WDialogGetRadioButton(IDF_ShowRange,RangeOption)
              IF (RangeOption .EQ. 1) THEN ! "Show Selected"
                CALL WDialogFieldState(IDF_Limit1,Enabled)
                CALL WDialogFieldState(IDF_Limit2,Enabled)
              ELSE
                CALL WDialogFieldState(IDF_Limit1,Disabled)
                CALL WDialogFieldState(IDF_Limit2,Disabled)
              ENDIF
          END SELECT
      END SELECT
!ep allows you to view pdb file of SA Solutions, each clicked
!   check box in fresh mercury window
      DO iRow = 1, NumOf_SA_Runs
        CALL WGridGetCellCheckBox(IDF_SA_summary,2,iRow,istatus)
        IF (istatus .EQ. 1) THEN
! calls subroutine which opens Mercury window with .pdb file
!C          CALL WGridGetCellString(IDF_SA_Summary,1,iRow,Grid_Buffer)
!C          CALL ViewStructure(Grid_Buffer)
! @@ at the moment, this is wrong. But the solutions should be sorted against chi-sqrd.
          Curr_SA_Run = iRow
          CALL SA_STRUCTURE_OUTPUT_PDB
          CALL ViewStructure('SA_best.pdb')
! calls subroutine which plots observed diffraction pattern with calculated pattern
          CALL organise_sa_result_data(iRow)
          CALL WGridPutCellCheckBox(IDF_SA_Summary,2,iRow,Unchecked)
        ENDIF
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE DealWithSaSummary
!
!*******************************************************************************
!
      SUBROUTINE DealWithProfilePlot
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 
       
! Required to handle the profile graphs plotted in child windows
      INTEGER  SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      SELECT CASE (EventType)
! Will close the profile plot window
        CASE (CloseRequest)
          CALL WindowCloseChild(EventInfo%win)
          SAUsedChildWindows(EventInfo%win) = 0
          CALL UnRegisterChildWindow(EventInfo%win)
! Exposing or resizing of profile plot windows - will replot
        CASE (expose, resize)
          CALL plot_pro_file(EventInfo%win)
      END SELECT

      END SUBROUTINE DealWithProfilePlot
!
!*******************************************************************************
!
