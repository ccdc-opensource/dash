!
!*******************************************************************************
!
      SUBROUTINE SaSummary
! ep July 2001 
! Called from Begin_Sa subroutine.  Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC' 
       
!ep    need the common block to identify the number rows in the grid          
      LOGICAL RESTART
      INTEGER SA_Run_Number
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MinMoves, MaxMoves, ChiMult
!     required to handle the profile graphs plotted in child windows
      INTEGER  SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)
        
      CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
      CALL WDialogShow(-1,-1,0,Modeless)
      DO WHILE (.TRUE.)
        CALL GetEvent
        SELECT CASE (EventType)
! will close the profile plot window
          CASE (CloseRequest)
            CALL WindowCloseChild(EventInfo%win)
            SAUsedChildWindows(EventInfo%win) = 0
! exposing or resizing of profile plot windows - will replot
          CASE (expose, resize)
            CALL plot_pro_file(EventInfo%win)
! Ok button of summary window pushed, dialog closed
          CASE (PushButton)
            IF (EventInfo%VALUE1 .EQ.IDCANCEL) THEN
              IF (EventInfo%win .EQ. IDD_SA_Multi_Completed_ep) THEN
                CALL WDialogSelect(IDD_SA_Multi_Completed_ep)
                CALL WDialogHide()
                EXIT
              ENDIF
            ENDIF
            IF (EventInfo%VALUE1 .EQ. IDOK_ep) THEN
! Closes all SA profile child windows which are still open when OK button clicked
              DO i = 1, MaxNumChildWin
                IF (SAUsedChildWindows(i).eq.1) THEN
                  CALL WindowCloseChild(i)
                  SAUsedChildWindows(i) = 0
                END IF
              END DO
              CALL WDialogHide()
              EXIT
            END IF
        END SELECT
!ep allows you to view pdb file of SA Solutions, each clicked
!   check box in fresh mercury window
        DO irow = 1,maxruns
          CALL WGridGetCellCheckBox(IDF_SA_summary,2,irow,istatus)
          IF (istatus.eq.1) THEN
! calls subroutine which opens Mercury window with .pdb file
            CALL viewpdb(irow)
! calls subroutine which plots observed diffraction pattern with calculated pattern
            CALL organise_sa_result_data(irow)
            CALL WGridPutCellCheckBox(IDF_SA_Summary,2,irow,Unchecked)
            istatus = 0
          ENDIF
        END DO               
      END DO

      END SUBROUTINE SaSummary
!
!*******************************************************************************
!
      SUBROUTINE Viewpdb(irow)
! ep July 2001
! This subroutine is the Viewbest subroutine (in sa_refresh.f90) hacked about a bit.
! Each time a "View" button is clicked in the summary window, this subroutine will
! open a new Mercury window and display the relevant .pdb file
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INTEGER, INTENT (IN  ) :: irow

      CHARACTER*255 Grid_Buffer

      CALL PushActiveWindowID
!     Get grid_buffer which contains the name of the correct pdb file
      CALL WDialogSelect(IDD_SA_Multi_completed_ep)
      CALL WGridGetCellString(IDF_SA_Summary,1,irow,Grid_Buffer)
      CALL ViewStructure(Grid_Buffer)
      CALL PopActiveWindowID

      END  SUBROUTINE Viewpdb
!
!*******************************************************************************
!
