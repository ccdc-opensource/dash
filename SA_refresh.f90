!
!*****************************************************************************
!
      SUBROUTINE DealWithSAStatusWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER              iMyExit, num_new_min
      COMMON / CMN000001 / iMyExit, num_new_min

      INTEGER                    ChiSqdChildWindows,                 ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiSqdChildWindows(MaxNumChildWin), ChiHandle

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Action1)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_View) ! 'View'
! Calls subroutine which opens Mercury window with .pdb file
              CALL SA_STRUCTURE_OUTPUT_PDB(Curr_SA_Run)
              CALL ViewStructure('SA_best.pdb')
            CASE (IDF_Pause_Annealing) ! 'Pause'
              iMyExit = 6
              CALL WDialogLoad(IDD_Pause)
              CALL WDialogSelect(IDD_Pause)
              CALL WDialogShow(-1,-1,IDOK,Modeless)
            CASE (IDB_Prog3)
              CALL OpenChiSqPlotWindow
              CALL plotting_Chi_sqd(ChiHandle)
            CASE (IDF_StartNext) ! 'Start Next'
              iMyExit = 4
            CASE (IDF_StopSA, IDCANCEL) ! 'Stop'
! Close Chi Squared plot window
              CALL Close_Chisq_plot
! Go to the SA results
              iMyExit = 3
            CASE (IDB_Edit)
! Close Chi Squared plot window
              CALL Close_Chisq_plot
              iMyExit = 5
            CASE (IDF_SA_Simplex_Button) ! 'Local minimisation'
              CALL LocalMinimise(.FALSE.)
            CASE (IDB_Summary)
              CALL WDialogFieldState(IDB_Summary,Disabled)
              CALL WDialogSelect(IDD_Summary)
              CALL WDialogShow(-1,-1,0,Modeless)
          END SELECT
      END SELECT  ! EventType
      CALL PopActiveWindowID

      END SUBROUTINE DealWithSAStatusWindow
!
!*****************************************************************************
!
      SUBROUTINE ViewStructure(TheFileName)
!
! This routine displays a molecular file
! TheFileName may include the full path, but cannot be a relative path ("..\molecule.pdb" is not allowed)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER I, J, M
      LOGICAL exists

! Get the argument for the viewer from the Configuration Window
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Configuration)
      CALL WDialogGetString(IDF_ViewExe,ViewExe)
      CALL WDialogGetString(IDF_ViewArg,ViewArg)
      CALL PopActiveWindowID
      I = LEN_TRIM(ViewExe)
      IF (I .EQ. 0) THEN
        CALL ErrorMessage("DASH could not launch your viewer. No viewer executable is currently specified."//CHAR(13)//&
                          "This can be changed in the Configuration... window"//CHAR(13)//&
                          "under Options in the menu bar.")
        RETURN
      ENDIF
      J = LEN_TRIM(ViewArg)
      INQUIRE(FILE = ViewExe(1:I),EXIST=exists)
      IF (.NOT. exists) GOTO 999
      M = InfoError(1) ! Clear errors
      IF (J .GT. 0) THEN
        CALL IOSCommand(ViewExe(1:I)//' '//ViewArg(1:J)//' '//'"'//TheFileName(1:LEN_TRIM(TheFileName))//'"')
      ELSE
        CALL IOSCommand(ViewExe(1:I)//' '//'"'//TheFileName(1:LEN_TRIM(TheFileName))//'"')
      ENDIF
      IF (InfoError(1) .NE. 0) GOTO 999
      RETURN
  999 CALL ErrorMessage("DASH could not launch your viewer. The viewer executable is currently configured"//CHAR(13)//&
                        "to launch the program "//ViewExe(1:I)//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")
      
      END SUBROUTINE ViewStructure
!
!*****************************************************************************
!
