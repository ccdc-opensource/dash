!
!*****************************************************************************
!
      SUBROUTINE DealWithSAStatusWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER              iMyExit 
      LOGICAL                       NewOptimumFound, WasMinimised
      COMMON / CMN000001 / iMyExit, NewOptimumFound, WasMinimised

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      CHARACTER*(15) file_name

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Action1)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_View) ! 'View'
! Calls subroutine which opens Mercury window with .pdb file
              CALL SA_STRUCTURE_OUTPUT_PDB(Curr_SA_Run, file_name)
              CALL ViewStructure(file_name)
            CASE (IDF_Pause_Annealing) ! 'Pause'
              CALL WDialogFieldState(IDF_Pause_Annealing,Disabled)
              iMyExit = 6
              CALL WDialogLoad(IDD_Pause)
              CALL WDialogSelect(IDD_Pause)
              CALL WDialogShow(-1, -1 , IDOK, Modeless)
            CASE (IDB_Prog3)
              CALL OpenChiSqPlotWindow
            CASE (IDF_StartNext) ! 'Start Next'
              IF (iMyExit .NE. 6) iMyExit = 4
            CASE (IDF_StopSA, IDCANCEL) ! 'Stop'
! Go to the SA results
              IF (iMyExit .NE. 6) iMyExit = 3
            CASE (IDB_Edit)             ! 'Edit'
              IF (iMyExit .NE. 6) THEN
! Close Chi Squared plot window
                CALL Close_Chisq_plot
                iMyExit = 5
              ENDIF
            CASE (IDF_SA_Simplex_Button) ! 'Local minimisation'
              CALL LocalMinimise(.FALSE.)
            CASE (IDB_Summary)           ! 'Solutions'
              CALL WDialogFieldState(IDB_Summary,Disabled)
              CALL WDialogSelect(IDD_Summary)
              CALL WDialogShow(-1, -1, 0, Modeless)
          END SELECT
      END SELECT
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
