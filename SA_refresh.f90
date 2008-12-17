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
      LOGICAL                       NewOptimumFound, WasMinimised, TestEarlyTermFlag
      COMMON / CMN000001 / iMyExit, NewOptimumFound, WasMinimised, TestEarlyTermFlag

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CHARACTER*(15) file_name

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SA_Action1)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_View) ! 'View'
! Calls subroutine which opens Mercury window with .pdb file
!             CALL SA_STRUCTURE_OUTPUT_PDB(Curr_SA_Run, file_name)
              CALL SA_STRUCTURE_OUTPUT_NON_OVERLAP(Curr_SA_Run, file_name)
              CALL ViewStructure(file_name, .FALSE.)
            CASE (IDF_Pause_Annealing) ! 'Pause'
              CALL WDialogFieldState(IDF_Pause_Annealing,Disabled)
              iMyExit = 6
              CALL LoadDASHDialog(IDD_Pause)
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
              CALL LocalMinimise(.FALSE., .FALSE.)
            CASE (IDB_Summary)           ! 'Solutions'
              CALL WDialogFieldState(IDB_Summary,Disabled)
              CALL SelectDASHDialog(IDD_Summary)
              CALL WDialogShow(-1, -1, 0, Modeless)
          END SELECT
        CASE (FieldChanged)
          IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2 .AND. EventInfo%VALUE2 .EQ. IDF_TestEarlyTerm) &
            TestEarlyTermFlag = DASHWDialogGetCheckBoxLogical(IDF_TestEarlyTerm)
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithSAStatusWindow
!
!*****************************************************************************
!
      SUBROUTINE ViewStructure(TheFileName, BuiltInOnly)
!
! This routine displays a molecular file
! TheFileName may include the full path, but cannot be a relative path ("..\molecule.pdb" is not allowed)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      LOGICAL,       INTENT (IN   ) :: BuiltInOnly

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER I, M
      LOGICAL exists, tBuiltInMercury, tUseClient
      CHARACTER(MaxPathLength+40) tArgStr, tExeStr

      IF (BuiltInOnly) THEN
        tBuiltInMercury = .TRUE.
        tUseClient = .TRUE.
      ELSE
! Get the argument for the viewer from the Configuration Window
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_Configuration)
        tBuiltInMercury = DASHWDialogGetCheckBoxLogical(IDF_BuiltIn_Mercury)
        tUseClient = DASHWDialogGetCheckBoxLogical(IDF_Use_Client)
        CALL DASHWDialogGetString(IDF_ViewExe,ViewExe)
        CALL DASHWDialogGetString(IDF_ViewArg,ViewArg)
        CALL PopActiveWindowID
      ENDIF
      IF (tBuiltInMercury) THEN
        tExeStr = TRIM(InstallationDirectory)//'Mercury\dash_mercury.exe'
        tArgStr = ' -lf "'//TRIM(PathToLicenseFile)//'" -load-all-files'
        IF (tUseClient) tArgStr = TRIM(tArgStr)//' -client'
      ELSE
        tExeStr = ViewExe
        tArgStr = ' '//ViewArg
      ENDIF
      I = LEN_TRIM(tExeStr)
      IF (I .EQ. 0) THEN
        CALL ErrorMessage('DASH could not launch the viewer. No viewer executable is currently specified.'//CHAR(13)//&
                          'This can be changed in the Configuration... window'//CHAR(13)//&
                          'under Options in the menu bar.')
        RETURN
      ENDIF
      INQUIRE(FILE = tExeStr(1:I),EXIST=exists)
      IF (.NOT. exists) GOTO 999
      M = InfoError(1) ! Clear errors
      CALL IOSCommand(tExeStr(1:I)//TRIM(tArgStr)//' "'//TRIM(TheFileName)//'"')
      IF (InfoError(1) .NE. 0) GOTO 999
      RETURN
  999 IF (tBuiltInMercury) THEN
        tArgStr = 'DASH could not find or launch the built in Mercury: '//CHAR(13)//tExeStr(1:I)
      ELSE
        tArgStr = 'DASH could not find or launch the viewer. The viewer executable is currently configured'//CHAR(13)//&
                  'to launch the program '//tExeStr(1:I)//CHAR(13)//&
                  'This can be changed in the Configuration... window'//CHAR(13)//&
                  'under Options in the menu bar.'
      ENDIF
      CALL ErrorMessage(TRIM(tArgStr))
      RETURN
      
      END SUBROUTINE ViewStructure
!
!*****************************************************************************
!
