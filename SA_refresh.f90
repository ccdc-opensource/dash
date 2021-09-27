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
!*****************************************************************************
!
      SUBROUTINE DealWithSAStatusWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'

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
      INTEGER I, M, ISTATUS, IEXCOD
      LOGICAL exists, tBuiltInMercury, tUseClient
      CHARACTER(MaxPathLength+40) tArgStr, tExeStr
      CHARACTER(1024) FullFileName
      
      
      CHARACTER(1) cUseClient
      INTEGER, DIMENSION(2) :: ID


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

          
         cUseClient = '0'
         IF (tUseClient) cUseClient = '1'
         
         CALL IOsFullPathname(TheFileName,FullFileName)
         M = WInfoError(3) ! Clear errors
         CALL IOSCommand( TRIM(InstallationDirectory)//DIRSPACER//'zmconv'//DIRSPACER//'dash_csd_connector'//CCDC_EXE_EXT//' --launch-mercury "'//TRIM(FullFileName)//'" '//cUseClient , ProcSilent, IDPROC=ID)      
         DO
             CALL IOsCommandCheck(ID, ISTATUS, IEXCOD)
             IF (ISTATUS==0) EXIT
             CALL IOsWait(5)
         END DO

         IF ( ISTATUS.EQ.0 ) &
            RETURN
         
         IF ( ISTATUS.EQ.2 ) THEN
             tArgStr = 'The file named '//TRIM(FullFileName)//' Doesnt seem to exist or is unreadable ...'
             CALL ErrorMessage(TRIM(tArgStr))
             RETURN
         ENDIF
      ELSE
        tExeStr = ViewExe
        tArgStr = ViewArg

        I = LEN_TRIM(tExeStr)
      
        IF (I .EQ. 0) THEN
           CALL ErrorMessage('DASH could not launch the viewer. '// &
                             'No viewer executable is currently specified.'//CHAR(13)//&
                             'This can be changed in the Configuration... window'//CHAR(13)//&
                             'under Options in the menu bar.')
          RETURN
        ENDIF
   
        INQUIRE(FILE = tExeStr(1:I),EXIST=exists)
        IF (.NOT. exists) GOTO 999
        M = InfoError(1) ! Clear errors
        CALL IOSCommand('"'//tExeStr(1:I)//'" '//TRIM(tArgStr)//' "'//TRIM(TheFileName)//'"')
        IF (InfoError(1) .NE. 0) GOTO 999
        RETURN
      ENDIF   
999   IF (tBuiltInMercury) THEN
        tArgStr = 'DASH could not find or launch the built in Mercury: '//CHAR(13)//tExeStr(1:I)
      ELSE
        tArgStr = 'DASH could not find or launch the viewer. '// &
                  'The viewer executable is currently configured'//CHAR(13)//&
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
