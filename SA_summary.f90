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
      USE PRJVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

! Need the common block to identify the number of rows in the grid          
      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

! Required to handle the profile graphs plotted in child windows
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      LOGICAL, EXTERNAL :: Get_AutoAlign, Confirm
      INTEGER, EXTERNAL :: PrjSaveAs
      INTEGER IV, iRow, iStatus, iLimit1, iLimit2, tInteger, iDummy

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page5)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL CloseOutputSolutionsChildWindows
              CALL EndWizardPastPawley
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogPutRadioButton(IDF_PW_Option5)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
              CALL SelectMode(ID_Peak_Fitting_Mode)
            CASE (IDCANCEL, IDCLOSE)
              CALL CloseOutputSolutionsChildWindows
              CALL EndWizardPastPawley
              CALL PopActiveWindowID
              RETURN
            CASE (IDB_SaveSol)
              PrjFileName = OutputFilesBaseName(1:OFBN_Len)//'.dash'
              iDummy = PrjSaveAs()
            CASE (IDB_LoadSol)
              CALL PrjFileBrowse
            CASE (IDBSAVE)
              CALL WDialogSelect(IDD_OutputSolutions)
              CALL WDialogShow(-1, -1, 0, ModeLess)
              CALL WDialogPutString(IDF_DirAndBaseName, OutputFilesBaseName(1:OFBN_Len))
              CALL UpdateOutputSolutionsWIndow
            CASE (IDB_Prog3)
              CALL OpenChiSqPlotWindow
            CASE (IDB_Select)
              CALL WDialogGetInteger(IDF_Limit1, iLimit1)
              CALL WDialogGetInteger(IDF_Limit2, iLimit2)
              IF (iLimit1 .GT. iLimit2) THEN
                tInteger = iLimit2
                iLimit2  = iLimit1
                iLimit1  = tInteger
              ENDIF
              IF (iLimit2 .GT. NumOf_SA_Runs) iLimit2 = NumOf_SA_Runs
              IF (iLimit1 .LT.             1) iLimit1 =             1
              DO iRow = 1, NumOf_SA_Runs
                IF ((iRow .GE. iLimit1) .AND. (iRow .LE. iLimit2)) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Checked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Unchecked)
                ENDIF
              ENDDO
              CALL UpdateOutputSolutionsWindow
            CASE (IDF_InvertSelection)
              DO iRow = 1, NumOf_SA_Runs
                CALL WGridGetCellCheckBox(IDF_SA_summary, 3, iRow, istatus)
                IF (istatus .EQ. 1) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Unchecked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Checked)
                ENDIF
              ENDDO
              CALL UpdateOutputSolutionsWindow
            CASE (IDB_ShowOverlap)
              CALL SA_STRUCTURE_OUTPUT_OVERLAP(IDD_SAW_Page5)
            CASE (IDB_Resume) ! Resumes the simulated annealing, appending the new runs to
                              ! the existing runs
              Resume_SA = .TRUE. ! Initialisation
              CALL CloseOutputSolutionsChildWindows
              CALL ShowWizardWindowParameterBounds
              CALL PopActiveWindowID
              RETURN
          END SELECT
        CASE (FieldChanged)
          CALL UpdateOutputSolutionsWindow
      END SELECT
! Allows you to view pdb file of SA Solutions, each clicked
! check box in fresh Mercury window
      DO iRow = 1, NumOf_SA_Runs
        CALL WGridGetCellCheckBox(IDF_SA_summary, 2, iRow, iStatus)
        IF (iStatus .EQ. 1) THEN
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 2, iRow, Unchecked)
! Calls subroutine which opens Mercury window with .pdb file
          CALL SA_STRUCTURE_OUTPUT_PDB(iSol2Run(iRow))
          CALL ViewStructure('SA_best.pdb')
! Calls subroutine which plots observed diffraction pattern with calculated pattern
          CALL organise_sa_result_data(iRow)
          CALL PopActiveWindowID
          RETURN
        ENDIF
      ENDDO
! Allows you to restart a run
      DO iRow = 1, NumOf_SA_Runs
        CALL WGridGetCellCheckBox(IDF_SA_summary, 6, iRow, iStatus)
        IF (iStatus .EQ. 1) THEN
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 6, iRow, Unchecked)
          IF (Confirm("Restarting the simulated annealing will erase all current solutions."//CHAR(13)// &
                      "In order to keep the current solutions and append new ones, choose 'Resume SA'."//CHAR(13)// &
                      "Do you wish to continue and erase all current solutions?")) THEN
            CALL CloseOutputSolutionsChildWindows
            CALL WDialogSelect(IDD_SAW_Page5)
! Fill SA Parameter Bounds Wizard Window with the values from this solution.
            CALL WDialogSelect(IDD_SA_Modal_input2)
            DO IV = 1, NVAR
              CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IV, BestValuesDoF(IV,iSol2Run(iRow)))
            ENDDO
! Untick "Randomise initial values"
            CALL WDialogPutCheckBoxLogical(IDF_RandomInitVal, .FALSE.)
            ! Change mode to structure solution
            CALL SelectMode(ID_Structure_Solution_Mode)
            CALL ShowWizardWindowParameterBounds
            CALL PopActiveWindowID
            RETURN
          ENDIF
        ENDIF
      ENDDO
! Rietveld refinement
      DO iRow = 1, NumOf_SA_Runs
        CALL WGridGetCellCheckBox(IDF_SA_summary, 7, iRow, iStatus)
        IF (iStatus .EQ. 1) THEN
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 7, iRow, Unchecked)
          CALL CloseOutputSolutionsChildWindows
          CALL ShowWizardWindowRietveld(iSol2Run(iRow))
          CALL PopActiveWindowID
          RETURN
        ENDIF
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE DealWithAnalyseSolutionsWindow
!
!*******************************************************************************
!
      SUBROUTINE CloseOutputSolutionsChildWindows
! A helper routine to close all windows that should be closed when the output solutions window
! is changed in any way, i.e.:
! - all the powder pattern windows (profile child windows)
! - the chi-sqrd progress window
! - the Save solutions window

      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

! Required to handle the profile graphs plotted in child windows
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      INTEGER I

      CALL PushActiveWindowID
! Close "Save Solutions" window which may be up
      CALL WDialogSelect(IDD_OutputSolutions)
      CALL WDialogHide
! Close all SA profile child windows that are still open
      DO i = 1, MaxNumChildWin
        IF (SAUsedChildWindows(i) .EQ. 1) THEN
          CALL WindowCloseChild(i)
          SAUsedChildWindows(i) = 0
          CALL UnRegisterChildWindow(i)
        ENDIF
      ENDDO
! Close Chi-sqd plot 
      CALL Close_Chisq_Plot
      CALL PopActiveWindowID

      END SUBROUTINE CloseOutputSolutionsChildWindows
!
!*******************************************************************************
!
      SUBROUTINE DealWithOutputSolutions
      
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER hFile, I, J, iFlags, iSol
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(100) FILTER

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_OutputSolutions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogSelect(IDD_OutputSolutions)
              CALL WDialogHide
            CASE (IDBBROWSE)
              iFlags = SaveDialog + PromptOn
              FILTER = 'Output files (*.pdb,*.cssr,*.ccl,*.cif,*.res,*.pro)|*.pdb;*.cssr;*.ccl;*.cif;*.res;*.pro|'
              tFileName = ''
              CALL WSelectFile(FILTER,iFlags,tFileName,'Directory + base name for solutions')
              IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
                CALL WDialogPutString(IDF_DirAndBaseName,tFileName)
              ENDIF
              CALL UpdateOutputSolutionsWIndow
            CASE (IDB_Output2)
              CALL SaveSolutions
            CASE (IDB_SaveTBL)
              iFlags = SaveDialog + AppendExt + PromptOn
              FILTER = 'Parameter table (*.tbl)|*.tbl|'
              CALL WDialogGetString(IDF_DirAndBaseName,tFileName)
              tFileName = tFileName(1:LEN_TRIM(tFileName))//'.tbl'
              CALL WSelectFile(FILTER, iFlags, tFileName, 'Save parameters')
! Problems here: 
! - quaternions are not normalised
! - torsion angles can be e.g. -238.8781
! Must only write wanted solutions, not all.
              IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
                hFile = 10
                OPEN(UNIT=hFile,FILE=tFileName,ERR=998)
                DO iSol = 1, NumOf_SA_Runs
                  WRITE(hFile,'(100(F9.4,1X))',ERR=998) (BestValuesDoF(J,iSol2Run(iSol)),J=1,nvar)
                ENDDO
                CLOSE(hFile)
              ENDIF
              CALL PopActiveWindowID
              RETURN
  998         CALL ErrorMessage('Error writing file.')
              CLOSE(hFile)
            CASE (IDB_OutputChiSqd)
              iFlags = SaveDialog + AppendExt + PromptOn
              FILTER = 'Chi-sqrd vs. number of moves (*.chi)|*.chi|'
              CALL WDialogGetString(IDF_DirAndBaseName,tFileName)
              tFileName = tFileName(1:LEN_TRIM(tFileName))//'.chi'
              CALL WSelectFile(FILTER,iFlags,tFileName,'Save chi-sqrd vs. number of moves')
              IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
                hFile = 10
                OPEN(UNIT=hFile,FILE=tFileName,ERR=999)
                DO I = 1, MaxIterationSoFar
                  WRITE(hFile,'(I10,1X,99(F9.2,1X))',ERR=999) I*nmpert,(chi_sqd(I,J),J=1,NumOf_SA_Runs) ! NumOf_SA_Runs = last completed SA run
                ENDDO
                CLOSE(hFile)
              ENDIF
              CALL PopActiveWindowID
              RETURN
  999         CALL ErrorMessage('Error writing file.')
              CLOSE(hFile)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_OutputPDB, IDF_OutputCSSR, IDF_OutputCCL, IDF_OutputCIF, IDF_OutputRES, &
                  IDF_OutputPRO, IDF_OutputSolMenu, IDF_DirAndBaseName)
              CALL UpdateOutputSolutionsWIndow
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithOutputSolutions
!
!*******************************************************************************
!
      SUBROUTINE UpdateOutputSolutionsWindow
      
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER iOption, NumOfFileTypes, iRow, iStatus, NumSelSol, CheckedSol(1:5)
      CHARACTER(100) LineStr(0:5)
      INTEGER iLine, I, J
      CHARACTER(MaxPathLength) tFileName
      INTEGER tLen
      CHARACTER(50) BaseStr
      INTEGER       BaseLen
      CHARACTER(4) ExtStr(6)
      CHARACTER(3) RunNumStr
      LOGICAL SaveButtonAvailable

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_OutputSolutions)
! Get directory + base name and form appropriate string
      CALL WDialogGetString(IDF_DirAndBaseName, tFileName)
      tLen = LEN_TRIM(tFileName)
! Maximum to be added: "_Best.cssr" = 10
! Total: 60
! If tLen + 10 > 60, insert "..."
      IF (tLen+10 .GT. 60) THEN
! Resultant length should be 50, we want to add "...", so cut down to 50 - 4 = 46 by removing,
! from the middle, equal parts to left and right
        BaseStr( 1:23) = tFileName(1:23)
        BaseStr(24:26) = "..."
        BaseStr(27:49) = tFileName(tLen-22:tLen)
        BaseLen = 49
      ELSE
        BaseStr = tFileName(1:tLen)
        BaseLen = tLen
      ENDIF
      BaseStr = BaseStr(1:BaseLen)//"_"
      BaseLen = BaseLen + 1
      NumOfFileTypes = 0
      IF (WDialogGetCheckBoxLogical(IDF_OutputPDB)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'pdb'
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCSSR)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'cssr'
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCCL)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'ccl'
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCIF)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'cif'
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputRES)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'res'
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputPRO)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'pro'
      ENDIF
      SaveButtonAvailable = (NumOfFileTypes .GT. 0)
      DO iLine = 1, 5
        LineStr(iLine) = ''
      ENDDO
      IF (NumOfFileTypes .EQ. 0) THEN
        LineStr(0) = "No output formats are currently selected."
      ELSE
        LineStr(0) = "The following files will be saved:"
        CALL WDialogGetMenu(IDF_OutputSolMenu,iOption)
        SELECT CASE (iOption)
          CASE (1) ! Best Solution
            J = NumOfFileTypes
            IF (NumOfFileTypes .GT. 5) J = 4
            DO iLine = 1, J
              LineStr(iLine) = BaseStr(1:BaseLen)//"Best."//ExtStr(iLine)
            ENDDO
            IF (NumOfFileTypes .GT. 5) LineStr(5) = "etc."
          CASE (2) ! Selected Solutions
! Check if any solution selected at all
            CALL WDialogSelect(IDD_SAW_Page5)
            NumSelSol = 0
            DO iRow = 1, NumOf_SA_Runs
              CALL WGridGetCellCheckBox(IDF_SA_summary, 3, iRow, istatus)
              IF (istatus .EQ. 1) THEN
                NumSelSol = NumSelSol + 1
                IF (NumSelSol .LE. 5) CheckedSol(NumSelSol) = iRow
              ENDIF
            ENDDO
            IF (NumSelSol .EQ.0) THEN
              LineStr(0) = 'No solutions are currently selected.'
              SaveButtonAvailable = .FALSE.
            ELSE
              iLine = 0
              DO I = 1, NumSelSol
                DO J = 1, NumOfFileTypes
                  iLine = iLine + 1
                  IF (iLine .LE. 5) THEN
                    WRITE (RunNumStr,'(I3.3)') iSol2Run(CheckedSol(I))
                    LineStr(iLine) = BaseStr(1:BaseLen)//RunNumStr//"."//ExtStr(J)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
            IF (NumSelSol*NumOfFileTypes .GT. 5) LineStr(5) = "etc."
            CALL WDialogSelect(IDD_OutputSolutions)
          CASE (3) ! All Solutions
            iLine = 0
            DO I = 1, NumOf_SA_Runs
              DO J = 1, NumOfFileTypes
                iLine = iLine + 1
                IF (iLine .LE. 5) THEN
                  WRITE (RunNumStr,'(I3.3)') iSol2Run(I)
                  LineStr(iLine) = BaseStr(1:BaseLen)//RunNumStr//"."//ExtStr(J)
                ENDIF
              ENDDO
            ENDDO
            IF (NumOf_SA_Runs*NumOfFileTypes .GT. 5) LineStr(5) = "etc."
        END SELECT
      ENDIF
      CALL WDialogPutString(IDF_egLine0, LineStr(0))
      CALL WDialogPutString(IDF_egLine1, LineStr(1))
      CALL WDialogPutString(IDF_egLine2, LineStr(2))
      CALL WDialogPutString(IDF_egLine3, LineStr(3))
      CALL WDialogPutString(IDF_egLine4, LineStr(4))
      CALL WDialogPutString(IDF_egLine5, LineStr(5))
      CALL WDialogFieldStateLogical(IDB_Output2, SaveButtonAvailable)
      CALL PopActiveWindowID

      END SUBROUTINE UpdateOutputSolutionsWindow
!
!*******************************************************************************
!
      SUBROUTINE SaveSolutions
      
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      CHARACTER(4) ExtStr(1:6)
      INTEGER      ExistingFiles(1:6) ! per extension
      INTEGER iOption, NumOfFileTypes, iStatus
      INTEGER tLen, tLen2, I, J, tExitButton
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(3) RunNumStr
      LOGICAL OutputExists, OverwriteAll, WriteThisFile
      CHARACTER(100) tString
      CHARACTER(255) tMessage
      INTEGER        Ext2Type(6)

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_OutputSolutions)
! Get directory + base name
      CALL WDialogGetString(IDF_DirAndBaseName,tFileName)
      tLen = LEN_TRIM(tFileName)
      tFileName = tFileName(1:tLen)//"_"
      tLen = tLen + 1
! First, generate all possible filenames and check that we wouldn't overwrite one
      NumOfFileTypes = 0
      IF (WDialogGetCheckBoxLogical(IDF_OutputPDB)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'pdb'
        Ext2Type(NumOfFileTypes) = 1
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCSSR)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'cssr'
        Ext2Type(NumOfFileTypes) = 2
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCCL)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'ccl'
        Ext2Type(NumOfFileTypes) = 3
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputCIF)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'cif'
        Ext2Type(NumOfFileTypes) = 4
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputRES)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'res'
        Ext2Type(NumOfFileTypes) = 5
      ENDIF
      IF (WDialogGetCheckBoxLogical(IDF_OutputPRO)) THEN
        CALL INC(NumOfFileTypes)
        ExtStr(NumOfFileTypes) = 'pro'
        Ext2Type(NumOfFileTypes) = 6
      ENDIF
      ExistingFiles = 0
      CALL WDialogGetMenu(IDF_OutputSolMenu,iOption)
! Try all possible output names, when match, set OutputExists to .TRUE.
      SELECT CASE (iOption)
        CASE (1) ! Best Solution
          DO J = 1, NumOfFileTypes
            tFileName = tFileName(1:tLen)//"Best."//ExtStr(J)
            INQUIRE(FILE=tFileName,EXIST=OutputExists)
            IF (OutputExists) ExistingFiles(J) = ExistingFiles(J) + 1
          ENDDO
        CASE (2) ! Selected Solutions
          CALL WDialogSelect(IDD_SAW_Page5)
          DO I = 1, NumOf_SA_Runs
            CALL WGridGetCellCheckBox(IDF_SA_summary,3,I,istatus)
            IF (istatus .EQ. 1) THEN
              DO J = 1, NumOfFileTypes
                WRITE (RunNumStr,'(I3.3)') iSol2Run(I)
                tFileName = tFileName(1:tLen)//RunNumStr//"."//ExtStr(J)
                INQUIRE(FILE=tFileName,EXIST=OutputExists)
                IF (OutputExists) ExistingFiles(J) = ExistingFiles(J) + 1
              ENDDO
            ENDIF
          ENDDO
        CASE (3) ! All Solutions
          DO I = 1, NumOf_SA_Runs
            DO J = 1, NumOfFileTypes
              WRITE (RunNumStr,'(I3.3)') iSol2Run(I)
              tFileName = tFileName(1:tLen)//RunNumStr//"."//ExtStr(J)
              INQUIRE(FILE=tFileName,EXIST=OutputExists)
              IF (OutputExists) ExistingFiles(J) = ExistingFiles(J) + 1
            ENDDO
          ENDDO
      END SELECT
      OutputExists = .FALSE.
      DO J = 1, NumOfFileTypes
        IF (ExistingFiles(J) .GT. 0) OutputExists = .TRUE.
      ENDDO
      IF (OutputExists) THEN
        tMessage = 'The following files already exist:'//CHAR(13)//CHAR(10)//CHAR(13)//CHAR(10)
        tLen2 = LEN_TRIM(tMessage)
        DO J = 1, NumOfFileTypes
          IF (ExistingFiles(J) .GT. 0) THEN
            WRITE(tString,'(I2," files of type .",A4)') ExistingFiles(J), ExtStr(J)
            tString = tString(1:LEN_TRIM(tString))//CHAR(13)//CHAR(10)
            tMessage = tMessage(1:tLen2)//tString
            tLen2 = LEN_TRIM(tMessage)
          ENDIF
        ENDDO
        tMessage = tMessage(1:tLen2)//CHAR(13)//CHAR(10)
        tLen2 = LEN_TRIM(tMessage)
        tMessage = tMessage(1:tLen2)//'Would you like to overwrite these files?'
        tLen2 = LEN_TRIM(tMessage)
        CALL WMessageBox(YesNoCancel, QuestionIcon, CommonCancel, tMessage(1:tLen2), "Overwrite files?")
        tExitButton = WInfoDialog(ExitButtonCommon)
        IF (tExitButton .EQ. CommonCancel) THEN
          CALL PopActiveWindowID
          RETURN
        ENDIF
        OverwriteAll = (tExitButton .EQ. CommonYes)
      ELSE
        OverwriteAll = .TRUE.
      ENDIF
! Go ahead and save
! Try all possible output names, when match, set OutputExists to .TRUE.
      CALL WCursorShape(CurHourGlass)
      WriteThisFile = .TRUE.
      SELECT CASE (iOption)
        CASE (1) ! Best Solution
          DO J = 1, NumOfFileTypes
            tFileName = tFileName(1:tLen)//"Best."//ExtStr(J)
            IF (.NOT. OverwriteAll) THEN
              INQUIRE(FILE=tFileName,EXIST=OutputExists)
              WriteThisFile = .NOT. OutputExists
            ENDIF
            IF (WriteThisFile) CALL SA_structure_output_2(1, tFileName, Ext2Type(J))
          ENDDO
        CASE (2) ! Selected Solutions
          CALL WDialogSelect(IDD_SAW_Page5)
          DO I = 1, NumOf_SA_Runs
            CALL WGridGetCellCheckBox(IDF_SA_summary,3,I,istatus)
            IF (istatus .EQ. 1) THEN
              DO J = 1, NumOfFileTypes
                WRITE (RunNumStr,'(I3.3)') iSol2Run(I)
                tFileName = tFileName(1:tLen)//RunNumStr//"."//ExtStr(J)
                IF (.NOT. OverwriteAll) THEN
                  INQUIRE(FILE=tFileName,EXIST=OutputExists) 
                  WriteThisFile = .NOT. OutputExists
                ENDIF
                IF (WriteThisFile) CALL SA_structure_output_2(I, tFileName, Ext2Type(J))
              ENDDO
            ENDIF
          ENDDO
        CASE (3) ! All Solutions
          DO I = 1, NumOf_SA_Runs
            DO J = 1, NumOfFileTypes
              WRITE (RunNumStr,'(I3.3)') iSol2Run(I)
              tFileName = tFileName(1:tLen)//RunNumStr//"."//ExtStr(J)
              IF (.NOT. OverwriteAll) THEN
                INQUIRE(FILE=tFileName,EXIST=OutputExists) 
                WriteThisFile = .NOT. OutputExists
              ENDIF
              IF (WriteThisFile) CALL SA_structure_output_2(I, tFileName, Ext2Type(J))
            ENDDO
          ENDDO
      END SELECT
      CALL WCursorShape(CurCrossHair)
      CALL PopActiveWindowID

      END SUBROUTINE SaveSolutions
!
!*******************************************************************************
!
      SUBROUTINE SA_structure_output_2(TheSolutionNr, TheFileName, TheFileType)

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheSolutionNr
      CHARACTER*(*), INTENT (IN   ) :: TheFileName
      INTEGER,       INTENT (IN   ) :: TheFileType
! 1 = pdb
! 2 = cssr
! 3 = ccl
! 4 = cif
! 5 = res
! 6 = pro

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      REAL, EXTERNAL         :: UnitCellVolume
      LOGICAL, EXTERNAL      :: Get_AutoLocalMinimisation, UseHydrogensDuringAuto
      INTEGER, EXTERNAL      :: WritePDBCommon, Get_HydrogenTreatment
      CHARACTER*1, EXTERNAL  :: ChrLowerCase
      CHARACTER*20, EXTERNAL :: Integer2String
      REAL                     qvals(4), qnrm
      LOGICAL tSavePDB, tSaveCSSR, tSaveCCL, tSaveCIF, tSaveRES, tSavePRO
      INTEGER ipcount, iScat, tElement, k1
      INTEGER hFileCSSR, hFilePDB, hFileCCL, hFileCIF, hFileRES, hFilePRO
      INTEGER I, J, II, K, iiact, iTotal, iFrg, IJ, iOrig
      REAL    x_pdb(1:3)
      INTEGER TotNumBonds, NumOfAtomsSoFar, iBond1, iBond2, iTem, tLen, iRadSelection
      INTEGER                   tLen1, tLen2
      CHARACTER*80              tString, tString1, tString2
      CHARACTER*2               LATT
      INTEGER                   NumOfAtmPerElm(1:MaxElm)
      CHARACTER*72              DASHRemarkStr
      REAL                      tIntChiSqd, tProChiSqd
      INTEGER                   tRunNr
      LOGICAL tLOG_HYDROGENS

      tRunNr = iSol2Run(TheSolutionNr)
      WRITE (DASHRemarkStr,100,ERR=999) IntensityChiSqd(tRunNr), ProfileChiSqd(tRunNr), MaxMoves
  100 FORMAT ('chi**2=',F7.2,' and profile chi**2=',F7.2,' max. moves=',I8)
! Just in case the user decides to change this in the options menu just while we are in this routine:
! make local copies of the variables that determine which files to save.
      tSavePDB  = .FALSE.
      tSaveCSSR = .FALSE.
      tSaveCCL  = .FALSE.
      tSaveCIF  = .FALSE.
      tSaveRES  = .FALSE.
      tSavePRO  = .FALSE.
      SELECT CASE (TheFileType)
        CASE (1)
          tSavePDB  = .TRUE.
        CASE (2)
          tSaveCSSR = .TRUE.
        CASE (3)
          tSaveCCL  = .TRUE.
        CASE (4)
          tSaveCIF  = .TRUE.
        CASE (5)
          tSaveRES  = .TRUE.
        CASE (6)
          tSavePRO  = .TRUE.
      END SELECT
!
!       Output a CSSR file to fort.64
!       Output a PDB  file to fort.65
!       Output a CCL  file to fort.66
!       Output a CIF  file to fort.67
!       Output a RES  file to fort.68
!       Output a PRO  file to fort.61 (the observed and calculated profile)
!
!       Write the file headers first
!
! CSSR ...
      IF (tSaveCSSR) THEN
        hFileCSSR = 64
        OPEN (UNIT=hFileCSSR,FILE=TheFileName,STATUS='unknown',ERR=999)
        WRITE (hFileCSSR,1000,ERR=999) (CellPar(ii),ii=1,3)
 1000   FORMAT (' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
        WRITE (hFileCSSR,1010,ERR=999) (CellPar(ii),ii=4,6), SGNumStr(NumberSGTable)(1:3)
 1010   FORMAT ('   ALPHA,BETA,GAMMA =',3F8.3,'    SPGR = ',A3)
        IF (PrefParExists) THEN
          WRITE (hFileCSSR,"(' ',I3,'   0 PO',4(1X,F6.3))",ERR=999) TotNumOfAtoms,(PrefPars(ii),ii=1,3),BestValuesDoF(iPrfPar,tRunNr)
        ELSE
          WRITE (hFileCSSR,"(' ',I3,'   0 DASH solution')",ERR=999) TotNumOfAtoms
        ENDIF
        WRITE (hFileCSSR,'(A)',ERR=999) ' '//DASHRemarkStr 
      ENDIF
! PDB ...
      IF (tSavePDB) THEN
        hFilePDB = 65
        OPEN (UNIT=hFilePDB,FILE=TheFileName,STATUS='unknown',ERR=999)
! Add in a Header record
        WRITE (hFilePDB,"('HEADER    PDB Solution File generated by DASH')",ERR=999)
        WRITE (hFilePDB,'(A)',ERR=999) 'REMARK '//DASHRemarkStr 
        IF (PrefParExists) THEN
          WRITE (hFilePDB,'(A,4(1X,F6.3))',ERR=999) 'REMARK PO',(PrefPars(ii),ii=1,3),BestValuesDoF(iPrfPar,tRunNr)
        ENDIF
        IF (WritePDBCommon(hFilePDB) .NE. 0) GOTO 999
      ENDIF
! CCL ...
      IF (tSaveCCL) THEN
        hFileCCL = 66
        OPEN (UNIT=hFileCCL,FILE=TheFileName,STATUS='unknown',ERR=999)
        WRITE (hFileCCL,'(A)',ERR=999) 'Z '//DASHRemarkStr 
        IF (PrefParExists) THEN
          WRITE (hFileCCL,'(A,4(1X,F6.3))',ERR=999) 'Z PO',(PrefPars(ii),ii=1,3),BestValuesDoF(iPrfPar,tRunNr)
        ENDIF
        WRITE (hFileCCL,1100,ERR=999) (CellPar(ii),ii=1,6)
 1100   FORMAT ('C ',6F10.5)
      ENDIF
! CIF ...
      IF (tSaveCIF) THEN
        hFileCIF = 67
        OPEN (UNIT=hFileCIF,FILE=TheFileName,STATUS='unknown',ERR=999)
        WRITE (hFileCIF,'("data_global")',ERR=999)
        WRITE (hFileCIF,'(A)',ERR=999) '# '//DASHRemarkStr
        tString = CrystalSystemString(LatBrav)
        tString(1:1) = ChrLowerCase(tString(1:1))
        tLen = LEN_TRIM(tString)
! Remove '-a' etc. from monoclinic
        IF (LatBrav .EQ. 2 .OR. LatBrav .EQ. 3 .OR. LatBrav .EQ. 4) tLen = tLen -2
        WRITE (hFileCIF,'("_symmetry_cell_setting ",A)',ERR=999) tString(1:tLen)
! Following line: issues with ":" in certain space group names
        tString = SGHMaStr(NumberSGTable)
        tLen = LEN_TRIM(tString)
        DO WHILE ((tLen .GT. 1) .AND. (tString(tLen:tLen) .NE. ':'))
          tLen = tLen - 1
        ENDDO
        IF (tString(tLen:tLen) .EQ. ':') THEN
          tLen = tLen - 1
        ELSE
          tLen = LEN_TRIM(tString)
        ENDIF
        tString = "'"//tString(1:tLen)//"'"
        tLen = tLen + 2
        WRITE (hFileCIF,"('_symmetry_space_group_name_H-M ',A)",ERR=999) tString(1:tLen)
        WRITE (hFileCIF,'("loop_")',ERR=999)
        WRITE (hFileCIF,'("  _symmetry_equiv_pos_as_xyz")',ERR=999)
        DO i = 1, npdbops
          tString = cpdbops(i)
          tLen = LEN_TRIM(tString)
          tString = "  '"//tString(1:tLen)//"'"
          tLen = tLen + 4
          WRITE (hFileCIF,"(A)",ERR=999) tString(1:tLen)
        ENDDO
        WRITE (hFileCIF,'("_cell_length_a    ",F8.4)',ERR=999) CellPar(1)
        WRITE (hFileCIF,'("_cell_length_b    ",F8.4)',ERR=999) CellPar(2)
        WRITE (hFileCIF,'("_cell_length_c    ",F8.4)',ERR=999) CellPar(3)
        WRITE (hFileCIF,'("_cell_angle_alpha ",F8.4)',ERR=999) CellPar(4)
        WRITE (hFileCIF,'("_cell_angle_beta  ",F8.4)',ERR=999) CellPar(5)
        WRITE (hFileCIF,'("_cell_angle_gamma ",F8.4)',ERR=999) CellPar(6)
        WRITE (hFileCIF,'("_cell_volume",F7.1)',ERR=999) UnitCellVolume(CellPar(1),CellPar(2),CellPar(3),CellPar(4),CellPar(5),CellPar(6))
        WRITE (hFileCIF,'("_cell_formula_units_Z  ?")',ERR=999)
        SELECT CASE (JRadOption)
          CASE (1) ! X-ray lab data
            CALL PushActiveWindowID
            CALL WDialogSelect(IDD_Data_Properties)
            CALL WDialogGetMenu(IDF_Wavelength_Menu,iRadSelection)
            CALL PopActiveWindowID
            SELECT CASE (iRadSelection)
! Winteracter menu:
!     1 = <...>
!     2 = Cu      <==  DEFAULT
!     3 = Mo
!     4 = Co
!     5 = Cr
!     6 = Fe
              CASE (1)
                tString = 'Xx'
              CASE (2)
                tString = 'Cu'
              CASE (3)
                tString = 'Mo'
              CASE (4)
                tString = 'Co'
              CASE (5)
                tString = 'Cr'
              CASE (6)
                tString = 'Fe'
            END SELECT
            tString = "'"//tString(1:2)//" K\a'"
          CASE (2) ! X-ray synchrotron data
            tString = 'synchrotron'
          CASE (3) ! Constant Wavelength Neutrons
          CASE (4) ! Time-of-Flight Neutrons
        END SELECT
        tLen = LEN_TRIM(tString)
        WRITE (hFileCIF,'("_diffrn_radiation_type ",A)',ERR=999) tString(1:tLen)
        WRITE (hFileCIF,'("_diffrn_radiation_wavelength",F10.5)',ERR=999) ALambda
        WRITE (hFileCIF,'("_refine_ls_goodness_of_fit_all ",F7.3)',ERR=999) SQRT(MAX(0.0,IntensityChiSqd(tRunNr)))
        IF (PrefParExists) THEN
          WRITE (hFileCIF,'("_pd_proc_ls_pref_orient_corr")',ERR=999)               
          WRITE (hFileCIF,'(";")',ERR=999)
          WRITE (hFileCIF,'("  March-Dollase function")',ERR=999)
          WRITE (hFileCIF,'("  Orientation =",3(1X,F6.3))',ERR=999) (PrefPars(ii),ii=1,3)
          WRITE (hFileCIF,'("  Magnitude   = ",F6.3)',ERR=999) BestValuesDoF(iPrfPar,tRunNr)
          WRITE (hFileCIF,'(";")',ERR=999)
        ENDIF
!C data_FILENAME
!C _symmetry_cell_setting            monoclinic
!C _symmetry_space_group_name_H-M    'P 1 21/n 1'
!C loop_
!C _symmetry_equiv_pos_as_xyz
!C   '   +x,   +y,   +z'
!C   '1/2-x,1/2+y,1/2-z'
!C   '   -x,   -y,   -z'
!C   'x+1/2,1/2-y,1/2+z'
!C _cell_length_a                    20.9674(3)
!C _cell_length_b                    14.5059(2)
!C _cell_length_c                    10.9862(1)
!C _cell_angle_alpha                 90.0
!C _cell_angle_beta                  117.825(2)
!C _cell_angle_gamma                 90.0
!C _cell_volume                      2955.0
!C _cell_formula_units_Z             4
!C _diffrn_radiation_wavelength      0.80008
!C # The following item is the same as CHI, the square root of 'CHI squared'
!C _refine_ls_goodness_of_fit_all    3.26
!C # 9. ATOMIC COORDINATES AND DISPLACEMENT PARAMETERS
!C loop_
!C        _atom_site_label
!C        _atom_site_fract_x
!C        _atom_site_fract_y
!C        _atom_site_fract_z
!C        _atom_site_occupancy
!C        _atom_site_adp_type
!C        _atom_site_B_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Biso 3.0
!C      C2     -0.05898   0.41596   0.27356  1.0 Biso 3.0

        WRITE (hFileCIF,'("loop_")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_label")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_fract_x")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_fract_y")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_fract_z")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_occupancy")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_adp_type")',ERR=999)
        WRITE (hFileCIF,'("  _atom_site_B_iso_or_equiv")',ERR=999)
      ENDIF
! RES ...
      IF (tSaveRES) THEN
        hFileRES = 68
        OPEN (UNIT=hFileRES,FILE=TheFileName,STATUS='unknown',ERR=999)
        WRITE (hFileRES,"('TITL Solution File generated by DASH')",ERR=999)
        WRITE (hFileRES,'(A)',ERR=999) 'REM  '//DASHRemarkStr
        IF (PrefParExists) THEN
          WRITE (hFileRES,'(A,4(1X,F6.3))',ERR=999) 'REM  PO',(PrefPars(ii),ii=1,3),BestValuesDoF(iPrfPar,tRunNr)
        ENDIF
        WRITE (hFileRES,1032,ERR=999) ALambda, (CellPar(ii),ii=1,6)
 1032   FORMAT ('CELL ',F7.5,1X,6(F8.4,1X))
        WRITE (hFileRES,'("ZERR ",I2," 0.000 0.000 0.000 0.000 0.000 0.000")',ERR=999) npdbops
        IF (SGShmStr(NumberSGTable)(3:3) .EQ. 'C') THEN
          LATT(1:1) = ' '
        ELSE
          LATT(1:1) = '-'
        ENDIF
        SELECT CASE (SGShmStr(NumberSGTable)(1:1))
          CASE ('P')
            LATT(2:2) = '1'
          CASE ('I')
            LATT(2:2) = '2'
          CASE ('R') ! See Int. Tables page 5
            LATT(2:2) = '3'
          CASE ('F')
            LATT(2:2) = '4'
          CASE ('A')
            LATT(2:2) = '5'
          CASE ('B')
            LATT(2:2) = '6'
          CASE ('C')
            LATT(2:2) = '7'
        END SELECT
        WRITE (hFileRES,'(A)',ERR=999) 'LATT '//LATT
!C LATT N [1] 
!C Lattice type: 1=P, 2=I, 3=rhombohedral obverse on hexagonal axes, 4=F, 5=A, 6=B, 7=C. N must be made
!C negative if the structure is non-centrosymmetric. 
        IF (npdbops .GE. 2) THEN
          DO i = 2, npdbops ! " +X, +Y, +Z" must be left out
            tString = cpdbops(i)
            tLen = LEN_TRIM(tString)
            WRITE (hFileRES,"('SYMM ',A)",ERR=999) tString(1:tLen)
          ENDDO
        ENDIF
        NumOfAtmPerElm = 0
        DO iFrg = 1, nFrag
          DO i = 1, natoms(iFrg)
            CALL INC(NumOfAtmPerElm(zmElementCSD(i,iFrg)))
          ENDDO
        ENDDO
        tString1 = 'SFAC'
        tString2 = 'UNIT'
        tLen1 = 4
        tLen2 = 4
        DO i = 1, MaxElm
          IF (NumOfAtmPerElm(i) .GE. 1) THEN
            tString1 = tString1(1:tLen1)//'  '//ElementStr(i)
            tLen1 = tLen1 + 4
            WRITE (tString2(tLen2+1:tLen2+4),'(I4)') NumOfAtmPerElm(i)*npdbops
            tLen2 = tLen2 + 4
          ENDIF
        ENDDO
        WRITE (hFileRES,'(A)',ERR=999) tString1(1:tLen1)
        WRITE (hFileRES,'(A)',ERR=999) tString2(1:tLen2)
      ENDIF
      iiact = 0
      itotal = 0
      ipcount = 0
      DO iFrg = 1, nFrag
        itotal = iiact
! Write out the translation/rotation information for each residue
        IF (tSavePDB) THEN
          WRITE (hFilePDB,1039,ERR=999) iFrg
 1039     FORMAT ('REMARK Start of molecule number ',I6)
          WRITE (hFilePDB,1037,ERR=999) (BestValuesDoF(ij,tRunNr),ij=ipcount+1,ipcount+3)
 1037     FORMAT ('REMARK Translations: ',3F10.6)
        ENDIF
        IF (natoms(iFrg) .GT. 1) THEN
! Normalise the Q-rotations before writing them out ...
          qvals(1) = BestValuesDoF(ipcount+4,tRunNr)
          qvals(2) = BestValuesDoF(ipcount+5,tRunNr)
          qvals(3) = BestValuesDoF(ipcount+6,tRunNr)
          qvals(4) = BestValuesDoF(ipcount+7,tRunNr)
          qnrm = SQRT(qvals(1)**2 + qvals(2)**2 + qvals(3)**2 + qvals(4)**2)
          qvals = qvals / qnrm
          IF (tSavePDB) THEN
            WRITE (hFilePDB,1038,ERR=999) (qvals(ij),ij=1,4)
 1038       FORMAT ('REMARK Q-Rotations : ',4F10.6)
          ENDIF
          ipcount = ipcount + izmpar(iFrg)
        ENDIF
        DO i = 1, natoms(iFrg)
          iiact = iiact + 1
          iOrig = izmbid(i,iFrg)
          ii = OrderedAtm(itotal + iOrig)
! The CSSR atom lines
          IF (tSaveCSSR) THEN
            WRITE (hFileCSSR,1110,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4),(XAtmCoords(k,ii,tRunNr),k=1,3), 0, 0, 0, 0, 0, 0, 0, 0, 0.0
 1110       FORMAT (I4,1X,A4,2X,3(F9.5,1X),8I4,1X,F7.3)
          ENDIF
          IF (tSavePDB) THEN
! The PDB atom lines
            CALL PremultiplyVectorByMatrix(f2cpdb, XAtmCoords(1,ii,tRunNr), x_pdb)
! Note that elements are right-justified
! WebLab viewer even wants the elements in the atom names to be right justified.
            IF (ElSym(iOrig,iFrg)(2:2).EQ.' ') THEN
              WRITE (hFilePDB,1120,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:3), x_pdb(1), x_pdb(2), x_pdb(3), &
                              occ(iOrig,iFrg), tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:1)
 1120         FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
            ELSE
              WRITE (hFilePDB,1130,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4), x_pdb(1), x_pdb(2), x_pdb(3), &
                              occ(iOrig,iFrg), tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:2)
 1130         FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
            ENDIF
          ENDIF
! The CCL atom lines
          IF (tSaveCCL) THEN
            WRITE (hFileCCL,1033,ERR=999) ElSym(iOrig,iFrg), (XAtmCoords(k,ii,tRunNr),k=1,3), tiso(iOrig,iFrg), occ(iOrig,iFrg) 
 1033       FORMAT ('A ',A2,'  ',F10.5,1X,F10.5,1X,F10.5,1X,F4.2,1X,F4.2)
          ENDIF
! The CIF atom lines
!C # 9. ATOMIC COORDINATES AND DISPLACEMENT PARAMETERS
!C loop_
!C        _atom_site_label
!C        _atom_site_fract_x
!C        _atom_site_fract_y
!C        _atom_site_fract_z
!C        _atom_site_occupancy
!C        _atom_site_adp_type
!C        _atom_site_B_iso_or_equiv
!C      C1     -0.10853   0.45223   0.14604  1.0 Biso 3.0
!C      C2     -0.05898   0.41596   0.27356  1.0 Biso 3.0
          IF (tSaveCIF) THEN
            WRITE (hFileCIF,1034,ERR=999) OriginalLabel(iOrig,iFrg), (XAtmCoords(k,ii,tRunNr),k=1,3), occ(iOrig,iFrg), tiso(iOrig,iFrg) 
 1034       FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Biso ',F5.2)
          ENDIF
          IF (tSaveRES) THEN
! Determine this atom's entry number in the scattering factor list
            tElement = zmElementCSD(iOrig,iFrg)
            iScat = 0
            DO k1 = 1, tElement
              IF (NumOfAtmPerElm(k1) .NE. 0) iScat = iScat + 1
            ENDDO
            WRITE (hFileRES,1035,ERR=999) OriginalLabel(iOrig,iFrg), iScat, (XAtmCoords(k,ii,tRunNr),k=1,3), &
                                  occ(iOrig,iFrg), tiso(iOrig,iFrg)/(8.0*(PI**2)) 
 1035       FORMAT (A5,1X,I2,1X,3(F10.5,1X),F5.3,1X,F5.3)
          ENDIF
        ENDDO ! loop over atoms
      ENDDO ! loop over Z-matrices
      IF (tSaveCSSR) CLOSE (hFileCSSR)
      IF (tSavePDB) THEN
! Per Z-matrix, write out the connectivity.
        TotNumBonds = 0
        NumOfAtomsSoFar = 0
        DO iFrg = 1, nFrag
          IF (NumberOfBonds(iFrg) .GT. 0) THEN
            DO J = 1, NumberOfBonds(iFrg)
! Due to the backmapping, it is possible that the original number of the first atom is greater than the
! original number of the second atom. Mercury can't always read pdb files where this is the case.
              iBond1 = izmoid(Bonds(1,J,iFrg),iFrg)+NumOfAtomsSoFar
              iBond2 = izmoid(Bonds(2,J,iFrg),iFrg)+NumOfAtomsSoFar
              IF (iBond1 .GT. iBond2) THEN
                iTem   = iBond1
                iBond1 = iBond2
                iBond2 = iTem
              ENDIF
              WRITE(hFilePDB,'(A6,I5,I5)',ERR=999) 'CONECT', iBond1, iBond2
            ENDDO
          ENDIF
          NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
          TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
        ENDDO ! loop over Z-matrices
        WRITE (hFilePDB,"('END')",ERR=999)
        CLOSE (hFilePDB)
      ENDIF
      IF (tSaveCCL) CLOSE (hFileCCL)
      IF (tSaveCIF) CLOSE (hFileCIF)
      IF (tSaveRES) THEN
        WRITE (hFileRES,"('END ')",ERR=999)
        CLOSE (hFileRES)
      ENDIF          
      IF (tSavePRO) THEN
! Fill Xato
        DO I = 1, TotNumOfAtoms
          Xato(1,I) = XAtmCoords(1,I,tRunNr)
          Xato(2,I) = XAtmCoords(2,I,tRunNr)
          Xato(3,I) = XAtmCoords(3,I,tRunNr)
        ENDDO
! Fill Preferred Orientation part
        IF (PrefParExists) THEN
          CALL PO_PRECFC(BestValuesDoF(iPrfPar,tRunNr))
        ENDIF
! VALCHI fills BICALC
        tLOG_HYDROGENS = LOG_HYDROGENS
        IF (Get_HydrogenTreatment() .EQ. 3) THEN
          LOG_HYDROGENS = .TRUE.
        ELSE
          LOG_HYDROGENS = (Get_AutoLocalMinimisation() .AND. UseHydrogensDuringAuto())
        ENDIF
        CALL VALCHI(tIntChiSqd,0)    ! Structural part
        LOG_HYDROGENS = tLOG_HYDROGENS
! Valchipro fills YCBIN
        CALL VALCHIPRO(tProChiSqd)
        hFilePRO = 61
        OPEN (UNIT=hFilePRO,FILE=TheFileName,STATUS='unknown',ERR=999)
        WRITE(hFilePRO,'(2(A,F7.2))',ERR=999) "Intensity chi-sqrd = ", tIntChiSqd, ", profile chi-sqrd = ", tProChiSqd
        WRITE(hFilePRO,'(A)',ERR=999) "      2theta"//CHAR(9)//"        Yobs"//CHAR(9)//"   ESD(Yobs)"//CHAR(9)//"       Ycalc"//CHAR(9)//"(Yobs - Ycalc) / ESD(Yobs)"
        DO I = 1, NBIN
          WRITE(hFilePRO,12,ERR=999) XBIN(I), CHAR(9), YOBIN(I), CHAR(9), EBIN(I), CHAR(9), YCBIN(I), CHAR(9), (YOBIN(I)-YCBIN(I))/EBIN(I)
   12     FORMAT(F12.4,4(A,F12.4))
        ENDDO
        CLOSE(hFilePRO)
      ENDIF
      RETURN
 1380 FORMAT ('REMARK 290 ')
  999 CALL ErrorMessage('Error writing SA output file.')
      CLOSE(hFilePDB)
      CLOSE(hFileCSSR)
      CLOSE(hFileCCL)
      CLOSE(hFileCIF)
      CLOSE(hFileRES)
      CLOSE(hFilePRO)

      END SUBROUTINE SA_structure_output_2
!
!*******************************************************************************
!
      SUBROUTINE DealWithProfilePlot
! April2002 Added Zoom functionality.  Still needs a bit of tidying....
      
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 
       
! Required to handle the profile graphs plotted in child windows
      INTEGER  SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)
      INTEGER iHandle
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
           IF (EventInfo%VALUE1 .EQ. KeyHome) THEN 
             CALL ResetProfPlotAxes(EventInfo%win)
             CALL WindowClear
             CALL plot_pro_file(EventInfo%win)
           ENDIF
      END SELECT

      END SUBROUTINE DealWithProfilePlot
!
!*****************************************************************************
!
      SUBROUTINE PlotZoom(iHandle)
!
!  Enable button up and mouse movement events
!
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iHandle

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      REAL xgcurold, ygcurold

      REAL                  Ymin,                   Ymax,                   XMin,                   XMax
      COMMON /PROFPLOTAXES/ Ymin(1:MaxNumChildWin), Ymax(1:MaxNumChildWin), XMin(1:MaxNumChildWin), XMax(1:MaxNumChildWin)

      CALL WindowSelect(Ihandle)
      CALL WMessageEnable(MouseButUp, Enabled)
! Set the scale correctly. 
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)
      CALL IPgArea(0.1,0.1,0.9,0.9)
      CALL IPgUnits(xmin(iHandle),ymin(iHandle),xmax(iHandle),ymax(iHandle))
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
        IF (EventType .NE. (-1)) THEN
          IF (EventInfo%WIN .GT. 0) THEN
            CALL WindowSelect(iHandle)
            CALL IGrUnits(0.0,0.0,1.0,1.0)
            CALL IPgArea(0.1,0.1,0.9,0.9)
            CALL IPgUnits(xmin(iHandle),ymin(iHandle),xmax(iHandle),ymax(iHandle))
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
                CALL WMessageEnable(MouseButUp, Disabled)
                IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                  CALL IGrColourN(KolNumRectSelect)
                  CALL IGrPlotMode('EOR')
                  CALL IGrFillPattern(0,1,1)
                  ! Remove old
                  CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
                  CALL IGrPlotMode('Normal')
                  CALL IGrColourN(InfoGrScreen(PrevColReq))
                  IF (ABS(XCUR(2)-XCUR(1)).LT.0.003*(xmax(iHandle)-Xmin(iHandle))) RETURN
                  IF (ABS(YCUR(2)-YCUR(1)).LT.0.003*(YMAX(iHandle)-YMIN(iHandle))) RETURN
                  XMin(iHandle) = MIN(XCUR(1),XCUR(2))
                  XMax(iHandle) = MAX(XCUR(1),XCUR(2))  
                  YMin(iHandle) = MIN(YCUR(1),YCUR(2))
                  YMax(iHandle) = MAX(YCUR(1),YCUR(2))
                ENDIF
                CALL WindowClear()
                CALL Plot_pro_file(iHandle)
                RETURN  
            END SELECT
          ENDIF
        ENDIF
      ENDDO

      END SUBROUTINE PlotZoom
!
!*****************************************************************************
!
      SUBROUTINE ResetProfPlotAxes(iHandle)
!
!  
!
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iHandle

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL                  Ymin,                   Ymax,                   XMin,                   XMax
      COMMON /PROFPLOTAXES/ Ymin(1:MaxNumChildWin), Ymax(1:MaxNumChildWin), XMin(1:MaxNumChildWin), XMax(1:MaxNumChildWin)

      YMin(iHandle) = MINVAL(YOBIN(1:NBIN))
      YMax(iHandle) = MAXVAL(YOBIN(1:NBIN))
      Xmin(iHandle) = XBIN(1)
      Xmax(iHandle) = XBIN(NBIN)

      END SUBROUTINE ResetProfPlotAxes
!
!*****************************************************************************
!
