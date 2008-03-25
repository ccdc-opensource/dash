!
!*****************************************************************************
!
      SUBROUTINE DealWithPlotOptionsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Poly_Colours.inc'
                
      TYPE(WIN_RGB) :: SelectedColour

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK,IDCANCEL)
              CALL WDialogHide()
            CASE (IDF_ObservedData_Colour)
              SelectedColour = KolObs
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumObs, SelectedColour%IRed,SelectedColour%IGreen, SelectedColour%IBlue)
                KolObs = SelectedColour
              ENDIF
            CASE (IDF_CalculatedData_Colour)
              SelectedColour = KolCal
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCal, SelectedColour%IRed,SelectedColour%IGreen, SelectedColour%IBlue)
                KolCal = SelectedColour
              ENDIF
            CASE (IDF_DifferenceData_Colour)
              SelectedColour = KolDif
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumDif, SelectedColour%IRed, SelectedColour%IGreen, SelectedColour%IBlue)
                KolDif = SelectedColour
              ENDIF
            CASE (IDF_Axes_Colour)
              SelectedColour = KolMain
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumMain, SelectedColour%IRed, SelectedColour%IGreen, SelectedColour%IBlue)
                KolMain = SelectedColour
              ENDIF
            CASE (IDF_TickMark_Colour)
              SelectedColour = KolCTic
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCTic, SelectedColour%IRed, SelectedColour%IGreen, SelectedColour%IBlue)
                KolCTic = SelectedColour
              ENDIF
            CASE (IDF_PeakFitting_Colour)
              SelectedColour = KolPeakFit
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumPeakFit, SelectedColour%IRed, SelectedColour%IGreen, SelectedColour%IBlue)
                KolPeakFit = SelectedColour
              ENDIF
          END SELECT
          CALL Profile_Plot
        CASE (FieldChanged)
          IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_ErrorBar_Check, IDF_Background_Check, IDF_ConnectObsPoints, IDF_PlotPeakFitDif, IDF_ShowCumChiSqd, IDF_DivDiffByEsd)
                CALL Profile_Plot
            END SELECT
          ENDIF
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPlotOptionsWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithConfiguration

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical, SetRRMethodRadioState
      LOGICAL tLogical
      INTEGER IFLAGS, IFTYPE, iOpt
      CHARACTER*MaxPathLength tFileName
      CHARACTER*75  FILTER

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
! Update with user's editing
              CALL DASHWDialogGetString(IDF_DICVOLExe, DICVOLEXE)
              CALL DASHWDialogGetString(IDF_McMailleExe, McMailleEXE)
              CALL DASHWDialogGetString(IDF_TOPASExe, TOPASEXE)
              CALL DASHWDialogGetString(IDF_EXPGUIExe, EXPGUIEXE)
              CALL DASHWDialogGetString(IDF_RIETANExe, RIETANEXE)
              tLogical = DASHWDialogGetCheckBoxLogical(IDC_cif_for_viewer)
              CALL WDialogHide()
! Update state of related radio/check
              CALL SelectDASHDialog(IDD_PW_Page7)
              IF ( LEN_TRIM(DICVOLEXE) .GT. 0 ) THEN
                CALL WDialogFieldState(IDF_RADIO2, Enabled)
              ELSE
                CALL DASHWDialogGetRadioButton(IDF_RADIO1, iOpt)
                IF ( iOpt .EQ. 2 ) CALL WDialogPutRadioButton(IDF_RADIO1)
                CALL WDialogFieldState(IDF_RADIO2, Disabled)
              ENDIF
              IF ( LEN_TRIM(McMailleEXE) .GT. 0 ) THEN
                CALL WDialogFieldState(IDF_RADIO3, Enabled)
              ELSE
                CALL DASHWDialogGetRadioButton(IDF_RADIO1, iOpt)
                IF ( iOpt .EQ. 3 ) CALL WDialogPutRadioButton(IDF_RADIO1)
                CALL WDialogFieldState(IDF_RADIO3, Disabled)
              ENDIF
              CALL SelectDASHDialog(IDD_SAW_Page6)
              IF ( SetRRMethodRadioState() ) CALL WDialogPutRadioButton(IDF_RADIO1)
              CALL InfoError(1) ! Clear errors
              CALL SelectDASHDialog(IDD_SAW_Page6a)
! As loaded by WizardWindowShow, select IDD_SAW_Page6a may fail
              IF ( InfoError(1) .EQ. 0 ) THEN
                IF ( SetRRMethodRadioState() ) THEN
                  iRietveldMethodOpt = 1
                  CALL WDialogPutRadioButton(IDF_RADIO1)
                ENDIF
              ENDIF
              IF ( tLogical ) THEN
                CALL SelectDASHDialog(IDD_Summary)
                CALL WDialogPutRadioButton(IDF_ColourByElement)
                CALL WDialogFieldState(IDF_ColourBySolution, Disabled)
                CALL SelectDASHDialog(IDD_SAW_Page5)
                CALL WDialogPutRadioButton(IDF_ColourByElement)
                CALL WDialogFieldState(IDF_ColourBySolution, Disabled)
              ELSE
                CALL SelectDASHDialog(IDD_Summary)
                CALL WDialogFieldState(IDF_ColourBySolution, Enabled)
                CALL SelectDASHDialog(IDD_SAW_Page5)
                CALL WDialogFieldState(IDF_ColourBySolution, Enabled)
              ENDIF
            CASE (IDBBROWSE)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = ViewExe
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select Viewer', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                VIEWEXE = tFileName
                CALL WDialogPutString(IDF_ViewExe, VIEWEXE)
              ENDIF
            CASE (IDBBROWSE2)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = MOGULEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select Mogul Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                MOGULEXE = tFileName
                CALL WDialogPutString(IDF_MogulExe, MOGULEXE)
              ENDIF
            CASE (IDBBROWSE3)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = DICVOLEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select DICVOL04 or later Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                DICVOLEXE = tFileName
                CALL WDialogPutString(IDF_DICVOLExe, DICVOLEXE)
              ENDIF
            CASE (IDBBROWSE4)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = TOPASEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select TOPAS Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                TOPASEXE = tFileName
                CALL WDialogPutString(IDF_TOPASExe, TOPASEXE)
              ENDIF
            CASE (IDBBROWSE5)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = EXPGUIEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select EXPGUI Tcl Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                EXPGUIEXE = tFileName
                CALL WDialogPutString(IDF_EXPGUIExe, EXPGUIEXE)
              ENDIF
            CASE (IDBBROWSE6)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = RIETANEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select RIETAN Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                RIETANEXE = tFileName
                CALL WDialogPutString(IDF_RIETANExe, RIETANEXE)
              ENDIF
            CASE (IDBBROWSE7)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = McMailleEXE
              CALL WSelectFile(FILTER, IFLAGS, tFileName, 'Select McMaille Executable', IFTYPE)
! Did the user press cancel?
              IF ( WInfoDialog(ExitButtonCommon) .EQ. CommonOK ) THEN
                McMailleEXE = tFileName
                CALL WDialogPutString(IDF_McMailleExe, McMailleEXE)
              ENDIF
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_BuiltIn_Mercury)
              IF (DASHWDialogGetCheckBoxLogical(IDF_BuiltIn_Mercury)) THEN
                CALL WDialogFieldState(IDF_Use_Client, Enabled)
                CALL WDialogFieldState(IDBBROWSE, Disabled)
                CALL WDialogFieldState(IDF_ViewExe, Disabled)
                CALL WDialogFieldState(IDF_ViewArg, Disabled)
              ELSE
                CALL WDialogFieldState(IDF_Use_Client, Disabled)
                CALL WDialogFieldState(IDBBROWSE, Enabled)
                CALL WDialogFieldState(IDF_ViewExe, Enabled)
                CALL WDialogFieldState(IDF_ViewArg, Enabled)
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithConfiguration
!
!*****************************************************************************
!
! Enable/disable Rietveld method ratio buttons on IDD_SAW_Page6 or IDD_SAW_Page6a
! Return if the currently selected radio botton is disabled
!
      LOGICAL FUNCTION SetRRMethodRadioState

      USE WINTERACTER
      USE DRUID_HEADER
      USE TAVAR

      IMPLICIT NONE

      INTEGER iOpt

      CALL DASHWDialogGetRadioButton(IDF_RADIO1, iOpt)
      SetRRMethodRadioState = .FALSE.
      IF ( LEN_TRIM(TOPASEXE) .GT. 0 ) THEN
        CALL WDialogFieldState(IDF_RADIO2, Enabled)
      ELSE
        CALL WDialogFieldState(IDF_RADIO2, Disabled)
        IF ( iOpt .EQ. 2 ) SetRRMethodRadioState = .TRUE.
      ENDIF
      IF ( LEN_TRIM(EXPGUIEXE) .GT. 0 ) THEN
        CALL WDialogFieldState(IDF_RADIO3, Enabled)
      ELSE
        CALL WDialogFieldState(IDF_RADIO3, Disabled)
        IF ( iOpt .EQ. 3 ) SetRRMethodRadioState = .TRUE.
      ENDIF
      IF ( LEN_TRIM(RIETANEXE) .GT. 0 ) THEN
        CALL WDialogFieldState(IDF_RADIO4, Enabled)
        Rietan_FP = ( INDEX(RIETANEXE, '-FP') .GT. 0 .OR. &
                         INDEX(RIETANEXE, '-fp') .GT. 0 )
      ELSE
        CALL WDialogFieldState(IDF_RADIO4, Disabled)
        IF ( iOpt .EQ. 4 ) SetRRMethodRadioState = .TRUE.
        Rietan_FP = .FALSE.
      ENDIF

      RETURN

      END FUNCTION SetRRMethodRadioState
!
!*****************************************************************************
!
      SUBROUTINE DealWithStructuralInformation
! This is the window containing the four tabs from the 'View' menu

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Structural_Information)
      IF (PastPawley) THEN ! Don't do anything
        SELECT CASE (EventType)
          CASE (PushButton) ! one of the buttons was pushed
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDOK, IDCANCEL)
                CALL WDialogHide
            END SELECT
        END SELECT
      ELSE
        SELECT CASE (EventType)
          CASE (PushButton) ! one of the buttons was pushed
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDOK) ! The 'OK' button
                CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
                CALL SelectDASHDialog(IDD_Crystal_Symmetry)
                CALL DASHWDialogGetReal(IDF_ZeroPoint, ZeroPoint)
                CALL Upload_ZeroPoint               
                CALL DownloadWavelength(IDD_Data_Properties)
                CALL Generate_TicMarks
                CALL SelectDASHDialog(IDD_Structural_Information)
                CALL CheckUnitCellConsistency
                CALL WDialogHide
              CASE (IDCANCEL)
                CALL WDialogHide
            END SELECT
            CALL Profile_Plot
          CASE (FieldChanged)
! Do nothing
          CASE (TabChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDD_Data_Properties)
                CALL DownloadWavelength(IDD_Data_Properties)
                CALL Generate_TicMarks
              CASE (IDD_Peak_Positions)
              CASE (IDD_Crystal_Symmetry)
                CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
                CALL Download_SpaceGroup(IDD_Crystal_Symmetry)
              CASE (IDD_Peak_Widths)
            END SELECT
        END SELECT
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE DealWithStructuralInformation
!
!*****************************************************************************
!
      SUBROUTINE DealWithDiffractionSetupPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
                
      INTEGER IRadSelection

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Data_Properties)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDAPPLY) ! The 'Apply' button
              CALL DownloadWavelength(IDD_Data_Properties)    
              CALL Generate_TicMarks
          END SELECT
          CALL Profile_Plot
        CASE (FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_LabX_Source, IDF_SynX_Source, IDF_CWN_Source, IDF_TOF_source)
                CALL DASHWDialogGetRadioButton(IDF_LabX_Source, JRadOption)
                CALL Upload_Source
                CALL Generate_TicMarks 
              CASE (IDF_Wavelength_Menu) ! Wavelength menu selection
                CALL DASHWDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
                CALL SetWavelengthToSelection(IRadSelection)
              CASE (IDF_wavelength1)
                CALL DownloadWavelength(IDD_Data_Properties)
                CALL Generate_TicMarks
            END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDiffractionSetupPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithPeakPositionsPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Peak_Positions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (ID_Index_Output)
! Set the wavelength
              CALL DownLoadWavelength(IDD_Data_Properties)
              CALL SelectDASHDialog(IDD_Index_Preparation)
! If this is synchrotron data, then set the default error in the peak positions to 0.02 rahter than 0.03.
! This decreases the number of solutions and increases the speed of the search.
              IF (JRadOption .EQ. 2) THEN
                CALL WDialogPutReal(IDF_eps, 0.02, '(F5.3)')
              ELSE
                CALL WDialogPutReal(IDF_eps, 0.03, '(F5.3)')
              ENDIF
              CALL WDialogShow(-1, -1, 0, Modeless)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPeakPositionsPane
!
!*******************************************************************************
!
      SUBROUTINE DealWithSaSummary
! ep July 2001 
! Calls window which contains summary of
! results from simulated annealing run.  Handles messages from the window.
! Grid includes a "view" button which allows the user to view the molecular
! model via Mercury and the profile data in a graph window
      
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC' 

!ep    need the common block to identify the number of rows in the grid          
      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

! Required to handle the profile graphs plotted in child windows
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      LOGICAL, EXTERNAL :: Get_AutoAlign
      INTEGER I, iRow, iStatus, iLimit1, iLimit2, tInteger
      CHARACTER*(15) file_name

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Summary)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL WdialogHide
              CALL SelectDASHDialog(IDD_SA_Action1)
              CALL WDialogFieldState(IDB_Summary, Enabled)
! Closes all SA profile child windows which are still open when OK button clicked
              DO i = 1, MaxNumChildWin
                IF (SAUsedChildWindows(i).EQ.1) THEN
                  CALL WindowCloseChild(i)
                  SAUsedChildWindows(i) = 0
                  CALL UnRegisterChildWindow(i)
                ENDIF
              ENDDO
              CALL PopActiveWindowID
              RETURN
            CASE (IDB_Select)
              CALL DASHWDialogGetInteger(IDF_Limit1, iLimit1)
              CALL DASHWDialogGetInteger(IDF_Limit2, iLimit2)
              IF (iLimit1 .GT. iLimit2) THEN
                tInteger = iLimit2
                iLimit2  = iLimit1
                iLimit1  = tInteger
              ENDIF
              IF (iLimit2 .GT. NumOf_SA_Runs+1) iLimit2 = NumOf_SA_Runs+1
              IF (iLimit1 .LT.               1) iLimit1 =               1
              DO iRow = 1, NumOf_SA_Runs+1
                IF ((iRow .GE. iLimit1) .AND. (iRow .LE. iLimit2)) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Checked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Unchecked)
                ENDIF
              ENDDO
              CALL WDialogPutInteger(IDF_Limit1, iLimit1)
              CALL WDialogPutInteger(IDF_Limit2, iLimit2)
            CASE (IDF_InvertSelection)
              DO iRow = 1, NumOf_SA_Runs+1
                CALL DASHWGridGetCellCheckBox(IDF_SA_summary, 3, iRow, istatus)
                IF (istatus .EQ. 1) THEN
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Unchecked)
                ELSE
                  CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iRow, Checked)
                ENDIF
              ENDDO
            CASE (IDB_ShowOverlap)
              IF (Get_AutoAlign()) CALL Align
              CALL SA_STRUCTURE_OUTPUT_OVERLAP(IDD_Summary)
          END SELECT
        CASE (FieldChanged)
      END SELECT
!ep Allows you to view pdb file of SA Solutions, each clicked
! check box in fresh Mercury window.
      DO iRow = 1, NumOf_SA_Runs+1
        CALL DASHWGridGetCellCheckBox(IDF_SA_summary, 2, iRow, istatus)
        IF (istatus .EQ. 1) THEN
! Calls subroutine which opens Mercury window with .pdb file.
!         CALL SA_STRUCTURE_OUTPUT_PDB(iSol2Run(iRow), file_name)
          CALL SA_STRUCTURE_OUTPUT_NON_OVERLAP(iSol2Run(iRow), file_name)
          CALL ViewStructure(file_name, .FALSE.)
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 2, iRow, Unchecked)
        ENDIF
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE DealWithSaSummary
!
!*****************************************************************************
!
      SUBROUTINE DealWithCrystalSymmetryPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'LATTICE.INC'

      INTEGER, EXTERNAL :: SGNrMenu2Table
      LOGICAL, EXTERNAL :: Confirm, ValidCellAxisLength, NearlyEqual
      REAL    tReal
      INTEGER ISPosSG

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDAPPLY) ! The 'Apply' button
              CALL DASHWDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL Generate_TicMarks
              CALL CheckUnitCellConsistency
              CALL Profile_Plot
            CASE (IDB_Delabc)
              CALL Clear_UnitCell_WithConfirmation
          END SELECT
        CASE (FieldChanged)
! Due to the way Winteracter works, a FieldChanged is generated for 'REAL' input boxes
! only when that was the previous field to have the input focus. It doesn't necessarily mean
! that the field content has changed.
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL DASHWDialogGetReal(IDF_a_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(1))) THEN
                CellPar(1) = tReal
                CALL UpdateCell
                CALL CheckUnitCellConsistency
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_a_latt)
              ENDIF
            CASE (IDF_b_latt)
              CALL DASHWDialogGetReal(IDF_b_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(2))) THEN
                CellPar(2) = tReal
                CALL UpdateCell
                CALL CheckUnitCellConsistency
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_b_latt)
              ENDIF
            CASE (IDF_c_latt)
              CALL DASHWDialogGetReal(IDF_c_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(3))) THEN
                CellPar(3) = tReal
                CALL UpdateCell
                CALL CheckUnitCellConsistency
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_c_latt)
              ENDIF
            CASE (IDF_alp_latt)
              CALL DASHWDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_bet_latt)
              CALL DASHWDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_gam_latt)
              CALL DASHWDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell               
              CALL CheckUnitCellConsistency
            CASE (IDF_Crystal_System_Menu)
              IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
                CALL DASHWDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
                CALL Upload_CrystalSystem
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_Space_Group_Menu)  
              IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
                CALL DASHWDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
                NumberSGTable = SGNrMenu2Table(ISPosSG)
! Update the wizard
                CALL SelectDASHDialog(IDD_PW_Page1)
                CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_ZeroPoint)
              CALL DASHWDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Generate_TicMarks
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithCrystalSymmetryPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithIndexPreparation

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL    Temp

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Index_Preparation)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL)
              CALL WDialogHide
            CASE (ID_Indexing_Create)
              CALL DASHWDialogGetReal(IDF_wavelength1,Temp)
              IF (Temp .LT. 0.00001) THEN
                CALL ErrorMessage("The radiation wavelength has not been entered!")
              ELSE                 
                CALL Create_DicvolIndexFile
                CALL SelectDASHDialog(IDD_Index_Preparation)
                CALL WDialogHide
              END IF
            CASE (IDF_RunDICVOL)
              CALL RunDICVOL
          END SELECT
          CALL Profile_Plot
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_wavelength1)
              CALL DownloadWavelength(IDD_Index_Preparation)
              CALL Generate_TicMarks   
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithIndexPreparation
!
!*****************************************************************************
!
      SUBROUTINE DealWithDVResults

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'lattice.inc'

      INTEGER irow, istatus

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_DV_Results)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide()
              CALL PopActiveWindowID
              RETURN
          END SELECT
      END SELECT
      DO irow = 1, NumOfDICVOLSolutions
        CALL DASHWGridGetCellCheckBox(IDF_DV_Summary_0,1,irow,istatus)
        IF (istatus .EQ. 1) THEN
! Import the unit-cell parameters into DASH
          CellPar(1) = DICVOLSolutions(irow)%a
          CellPar(2) = DICVOLSolutions(irow)%b
          CellPar(3) = DICVOLSolutions(irow)%c
          CellPar(4) = DICVOLSolutions(irow)%alpha
          CellPar(5) = DICVOLSolutions(irow)%beta
          CellPar(6) = DICVOLSolutions(irow)%gamma
          LatBrav = DICVOLSolutions(irow)%CrystalSystem
          CALL WGridPutCellCheckBox(IDF_DV_Summary_0,1,irow,0)
          CALL Upload_CrystalSystem
          CALL UpdateCell()
          CALL PopActiveWindowID
          RETURN
        ENDIF
      ENDDO               
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDVResults
!
!*****************************************************************************
!
      SUBROUTINE RunDICVOL

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      LOGICAL, EXTERNAL :: Confirm
      REAL, EXTERNAL :: TwoTheta2dSpacing
      REAL    Rvpar(2), Lambda, Rdens, Rmolwt, Rexpzp
      INTEGER Isystem(6), UseErr, I, Iord
      INTEGER iHandle
      REAL    Epsilon
      REAL    MaxLen
      REAL    MaxSinBeta
      REAL    tBeta
      INTEGER NumDoF

      CALL CheckIfPeaksFitted
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Index_Preparation)
      CALL DASHWDialogGetReal(IDF_wavelength1, Lambda)
      CALL DASHWDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
      CALL DASHWDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
      CALL DASHWDialogGetReal(IDF_Indexing_Maxa, amax)
      CALL DASHWDialogGetReal(IDF_Indexing_Maxb, Bmax)
      CALL DASHWDialogGetReal(IDF_Indexing_Maxc, Cmax)
      CALL DASHWDialogGetReal(IDF_Indexing_MinAng, Bemin)
      CALL DASHWDialogGetReal(IDF_Indexing_MaxAng, Bemax)
      IF (Bemin .GT. Bemax) THEN
        tBeta = Bemin
        Bemin = Bemax
        Bemax = tBeta
      ENDIF
      CALL DASHWDialogGetReal       (IDF_Indexing_Density,     Rdens)
      CALL DASHWDialogGetReal       (IDF_Indexing_MolWt,       Rmolwt)
      CALL DASHWDialogGetReal       (IDF_ZeroPoint,            Rexpzp)
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Cubic,      Isystem(1))
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Tetra,      Isystem(2))
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Hexa,       Isystem(3))
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Ortho,      Isystem(4))
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Monoclinic, Isystem(5))
      CALL DASHWDialogGetCheckBox(IDF_Indexing_Triclinic,  Isystem(6))
      CALL DASHWDialogGetRadioButton(IDF_Indexing_UseErrors,   UseErr)
      CALL DASHWDialogGetReal       (IDF_eps,                  Epsilon)
      CALL DASHWDialogGetReal       (IDF_Indexing_Fom,         fom)
      CALL DASHWDialogGetReal       (IDF_Indexing_ScaleFactor, DV_ScaleFactor)
! Number of degrees of freedom, we don't even count the zero point
      NumDoF = 0
      IF (Isystem(1) .EQ. 1) NumDof = MAX(NumDoF,1)
      IF (Isystem(2) .EQ. 1) NumDof = MAX(NumDoF,2)
      IF (Isystem(3) .EQ. 1) NumDof = MAX(NumDoF,2)
      IF (Isystem(4) .EQ. 1) NumDof = MAX(NumDoF,3)
      IF (Isystem(5) .EQ. 1) NumDof = MAX(NumDoF,4)
      IF (Isystem(6) .EQ. 1) NumDof = MAX(NumDoF,6)
! Check if any crystal system checked at all
      IF (NumDoF .EQ. 0) THEN
        CALL ErrorMessage('Please check at least one crystal system.')
        GOTO 999
      ENDIF
! Check if the number of observed lines is consistent with the crystal systems
      IF (NTPeak .LT. NumDoF) THEN
        CALL ErrorMessage('The number of observed lines is less than the number of degrees of freedom.')
        GOTO 999
      ENDIF
! Warn the user if we have less observed lines than twice the number of degrees of freedom including the zero point
      IF ((2*(NumDoF+1)) .GT. NTPeak) THEN
        IF (.NOT. Confirm('The number of observed lines is less than twice the number of degrees of freedom,'//CHAR(13)//&
        'do you wish to continue anyway?')) GOTO 999
      ENDIF
      IF (DV_ScaleFactor .LT. 0.1) DV_ScaleFactor = 0.1
      IF (DV_ScaleFactor .GT. 5.0) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
      IF (Isystem(5) .EQ. 1) THEN
        IF ((Bemin .LT. 45.0) .OR. (Bemax .GT. 150.0)) THEN
          CALL ErrorMessage('The range of the angle beta does not make sense.')
          GOTO 999
        ELSE
! Correct maximum cell length for the angle beta
 ! If 90.0 is inside the range, then that's the maximum
          IF ((Bemin .LT. 90.0) .AND. (Bemax .GT. 90.0)) THEN
            MaxSinBeta = 1.0 ! Beta = 90.0
          ELSE         
            MaxSinBeta = MAX(SIN(Bemin),SIN(Bemax))
          ENDIF
        ENDIF
      ELSE
        MaxSinBeta = 1.0 ! Beta = 90.0
      ENDIF
! Add in very quick check: is the d-spacing belonging to the first peak greater
! than the maximum cell length requested? If so, tell the user he/she is a moron.
      MaxLen = MAX(MaxSinBeta*amax,Bmax)
      MaxLen = MAX(MaxLen,MaxSinBeta*Cmax)
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
      IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
        IF (.NOT. Confirm('WARNING: the maximum cell axis length is shorter than required for indexing the first peak.'//CHAR(13)// &
        'Do you wish to continue anyway?')) GOTO 999
      ENDIF
      n = NTPeak
      wave2 = (Lambda / 2) * DV_ScaleFactor
      IF (UseErr .EQ. 2) THEN
        epst = 0.0
        DO I = 1, n
          iOrd = IOrdTem(I)
          IF (AllPkPosEsd(IOrd) .LE. 0.0001) THEN
            epsil(I) = 0.001
          ELSE 
            epsil(I) = AllPkPosEsd(IOrd) * 10.0
          ENDIF
          IF (((epsil(I) + 0.015) * DV_ScaleFactor) .GT. epst) epst = ((epsil(I) + 0.015) * DV_ScaleFactor)
          epsil(I) = epsil(I) * DV_ScaleFactor
        ENDDO
      ELSE
        epst = (Epsilon + 0.015) * DV_ScaleFactor
        DO I = 1, n
          epsil(I) = Epsilon * DV_ScaleFactor
        ENDDO
      ENDIF
      DO I = 1, NTPeak
        IOrd = IOrdTem(I)
        d(I) = AllPkPosVal(IOrd) - Rexpzp
      ENDDO
      CALL WCursorShape(CurHourGlass)
      NumOfDICVOLSolutions = 0
      CALL DICVOL91(Isystem(1) .EQ. 1,Isystem(2) .EQ. 1,Isystem(3) .EQ. 1, &
                    Isystem(4) .EQ. 1,Isystem(5) .EQ. 1,Isystem(6) .EQ. 1,Rvpar(1),Rvpar(2),Rmolwt,Rdens,Rdens/50.0)
      CALL WCursorShape(CurCrossHair)
! Pop up a window showing the DICVOL output file in a text editor
      CALL WindowOpenChild(iHandle)
      CALL WEditFile(DV_FileName,Modeless,0,0,4)
      CALL SetChildWinAutoClose(iHandle)
      IF (NumOfDICVOLSolutions .EQ. 0) THEN
        CALL SelectDASHDialog(IDD_DV_Results)
        CALL WDialogHide
        CALL ErrorMessage('No solutions were found.')
        GOTO 999
      ENDIF
      IF (DICVOL_Error .EQ. cDICVOL_TooManySolutions) CALL WarningMessage('More than 30 solutions found, please check your data.')
! Pop up a window showing the solutions, so that the user can choose one to be imported into DASH
      CALL SelectDASHDialog(IDD_DV_Results)
! Clear all fields in the grid
      CALL WDialogClearField(IDF_DV_Summary_0)
! Set the number of rows in the grid to the number of solutions.
      CALL WGridRows(IDF_DV_Summary_0,NumOfDICVOLSolutions)
      DO I = 1, NumOfDICVOLSolutions
        CALL WGridPutCellString(IDF_DV_Summary_0, 2,I,CrystalSystemString(DICVOLSolutions(I)%CrystalSystem))
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 3,I,DICVOLSolutions(I)%a,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 4,I,DICVOLSolutions(I)%b,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 5,I,DICVOLSolutions(I)%c,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 6,I,DICVOLSolutions(I)%alpha,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 7,I,DICVOLSolutions(I)%beta,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 8,I,DICVOLSolutions(I)%gamma,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 9,I,DICVOLSolutions(I)%Volume,'(F9.2)')
        IF (DICVOLSolutions(I)%M .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,10,I,DICVOLSolutions(I)%M,'(F7.1)')
        IF (DICVOLSolutions(I)%F .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,11,I,DICVOLSolutions(I)%F,'(F7.1)')
      ENDDO
      CALL WDialogShow(-1, -1, 0, Modeless)
  999 CALL PopActiveWindowID

      END SUBROUTINE RunDICVOL
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOLSolution(TheCrystalSystem,The_a,The_b,The_c,The_alpha,The_beta,The_gamma,TheVolume)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem
      REAL,    INTENT (IN   ) :: The_a, The_b, The_c, The_alpha, The_beta, The_gamma, TheVolume

      NumOfDICVOLSolutions = NumOfDICVOLSolutions + 1
      IF (NumOfDICVOLSolutions .GT. MaxDICVOLSolutions) THEN
        CALL DebugErrorMessage('NumOfDICVOLSolutions > MaxDICVOLSolutions in AddDICVOLSolution.')
        NumOfDICVOLSolutions = MaxDICVOLSolutions
      ENDIF
      DICVOLSolutions(NumOfDICVOLSolutions)%CrystalSystem = TheCrystalSystem
      DICVOLSolutions(NumOfDICVOLSolutions)%a     = The_a / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%b     = The_b / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%c     = The_c / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%alpha = The_alpha
      DICVOLSolutions(NumOfDICVOLSolutions)%beta  = The_beta
      DICVOLSolutions(NumOfDICVOLSolutions)%gamma = The_gamma
      DICVOLSolutions(NumOfDICVOLSolutions)%Volume = TheVolume / (DV_ScaleFactor**3)
      DICVOLSolutions(NumOfDICVOLSolutions)%F = -1.0
      DICVOLSolutions(NumOfDICVOLSolutions)%M = -1.0
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOLSolution
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOL_F(TheF)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE DICVAR

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: TheF

      DICVOLSolutions(NumOfDICVOLSolutions)%F = TheF
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOL_F
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOL_M(TheM)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE DICVAR

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: TheM

      DICVOLSolutions(NumOfDICVOLSolutions)%M = TheM
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOL_M
!
!*****************************************************************************
!
