!
!*****************************************************************************
!
      SUBROUTINE ShowWizardWindowZmatrices
!
! The wizard window containing the z-matrices needs a lot of initialisation and
! can be called from more than one point in DASH, so here is a special routine
! to open that window.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE 
           
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'statlog.inc'

      CALL WDialogSelect(IDD_SAW_Page1)
! @ Enable or disable the "Next" button
      CALL WDialogFieldState(IDNEXT,Enabled)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      CALL SA_SetOutputFilenamesToDefaults()

      END SUBROUTINE ShowWizardWindowZmatrices
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowZmatrices

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      CHARACTER*80 SDIFile
      INTEGER      IFlags

      INCLUDE 'DialogPosCmn.inc'

! JCC Added in declarations
! The implementation has changed - this is now a function
      INTEGER, EXTERNAL :: Read_One_Zm
      INTEGER zmread
      LOGICAL ZmStateChanged
      INTEGER ifrg
      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
! Start the message loop
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
          CASE (IDBACK)
! Go back to the Pawley refinement or the initial wizard
            CALL WizardWindowHide
            CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
            CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
          CASE (IDNEXT)
! Go to the next stage of the SA input
            CALL WizardWindowHide
            PastPawley = .TRUE.
            CALL SA_Parameter_Set
            CALL WDialogSelect(IDD_SA_input2)
            CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
          CASE (IDCANCEL, IDCLOSE)
            CALL EndWizardCommon
          CASE (IDB_SA_Project_Browse)
            CALL SDIFileBrowse
          CASE (IDB_SA_Project_Open)
            CALL WDialogGetString(IDF_SA_Project_Name,SDIFile)
            CALL SDIFileOpen(SDIFile)
          CASE (IDB_SA_Project_Import)
! JCC Import .. convert a mol/pdb/mol2 file into a zmatrix
            CALL ImportZmatrix
          CASE (IDB_ZmatrixDelete1, IDB_ZmatrixDelete2, IDB_ZmatrixDelete3, IDB_ZmatrixDelete4, IDB_ZmatrixDelete5)
            IF (Confirm('Do you want to clear this z-matrix?')) THEN
              ZmStateChanged = .TRUE.
              ifrg = 1
              DO WHILE (IDBZMDelete(ifrg) .NE. EventInfo%VALUE1)
                ifrg = ifrg + 1
              ENDDO
              gotzmfile(ifrg) = .FALSE.
              frag_file(ifrg) = ' '
            ENDIF ! Delete this z-matrix
          CASE (IDB_ZMatrix_Browse1, IDB_ZMatrix_Browse2, IDB_ZMatrix_Browse3, IDB_ZMatrix_Browse4, IDB_ZMatrix_Browse5)
            ZmStateChanged = .TRUE.
            ifrg = 1
            DO WHILE (IDBZMBrowse(ifrg) .NE. EventInfo%VALUE1)
              ifrg = ifrg + 1
            ENDDO
            IFlags = PromptOn + DirChange + AppendExt
            CALL WSelectFile('z-matrix files (*.zmatrix)|*.zmatrix|',IFlags,frag_file(ifrg),'Load z-matrix file')
! JCC Need to check here to see if the user hit cancel
! So I added a check here
! Did the user press cancel?
            IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) GOTO 999
! I don't think the following answer is allowed by Winteracter
            IF (LEN_TRIM(frag_file(ifrg)) .EQ. 0) THEN
              gotzmfile(ifrg) = .FALSE.
              frag_file(ifrg) = ' '
              GOTO 999
            ENDIF
            zmread = Read_One_ZM(ifrg)
            IF (zmread .EQ. 0) THEN ! successful read
              gotzmfile(ifrg) = .TRUE.
! JCC traps for zmatrix reading
            ELSE 
              gotzmfile(ifrg) = .FALSE. 
              CALL FileErrorPopup(frag_file(ifrg),zmread)
              frag_file(ifrg) = ' '
            ENDIF ! If the read on the zmatrix was ok
! View individual z-matrices in e.g. Mercury
          CASE (IDB_ZMatrixView1, IDB_ZMatrixView2, IDB_ZMatrixView3, IDB_ZMatrixView4, IDB_ZMatrixView5)
            ifrg = 1
            DO WHILE (IDBZMView(ifrg) .NE. EventInfo%VALUE1)
              ifrg = ifrg + 1
            ENDDO
            IF (.NOT. gotzmfile(ifrg)) THEN
              CALL ErrorMessage('File not found.')
            ELSE
              CALL ViewZmatrix(ifrg)
            ENDIF
! View individual z-matrices in e.g. Mercury
          CASE (IDB_ZMatrixEdit1, IDB_ZMatrixEdit2, IDB_ZMatrixEdit3, IDB_ZMatrixEdit4, IDB_ZMatrixEdit5)
            ifrg = 1
            DO WHILE (IDBZMEdit(ifrg) .NE. EventInfo%VALUE1)
              ifrg = ifrg + 1
            ENDDO
            IF (.NOT. gotzmfile(ifrg)) THEN
              CALL ErrorMessage('File not found.')
            ELSE
              CALL WDialogSelect(IDD_EditZMatrix)
              CALL WDialogShow(-1,-1,0,Modal)
            ENDIF
        END SELECT
      END SELECT
  999 CALL UpdateZmatrixSelection
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowZmatrices
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowParameterBounds

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INCLUDE 'DialogPosCmn.inc'

      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      INTEGER MVAR
      PARAMETER (mvar=100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      DOUBLE PRECISION prevub, prevlb ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)
      COMMON /presetr/ xpreset(mvar)
      LOGICAL log_preset
      COMMON /presetl/ log_preset

      LOGICAL LimsChanged
      DATA LimsChanged / .FALSE. /
      SAVE LimsChanged
      LOGICAL Confirm ! Function
      REAL T0, RT, xtem
      INTEGER JPOS, NMOVES, IFCOl, IFRow, ICHK
      REAL         rpos
      INTEGER      ipos

! We are now on window number 2
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input2)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardCommon
              RETURN
            CASE (IDBACK)
! Go back to the 1st window
! JCC Check if the limits have changed and warn about it 
              IF (LimsChanged) THEN
                IF (Confirm("Note: Going back will erase the edits made to the current parameters, overwrite changes?")) LimsChanged = .FALSE.
              ENDIF
              IF (.NOT. LimsChanged) THEN
                CALL WizardWindowHide
                CALL WDialogSelect(IDD_SAW_Page1)
                CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
              ENDIF
            CASE (IDNEXT)
! Go to the next stage of the SA input
              CALL WizardWindowHide
              CALL WDialogSelect(IDD_SA_input3)
              T0 = 0.0
              RPOS = T0
              CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
              IPOS = 1000 - NINT(RPOS)
              CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
              RT = 0.02
              RPOS = RT
              CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(F6.3)')
              IPOS = 501 - NINT(1000.*RPOS)
              CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
              JPOS = 20
              CALL WDialogPutInteger(IDF_SA_NS,JPOS)
              IPOS = 101 - JPOS
              NS = JPOS
              CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
              JPOS = 25
              CALL WDialogPutInteger(IDF_SA_NT,JPOS)
              IPOS = 101 - JPOS
              NT = JPOS
              CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
              NMoves = NT * NS * NVAR
              CALL WDialogPutInteger(IDF_SA_Moves,NMoves)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_parameter_grid)
              CALL WGridPos(EventInfo%X,IFCol,IFRow)
              SELECT CASE (IFCol)
                CASE (1) ! parameter
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MAX(SNGL(lb(IFRow)),xtem)
                    IF (ABS(xtem - x(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    X(IFRow)=DBLE(MIN(SNGL(ub(IFRow)),xtem))
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,sngl(x(IFRow)),'(F12.5)')
                  ENDIF
                CASE (2) ! lower bound
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MIN(SNGL(ub(IFRow)),xtem)
                    IF (ABS(xtem - lb(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    lb(IFRow) = DBLE(xtem)
                    prevlb(IFRow) = lb(IFRow)
                    CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                    xtem = MAX(lb(IFRow),x(IFRow))
                    X(IFRow) = DBLE(xtem)
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,SNGL(x(IFRow)),'(F12.5)')
                  ENDIF
                CASE (3) ! upper bound
! JCC Check the bounding - only update if parameter is set to vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,IFCol,IFRow,xtem)
                    xtem = MAX(SNGL(lb(IFRow)),xtem)
                    IF (ABS(xtem - ub(IFRow)) .GT. 0.000001) LimsChanged = .TRUE.
                    ub(IFRow) = DBLE(xtem)
                    prevub(IFRow) = ub(IFRow)
                    CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                    xtem = MIN(ub(IFRow),x(IFRow))
                    X(IFRow) = DBLE(xtem)
                    CALL WGridPutCellReal(IDF_parameter_grid,1,IFRow,SNGL(x(IFRow)),'(F12.5)')
                  ENDIF
                CASE (4) ! fix or vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid,IFCol,IFRow,ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid,1,IFRow,xtem)
                    lb(IFRow) = DBLE(xtem-1.0D-5)
                    ub(IFRow) = DBLE(xtem+1.0D-5)
                    CALL WGridStateCell(IDF_parameter_grid,1,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid,2,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid,3,IFRow,DialogReadOnly)
                  ELSE
                    lb(IFRow) = prevlb(IFRow)
                    ub(IFRow) = prevub(IFRow)
                    CALL WGridStateCell(IDF_parameter_grid,1,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid,2,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid,3,IFRow,Enabled)
                  ENDIF
                  CALL WGridPutCellReal(IDF_parameter_grid,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                  CALL WGridPutCellReal(IDF_parameter_grid,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                  LimsChanged = .TRUE.
              END SELECT ! IFCol
          END SELECT ! EventInfo%Value1 Field Changed Options
      END SELECT  ! EventType
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowParameterBounds
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowSASettings

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INCLUDE 'DialogPosCmn.inc'
      
      DOUBLE PRECISION T0, rt
      COMMON /saparl/  T0, rt
      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      INTEGER         JMyExit
      COMMON /MyExit/ JMyExit

      INTEGER IHANDLE, JPOS, KPOS
      REAL         rpos
      INTEGER      ipos
      INTEGER, EXTERNAL :: WriteSAParametersToFile

! We are now on window number 3
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardCommon
            CASE (IDBACK)
! Go back to the 2nd window
              CALL WizardWindowHide
              CALL WDialogSelect(IDD_SA_input2)
              CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
            CASE (IDF_PrintSA)
              IF (WriteSAParametersToFile('SA_PARAMS.TXT') .EQ. 0) THEN
                CALL WindowOpenChild(IHANDLE)
                CALL WEditFile('SA_PARAMS.TXT',Modeless,0,FileMustExist,4)
! Note that the implementation of this editor child window is different from those used for
! the z-matrix and the DICVOL results. This editor window is not 'ViewOnly', which has three consequences:
! 1. The file can be edited. The user can add a titel, for instance.
! 2. The file can be saved using the 'Save as...' option from the menu.
! 3. The file cannot be accessed by DASH while it is being viewed by the user. This
!    means that the user can press the 'Print' button only once (or he must close the editor window).
! This shouldn't be a problem as it is not too likely that someone wants to compare
! two 'Print' outputs on screen. The possibility of editing the file is probably more useful.
                CALL SetChildWinAutoClose(IHANDLE)
              ENDIF
            CASE (IDB_SA3_finish) ! 'Solve >' button
! We've finished the SA input
              CALL WizardWindowHide
              CALL MakRHm()
              CALL CalCosArx()
              CALL Create_AtomicWeightings
              CALL BeginSA(JMyExit)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE(IDF_SA_T0_trackbar)
              IF (EventInfo%VALUE2 .EQ. IDF_SA_T0_trackbar) THEN
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetTrackBar(IDF_SA_T0_trackbar,IPOS)
                RPOS = 1000 - IPOS
                T0 = RPOS
                CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
              ENDIF
            CASE (IDF_SA_T0) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetReal(IDF_SA_T0,RPOS)
              T0 = RPOS
              IPOS=1000 - NINT(RPOS)
              CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
            CASE (IDF_SA_Tredrate_trackbar)
              IF (EventInfo%VALUE2 .EQ. IDF_SA_Tredrate_trackbar) THEN
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetTrackBar(IDF_SA_Tredrate_trackbar,IPOS)
                RPOS = 0.001 * (501.-FLOAT(IPOS))
                RT = RPOS
                CALL WDialogPutReal(IDF_SA_Tredrate,RPOS,'(F6.3)')
              ENDIF
            CASE (IDF_SA_Tredrate) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetReal(IDF_SA_Tredrate,RPOS)
              RT = RPOS
              IPOS = 501 - NINT(1000.0 * RPOS)
              CALL WDialogPutTrackbar(IDF_SA_Tredrate_trackbar,IPOS)
            CASE (IDF_SA_NS_trackbar)
              IF (EventInfo%VALUE2 .EQ. IDF_SA_NS_trackbar) THEN
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetTrackBar(IDF_SA_NS_trackbar,IPOS)
                JPOS = 101 - IPOS
                CALL WDialogPutInteger(IDF_SA_NS,JPOS)
                NS = JPOS
                KPOS = NS * NT * NVAR
                CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
              ENDIF
            CASE (IDF_SA_NS) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetInteger(IDF_SA_NS,JPOS)
              IPOS =101 - JPOS
              NS = JPOS
              CALL WDialogPutTrackbar(IDF_SA_NS_trackbar,IPOS)
              KPOS = NS * NT * NVAR
              CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
            CASE (IDF_SA_NT_trackbar)
              IF (EventInfo%VALUE2 .EQ. IDF_SA_NT_trackbar) THEN
                CALL WDialogSelect(IDD_SA_input3)
                CALL WDialogGetTrackBar(IDF_SA_NT_trackbar,IPOS)
                JPOS = 101 - IPOS
                CALL WDialogPutInteger(IDF_SA_NT,JPOS)
                NT = JPOS
                KPOS = NS * NT * NVAR
                CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
              ENDIF
            CASE (IDF_SA_NT) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetInteger(IDF_SA_NT,JPOS)
              IPOS = 101 - JPOS
              NT = JPOS
              CALL WDialogPutTrackbar(IDF_SA_NT_trackbar,IPOS)
              KPOS = NS * NT * NVAR
              CALL WDialogPutInteger(IDF_SA_Moves,KPOS)
            CASE (IDF_SA_RandomSeed1) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetInteger(IDF_SA_RandomSeed1,ISeed1)
            CASE (IDF_SA_RandomSeed2) 
              CALL WDialogSelect(IDD_SA_input3)
              CALL WDialogGetInteger(IDF_SA_RandomSeed2,ISeed2)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSASettings
!
!*****************************************************************************
!
