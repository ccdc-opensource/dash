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

! @@ Enable or disable the "Next" button, only partially taken care of in UpdateZmatrixSelection
      CALL UpdateZmatrixSelection
      CALL WizardWindowShow(IDD_SAW_Page1)
      PastPawley = .TRUE.
! Grey out 'Delete all peak fit ranges' button on toolbar
      CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOff)
! Grey out 'Remove Background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background,ItemEnabled,WintOff)
! Grey out 'Load diffraction pattern' button on toolbar
      CALL WMenuSetState(ID_import_xye_file,ItemEnabled,WintOff)
! Make unit cell etc. read only under 'View' 
      CALL Upload_Cell_Constants
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogFieldState(IDF_Space_Group_Menu,DialogReadOnly)
      CALL WDialogFieldState(IDF_Crystal_System_Menu,DialogReadOnly)
      CALL WDialogFieldState(IDF_ZeroPoint,DialogReadOnly)
      CALL WDialogFieldState(IDAPPLY,DialogReadOnly)
      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogFieldState(IDAPPLY,DialogReadOnly)
      CALL WDialogFieldState(IDF_wavelength1,DialogReadOnly)
      CALL WDialogFieldState(IDF_Wavelength_Menu,DialogReadOnly)
      CALL WDialogFieldState(IDF_LabX_Source,DialogReadOnly)
      CALL WDialogFieldState(IDF_SynX_Source,DialogReadOnly)
      CALL WDialogFieldState(IDF_CWN_Source,DialogReadOnly)
      CALL WDialogFieldState(IDF_TOF_source,DialogReadOnly)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogFieldState(ID_Index_Output,DialogReadOnly)
! Grey out 'Remove background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background,ItemEnabled,WintOff)
      CALL SetModeMenuState(-1,-1)
      CALL SelectMode(ID_Structure_Solution_Mode)
      CALL Profile_Plot

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

      CHARACTER(LEN=MaxPathLength) SDIFile
      INTEGER      IFlags

      INTEGER, EXTERNAL :: Read_One_Zm
      INTEGER zmread
      INTEGER ifrg
      LOGICAL, EXTERNAL :: Confirm
      CHARACTER(LEN=MaxPathLength) DirName
      CHARACTER*80 FileName

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the Pawley refinement or the initial wizard
              CALL EndWizardPastPawley
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT)
! Go to the next stage of the SA input
! Grey out 'Load DASH Pawley file' button on toolbar
              CALL WMenuSetState(ID_import_dpj_file,ItemEnabled,WintOff)
! Initialise the 'Additional SA Parameters' dialogue
              CALL WDialogSelect(IDD_SAW_Page2)
! Set PO checkbox to 'Do not use preferred orientation'
              CALL WDialogPutCheckBoxLogical(IDF_Use_PO,.FALSE.)
              CALL WDialogFieldState(IDF_PO_a,Disabled)
              CALL WDialogFieldState(IDF_PO_b,Disabled)
              CALL WDialogFieldState(IDF_PO_c,Disabled)
              CALL WDialogFieldState(IDF_LABELa,Disabled)
              CALL WDialogFieldState(IDF_LABELb,Disabled)
              CALL WDialogFieldState(IDF_LABELc,Disabled)
              DO iFrg = 1, maxfrg
                IF (gotzmfile(iFrg)) THEN
! Get the number of copies to use. If zero, set gotzmfile to .FALSE.
                  CALL WDialogSelect(IDD_SAW_Page1)
                  CALL WDialogGetInteger(IDFzmNumber(iFrg),zmNumberOfCopies(iFrg))
                  IF (zmNumberOfCopies(iFrg) .EQ. 0) gotzmfile(iFrg) = .FALSE.
                  CALL WDialogSelect(IDD_SAW_Page2)
! Its label
                  CALL SplitPath(frag_file(iFrg),DirName,FileName)
                  CALL WGridLabelRow(IDF_RotationsGrid,iFrg,FileName)
! Initialise "None" | "Quaternions" | "Single axis" menu to quaternions
                  CALL WGridPutCellOption(IDF_RotationsGrid,1,iFrg,2)
! Ungrey all "a or alpha" fields
                  CALL WGridStateCell(IDF_RotationsGrid,2,iFrg,Enabled)
                  CALL WGridStateCell(IDF_RotationsGrid,3,iFrg,Enabled)
                  CALL WGridStateCell(IDF_RotationsGrid,4,iFrg,Enabled)
                ELSE
! Its label
                  CALL WGridLabelRow(IDF_RotationsGrid,iFrg,'')
! Initialise "None" | "Quaternions" | "Single axis" menu to none
                  CALL WGridPutCellOption(IDF_RotationsGrid,1,iFrg,1)
! Grey out all "a or alpha" fields
                  CALL WGridStateCell(IDF_RotationsGrid,2,iFrg,DialogReadOnly)
                  CALL WGridStateCell(IDF_RotationsGrid,3,iFrg,DialogReadOnly)
                  CALL WGridStateCell(IDF_RotationsGrid,4,iFrg,DialogReadOnly)
                ENDIF
              ENDDO
              CALL WizardWindowShow(IDD_SAW_Page2)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDB_SA_Project_Browse)
              CALL SDIFileBrowse
            CASE (IDB_SA_Project_Open)
              CALL WDialogGetString(IDF_SA_Project_Name,SDIFile)
              CALL SDIFileOpen(SDIFile)
            CASE (IDB_SA_Project_Import)
! JCC Import .. convert a mol/pdb/mol2 file into a Z-matrix
              CALL ImportZmatrix
            CASE (IDB_zmDelete1, IDB_zmDelete2, IDB_zmDelete3, IDB_zmDelete4)
              IF (Confirm('Do you want to clear this Z-matrix?')) THEN
                iFrg = 1
                DO WHILE (IDBZMDelete(ifrg) .NE. EventInfo%VALUE1)
                  iFrg = iFrg + 1
                ENDDO
                gotzmfile(iFrg) = .FALSE.
              ENDIF ! Delete this Z-matrix
            CASE (IDB_zmBrowse1, IDB_zmBrowse2, IDB_zmBrowse3, IDB_zmBrowse4)
              iFrg = 1
              DO WHILE (IDBZMBrowse(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              IFlags = PromptOn + DirChange + AppendExt
              CALL WSelectFile('Z-matrix files (*.zmatrix)|*.zmatrix|',IFlags,frag_file(iFrg),'Load Z-matrix file')
! Did the user press cancel?
              IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) GOTO 999
! I don't think the following answer is allowed by Winteracter
              IF (LEN_TRIM(frag_file(iFrg)) .EQ. 0) THEN
                gotzmfile(iFrg) = .FALSE.
                GOTO 999
              ENDIF
              zmread = Read_One_ZM(iFrg)
              IF (zmread .EQ. 0) THEN ! successful read
                gotzmfile(iFrg) = .TRUE.
! Initialise 'Number of' field to 1
                CALL WDialogPutInteger(IDFzmNumber(iFrg),1)
! JCC traps for Z-matrix reading
              ELSE 
                gotzmfile(iFrg) = .FALSE. 
                CALL FileErrorPopup(frag_file(iFrg),zmread)
              ENDIF ! If the read on the Z-matrix was ok
! View individual Z-matrices in e.g. Mercury
            CASE (IDB_zmView1, IDB_zmView2, IDB_zmView3, IDB_zmView4)
              iFrg = 1
              DO WHILE (IDBZMView(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              CALL ViewZmatrix(iFrg)
! View individual Z-matrices in e.g. Mercury
            CASE (IDB_zmEdit1, IDB_zmEdit2, IDB_zmEdit3, IDB_zmEdit4)
              iFrg = 1
              DO WHILE (IDBzmEdit(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              CALL ShowEditZMatrixWindow(iFrg)
          END SELECT
      END SELECT
  999 CALL UpdateZmatrixSelection
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowZmatrices
!
!*****************************************************************************
!
      SUBROUTINE ShowEditZMatrixWindow(iFrg)

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      CALL WDialogSelect(IDD_zmEdit)
      CurrentlyEditedFrag = iFrg
! Make temporary copy
      CALL zmCopy(iFrg,0)
      CALL zmCopyTemp2Dialog
      CALL WDialogPutRadioButton(IDF_OrderOrig)
      CurrentOrderOption = 1
      CALL WDialogFieldState(IDF_DelLatAtm,Disabled)
      CALL WDialogShow(-1,-1,0,SemiModeLess)

      END SUBROUTINE ShowEditZMatrixWindow
!
!*****************************************************************************
!
      SUBROUTINE zmCopy(iFrg1,iFrg2)

! Copies Z-matrix 1 to Z-matrix 2

      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg1, iFrg2

      INTEGER iAtomNr, iBondNr

      frag_file(iFrg2) = frag_file(iFrg1)
      icomflg(iFrg2)   = icomflg(iFrg1)
      natoms(iFrg2)    = natoms(iFrg1)
      DO iAtomNr = 1, natoms(iFrg1)
        ioptb(iAtomNr,iFrg2)         = ioptb(iAtomNr,iFrg1)
        iopta(iAtomNr,iFrg2)         = iopta(iAtomNr,iFrg1)
        ioptt(iAtomNr,iFrg2)         = ioptt(iAtomNr,iFrg1)
        iz1(iAtomNr,iFrg2)           = iz1(iAtomNr,iFrg1)
        iz2(iAtomNr,iFrg2)           = iz2(iAtomNr,iFrg1)
        iz3(iAtomNr,iFrg2)           = iz3(iAtomNr,iFrg1)
        blen(iAtomNr,iFrg2)          = blen(iAtomNr,iFrg1)
        alph(iAtomNr,iFrg2)          = alph(iAtomNr,iFrg1)
        bet(iAtomNr,iFrg2)           = bet(iAtomNr,iFrg1)
        asym(iAtomNr,iFrg2)          = asym(iAtomNr,iFrg1)
        OriginalLabel(iAtomNr,iFrg2) = OriginalLabel(iAtomNr,iFrg1)
        tiso(iAtomNr,iFrg2)          = tiso(iAtomNr,iFrg1)
        occ(iAtomNr,iFrg2)           = occ(iAtomNr,iFrg1)
        izmoid(iAtomNr,iFrg2)        = izmoid(iAtomNr,iFrg1)
        izmbid(iAtomNr,iFrg2)        = izmbid(iAtomNr,iFrg1)
      ENDDO
      NumberOfBonds(iFrg2) = NumberOfBonds(iFrg1)
      IF (NumberOfBonds(iFrg1) .GT. 0) THEN
        DO iBondNr = 1, NumberOfBonds(iFrg1)
          BondType(iBondNr,iFrg2) = BondType(iBondNr,iFrg1)
          Bonds(1,iBondNr,iFrg2)  = Bonds(1,iBondNr,iFrg1)
          Bonds(2,iBondNr,iFrg2)  = Bonds(2,iBondNr,iFrg1)
        ENDDO
      ENDIF

      END SUBROUTINE zmCopy
!
!*****************************************************************************
!
      SUBROUTINE DealWithEditZMatrixWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER iFrg, iOption, iColumn, iAtomNr, iDummy, iBondNr
      REAL    tReal
      LOGICAL ThisOne
      INTEGER, EXTERNAL :: zmSaveAs

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Set)
              CALL WDialogGetMenu(IDF_UisoOccMenu,iOption)
              SELECT CASE (iOption)
                CASE (1)
                  iColumn = 3 ! Uiso
                CASE (2)
                  iColumn = 4 ! Occupancies
              END SELECT
              CALL WDialogGetReal(IDF_UisoOccValue,tReal)
! Check its value
              
              CALL WDialogGetMenu(IDF_WhichAtomMenu,iOption)
              DO iAtomNr = 1, natoms(iFrg)
! Uiso / occupancies
                SELECT CASE (iOption)
                  CASE (1) ! All atoms
                    ThisOne = .TRUE.
                  CASE (2) ! All non-hydrogens
                    ThisOne = (asym(izmbid(iAtomNr,iFrg),iFrg) .NE. 'H  ')
                  CASE (3) ! All hydrogens
                    ThisOne = (asym(izmbid(iAtomNr,iFrg),iFrg) .EQ. 'H  ')
                END SELECT
                IF (ThisOne) CALL WGridPutCellReal(IDF_AtomPropGrid,iColumn,iAtomNr,tReal,'(F5.3)')
              ENDDO
              CALL zmCopyDialog2Temp
            CASE (IDB_Relabel)
              CALL zmCopyDialog2Temp
              CALL zmRelabel(0)
              CALL zmCopyTemp2Dialog
            CASE (IDB_ReOrder)
              CALL zmCopyDialog2Temp
              CALL zmReOrder(0)
              CALL zmCopyTemp2Dialog
              CALL WDialogPutRadioButton(IDF_OrderOrig)
              CALL WDialogFieldState(IDF_DelLatAtm,Disabled)
            CASE (IDOK)
              CALL zmCopyDialog2Temp
              CALL zmCopy(0,CurrentlyEditedFrag)
              CALL WDialogHide()
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide()
            CASE (IDBSAVE)
              CALL zmCopyDialog2Temp
              CALL zmSave(iFrg)
            CASE (IDB_SaveAs)
              CALL zmCopyDialog2Temp
              iDummy = zmSaveAs(0)
            CASE (IDF_DelLatAtm)
! Remove any bonds this atom was involved in
              IF (NumberOfBonds(iFrg) .GT. 0) THEN
                DO iBondNr = 1, NumberOfBonds(iFrg)
                  IF ((Bonds(1,iBondNr,iFrg) .EQ. natoms(iFrg)) .OR. (Bonds(2,iBondNr,iFrg) .EQ. natoms(iFrg))) THEN
! If this is not the last bond in the list, then replace it with the last bond in the list
                    IF (iBondNr .LT. NumberOfBonds(iFrg)) THEN                   
                      BondType(iBondNr,iFrg) = BondType(NumberOfBonds(iFrg),iFrg)
                      Bonds(1,iBondNr,iFrg)  = Bonds(1,NumberOfBonds(iFrg),iFrg)
                      Bonds(2,iBondNr,iFrg)  = Bonds(2,NumberOfBonds(iFrg),iFrg)
                    ENDIF
                    NumberOfBonds(iFrg) = NumberOfBonds(iFrg) - 1
                  ENDIF
                ENDDO
              ENDIF
! If this was the pivot atom, use centre of mass
              IF (icomflg(iFrg) .EQ. natoms(iFrg)) icomflg(iFrg) = 0 
              IF (natoms(iFrg) .NE. 0) THEN
                CALL WGridSetCell(IDF_AtomPropGrid,1,1)
                natoms(iFrg) = natoms(iFrg) - 1
              ENDIF
              IF (natoms(iFrg) .NE. 0) THEN
! The last atom in the Z-matrix corresponds to a random atom in the original ordering.
! It is now very likely that we have ended up with one of the atoms having an 'orignal ID' that
! is greater than the current number of atoms. This would give boundary overflows.
! We can retain the original order (minus one atom) but that involves subtracting 1
! from atom IDs following the one we have deleted
                DO iAtomNr = 1, natoms(iFrg)
                  IF (izmoid(iAtomNr,iFrg) .GT. izmoid(natoms(iFrg)+1,iFrg)) izmoid(iAtomNr,iFrg) = izmoid(iAtomNr,iFrg) - 1
                  izmbid(izmoid(iAtomNr,iFrg),iFrg) = iAtomNr ! the backward mapping
                ENDDO
              ENDIF
              CALL zmCopyTemp2Dialog
            CASE (IDB_View)
              CALL zmCopyDialog2Temp
              CALL ViewZmatrix(0)
            CASE (IDB_Rotations)
              CALL WDialogSelect(IDD_zmEditRotations)
              CALL WDialogShow(-1,-1,0,SemiModeLess)
          END SELECT
        CASE (FieldChanged)
          CALL WDialogGetRadioButton(IDF_OrderOrig,iOption)
          IF (iOption .NE. CurrentOrderOption) THEN
            SELECT CASE (iOption)
              CASE (1) ! Original
                CurrentOrderOption = 1
                CALL WDialogFieldState(IDF_DelLatAtm,Disabled)
              CASE (2) ! Z-matrix
                CurrentOrderOption = 2
                IF (natoms(iFrg) .NE. 0) CALL WDialogFieldState(IDF_DelLatAtm,Enabled)
            END SELECT
            CALL zmCopyTemp2Dialog
          ENDIF
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithEditZMatrixWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithEditZMatrixRotationsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER iFrg

      iFrg = CurrentlyEditedFrag
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK, IDCANCEL)
              CALL WDialogHide()
          END SELECT
        CASE (FieldChanged)
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithEditZMatrixRotationsWindow
!
!*****************************************************************************
!
      SUBROUTINE zmCopyTemp2Dialog

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE 
      
      CHARACTER(3) RowLabelStr
      INTEGER iFrg, iAtomNr, iOrig     

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
! filename
      CALL WDialogPutString(IDF_FileName,frag_file(iFrg))
! Fill grid with atom properties
! Set number of rows
      CALL WGridRows(IDF_AtomPropGrid,natoms(iFrg))
      DO iAtomNr = 1, natoms(iFrg)
        IF (CurrentOrderOption .EQ. 2) THEN
          iOrig = iAtomNr
        ELSE
          iOrig = izmbid(iAtomNr,iFrg)
        ENDIF
! Show the number of the atom in the zeroth column
        WRITE(RowLabelStr,'(I3)') iAtomNr
        CALL WGridLabelRow(IDF_AtomPropGrid,iAtomNr,RowLabelStr)
! atom labels
        CALL WGridPutCellString(IDF_AtomPropGrid,1,iAtomNr,OriginalLabel(iOrig,iFrg))
! atom elements
        CALL WGridPutCellString(IDF_AtomPropGrid,2,iAtomNr,asym(iOrig,iFrg))
! Uiso
        CALL WGridPutCellReal(IDF_AtomPropGrid,3,iAtomNr,tiso(iOrig,iFrg),'(F5.3)')
! occupancies
        CALL WGridPutCellReal(IDF_AtomPropGrid,4,iAtomNr,occ(iOrig,iFrg),'(F5.3)')
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE zmCopyTemp2Dialog
!
!*****************************************************************************
!
      SUBROUTINE zmCopyDialog2Temp

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE 
      
      INTEGER iFrg, iAtomNr, iOrig     

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
! filename
      CALL WDialogGetString(IDF_FileName,frag_file(iFrg))
! Fill grid with atom properties
!U! Set number of rows
!U      CALL WGridRows(IDF_AtomPropGrid,natoms(iFrg))
      DO iAtomNr = 1, natoms(iFrg)
        IF (CurrentOrderOption .EQ. 2) THEN
          iOrig = iAtomNr
        ELSE
          iOrig = izmbid(iAtomNr,iFrg)
        ENDIF
! atom labels
        CALL WGridGetCellString(IDF_AtomPropGrid,1,iAtomNr,OriginalLabel(iOrig,iFrg))
! atom elements
        CALL WGridGetCellString(IDF_AtomPropGrid,2,iAtomNr,asym(iOrig,iFrg))
! Uiso
        CALL WGridGetCellReal(IDF_AtomPropGrid,3,iAtomNr,tiso(iOrig,iFrg))
! occupancies
        CALL WGridGetCellReal(IDF_AtomPropGrid,4,iAtomNr,occ(iOrig,iFrg))
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE zmCopyDialog2Temp
!
!*****************************************************************************
!
      SUBROUTINE zmRelabel(iFrg)

! This routine re-labels atoms in Z-matrix number iFrg
! The new label consists of element + sequential number per element

      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

!      INTEGER LastNumberSoFar(1:109) ! 1 for each element
      INTEGER iAtomNr, iLen, iOrig
      CHARACTER*3 LastNumberSoFarStr

      DO iAtomNr = 1, natoms(iFrg)
        IF (CurrentOrderOption .EQ. 2) THEN
          iOrig = iAtomNr
        ELSE
          iOrig = izmoid(iAtomNr,iFrg)
        ENDIF
        WRITE(LastNumberSoFarStr,'(I3)') iOrig
! Left-justify this string: " 12" => "12 "
        CALL StrClean(LastNumberSoFarStr,iLen)
        OriginalLabel(iAtomNr,iFrg) = asym(iAtomNr,iFrg)(1:LEN_TRIM(asym(iAtomNr,iFrg)))// &
                                      LastNumberSoFarStr(1:LEN_TRIM(LastNumberSoFarStr))
      ENDDO

      END SUBROUTINE zmRelabel
!
!*****************************************************************************
!
      SUBROUTINE zmReOrder(iFrg)

! This routine re-orders atoms in the temporary Z-matrix (iFrg = 0)
! The new order is: Carbons first, rest in alphabetical order, Hydrogens last
! Extremely simple implementation: keep swapping two entries until no more swaps

      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER iAtomNr
      INTEGER iTem
      LOGICAL Swaps, ShouldBeSwapped

      Swaps = .TRUE. ! Stupid initialisation to emulate 'REPEAT ... UNTIL' in restricted FORTRAN syntax.
      DO WHILE (Swaps)
        Swaps = .FALSE.
        DO iAtomNr = 1, natoms(iFrg)-1
! Compare entry to next
! Never swap if same
          IF ((asym(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. asym(izmbid(iAtomNr+1,iFrg),iFrg))) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, never swap if first is Carbon or second is Hydrogen
          ELSE IF ((asym(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. 'C  ') .OR. (asym(izmbid(iAtomNr+1,iFrg),iFrg) .EQ. 'H  ')) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, always swap if second is Carbon or first is Hydrogen
          ELSE IF ((asym(izmbid(iAtomNr+1,iFrg),iFrg) .EQ. 'C  ') .OR. (asym(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. 'H  ')) THEN
            ShouldBeSwapped = .TRUE.
! Otherwise, swap if first > second
          ELSE IF ((asym(izmbid(iAtomNr  ,iFrg),iFrg) .GT. asym(izmbid(iAtomNr+1,iFrg),iFrg))) THEN
            ShouldBeSwapped = .TRUE.
          ELSE
            ShouldBeSwapped = .FALSE.
          ENDIF
          IF (ShouldBeSwapped) THEN
            iTem                   = izmbid(iAtomNr  ,iFrg)
            izmbid(iAtomNr  ,iFrg) = izmbid(iAtomNr+1,iFrg)
            izmbid(iAtomNr+1,iFrg) = iTem
            Swaps = .TRUE.
          ENDIF
        ENDDO
      ENDDO
      DO iAtomNr = 1, natoms(iFrg)
        izmoid(izmbid(iAtomNr,iFrg),iFrg) = iAtomNr ! the backward mapping
      ENDDO

      END SUBROUTINE zmReOrder
!
!*****************************************************************************
!
      INTEGER FUNCTION zmSaveAs(iFrg)

! This routine saves Z-matrix number iFrg

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      CHARACTER(MaxPathLength) :: zmFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFLAGS
      INTEGER, EXTERNAL :: zmSave
      
! Save the Z-matrix
      zmSaveAs = 1 ! Failure
      iFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Z-matrix files (*.zmatrix)|*.zmatrix|'
      zmFileName = frag_file(iFrg)
      CALL WSelectFile(FILTER,iFLAGS,zmFileName,'Save Z-matrix')
      IF ((WinfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(zmFileName) .NE. 0)) THEN
        frag_file(iFrg) = zmFileName
        zmSaveAs = zmSave(iFrg)
      ENDIF

      END FUNCTION zmSaveAs
!
!*****************************************************************************
!
      INTEGER FUNCTION zmSave(iFrg)

! This routine saves Z-matrix number iFrg

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER tFileHandle, i
      
! Save the Z-matrix
      zmSave = 1 ! Failure
      tFileHandle = 19
      OPEN (UNIT=tFileHandle,FILE=frag_file(iFrg),ERR=999)
! First line is a title
      WRITE (tFileHandle,'(A)',ERR=999) 'Z-matrix generated by '//ProgramVersion
! Second line contains unit cell parameters
!1.0 1.0 1.0 90.0 90.0 90.0
! These are never used, so just write dummy values.
      WRITE (tFileHandle,'(A)',ERR=999) '1.0 1.0 1.0 90.0 90.0 90.0'
! Third line contains number of atoms and an integer IAT, defining the centre for the rotations.
! IAT = 0        : use centre of mass
! IAT = non-zero : use atom nr. IAT   (necessary if atom on special position).
!  59   0
      WRITE (tFileHandle,'(1X,I3,1X,I3)',ERR=999) natoms(iFrg), icomflg(iFrg)
! Remaining lines contain the Z-matrix
!  C      1.5152617  0  113.2370014  0 -179.8250018  0   54   51   48  3.0  1.0   58 C6 C7 C8 C9
      DO i = 1, natoms(iFrg)
        WRITE (tFileHandle,'(2X,A3,3(F13.7,2X,I1),3I5,2F7.3,I5,1X,A5)',ERR=999)  Asym(i,iFrg), &
              blen(i,iFrg), ioptb(i,iFrg), alph(i,iFrg), iopta(i,iFrg), bet(i,iFrg), ioptt(i,iFrg), &
              iz1(i,iFrg), iz2(i,iFrg), iz3(i,iFrg),   &
              tiso(i,iFrg), occ(i,iFrg), izmoid(i,iFrg), OriginalLabel(i,iFrg)

      ENDDO
      CLOSE(tFileHandle,ERR=999)
      zmSave = 0 ! Success
      RETURN
  999 CALL ErrorMessage('Error while saving Z-matrix file.')

      END FUNCTION zmSave
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowAdditionalSAParams

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER tFieldState
      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page2)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Ungrey 'Load DASH Pawley file' button on toolbar
              CALL WMenuSetState(ID_import_dpj_file,ItemEnabled,WintOn)
              CALL WizardWindowShow(IDD_SAW_Page1)
            CASE (IDNEXT)
              CALL SA_Parameter_Set
              CALL WizardWindowShow(IDD_SA_input2)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_RotationsGrid)
            CASE (IDF_Use_PO)
              IF (WDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
              ENDIF
              CALL WDialogFieldState(IDF_PO_a,tFieldState)
              CALL WDialogFieldState(IDF_PO_b,tFieldState)
              CALL WDialogFieldState(IDF_PO_c,tFieldState)
              CALL WDialogFieldState(IDF_LABELa,tFieldState)
              CALL WDialogFieldState(IDF_LABELb,tFieldState)
              CALL WDialogFieldState(IDF_LABELc,tFieldState)
          END SELECT ! EventInfo%Value1 Field Changed Options
      END SELECT
  999 CALL UpdateZmatrixSelection
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowAdditionalSAParams
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowParameterBounds

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      INTEGER    MVAR
      PARAMETER (MVAR = 100)
      DOUBLE PRECISION x,lb,ub,vm,xpreset
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      DOUBLE PRECISION prevub, prevlb ! For saving the previous range
      COMMON /pvalues/ prevub(mvar), prevlb(mvar)
      COMMON /presetr/ xpreset(mvar)
      LOGICAL log_preset
      COMMON /presetl/ log_preset

      DOUBLE PRECISION T0, rt
      COMMON /saparl/  T0, rt

      LOGICAL LimsChanged
      DATA LimsChanged / .FALSE. /
      SAVE LimsChanged

      LOGICAL, EXTERNAL :: Confirm
      REAL    xtem
      INTEGER JPOS, NMOVES, IFCOl, IFRow, ICHK
      REAL    rpos
      INTEGER ipos, tMaxRuns, tFieldState

! We are now on window number 2
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input2)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the 1st window
! JCC Check if the limits have changed and warn about it 
              IF (LimsChanged) THEN
                IF (Confirm("Note: Going back will erase the edits made to the current parameters, overwrite changes?")) LimsChanged = .FALSE.
              ENDIF
              IF (.NOT. LimsChanged) CALL WizardWindowShow(IDD_SAW_Page2)
            CASE (IDNEXT)
! Go to the next stage of the SA input
              CALL WDialogSelect(IDD_SA_input3)
              RPOS = T0
              CALL WDialogPutReal(IDF_SA_T0,RPOS,'(F7.2)')
              IPOS = 1000 - NINT(RPOS)
              CALL WDialogPutTrackbar(IDF_SA_T0_trackbar,IPOS)
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
              CALL WDialogGetInteger(IDF_SA_MaxRepeats,tMaxRuns)
              IF (tMaxRuns .EQ. 1) THEN
                tFieldState = Disabled
              ELSE
                tFieldState = Enabled
              ENDIF
              CALL WDialogFieldState(IDF_SA_ChiTest_Label,tFieldState)
              CALL WDialogFieldState(IDF_SA_ChiTest,tFieldState)
              CALL WDialogFieldState(IDF_SA_MaxMoves_Label,tFieldState)
              CALL WDialogFieldState(IDF_MaxMoves1,tFieldState)
              CALL WDialogFieldState(IDF_LABEL21,tFieldState)
              CALL WDialogFieldState(IDF_MaxMoves2,tFieldState)
              CALL WizardWindowShow(IDD_SA_input3)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
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
      USE PO_VAR
      USE ZMVAR

      IMPLICIT NONE      

      DOUBLE PRECISION T0, rt
      COMMON /saparl/  T0, rt

      INTEGER         nvar, ns, nt, maxevl, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, maxevl, iseed1, iseed2

      INTEGER IHANDLE, JPOS, KPOS
      REAL    rpos
      INTEGER ipos
      INTEGER, EXTERNAL :: WriteSAParametersToFile
      INTEGER tMaxRuns, tFieldState

! We are now on window number 3
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the 2nd window
              CALL WizardWindowShow(IDD_SA_input2)
            CASE (IDB_SA3_finish) ! 'Solve >' button
! We've finished the SA input
              CALL WizardWindowHide
              CALL MakRHm()
              CALL CalCosArx()
              CALL Create_AtomicWeightings
              IF (PrefParExists) THEN
                CALL PO_Init
                CALL PO_PRECFC
              ENDIF
              CALL BeginSA
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDF_PrintSA)
              IF (WriteSAParametersToFile('SA_PARAMS.TXT') .EQ. 0) THEN
                CALL WindowOpenChild(IHANDLE)
                CALL WEditFile('SA_PARAMS.TXT',Modeless,0,FileMustExist,4)
! Note that the implementation of this editor child window is different from those used for
! the Z-matrix and the DICVOL results. This editor window is not 'ViewOnly', which has three consequences:
! 1. The file can be edited. The user can add a titel, for instance.
! 2. The file can be saved using the 'Save as...' option from the menu.
! 3. The file cannot be accessed by DASH while it is being viewed by the user. This
!    means that the user can press the 'Print' button only once (or he must close the editor window).
! This shouldn't be a problem as it is not too likely that someone wants to compare
! two 'Print' outputs on screen. The possibility of editing the file is probably more useful.
                CALL SetChildWinAutoClose(IHANDLE)
              ENDIF
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
              IPOS = 1000 - NINT(RPOS)
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
            CASE (IDF_SA_MaxRepeats)
              CALL WDialogGetInteger(IDF_SA_MaxRepeats,tMaxRuns)
              IF (tMaxRuns .EQ. 1) THEN
                tFieldState = Disabled
              ELSE
                tFieldState = Enabled
              ENDIF
                CALL WDialogFieldState(IDF_SA_ChiTest_Label,tFieldState)
                CALL WDialogFieldState(IDF_SA_ChiTest,tFieldState)
                CALL WDialogFieldState(IDF_SA_MaxMoves_Label,tFieldState)
                CALL WDialogFieldState(IDF_MaxMoves1,tFieldState)
                CALL WDialogFieldState(IDF_LABEL21,tFieldState)
                CALL WDialogFieldState(IDF_MaxMoves2,tFieldState)
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
