!
!*****************************************************************************
!
      SUBROUTINE ShowWizardWindowZmatrices
!
! The wizard window containing the Z-matrices needs a lot of initialisation and
! can be called from more than one point in DASH, so here is a special routine
! to open that window.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE 

      INCLUDE 'GLBVAR.INC'
      INTEGER        IDFZMFile,                                                &
                     IDBZMDelete,                    IDBZMBrowse,              &
                     IDBZMView,                      IDBZMEdit,                &
                     IDFZMpars,                      IDFZMLabel,               &
                     first_zm_in_win
      COMMON /IDFZM/ IDFZMFile(1:maxfrginterface),                                      &
                     IDBZMDelete(1:maxfrginterface), IDBZMBrowse(1:maxfrginterface),    &
                     IDBZMView(1:maxfrginterface),   IDBZMEdit(1:maxfrginterface),      &
                     IDFZMpars(1:maxfrginterface),   IDFZMLabel(1:maxfrginterface),     &
                     first_zm_in_win

! @@ Enable or disable the "Next" button, only partially taken care of in UpdateZmatrixSelection
      CALL UpdateZmatrixSelection
      CALL WizardWindowShow(IDD_SAW_Page1)
      CALL WDialogFieldStateLogical(IDB_Up, first_zm_in_win .NE. 1)
      CALL WDialogFieldStateLogical(IDB_Down, first_zm_in_win+maxfrginterface-1 .NE. maxfrg)
      PastPawley = .TRUE.
! Grey out 'Delete all peak fit ranges' button on toolbar
      CALL WMenuSetState(ID_ClearPeakFitRanges, ItemEnabled, WintOff)
! Grey out 'Fit peaks' button on toolbar
      CALL WMenuSetState(ID_FitPeaks, ItemEnabled, WintOff)
! Grey out 'Clear cell parameters' button on toolbar
      CALL WMenuSetState(ID_Delabc, ItemEnabled,WintOff)
! Grey out 'Remove Background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOff)
! Grey out 'Load diffraction pattern' button on toolbar
      CALL WMenuSetState(ID_import_xye_file, ItemEnabled, WintOff)
! Make unit cell etc. read only under 'View' 
      CALL Upload_Cell_Constants
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogFieldState(IDF_Space_Group_Menu, DialogReadOnly)
      CALL WDialogFieldState(IDF_Crystal_System_Menu, DialogReadOnly)
      CALL WDialogFieldState(IDF_ZeroPoint, DialogReadOnly)
      CALL WDialogFieldState(IDAPPLY, DialogReadOnly)
      CALL WDialogFieldState(IDB_Delabc, Disabled)
      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogFieldState(IDAPPLY, DialogReadOnly)
      CALL WDialogFieldState(IDF_wavelength1, DialogReadOnly)
      CALL WDialogFieldState(IDF_Wavelength_Menu, DialogReadOnly)
      CALL WDialogFieldState(IDF_LabX_Source, DialogReadOnly)
      CALL WDialogFieldState(IDF_SynX_Source, DialogReadOnly)
      CALL WDialogFieldState(IDF_CWN_Source, DialogReadOnly)
      CALL WDialogFieldState(IDF_TOF_source, DialogReadOnly)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogFieldState(ID_Index_Output, DialogReadOnly)
      CALL WDialogSelect(IDD_ViewPawley)
      CALL WDialogFieldState(IDF_Sigma1, DialogReadOnly)
      CALL WDialogFieldState(IDF_Sigma2, DialogReadOnly)
      CALL WDialogFieldState(IDF_Gamma1, DialogReadOnly)
      CALL WDialogFieldState(IDF_Gamma2, DialogReadOnly)
      CALL WDialogFieldState(IDF_HPSL, DialogReadOnly)
      CALL WDialogFieldState(IDF_HMSL, DialogReadOnly)
! Grey out 'Remove background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOff)
      CALL SetModeMenuState(-1, -1)
      CALL SelectMode(ID_Structure_Solution_Mode)
      IPTYPE = 1
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

! The following variables are there to allow the dialogue fields in the
! window dealing with Z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

      INTEGER        IDFZMFile,                                                &
                     IDBZMDelete,                    IDBZMBrowse,              &
                     IDBZMView,                      IDBZMEdit,                &
                     IDFZMpars,                      IDFZMLabel,               &
                     first_zm_in_win
      COMMON /IDFZM/ IDFZMFile(1:maxfrginterface),                                      &
                     IDBZMDelete(1:maxfrginterface), IDBZMBrowse(1:maxfrginterface),    &
                     IDBZMView(1:maxfrginterface),   IDBZMEdit(1:maxfrginterface),      &
                     IDFZMpars(1:maxfrginterface),   IDFZMLabel(1:maxfrginterface),     &
                     first_zm_in_win

      INTEGER, EXTERNAL :: Read_One_Zm
      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical
      INTEGER        iFlags
      INTEGER        zmread
      INTEGER        iFrg, iSelection, DelFrg
      CHARACTER(MaxPathLength) SDIFile, DirName, tFileName
      CHARACTER*(80) FileName
      CHARACTER*250  FilterStr
      INTEGER        tNumZMatrices, tLen
      CHARACTER*(80) tZmatrices(10)
      INTEGER        tNextzmNum
      CHARACTER*(7)  tExtension

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page1)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the Pawley refinement or the initial wizard
              CALL EndWizardPastPawley
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT, IDB_PO)
              Resume_SA = .FALSE. ! Initialisation
! Go to the next stage of the SA input
! Grey out 'Load DASH Pawley file' button on toolbar
              CALL WMenuSetState(ID_import_dpj_file, ItemEnabled, WintOff)
! If the user has requested preferred orientation, make sure we pass the pertinent Wizard window
              CALL WDialogSelect(IDD_SAW_Page2)
              IF ((EventInfo%VALUE1 .EQ. IDB_PO) .OR. WDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                CALL WizardWindowShow(IDD_SAW_Page2)
              ELSE
                CALL SA_Parameter_Set
                CALL ShowWizardWindowParameterBounds
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDB_Relabel)
              CALL zmRelabelAll
            CASE (IDB_SA_Project_Browse)
              CALL SDIFileBrowse
            CASE (IDB_SA_Project_Open)
              CALL WDialogGetString(IDF_SA_Project_Name, SDIFile)
              CALL SDIFileOpen(SDIFile)
            CASE (IDB_SA_Project_Import)
! Import .. convert a mol/pdb/mol2 file into a Z-matrix
              CALL ImportZmatrix('')
            CASE (IDB_zmDelete1, IDB_zmDelete2, IDB_zmDelete3, IDB_zmDelete4)
              IF (Confirm('Do you want to clear this Z-matrix?')) THEN
                DelFrg = 1
                DO WHILE (IDBZMDelete(DelFrg) .NE. EventInfo%VALUE1)
                  DelFrg = DelFrg + 1
                ENDDO
                DO iFrg = DelFrg+1, nFrag
                  CALL zmCopy(iFrg, iFrg-1)
                ENDDO
                nFrag = nFrag - 1
              ENDIF
            CASE (IDB_zmBrowse1, IDB_zmBrowse2, IDB_zmBrowse3, IDB_zmBrowse4)
              iFrg = 1
              DO WHILE (IDBZMBrowse(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              IF (iFrg .GT. nFrag) iFrg = nFrag + 1
              iFlags = LoadDialog + PromptOn + DirChange + AppendExt
              FilterStr = "All files (*.*)|*.*|"//&
                          "Z-matrix and molecular model files|*.zmatrix;*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"//&
                          "Z-matrix files (*.zmatrix)|*.zmatrix|"//&
                          "Molecular model files|*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"
              iSelection = 2
              CALL WSelectFile(FilterStr, iFlags, tFileName, 'Load Z-matrix file', iSelection)
! Did the user press cancel?
              IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) GOTO 999
              CALL SplitPath(tFileName, DirName, FileName)
! Determine the extension. If Z-matrix, load it
              tLen = LEN_TRIM(FileName)
              tExtension = ''
              IF (tLen .GE. 9) THEN
                tExtension = FileName(tLen-6:tLen)
                CALL StrUpperCase(tExtension)
              ENDIF
              IF (tExtension .EQ. 'ZMATRIX') THEN
                frag_file(iFrg) = tFileName
                zmread = Read_One_ZM(iFrg)
                IF (zmread .EQ. 0) THEN ! successful read
                  IF (iFrg .GT. nFrag) nFrag = nFrag + 1
                ELSE 
                  CALL FileErrorPopup(frag_file(iFrg), zmread)
                ENDIF ! If the read on the Z-matrix was ok
              ELSE
                CALL zmConvert(tFileName, tNumZMatrices, tZmatrices)
                IF (tNumZMatrices .EQ. 0) RETURN
                tNextzmNum  = 1
   10           CONTINUE
                frag_file(iFrg) = DirName(1:LEN_TRIM(DirName))//tZmatrices(tNextzmNum)
                zmread = Read_One_ZM(iFrg)
                IF (zmread .EQ. 0) THEN ! successful read
                  IF (iFrg .GT. nFrag) THEN ! iFrg could have been lower for the _first_ Z-matrix
                    nFrag = nFrag + 1
                    IF (nFrag .EQ. maxfrg) THEN
! More Z-matrices to read?
                      IF (tNextzmNum .LT. tNumZMatrices) CALL InfoMessage('File contained more Z-matrices than available slots.')
                      GOTO 999
                    ENDIF
                  ENDIF
                  iFrg = nFrag + 1
                ELSE
                  CALL FileErrorPopup(frag_file(iFrg), zmread)
! Slot still free, so iFrg still OK.
                ENDIF
                tNextzmNum = tNextzmNum + 1
                IF (tNextzmNum .GT. tNumZMatrices) GOTO 999
! Read next Z-matrix.
                GOTO 10
              ENDIF
! View individual Z-matrices in e.g. Mercury
            CASE (IDB_zmView1, IDB_zmView2, IDB_zmView3, IDB_zmView4)
              iFrg = 1
              DO WHILE (IDBZMView(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              CALL zmView(iFrg)
! Edit individual Z-matrices
            CASE (IDB_zmEdit1, IDB_zmEdit2, IDB_zmEdit3, IDB_zmEdit4)
              iFrg = 1
              DO WHILE (IDBzmEdit(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              CALL ShowEditZMatrixWindow(iFrg)
            CASE (IDB_Up)
              first_zm_in_win = first_zm_in_win - 1
              CALL WDialogFieldStateLogical(IDB_Up, first_zm_in_win .NE. 1)
              CALL WDialogFieldStateLogical(IDB_Down, first_zm_in_win+maxfrginterface-1 .NE. maxfrg)
            CASE (IDB_Down)
              first_zm_in_win = first_zm_in_win + 1
              CALL WDialogFieldStateLogical(IDB_Up, first_zm_in_win .NE. 1)
              CALL WDialogFieldStateLogical(IDB_Down, first_zm_in_win+maxfrginterface-1 .NE. maxfrg)
          END SELECT
        CASE (FieldChanged)
      END SELECT
  999 CONTINUE
      CALL UpdateZmatrixSelection
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowZmatrices
!
!*****************************************************************************
!
      SUBROUTINE ShowEditZMatrixWindow(iFrg)

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER I, BondNr

      CALL WDialogSelect(IDD_zmEdit)
      CurrentlyEditedFrag = iFrg
! Make temporary copy
      CALL zmCopy(iFrg,0)
      zmAtomDeleted = .FALSE.
      CALL zmCopyTemp2Dialog
! In order to be able to delete atoms from the Z-matrix at random, we need their
! Cartesian co-ordinates.
      natcry = natoms(iFrg)
      CALL makexyz(natcry,BLEN(1, iFrg),ALPH(1, iFrg),BET(1, iFrg),IZ1(1, iFrg),IZ2(1, iFrg),IZ3(1, iFrg),axyzo)
      DO I = 1, natcry
        aelem(I) = zmElementCSD(I, iFrg)
        atomlabel(I) = OriginalLabel(I, iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr, iFrg)
        bond(BondNr,1) = Bonds(1,BondNr, iFrg)
        bond(BondNr,2) = Bonds(2,BondNr, iFrg)
      ENDDO
! Bonds have already been calculated and will be updated automatically: there is no option
! to add atoms, so we can only remove atoms and their bonds.
! Only problem: changing a hydrogen into, say, a carbon
! might give rise to new bonds that will not be taken into account.
! Oh, and removing an atom can change a bond type, e.g. from "aromatic" to "double"
      CALL WDialogShow(-1, -1, 0, SemiModeLess)

      END SUBROUTINE ShowEditZMatrixWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithEditZMatrixWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, EXTERNAL :: zmSave, zmSaveAs, WriteMol2, zmRebuild
      INTEGER iFrg, iOption, iColumn, iAtomNr, iDummy, iBondNr, iRow, iCol
      REAL    tReal
      LOGICAL ThisOne
      INTEGER tLength, I, iBondNr2
      CHARACTER(MaxPathLength) temp_file

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Set)
              CALL WDialogGetReal(IDF_BisoOccValue, tReal)
              CALL WDialogGetMenu(IDF_BisoOccMenu, iOption)
              SELECT CASE (iOption)
                CASE (1)
                  iColumn = 4 ! Biso
                  IF (tReal .LT. -10.0) tReal = -10.0
                  IF (tReal .GT. 100.0) tReal = 100.0
                CASE (2)
                  iColumn = 5 ! Occupancies
                  IF (tReal .LT.   0.0) tReal =  0.0
                  IF (tReal .GT.  10.0) tReal = 10.0
              END SELECT              
              CALL WDialogGetMenu(IDF_WhichAtomMenu, iOption)
              DO iAtomNr = 1, natoms(iFrg)
! Biso / occupancies
                SELECT CASE (iOption)
                  CASE (1) ! All atoms
                    ThisOne = .TRUE.
                  CASE (2) ! All non-hydrogens
                    ThisOne = (zmElementCSD(izmbid(iAtomNr, iFrg), iFrg) .NE. 2)
                  CASE (3) ! All hydrogens
                    ThisOne = (zmElementCSD(izmbid(iAtomNr, iFrg), iFrg) .EQ. 2)
                END SELECT
                IF (ThisOne) CALL WGridPutCellReal(IDF_AtomPropGrid, iColumn, iAtomNr, tReal, '(F5.3)')
              ENDDO
              CALL zmCopyDialog2Temp
            CASE (IDB_Relabel)
              CALL zmCopyDialog2Temp
              CALL zmRelabel(iFrg)
              CALL zmCopyTemp2Dialog
            CASE (IDB_ReOrder)
              CALL zmCopyDialog2Temp
              CALL zmReOrder(iFrg)
              CALL zmCopyTemp2Dialog
            CASE (IDOK)
! If an atom has been deleted, rebuild the Z-matrix.
              IF (zmRebuild() .EQ. 1) THEN
                CALL ErrorMessage('Could not rebuild the Z-matrix')
              ELSE
                CALL WDialogHide
              ENDIF
              CALL zmCopy(0,CurrentlyEditedFrag)
              CALL UpdateZmatrixSelection
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide
            CASE (IDBSAVE)
              CALL zmCopyDialog2Temp
              iDummy = zmSave(iFrg)
            CASE (IDB_SaveAs)
              CALL zmCopyDialog2Temp
              iDummy = zmSaveAs(iFrg)
            CASE (IDB_View)
              CALL zmCopyDialog2Temp
              natcry = NATOMS(iFrg)
              DO iAtomNr = 1, natcry
                atomlabel(iAtomNr) = OriginalLabel(iAtomNr, iFrg)
                aelem(iAtomNr) = zmElementCSD(iAtomNr, iFrg)
              ENDDO
              tLength = LEN_TRIM(frag_file(iFrg))
              temp_file = frag_file(iFrg)(1:tLength-8)//'_temp.mol2'
! Show the mol2 file
!C      INTEGER FUNCTION WriteMol2(TheFileName)
!C!
!C! Takes number of atoms    from natcry    in SAMVAR
!C! Takes atomic coordinates from axyzo     in SAMVAR  (orthogonal)
!C! Takes element types      from aelem     in SAMVAR  (CSD style)
!C! Takes atom labels        from atomlabel in SAMVAR
!C! Takes bonds              from bond      in SAMVAR
!C! Takes bond types         from btype     in SAMVAR
!C! and writes out a .mol2 file
              IF (WriteMol2(temp_file, .TRUE., iFrg) .EQ. 1) CALL ViewStructure(temp_file)
              CALL IOSDeleteFile(temp_file)
            CASE (IDB_Rotations)
              CALL ShowEditZMatrixRotationsWindow
          END SELECT
        CASE (FieldChanged)
          IF (EventInfo%VALUE2 .EQ. IDF_AtomPropGrid) THEN
            CALL WGridPos(EventInfo%Y,iCol,iRow)
            IF (iCol .EQ. 2) THEN
              IF (natoms(iFrg) .EQ. 1) THEN
                CALL ErrorMessage('The last atom cannot be deleted.')
              ELSE
                CALL zmCopyDialog2Temp
! Add in a check to see if the atom to be deleted is used in defining rotations
! and ask the user for confirmation



! @@



                zmAtomDeleted = .TRUE.
! Delete the atom
                iAtomNr = izmbid(iRow, iFrg)
! It is very likely that we will end up with one of the atoms having an 'orignal ID' that
! is greater than the current number of atoms. This would give boundary overflows.
! We can retain the original order (minus one atom) but that involves subtracting 1
! from atom IDs following the one we have deleted
                DO i = 1, natoms(iFrg)
                  IF (izmoid(i, iFrg) .GT. izmoid(iAtomNr, iFrg)) izmoid(i, iFrg) = izmoid(i, iFrg) - 1
                ENDDO
! Remove any bonds this atom was involved in
                IF (NumberOfBonds(iFrg) .GT. 0) THEN
                  iBondNr = 1
                  DO WHILE (iBondNr .LE. NumberOfBonds(iFrg))
                    IF ((Bonds(1,iBondNr, iFrg) .EQ. iAtomNr) .OR. (Bonds(2,iBondNr, iFrg) .EQ. iAtomNr)) THEN
                      DO iBondNr2 = iBondNr, NumberOfBonds(iFrg)-1
                        BondType(iBondNr2, iFrg) = BondType(iBondNr2+1, iFrg)
                        Bonds(1,iBondNr2, iFrg)  = Bonds(1,iBondNr2+1, iFrg)
                        Bonds(2,iBondNr2, iFrg)  = Bonds(2,iBondNr2+1, iFrg)
                      ENDDO
                      NumberOfBonds(iFrg) = NumberOfBonds(iFrg) - 1
                    ELSE
                      CALL INC(iBondNr)
                    ENDIF
                  ENDDO
                  IF (NumberOfBonds(iFrg) .GT. 0) THEN
                    DO iBondNr = 1, NumberOfBonds(iFrg)
                      IF (Bonds(1,iBondNr, iFrg) .GT. iAtomNr) Bonds(1,iBondNr, iFrg) = Bonds(1,iBondNr, iFrg) - 1
                      IF (Bonds(2,iBondNr, iFrg) .GT. iAtomNr) Bonds(2,iBondNr, iFrg) = Bonds(2,iBondNr, iFrg) - 1
                    ENDDO
                  ENDIF
                ENDIF
! If not last atom in list, shuffle remaining
                IF (iAtomNr .NE. NATOMS(iFrg)) THEN 
                  DO I = iAtomNr, NATOMS(iFrg)-1
                    ElSym(I, iFrg) = ElSym(I+1, iFrg)
                    zmElementCSD(I, iFrg) = zmElementCSD(I+1, iFrg)
                    tiso(I, iFrg) = tiso(I+1, iFrg)
                    occ(I, iFrg)  = occ(I+1, iFrg)
                    OriginalLabel(I, iFrg) = OriginalLabel(I+1, iFrg)
                    izmoid(I, iFrg) = izmoid(I+1, iFrg)
                  ENDDO
                ENDIF
                natcry = NATOMS(iFrg)
! If not last atom in list, shuffle remaining
                IF (iAtomNr .NE. natcry) THEN 
                  DO I = iAtomNr, natcry-1
                    axyzo(1,I) = axyzo(1,I+1)
                    axyzo(2,I) = axyzo(2,I+1)
                    axyzo(3,I) = axyzo(3,I+1)
                  ENDDO
                ENDIF
                natcry = natcry - 1
                nbocry = NumberOfBonds(iFrg)
                DO iBondNr = 1, nbocry
                  btype(iBondNr)  = BondType(iBondNr, iFrg)
                  bond(iBondNr,1) = Bonds(1,iBondNr, iFrg)
                  bond(iBondNr,2) = Bonds(2,iBondNr, iFrg)
                ENDDO
! If this was the pivot atom, use centre of mass
                IF (icomflg(iFrg) .EQ. iAtomNr) icomflg(iFrg) = 0 
                IF (natoms(iFrg) .NE. 0) THEN
                  CALL WGridSetCell(IDF_AtomPropGrid, 1, 1)
                  natoms(iFrg) = natoms(iFrg) - 1
                ENDIF
                DO i = 1, natoms(iFrg)
                  izmbid(izmoid(i, iFrg), iFrg) = i ! the back mapping
                ENDDO
                CALL zmCopyTemp2Dialog
              ENDIF
              CALL WGridPutCellCheckBox(IDF_AtomPropGrid, 2, irow, Unchecked)
            ENDIF
          ENDIF
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithEditZMatrixWindow
!
!*****************************************************************************
!
      SUBROUTINE ShowEditZMatrixRotationsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE

      INCLUDE 'lattice.inc'  

      LOGICAL, EXTERNAL :: FnUnitCellOK

! Calculate the unit cell axes in terms of the orthogonal lattice from
! the unit cell parameters
      IF (FnUnitCellOK()) THEN
        CALL LatticeCellParameters2Lattice(CellPar(1), CellPar(2), CellPar(3), &
                                           CellPar(4), CellPar(5), CellPar(6), f2cmat)
      ELSE
        f2cmat(1,1) = 1.0
        f2cmat(1,2) = 0.0
        f2cmat(1,3) = 0.0
        f2cmat(2,1) = 0.0
        f2cmat(2,2) = 1.0
        f2cmat(2,3) = 0.0
        f2cmat(3,1) = 0.0
        f2cmat(3,2) = 0.0
        f2cmat(3,3) = 1.0
        CALL InfoMessage("It is recommended that the .sdi file is loaded first, because"//CHAR(13)// &
                         "the unit-cell parameters are required to view the origin and"//CHAR(13)// &
                         "the initial orientation of the Z-matrix in the unit cell.")
      ENDIF
      CALL zmCopyDialog2Temp
      CALL zmRotCopyTemp2Dialog
      CALL WDialogSelect(IDD_zmEditRotations)
      CALL WDialogShow(-1, -1, 0, SemiModeLess)

      END SUBROUTINE ShowEditZMatrixRotationsWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithEditZMatrixRotationsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE    

      INTEGER, EXTERNAL :: WriteMol2, Get_HydrogenTreatment, WDialogGetRadioButtonInt
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, Get_UseCrystallographicCoM
      REAL, EXTERNAL :: Degrees2Radians
      INTEGER I, iFrg, iOption, iOpt1State, iOpt2State, iOpt3State
      INTEGER iAtomNr
      REAL    Alpha, Beta, Gamma
      REAL    taxyzo(1:3, 1:MAXATM_2)
      CHARACTER(50) temp_file
      REAL    RotMat(1:3,1:3)
      REAL    COM(1:3), v(1:3), v1(1:3), v2(1:3)
      LOGICAL tUseSingleAxis
      REAL    Point1(1:3), Point2(1:3), Point3(1:3) 
      REAL    Q1(0:3), Q2(0:3)

      iFrg = 0
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Relabel)
              CALL zmCopyDialog2Temp
              CALL zmRelabel(iFrg)
              CALL zmCopyTemp2Dialog
            CASE (IDOK)
              CALL zmRotCopyDialog2Temp
              CALL WDialogHide
            CASE (IDCANCEL)
              CALL WDialogHide
            CASE (IDB_View)
              CALL zmRotCopyDialog2Temp
              natcry = NATOMS(iFrg)
              DO iAtomNr = 1, natcry
                taxyzo(1,iAtomNr) = axyzo(1,iAtomNr)
                taxyzo(2,iAtomNr) = axyzo(2,iAtomNr)
                taxyzo(3,iAtomNr) = axyzo(3,iAtomNr)
              ENDDO
! Subtract origin from co-ordinates
              CALL WDialogGetRadioButton(IDF_RotOrgCOM, iOption)
              SELECT CASE (iOption)
                CASE (1) ! C.O.M.
! If user set centre of mass flag to 0, then use the molecule's centre of mass
                  COM = 0.0
                  IF (Get_UseCrystallographicCoM()) THEN
                    CALL zmCreate_AtomicWeightings(iFrg, Get_HydrogenTreatment())
                  ELSE
                    DO iAtomNr = 1, natcry
                      AtomicWeighting(iAtomNr, iFrg) = 1.0 / FLOAT(natcry)
                    ENDDO
                  ENDIF
                  DO iAtomNr = 1, natcry
                    COM(1) = COM(1) + AtomicWeighting(iAtomNr, iFrg)*axyzo(1,iAtomNr)
                    COM(2) = COM(2) + AtomicWeighting(iAtomNr, iFrg)*axyzo(2,iAtomNr)
                    COM(3) = COM(3) + AtomicWeighting(iAtomNr, iFrg)*axyzo(3,iAtomNr)
                  ENDDO
! Otherwise, use atom number ICFRG
                CASE (2) ! Use atom nr.
                  CALL WDialogGetInteger(IDF_RotOrgAtomNr, iAtomNr)
                  IF ((iAtomNr .LT. 1) .OR. (iAtomNr .GT. natoms(CurrentlyEditedFrag))) THEN
                    CALL ErrorMessage("Please enter a correct atom number for the centre of rotation.")
                    CALL PopActiveWindowID
                    RETURN
                  ENDIF
                  COM(1) = axyzo(1,izmbid(iAtomNr, iFrg))
                  COM(2) = axyzo(2,izmbid(iAtomNr, iFrg))
                  COM(3) = axyzo(3,izmbid(iAtomNr, iFrg))
              END SELECT
              DO iAtomNr = 1, natcry
                axyzo(1,iAtomNr) = axyzo(1,iAtomNr) - COM(1)
                axyzo(2,iAtomNr) = axyzo(2,iAtomNr) - COM(2)
                axyzo(3,iAtomNr) = axyzo(3,iAtomNr) - COM(3)
              ENDDO
! Apply initial orientation
              SELECT CASE (zmSingleRAIniOrDef(iFrg))
                CASE (1) ! Define from axis (only possible when axis is defined from atoms, not from another axis)
                  SELECT CASE (zmSingleRotAxDef(iFrg))
                    CASE (1)
                      v1(1) = axyzo(1,zmSingleRotAxAtm(2, iFrg)) - axyzo(1,zmSingleRotAxAtm(1, iFrg))
                      v1(2) = axyzo(2,zmSingleRotAxAtm(2, iFrg)) - axyzo(2,zmSingleRotAxAtm(1, iFrg))
                      v1(3) = axyzo(3,zmSingleRotAxAtm(2, iFrg)) - axyzo(3,zmSingleRotAxAtm(1, iFrg))
                    CASE (3)
                      Point1 = axyzo(:, zmSingleRotAxPlnAtm(1, iFrg))
                      Point2 = axyzo(:, zmSingleRotAxPlnAtm(2, iFrg))
                      Point3 = axyzo(:, zmSingleRotAxPlnAtm(3, iFrg))
                      Point1 = Point1 - Point2
                      Point3 = Point3 - Point2
                      CALL VectorCrossProduct(Point1, Point3, v1)
                  END SELECT
                  CALL PremultiplyVectorByMatrix(f2cmat, zmSingleRAIniOrFrac(1,iFrg), v2) ! frac -> cart
                  CALL Vector2Quaternion(v1, Q1)
                  CALL Vector2Quaternion(v2, Q2)
                  CALL QuaternionInverse(Q1)
                  CALL QuaternionMultiply(Q2, Q1, zmInitialQs(0, iFrg))
                CASE (2) ! Defined as Euler angles => convert to Quaternions
                  Alpha = zmSingleRAIniOrEuler(1, iFrg)
                  Beta  = zmSingleRAIniOrEuler(2, iFrg)
                  Gamma = zmSingleRAIniOrEuler(3, iFrg)
                  zmInitialQs(0, iFrg) = COS(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha+Gamma))
                  zmInitialQs(1, iFrg) = SIN(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha-Gamma))
                  zmInitialQs(2, iFrg) = SIN(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha-Gamma))
                  zmInitialQs(3, iFrg) = COS(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha+Gamma))
                CASE (3) ! Defined as quaternions
                  zmInitialQs(0, iFrg) = zmSingleRAIniOrQuater(0,iFrg)
                  zmInitialQs(1, iFrg) = zmSingleRAIniOrQuater(1,iFrg)
                  zmInitialQs(2, iFrg) = zmSingleRAIniOrQuater(2,iFrg)
                  zmInitialQs(3, iFrg) = zmSingleRAIniOrQuater(3,iFrg)
              END SELECT
              CALL Quaternion2Matrix(zmInitialQs(0, iFrg), RotMat)
              DO I = 1, natcry
                CALL PremultiplyVectorByMatrix(RotMat, axyzo(1,I), v)
                axyzo(1,I) = v(1)
                axyzo(2,I) = v(2)
                axyzo(3,I) = v(3)
              ENDDO
              DO iAtomNr = 1, natcry
                atomlabel(iAtomNr) = OriginalLabel(iAtomNr, iFrg)
                aelem(iAtomNr) = zmElementCSD(iAtomNr, iFrg)
              ENDDO
              temp_file = 'temp.mol2'
! Show the mol2 file
!C      INTEGER FUNCTION WriteMol2(TheFileName)
!C!
!C! Takes number of atoms    from natcry    in SAMVAR
!C! Takes atomic coordinates from axyzo     in SAMVAR  (orthogonal)
!C! Takes element types      from aelem     in SAMVAR  (CSD style)
!C! Takes atom labels        from atomlabel in SAMVAR
!C! Takes bonds              from bond      in SAMVAR
!C! Takes bond types         from btype     in SAMVAR
!C! and writes out a .mol2 file
              IF (WriteMol2(temp_file, .TRUE., iFrg) .EQ. 1) CALL ViewStructure(temp_file)
              CALL IOSDeleteFile(temp_file)
              DO iAtomNr = 1, natcry
                axyzo(1,iAtomNr) = taxyzo(1,iAtomNr)
                axyzo(2,iAtomNr) = taxyzo(2,iAtomNr)
                axyzo(3,iAtomNr) = taxyzo(3,iAtomNr)
              ENDDO
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_RotOrgCOM, IDF_RotOrgAtom)
              CALL WDialogGetRadioButton(IDF_RotOrgCOM, iOption)
              CALL WDialogFieldStateLogical(IDF_RotOrgAtomNr, iOption .EQ. 2)
            CASE (IDF_UseSingleAxis, IDF_RotAxAtom, IDF_RotAxFrac, IDF_RotAxPln, IDF_IniOrAxis, IDF_IniOrEuler, IDF_IniOrQuater)
              tUseSingleAxis = WDialogGetCheckBoxLogical(IDF_UseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_GROUP2,      tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxAtom,   tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxFrac,   tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxPln,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_GROUP3,      tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_IniOrEuler,  tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_IniOrQuater, tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_IniOrAxis,   tUseSingleAxis)
              iOpt1State = Disabled
              iOpt2State = Disabled
              iOpt3State = Disabled
              IF (tUseSingleAxis) THEN
                CALL WDialogGetRadioButton(IDF_RotAxAtom, iOption)
                SELECT CASE (WDialogGetRadioButtonInt(IDF_RotAxAtom))
                  CASE (1)
                    iOpt1State = Enabled
                  CASE (2)
                    iOpt2State = Enabled
                    IF (WDialogGetRadioButtonInt(IDF_IniOrAxis) .EQ. 1) CALL WDialogPutRadioButton(IDF_IniOrQuater)
                    CALL WDialogFieldStateLogical(IDF_IniOrAxis, .FALSE.)
                  CASE (3)
                    iOpt3State = Enabled
                END SELECT
              ENDIF
              CALL WDialogFieldState(IDF_AtomNr1,      iOpt1State)
              CALL WDialogFieldState(IDF_LABEL12,      iOpt1State)
              CALL WDialogFieldState(IDF_AtomNr2,      iOpt1State)
              CALL WDialogFieldState(IDF_LABELa,       iOpt2State)
              CALL WDialogFieldState(IDF_LABELb,       iOpt2State)
              CALL WDialogFieldState(IDF_LABELc,       iOpt2State)
              CALL WDialogFieldState(IDF_a1,           iOpt2State)
              CALL WDialogFieldState(IDF_b1,           iOpt2State)
              CALL WDialogFieldState(IDF_c1,           iOpt2State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm1, iOpt3State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm2, iOpt3State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm3, iOpt3State)
              iOpt1State = Disabled
              iOpt2State = Disabled
              iOpt3State = Disabled
              IF (tUseSingleAxis) THEN
                SELECT CASE (WDialogGetRadioButtonInt(IDF_IniOrAxis))
                  CASE (1)
                    iOpt1State = Enabled
                  CASE (2)
                    iOpt2State = Enabled
                  CASE (3)
                    iOpt3State = Enabled
                END SELECT
              ENDIF
              CALL WDialogFieldState(IDF_a2,    iOpt1State)
              CALL WDialogFieldState(IDF_b2,    iOpt1State)
              CALL WDialogFieldState(IDF_c2,    iOpt1State)
              CALL WDialogFieldState(IDF_Alpha, iOpt2State)
              CALL WDialogFieldState(IDF_Beta,  iOpt2State)
              CALL WDialogFieldState(IDF_Gamma, iOpt2State)
              CALL WDialogFieldState(IDF_Q0,    iOpt3State)
              CALL WDialogFieldState(IDF_Q1,    iOpt3State)
              CALL WDialogFieldState(IDF_Q2,    iOpt3State)
              CALL WDialogFieldState(IDF_Q3,    iOpt3State)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithEditZMatrixRotationsWindow
!
!*****************************************************************************
!
      SUBROUTINE zmCopy(iFrg1, iFrg2)

! Copies Z-matrix 1 to Z-matrix 2

      USE ZMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg1, iFrg2

      INTEGER iAtomNr, iBondNr

      frag_file(iFrg2) = frag_file(iFrg1)
      natoms(iFrg2)    = natoms(iFrg1)
      icomflg(iFrg2)   = icomflg(iFrg1)
      UseQuaternions(iFrg2)           = UseQuaternions(iFrg1)
      zmSingleRAIniOrDef(iFrg2)       = zmSingleRAIniOrDef(iFrg1)
      zmSingleRAIniOrFrac(:, iFrg2)   = zmSingleRAIniOrFrac(:, iFrg1)
      zmSingleRAIniOrEuler(:, iFrg2)  = zmSingleRAIniOrEuler(:, iFrg1)
      zmSingleRAIniOrQuater(:, iFrg2) = zmSingleRAIniOrQuater(:, iFrg1)
      zmSingleRotAxDef(iFrg2)         = zmSingleRotAxDef(iFrg1)
      zmSingleRotAxAtm(1, iFrg2)      = zmSingleRotAxAtm(1, iFrg1)
      zmSingleRotAxAtm(2, iFrg2)      = zmSingleRotAxAtm(2, iFrg1)
      zmSingleRotAxFrac(:, iFrg2)     = zmSingleRotAxFrac(:, iFrg1)
      zmSingleRotAxPlnAtm(:, iFrg2)   = zmSingleRotAxPlnAtm(:, iFrg1)
      zmInitialQs(:, iFrg2)           = zmInitialQs(:, iFrg1)
      DO iAtomNr = 1, natoms(iFrg1)
        ioptb(iAtomNr, iFrg2)         = ioptb(iAtomNr, iFrg1)
        iopta(iAtomNr, iFrg2)         = iopta(iAtomNr, iFrg1)
        ioptt(iAtomNr, iFrg2)         = ioptt(iAtomNr, iFrg1)
        iz1(iAtomNr, iFrg2)           = iz1(iAtomNr, iFrg1)
        iz2(iAtomNr, iFrg2)           = iz2(iAtomNr, iFrg1)
        iz3(iAtomNr, iFrg2)           = iz3(iAtomNr, iFrg1)
        blen(iAtomNr, iFrg2)          = blen(iAtomNr, iFrg1)
        alph(iAtomNr, iFrg2)          = alph(iAtomNr, iFrg1)
        bet(iAtomNr, iFrg2)           = bet(iAtomNr, iFrg1)
        ElSym(iAtomNr, iFrg2)         = ElSym(iAtomNr, iFrg1)
        zmElementCSD(iAtomNr, iFrg2)  = zmElementCSD(iAtomNr, iFrg1)
        OriginalLabel(iAtomNr, iFrg2) = OriginalLabel(iAtomNr, iFrg1)
        tiso(iAtomNr, iFrg2)          = tiso(iAtomNr, iFrg1)
        occ(iAtomNr, iFrg2)           = occ(iAtomNr, iFrg1)
        izmoid(iAtomNr, iFrg2)        = izmoid(iAtomNr, iFrg1)
        izmbid(iAtomNr, iFrg2)        = izmbid(iAtomNr, iFrg1)
      ENDDO
      NumberOfBonds(iFrg2) = NumberOfBonds(iFrg1)
      IF (NumberOfBonds(iFrg1) .GT. 0) THEN
        DO iBondNr = 1, NumberOfBonds(iFrg1)
          BondType(iBondNr, iFrg2) = BondType(iBondNr, iFrg1)
          Bonds(1,iBondNr, iFrg2)  = Bonds(1,iBondNr, iFrg1)
          Bonds(2,iBondNr, iFrg2)  = Bonds(2,iBondNr, iFrg1)
        ENDDO
      ENDIF
      CALL zmDoAdmin(iFrg2)

      END SUBROUTINE zmCopy
!
!*****************************************************************************
!
      INTEGER FUNCTION zmRebuild
!
! In case an atom has been deleted.
!
! RETURNS : 0 for success
!
      USE WINTERACTER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE      

      INTEGER, EXTERNAL :: Read_One_ZM, WriteMol2
      INTEGER iFrg
      INTEGER tNumZMatrices, iAtomNr
      CHARACTER(80) tZmatrices
      DIMENSION tZmatrices(10)
      CHARACTER(MaxPathLength) tOldFileName

! Initialise to failure
      zmRebuild = 1
      iFrg = 0
      CALL zmCopyDialog2Temp
! If an atom has been deleted, rebuild the Z-matrix.
      IF (.NOT. zmAtomDeleted) THEN
        zmRebuild = 0
        RETURN
      ENDIF
      tOldFileName = frag_file(iFrg)
      DO iAtomNr = 1, natcry
        atomlabel(iAtomNr) = OriginalLabel(iAtomNr, iFrg)
        aelem(iAtomNr) = zmElementCSD(iAtomNr, iFrg)
      ENDDO
      IF (WriteMol2('Rebuild_temp.mol2',.FALSE., iFrg) .NE. 1) GOTO 999 ! Writing mol2 file failed
      CALL zmConvert('Rebuild_temp.mol2',tNumZMatrices,tZmatrices)
! Check that we still have 1 Z-matrix
      IF (tNumZMatrices .EQ. 0) GOTO 999 ! Conversion failed
      IF (tNumZMatrices .GT. 1) THEN
        CALL WarningMessage('More than 1 Z-matrix generated.'//&
                            'Only the first will be retained.')
      ENDIF
      CALL IOsCopyFile('Rebuild_temp_1.zmatrix','Rebuild_temp.zmatrix')
      frag_file(iFrg) = 'Rebuild_temp.zmatrix'
! Reading a Z-matrix is going to reset all the rotational stuff that isn't present in a .zmatrix file
      CALL zmRotCopyTemp2Dialog
      IF (Read_One_ZM(iFrg) .NE. 0) GOTO 999 ! reading failed
      zmAtomDeleted = .FALSE.
      zmRebuild = 0
  999 CALL zmRotCopyDialog2Temp
      frag_file(iFrg) = tOldFileName

      END FUNCTION zmRebuild
!
!*****************************************************************************
!
      SUBROUTINE zmCopyTemp2Dialog

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE 
      
      CHARACTER(3) RowLabelStr
      INTEGER iFrg, iRow, iAtomNr     

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
! Fill grid with atom properties
! Set number of rows
      CALL WGridRows(IDF_AtomPropGrid, natoms(iFrg))
      DO iRow = 1, natoms(iFrg)
        iAtomNr = izmbid(iRow, iFrg)
! Show the number of the atom in the zeroth column
        WRITE(RowLabelStr,'(I3)') iRow
        CALL WGridLabelRow(IDF_AtomPropGrid, iRow, RowLabelStr)
! atom labels
        CALL WGridPutCellString(IDF_AtomPropGrid, 1, iRow, OriginalLabel(iAtomNr, iFrg))
! atom elements
        CALL WGridPutCellString(IDF_AtomPropGrid, 3, iRow, ElementStr(zmElementCSD(iAtomNr, iFrg)))
! Biso
        CALL WGridPutCellReal(IDF_AtomPropGrid, 4, iRow, tiso(iAtomNr, iFrg), '(F5.3)')
! occupancies
        CALL WGridPutCellReal(IDF_AtomPropGrid, 5, iRow, occ(iAtomNr, iFrg), '(F5.3)')
      ENDDO
! If only a single atom left, grey out "Rotations..." and "Re-order"
      CALL WDialogFieldStateLogical(IDB_Rotations, natoms(iFrg) .GT. 1)
      CALL WDialogFieldStateLogical(IDB_ReOrder, natoms(iFrg) .GT. 1)
      CALL PopActiveWindowID

      END SUBROUTINE zmCopyTemp2Dialog
!
!*****************************************************************************
!
      SUBROUTINE zmRotCopyTemp2Dialog

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE 
      
      INTEGER iFrg, iOpt1State, iOpt2State, iOpt3State
      LOGICAL tUseSingleAxis

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      iFrg = 0
      CALL WDialogFieldStateLogical(IDF_RotOrgAtomNr, icomflg(iFrg) .NE. 0)
      IF (icomflg(iFrg) .EQ. 0) THEN ! Use centre of mass
        CALL WDialogPutRadioButton(IDF_RotOrgCOM)
        CALL WDialogPutInteger(IDF_RotOrgAtomNr, 1)
      ELSE ! use atom number
        CALL WDialogPutRadioButton(IDF_RotOrgAtom)
        CALL WDialogPutInteger(IDF_RotOrgAtomNr, izmoid(icomflg(iFrg), iFrg))
      ENDIF
      tUseSingleAxis = .NOT. UseQuaternions(iFrg)
      CALL WDialogPutCheckBoxLogical(IDF_UseSingleAxis, tUseSingleAxis)
      CALL WDialogPutInteger(IDF_AtomNr1, izmoid(zmSingleRotAxAtm(1, iFrg), iFrg))
      CALL WDialogPutInteger(IDF_AtomNr2, izmoid(zmSingleRotAxAtm(2, iFrg), iFrg))
      CALL WDialogPutReal(IDF_a1, zmSingleRotAxFrac(1, iFrg))
      CALL WDialogPutReal(IDF_b1, zmSingleRotAxFrac(2, iFrg))
      CALL WDialogPutReal(IDF_c1, zmSingleRotAxFrac(3, iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm1, izmoid(zmSingleRotAxPlnAtm(1, iFrg), iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm2, izmoid(zmSingleRotAxPlnAtm(2, iFrg), iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm3, izmoid(zmSingleRotAxPlnAtm(3, iFrg), iFrg))
      CALL WDialogPutReal(IDF_a2, zmSingleRAIniOrFrac(1, iFrg))
      CALL WDialogPutReal(IDF_b2, zmSingleRAIniOrFrac(2, iFrg))
      CALL WDialogPutReal(IDF_c2, zmSingleRAIniOrFrac(3, iFrg))
      CALL WDialogPutReal(IDF_Alpha, zmSingleRAIniOrEuler(1, iFrg))
      CALL WDialogPutReal(IDF_Beta,  zmSingleRAIniOrEuler(2, iFrg))
      CALL WDialogPutReal(IDF_Gamma, zmSingleRAIniOrEuler(3, iFrg))
      CALL WDialogPutReal(IDF_Q0, zmSingleRAIniOrQuater(0, iFrg))
      CALL WDialogPutReal(IDF_Q1, zmSingleRAIniOrQuater(1, iFrg))
      CALL WDialogPutReal(IDF_Q2, zmSingleRAIniOrQuater(2, iFrg))
      CALL WDialogPutReal(IDF_Q3, zmSingleRAIniOrQuater(3, iFrg))
      CALL WDialogFieldStateLogical(IDF_GROUP2,      tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_RotAxAtom,   tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_RotAxFrac,   tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_RotAxPln,    tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_GROUP3,      tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_IniOrAxis,   tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_IniOrEuler,  tUseSingleAxis)
      CALL WDialogFieldStateLogical(IDF_IniOrQuater, tUseSingleAxis)
      iOpt1State = Disabled
      iOpt2State = Disabled
      iOpt3State = Disabled
      SELECT CASE (zmSingleRotAxDef(iFrg))
        CASE (1)
          CALL WDialogPutRadioButton(IDF_RotAxAtom)
          IF (.NOT. UseQuaternions(iFrg)) iOpt1State = Enabled
        CASE (2)
          CALL WDialogPutRadioButton(IDF_RotAxFrac)
          IF (.NOT. UseQuaternions(iFrg)) iOpt2State = Enabled
        CASE (3)
          CALL WDialogPutRadioButton(IDF_RotAxPln)
          IF (.NOT. UseQuaternions(iFrg)) iOpt3State = Enabled
      END SELECT
      CALL WDialogFieldState(IDF_AtomNr1,      iOpt1State)
      CALL WDialogFieldState(IDF_LABEL12,      iOpt1State)
      CALL WDialogFieldState(IDF_AtomNr2,      iOpt1State)
      CALL WDialogFieldState(IDF_LABELa,       iOpt2State)
      CALL WDialogFieldState(IDF_LABELb,       iOpt2State)
      CALL WDialogFieldState(IDF_LABELc,       iOpt2State)
      CALL WDialogFieldState(IDF_a1,           iOpt2State)
      CALL WDialogFieldState(IDF_b1,           iOpt2State)
      CALL WDialogFieldState(IDF_c1,           iOpt2State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm1, iOpt3State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm2, iOpt3State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm3, iOpt3State)
      iOpt1State = Disabled
      iOpt2State = Disabled
      iOpt3State = Disabled
      SELECT CASE (zmSingleRAIniOrDef(iFrg))
        CASE (1)
          CALL WDialogPutRadioButton(IDF_IniOrAxis)
          IF (.NOT. UseQuaternions(iFrg)) iOpt1State = Enabled
        CASE (2)
          CALL WDialogPutRadioButton(IDF_IniOrEuler)
          IF (.NOT. UseQuaternions(iFrg)) iOpt2State = Enabled
        CASE (3)
          CALL WDialogPutRadioButton(IDF_IniOrQuater)
          IF (.NOT. UseQuaternions(iFrg)) iOpt3State = Enabled
      END SELECT
      CALL WDialogFieldState(IDF_a2,    iOpt1State)
      CALL WDialogFieldState(IDF_b2,    iOpt1State)
      CALL WDialogFieldState(IDF_c2,    iOpt1State)
      CALL WDialogFieldState(IDF_Alpha, iOpt2State)
      CALL WDialogFieldState(IDF_Beta,  iOpt2State)
      CALL WDialogFieldState(IDF_Gamma, iOpt2State)
      CALL WDialogFieldState(IDF_Q0,    iOpt3State)
      CALL WDialogFieldState(IDF_Q1,    iOpt3State)
      CALL WDialogFieldState(IDF_Q2,    iOpt3State)
      CALL WDialogFieldState(IDF_Q3,    iOpt3State)
      CALL PopActiveWindowID

      END SUBROUTINE zmRotCopyTemp2Dialog
!
!*****************************************************************************
!
      SUBROUTINE zmCopyDialog2Temp

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE

      INTEGER, EXTERNAL :: ElmSymbol2CSD
      INTEGER iFrg, iRow, iAtomNr

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
! Fill grid with atom properties
!U! Set number of rows
!U      CALL WGridRows(IDF_AtomPropGrid,natoms(iFrg))
      DO iRow = 1, natoms(iFrg)
        iAtomNr = izmbid(iRow, iFrg)
! atom labels
        CALL WGridGetCellString(IDF_AtomPropGrid, 1, iRow, OriginalLabel(iAtomNr, iFrg))
! atom elements
        CALL WGridGetCellString(IDF_AtomPropGrid, 3, iRow, ElSym(iAtomNr, iFrg))
        zmElementCSD(iAtomNr, iFrg) = ElmSymbol2CSD(ElSym(iAtomNr, iFrg))
! Biso
        CALL WGridGetCellReal(IDF_AtomPropGrid, 4, iRow, tiso(iAtomNr, iFrg))
! occupancies
        CALL WGridGetCellReal(IDF_AtomPropGrid, 5, iRow, occ(iAtomNr, iFrg))
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE zmCopyDialog2Temp
!
!*****************************************************************************
!
      SUBROUTINE zmRotCopyDialog2Temp

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE 
      
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER iFrg, iOption, tInteger

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      iFrg = 0
      CALL WDialogGetRadioButton(IDF_RotOrgCOM, iOption)
      SELECT CASE (iOption)
        CASE (1) ! C.O.M.
          icomflg(iFrg) = 0
        CASE (2) ! Atom number
          CALL WDialogGetInteger(IDF_RotOrgAtomNr,tInteger)
          icomflg(iFrg) = izmbid(tInteger, iFrg)
      END SELECT
      UseQuaternions(iFrg) = .NOT. WDialogGetCheckBoxLogical(IDF_UseSingleAxis)
      CALL WDialogGetRadioButton(IDF_RotAxAtom, zmSingleRotAxDef(iFrg))
      CALL WDialogGetInteger(IDF_AtomNr1, tInteger)
      zmSingleRotAxAtm(1, iFrg) = izmbid(tInteger, iFrg)
      CALL WDialogGetInteger(IDF_AtomNr2, tInteger)
      zmSingleRotAxAtm(2, iFrg) = izmbid(tInteger, iFrg)
      CALL WDialogGetReal(IDF_a1, zmSingleRotAxFrac(1, iFrg))
      CALL WDialogGetReal(IDF_b1, zmSingleRotAxFrac(2, iFrg))
      CALL WDialogGetReal(IDF_c1, zmSingleRotAxFrac(3, iFrg))
      CALL WDialogGetInteger(IDF_RotAxPlnAtm1, tInteger)
      zmSingleRotAxPlnAtm(1, iFrg) = izmbid(tInteger, iFrg)
      CALL WDialogGetInteger(IDF_RotAxPlnAtm2, tInteger)
      zmSingleRotAxPlnAtm(2, iFrg) = izmbid(tInteger, iFrg)
      CALL WDialogGetInteger(IDF_RotAxPlnAtm3, tInteger)
      zmSingleRotAxPlnAtm(3, iFrg) = izmbid(tInteger, iFrg)
      CALL WDialogGetRadioButton(IDF_IniOrAxis, zmSingleRAIniOrDef(iFrg))
      CALL WDialogGetReal(IDF_a2, zmSingleRAIniOrFrac(1, iFrg))
      CALL WDialogGetReal(IDF_b2, zmSingleRAIniOrFrac(2, iFrg))
      CALL WDialogGetReal(IDF_c2, zmSingleRAIniOrFrac(3, iFrg))
      CALL WDialogGetReal(IDF_Alpha, zmSingleRAIniOrEuler(1, iFrg))
      CALL WDialogGetReal(IDF_Beta , zmSingleRAIniOrEuler(2, iFrg))
      CALL WDialogGetReal(IDF_Gamma, zmSingleRAIniOrEuler(3, iFrg))
      CALL WDialogGetReal(IDF_Q0, zmSingleRAIniOrQuater(0, iFrg))
      CALL WDialogGetReal(IDF_Q1, zmSingleRAIniOrQuater(1, iFrg))
      CALL WDialogGetReal(IDF_Q2, zmSingleRAIniOrQuater(2, iFrg))
      CALL WDialogGetReal(IDF_Q3, zmSingleRAIniOrQuater(3, iFrg))
      CALL zmDoAdmin(iFrg)
      CALL PopActiveWindowID

      END SUBROUTINE zmRotCopyDialog2Temp
!
!*****************************************************************************
!
      SUBROUTINE zmView(iFrg)

      USE WINTERACTER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER, EXTERNAL :: WriteMol2
      LOGICAL, EXTERNAL :: Get_ColourFlexibleTorsions
      INTEGER I
      CHARACTER(MaxPathLength) temp_file
      INTEGER atom
      INTEGER Element
      INTEGER NumOfFlexTorsions
      INTEGER tLength, BondNr

      natcry = NATOMS(iFrg)
      CALL makexyz(natcry,BLEN(1, iFrg),ALPH(1, iFrg),BET(1, iFrg),IZ1(1, iFrg),IZ2(1, iFrg),IZ3(1, iFrg),axyzo)
      DO I = 1, natcry
        aelem(I) = zmElementCSD(I, iFrg)
        atomlabel(I) = OriginalLabel(I, iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr, iFrg)
        bond(BondNr,1) = Bonds(1,BondNr, iFrg)
        bond(BondNr,2) = Bonds(2,BondNr, iFrg)
      ENDDO
! Q & D hack to display flexible torsion angles in different colors by forcing different
! element types.
      IF (Get_ColourFlexibleTorsions() .AND. (natcry.GE.4)) THEN
        DO I = 1, natcry
          aelem(I) = 1       ! Carbon        Grey
        ENDDO
        NumOfFlexTorsions = 0
        DO atom = 4, natcry
          IF (ioptt(atom, iFrg) .EQ. 1) THEN
            NumOfFlexTorsions = NumOfFlexTorsions + 1
            SELECT CASE(NumOfFlexTorsions)
              CASE (1)
                Element = 81 ! Sulphur       Yellow
              CASE (2)
                Element = 64 ! Oxygen        Red
              CASE (3)
                Element = 23 ! Cobalt        Blue
              CASE (4)
                Element = 21 ! Chlorine      Green
              CASE (5)
                Element = 56 ! Nitrogen      Light blue
              CASE (6)
                Element = 16 ! Bromine       Brown
              CASE (7)
                Element = 43 ! Iodine        Pink
              CASE (8)
                Element =  2 ! Hydrogen      White
              CASE (9)
                Element = 66 ! Phosphorus
            END SELECT
            aelem(atom) = Element
            aelem(IZ1(atom, iFrg)) = Element
            aelem(IZ2(atom, iFrg)) = Element
            aelem(IZ3(atom, iFrg)) = Element
          ENDIF
        ENDDO
      ENDIF
      tLength = LEN_TRIM(frag_file(iFrg))
      temp_file = frag_file(iFrg)(1:tLength-8)//'_temp.mol2'
! Show the mol2 file
      IF (WriteMol2(temp_file,.TRUE., iFrg) .EQ. 1) THEN
        CALL ViewStructure(temp_file)
      ELSE
        CALL DebugErrorMessage('Error writing temporary file.')
      ENDIF
      CALL IOSDeleteFile(temp_file)

      END SUBROUTINE zmView
!
!*****************************************************************************
!
      SUBROUTINE zmRelabel(iFrg)

! This routine re-labels atoms in Z-matrix number iFrg
! The new labels consist of element + sequential number
! Note: it's less convenient to have C1, C2, ..., N1, N2, because we need the atom numbers
! when defining rotations

      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      CHARACTER*(20), EXTERNAL :: Integer2String
      INTEGER iAtomNr
      CHARACTER*(20) LastNumberSoFarStr

      DO iAtomNr = 1, natoms(iFrg)
        LastNumberSoFarStr = Integer2String(izmoid(iAtomNr, iFrg))
        OriginalLabel(iAtomNr, iFrg) = ElSym(iAtomNr, iFrg)(1:LEN_TRIM(ElSym(iAtomNr, iFrg)))// &
                                       LastNumberSoFarStr(1:LEN_TRIM(LastNumberSoFarStr))
      ENDDO
      CALL zmDoAdmin(iFrg)

      END SUBROUTINE zmRelabel
!
!*****************************************************************************
!
      SUBROUTINE zmRelabelAll
!
! This routine re-labels the atoms in all Z-matrices
! The new labels consist of element + sequential number
!
! In the interface, these labels are displayed in two dialogues:
!  - the SA Parameter boundaries Wizard window
!  - the Rietveld refinement window.

      USE ZMVAR

      IMPLICIT NONE

      INTEGER iFrg

      DO iFrg = 1, nFrag
        CALL zmRelabel(iFrg)
      ENDDO

      END SUBROUTINE zmRelabelAll
!
!*****************************************************************************
!
      SUBROUTINE zmReOrder(iFrg)

! This routine re-orders atoms in Z-matrix iFrg
! The new order is: Carbons first, rest in alphabetical order, Hydrogens last
! Extremely simple implementation: keep swapping two consecutive entries until no more swaps

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
          IF ((zmElementCSD(izmbid(iAtomNr  , iFrg), iFrg) .EQ. zmElementCSD(izmbid(iAtomNr+1, iFrg), iFrg))) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, never swap if first is Carbon or second is Hydrogen
          ELSE IF ((zmElementCSD(izmbid(iAtomNr  , iFrg), iFrg) .EQ. 1) .OR. (zmElementCSD(izmbid(iAtomNr+1, iFrg), iFrg) .EQ. 2)) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, always swap if second is Carbon or first is Hydrogen
          ELSE IF ((zmElementCSD(izmbid(iAtomNr+1, iFrg), iFrg) .EQ. 1) .OR. (zmElementCSD(izmbid(iAtomNr  , iFrg), iFrg) .EQ. 2)) THEN
            ShouldBeSwapped = .TRUE.
! Otherwise, swap if first > second
          ELSE
            ShouldBeSwapped = ((ElSym(izmbid(iAtomNr  , iFrg), iFrg) .GT. ElSym(izmbid(iAtomNr+1, iFrg), iFrg)))
          ENDIF
          IF (ShouldBeSwapped) THEN
            iTem                   = izmbid(iAtomNr  , iFrg)
            izmbid(iAtomNr  , iFrg) = izmbid(iAtomNr+1, iFrg)
            izmbid(iAtomNr+1, iFrg) = iTem
            Swaps = .TRUE.
          ENDIF
        ENDDO
      ENDDO
      DO iAtomNr = 1, natoms(iFrg)
        izmoid(izmbid(iAtomNr, iFrg), iFrg) = iAtomNr ! the backward mapping
      ENDDO

      END SUBROUTINE zmReOrder
!
!*****************************************************************************
!
      INTEGER FUNCTION zmSaveAs(iFrg)

! This routine saves Z-matrix number iFrg
!
! RETURNS : 0 for success
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER, EXTERNAL :: zmSave
      CHARACTER(MaxPathLength) :: zmFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFLAGS
      
! Save the Z-matrix
      zmSaveAs = 1 ! Failure
      iFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Z-matrix files (*.zmatrix)|*.zmatrix|'
      zmFileName = frag_file(iFrg)
      CALL WSelectFile(FILTER, iFLAGS, zmFileName, 'Save Z-matrix')
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
!
! RETURNS : 0 for success
!
      USE WINTERACTER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER, EXTERNAL :: zmRebuild
      INTEGER tFileHandle, i
      
! Save the Z-matrix
      zmSave = 1 ! Failure
! If an atom has been deleted, try to rebuild the Z-matrix
      IF (zmRebuild() .NE. 0) THEN
        CALL ErrorMessage('Could not rebuild Z-matrix.')
        RETURN
      ENDIF
      tFileHandle = 19
      OPEN (UNIT=tFileHandle, FILE=frag_file(iFrg), ERR=999)
! First line is a title
      WRITE (tFileHandle, '(A)', ERR=999) 'Z-matrix generated by '//ProgramVersion
! Second line contains unit cell parameters
!1.0 1.0 1.0 90.0 90.0 90.0
! These are never used, so just write dummy values.
      WRITE (tFileHandle, '(A)', ERR=999) '1.0 1.0 1.0 90.0 90.0 90.0'
! Third line contains number of atoms and an integer IAT, defining the centre for the rotations.
! IAT = 0        : use centre of mass
! IAT = non-zero : use atom nr. IAT   (necessary if atom on special position).
!  59   0
      WRITE (tFileHandle, '(1X,I3,1X,I3)', ERR=999) natoms(iFrg), icomflg(iFrg)
! Remaining lines contain the Z-matrix
!  C      1.5152617  0  113.2370014  0 -179.8250018  0   54   51   48  3.0  1.0   58 C6 C7 C8 C9
      DO i = 1, natoms(iFrg)
        IF (i .EQ. 1) THEN
          WRITE (tFileHandle,'(2X,A2,X,3(F13.7,2X,I1),3I5,2F7.3,I5,1X,A5)',ERR=999) ElSym(i, iFrg), &
                blen(i, iFrg), ioptb(i, iFrg), alph(i, iFrg), iopta(i, iFrg), bet(i, iFrg), ioptt(i, iFrg), &
                iz1(i, iFrg), iz2(i, iFrg), iz3(i, iFrg),   &
                tiso(i, iFrg), occ(i, iFrg), izmoid(i, iFrg), OriginalLabel(i, iFrg)
        ELSE IF (i .EQ. 2) THEN
          WRITE (tFileHandle,'(2X,A2,X,3(F13.7,2X,I1),3I5,2F7.3,I5,1X,A5,1X,A5)',ERR=999) ElSym(i, iFrg), &
                blen(i, iFrg), ioptb(i, iFrg), alph(i, iFrg), iopta(i, iFrg), bet(i, iFrg), ioptt(i, iFrg), &
                iz1(i, iFrg), iz2(i, iFrg), iz3(i, iFrg),   &
                tiso(i, iFrg), occ(i, iFrg), izmoid(i, iFrg), OriginalLabel(i, iFrg), OriginalLabel(iz1(i, iFrg), iFrg)
        ELSE IF (i .EQ. 3) THEN
          WRITE (tFileHandle,'(2X,A2,X,3(F13.7,2X,I1),3I5,2F7.3,I5,1X,A5,1X,A5,1X,A5)',ERR=999) ElSym(i, iFrg), &
                blen(i, iFrg), ioptb(i, iFrg), alph(i, iFrg), iopta(i, iFrg), bet(i, iFrg), ioptt(i, iFrg), &
                iz1(i, iFrg), iz2(i, iFrg), iz3(i, iFrg),   &
                tiso(i, iFrg), occ(i, iFrg), izmoid(i, iFrg), OriginalLabel(i, iFrg), OriginalLabel(iz1(i, iFrg), iFrg), &
                OriginalLabel(iz2(i, iFrg), iFrg)
        ELSE
          WRITE (tFileHandle,'(2X,A2,X,3(F13.7,2X,I1),3I5,2F7.3,I5,1X,A5,1X,A5,1X,A5,1X,A5)',ERR=999) ElSym(i, iFrg), &
                blen(i, iFrg), ioptb(i, iFrg), alph(i, iFrg), iopta(i, iFrg), bet(i, iFrg), ioptt(i, iFrg), &
                iz1(i, iFrg), iz2(i, iFrg), iz3(i, iFrg),   &
                tiso(i, iFrg), occ(i, iFrg), izmoid(i, iFrg), OriginalLabel(i, iFrg), OriginalLabel(iz1(i, iFrg), iFrg), &
                OriginalLabel(iz2(i, iFrg), iFrg), OriginalLabel(iz3(i, iFrg), iFrg)
        ENDIF
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

      IMPLICIT NONE      

      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical
      INTEGER tFieldState
      INTEGER h, k, l

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page2)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Ungrey 'Load DASH Pawley file' button on toolbar
              CALL WMenuSetState(ID_import_dpj_file, ItemEnabled, WintOn)
              CALL WizardWindowShow(IDD_SAW_Page1)
            CASE (IDNEXT)
              IF (WDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                CALL WDialogGetInteger(IDF_PO_a, h)
                CALL WDialogGetInteger(IDF_PO_b, k)
                CALL WDialogGetInteger(IDF_PO_c, l)
                IF ((h .EQ. 0) .AND. (k .EQ. 0) .AND. (l .EQ. 0)) THEN
                  CALL ErrorMessage("h, k and l cannot all be zero.")
                  RETURN
                ENDIF
              ENDIF
              CALL SA_Parameter_Set
              CALL ShowWizardWindowParameterBounds
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Use_PO)
              IF (WDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
              ENDIF
              CALL WDialogFieldState(IDF_PO_a, tFieldState)
              CALL WDialogFieldState(IDF_PO_b, tFieldState)
              CALL WDialogFieldState(IDF_PO_c, tFieldState)
              CALL WDialogFieldState(IDF_LABELa, tFieldState)
              CALL WDialogFieldState(IDF_LABELb, tFieldState)
              CALL WDialogFieldState(IDF_LABELc, tFieldState)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowAdditionalSAParams
!
!*****************************************************************************
!
      SUBROUTINE ShowWizardWindowParameterBounds

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE      

      INCLUDE 'PARAMS.INC'

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      CHARACTER*20, EXTERNAL :: Integer2String
      INTEGER I, iCheck, iFrg, KK
      CHARACTER(LEN=3) :: MenuOptions(1:maxfrg+1)
      CHARACTER*20 tStr

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Modal_input2)
      DO I = 1, NVAR
        CALL WGridGetCellReal(IDF_parameter_grid_modal, 1, I, prevx(I))
        CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, 4, I, iCheck)
        IF (iCheck .EQ. UnChecked) THEN
          CALL WGridGetCellReal(IDF_parameter_grid_modal, 2, I, prevlb(I))
          CALL WGridGetCellReal(IDF_parameter_grid_modal, 3, I, prevub(I))
        ENDIF
! Disable modal button for everything but torsion angles, angles and bonds
! This allows angles and bonds to be searched in Mogul too.
!O        IF (ModalFlag(i) .EQ. 0) CALL WGridStateCell(IDF_parameter_grid_modal, 5, i, DialogReadOnly)
         IF ((kzmpar2(i) .LT. 3) .OR. (kzmpar2(i) .GT. 5)) THEN
           CALL WGridStateCell(IDF_parameter_grid_modal, 5, i, DialogReadOnly)
         ELSE
           CALL WGridStateCell(IDF_parameter_grid_modal, 5, i, Enabled)
         ENDIF
      ENDDO
      LimsChanged = .FALSE.
      KK = 0
      DO iFrg = 1, nFrag
        KK = KK + 1
        tStr = Integer2String(iFrg)
        MenuOptions(KK) = tStr(1:3)
      ENDDO
      MenuOptions(KK+1) = "All"
      CALL WDialogPutMenu(IDF_MENU1, MenuOptions, nfrag+1, 1)
      CALL WizardWindowShow(IDD_SA_Modal_input2)
      CALL PopActiveWindowID

      END SUBROUTINE ShowWizardWindowParameterBounds
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowParameterBounds

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INCLUDE 'PARAMS.INC'

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical
      LOGICAL, EXTERNAL :: NearlyEqual
      REAL    xtem
      INTEGER IFCOl, IFRow, ICHK
      INTEGER I
      INTEGER iRow, iStatus
      INTEGER iFrg
      INTEGER kk, iOption, jFrg
      CHARACTER*36 parlabel(mvar)

! We are now on window number 2
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Modal_input2)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the 1st window
! JCC Check if the limits have changed and warn about it 
              IF (LimsChanged) THEN
                IF (Confirm("Note: Going back will erase the edits made to the current parameters, overwrite changes?")) THEN
                  LimsChanged = .FALSE. 
                  DO I = 1, nvar
                    ModalFlag(I) = 0 ! 0 means: not a torsion angle
                    CALL WGridColourRow(IDF_parameter_grid_modal, I, WIN_RGB(256, 256, 256), WIN_RGB(256, 256, 256))
                  ENDDO
                ENDIF                 
              ENDIF
              IF (.NOT. LimsChanged) THEN
! If the user has requested preferred orientation, make sure we pass the pertinent Wizard window
                CALL WDialogSelect(IDD_SAW_Page2)
                IF (WDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                  CALL WizardWindowShow(IDD_SAW_Page2)
                ELSE
                  CALL WizardWindowShow(IDD_SAW_Page1)
                ENDIF
              ENDIF
            CASE (IDNEXT)
! Go to the next stage of the SA input
              CALL ShowWithWizardWindowSASettings
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDB_Relabel)
              CALL WDialogGetMenu(IDF_MENU1, iOption)
              ! Update memory
              IF (iOption .EQ. nfrag+1) THEN
                CALL zmRelabelAll
              ELSE
                KK = 0
                jFrg = -1
                DO iFrg = 1, nFrag
                  KK = KK + 1
                  IF (KK .EQ. iOption) jFrg = iFrg 
                ENDDO
                IF (jFrg .EQ. -1) CALL DebugErrorMessage("jFrg .EQ. -1")
                CALL zmRelabel(jFrg)
              ENDIF
              ! Update current Wizard window
              ! Run through all possible fragments
              kk = 0
              DO iFrg = 1, nFrag
                DO i = 1, izmpar(iFrg)
                  kk = kk + 1
                  parlabel(kk) = czmpar(i, iFrg)
                ENDDO
              ENDDO
              ! Note that we do not update "Preferred orientation"
              DO i = 1, kk
                CALL WGridLabelRow(IDF_parameter_grid_modal, i, parlabel(i))
              ENDDO
            CASE (IDB_View)
              CALL WDialogGetMenu(IDF_MENU1, iOption)
              ! Update memory
              IF (iOption .EQ. nFrag+1) THEN ! "All"
                DO iFrg = 1, nFrag
                  CALL zmView(iFrg)
                ENDDO
              ELSE
                CALL zmView(iOption)
              ENDIF
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_parameter_grid_modal)
              CALL WGridPos(EventInfo%X,IFCol,IFRow)
              SELECT CASE (IFCol)
                CASE (1) ! parameter
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, 4, IFRow, ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal, IFCol, IFRow, xtem)
                    xtem = MAX(xtem,prevlb(IFRow))
                    xtem = MIN(xtem,prevub(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevx(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IFRow, xtem, '(F12.5)')
                      prevx(IFRow) = xtem
                    ENDIF
                  ENDIF
                CASE (2) ! lower bound
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, 4, IFRow, ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal, IFCol, IFRow, xtem)
                    xtem = MIN(xtem,prevub(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevlb(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, IFRow, xtem, '(F12.5)')
                      prevlb(IFRow) = xtem
                      lb(IFRow) = xtem
                    ENDIF
                    xtem = MAX(prevlb(IFRow),prevx(IFRow))
                    CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IFRow, xtem, '(F12.5)')
                    prevx(IFRow) = xtem
                  ENDIF
                CASE (3) ! upper bound
! Check the bounding - only update if parameter is set to vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, 4, IFRow, ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal, IFCol, IFRow, xtem)
                    xtem = MAX(xtem,prevlb(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevub(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, IFRow, xtem, '(F12.5)')
                      prevub(IFRow) = xtem
                      ub(IFRow) = xtem
                    ENDIF
                    xtem = MIN(prevub(IFRow),prevx(IFRow))
                    CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IFRow, xtem, '(F12.5)')
                    prevx(IFRow) = xtem
                  ENDIF
                CASE (4) ! fix or vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, IFCol, IFRow, ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal,1,IFRow,xtem)
                    lb(IFRow) = xtem-1.0E-5
                    ub(IFRow) = xtem+1.0E-5
                    CALL WGridStateCell(IDF_parameter_grid_modal, 1, IFRow, DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 2, IFRow, DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 3, IFRow, DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 5, IFRow, Disabled)
                  ELSE
                    lb(IFRow) = prevlb(IFRow)
                    ub(IFRow) = prevub(IFRow)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 1, IFRow, Enabled)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 2, IFRow, Enabled)
                    CALL WGridStateCell(IDF_parameter_grid_modal, 3, IFRow, Enabled)
                    IF (ModalFlag(IFRow) .NE. 0) THEN ! It's a torsion angle
                      CALL WGridStateCell(IDF_parameter_grid_modal, 5, IFRow, Enabled)
                    ENDIF
                  ENDIF
                  CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, IFRow, lb(IFRow), '(F12.5)')
                  CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, IFRow, ub(IFRow), '(F12.5)')
                  LimsChanged = .TRUE.
              END SELECT ! IFCol
          END SELECT ! EventInfo%Value1 Field Changed Options
      END SELECT  ! EventType
! Modal Button
      DO iRow = 1, NVAR
	    iStatus = 0
        CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, 5, iRow, iStatus)
        IF (iStatus .EQ. Checked) THEN
          CALL WGridGetCellReal(IDF_parameter_grid_modal, 1, IFRow, xtem)  	      
		  CALL WriteMogulMol2(iFRow) !Call Mogul
          IF (kzmpar2(IFrow) .EQ. 3) THEN ! Modal Torsion Angle so show dialog
            CALL ShowBiModalDialog(IFRow, xtem)
          ELSE
            CALL WGridPutCellCheckBox(IDF_parameter_grid_modal, 5, iRow, 0)
          ENDIF
        ENDIF
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowParameterBounds      
!
!*****************************************************************************
!
! This window needs some initialisation and can be called from more than one location
! now that we can load old solutions.
      SUBROUTINE ShowWithWizardWindowSASettings

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER NMoves

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3_2)
      CALL WDialogPutReal(IDF_SA_T0, T0, '(F7.2)')
      CALL WDialogPutReal(IDF_SA_Tredrate, RT, '(F6.3)')
      CALL WDialogPutInteger(IDF_SA_NS, NS)
      CALL WDialogPutInteger(IDF_SA_NT, NT)
      NMoves = NT * NS * NVAR
      CALL WDialogPutInteger(IDF_SA_Moves, NMoves)
      CALL WizardWindowShow(IDD_SA_input3_2)
      CALL SelectMode(ID_Structure_Solution_Mode)
      CALL PopActiveWindowID

      END SUBROUTINE ShowWithWizardWindowSASettings      
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

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      INTEGER, EXTERNAL :: WriteSAParametersToFile
      INTEGER IHANDLE, KPOS

! We are now on window number 3
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3_2)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the 2nd window
              CALL WizardWindowShow(IDD_SA_Modal_input2)
            CASE (IDB_SA3_finish) ! 'Solve >' button
! We've finished the SA input
              ! It is possible to click "Resume SA" after having completed all runs and to
              ! forget to specify more runs. That way, we will already have completed all runs.
              CALL WDialogGetInteger(IDF_SA_MaxRepeats, MaxRuns)
              IF (Resume_SA .AND. (NumOf_SA_Runs .GE. MaxRuns)) THEN
                CALL InfoMessage("Number of requested runs already completed: please increase number of runs.")
              ELSE
                CALL WizardWindowHide
                CALL BeginSA
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDF_PrintSA)
              IF (WriteSAParametersToFile('SA_PARAMS.TXT') .EQ. 0) THEN
                CALL WindowOpenChild(IHANDLE)
                CALL WEditFile('SA_PARAMS.TXT', Modeless, 0, FileMustExist, 4)
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
            CASE (IDF_SA_T0) 
              CALL WDialogGetReal(IDF_SA_T0, T0)
            CASE (IDF_SA_NS) 
              CALL WDialogGetInteger(IDF_SA_NS, NS)
              KPOS = NS * NT * NVAR
              CALL WDialogPutInteger(IDF_SA_Moves, KPOS)
            CASE (IDF_SA_NT) 
              CALL WDialogGetInteger(IDF_SA_NT, NT)
              KPOS = NS * NT * NVAR
              CALL WDialogPutInteger(IDF_SA_Moves, KPOS)
            CASE (IDF_SA_RandomSeed1) 
              CALL WDialogGetInteger(IDF_SA_RandomSeed1, ISeed1)
            CASE (IDF_SA_RandomSeed2) 
              CALL WDialogGetInteger(IDF_SA_RandomSeed2, ISeed2)
            CASE (IDF_SA_Tredrate)
              CALL WDialogGetReal(IDF_SA_Tredrate, RT)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowSASettings
!
!*****************************************************************************
!
      SUBROUTINE ShowBimodalDialog(IFrow, Xinitial)

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: IFrow
      REAL,    INTENT (IN   ) :: Xinitial

      INCLUDE 'PARAMS.INC'

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      INTEGER ICol, NumColumns
      INTEGER i, k, frag, dof
      INTEGER Upper, Lower
      REAL    Zero, OneEighty, xtem

! Initialise variables          
      ICol = 0
      NumColumns = 3
      Upper = 1
      Lower = 2
      Zero = 0.0000
      OneEighty = 180.0000
!     Given the number of the parameter want to know
!     which zmatrix, fragment it belongs to.
      frag = 0
      DO i = 1, maxDOF
        DO k = 1, nfrag
          IF (IFRow .EQ. zm2par(i,k)) THEN
            dof = i
            frag = k
            EXIT
          ENDIF
        ENDDO
        IF (frag .NE. 0) EXIT
      ENDDO
      CALL WDialogSelect(IDD_ModalDialog)
!     Clear Fields
      CALL WDialogClearField(IDF_ModalUpper)
      CALL WDialogClearField(IDF_ModalLower)
      CALL WDialogClearField(IDF_ReportLower1)
      CALL WDialogClearField(IDF_ReportLower2)
      CALL WDialogClearField(IDF_ReportUpper1)
      CALL WDialogClearField(IDF_ReportUpper2)

!     Initialise fields 
      CALL WDialogPutString(IDF_TorsionName, czmpar(dof,frag))
      CALL WDialogPutReal(IDF_Initial, Xinitial, '(F12.5)')
      IF (ModalFlag(IfRow) .EQ. 1) THEN ! Not been set before
        CALL WDialogPutRadioButton(IDF_BiModalRadio) 
        IF (XInitial .GE. 0.00) THEN
          CALL WDialogPutReal(IDF_ModalLower, Zero, '(F12.5)')
          CALL WDialogPutReal(IDF_ModalUpper, OneEighty, '(F12.5)')
          CALL WDialogPutReal(IDF_ReportLower1, -OneEighty, '(F12.5)')
          CALL WDialogPutReal(IDF_ReportUpper1, Zero,'(F12.5)')
        ELSE
          CALL WDialogPutReal(IDF_ModalLower, -OneEighty, '(F12.5)')
          CALL WDialogPutReal(IDF_ModalUpper, Zero, '(F12.5)')
          CALL WDialogPutReal(IDF_ReportLower1, Zero, '(F12.5)')
          CALL WDialogPutReal(IDF_ReportUpper1, OneEighty, '(F12.5)')
        ENDIF
      ELSE
        CALL WDialogPutReal(IDF_ModalLower, lb(IFRow), '(F12.5)')
        CALL WDialogPutReal(IDF_ModalUpper, ub(IFRow), '(F12.5)')
        IF (ModalFlag(IFRow) .EQ. 2) THEN
          CALL WDialogPutRadioButton(IDF_BiModalRadio)
          IF ((UB(IFRow) * LB(IFRow)) .LT. 0.00) THEN
            CALL WDialogGetReal(IDF_ModalUpper, xtem)
            CALL WDialogPutReal(IDF_ReportLower1, (xtem - 180.0))
            CALL WDialogGetReal(IDF_ModalLower, xtem)
            CALL WDialogPutReal(IDF_ReportUpper1, (xtem + 180.0))
          ELSE
            CALL WDialogGetReal(IDF_ModalUpper, xtem)
            CALL WDialogPutReal(IDF_ReportLower1, -xtem)
            CALL WDialogGetReal(IDF_ModalLower, xtem)
            CALL WDialogPutReal(IDF_ReportUpper1, -xtem)
          ENDIF
        ELSEIF (ModalFlag(IFRow) .EQ. 3) THEN
          CALL WDialogPutRadioButton(IDF_TriModalRadio)          
          CALL WDialogGetReal(IDF_ModalUpper, xtem)
          CALL DetermineTrimodalBounds(xtem, Upper)              
          CALL WDialogGetReal(IDF_ModalLower, xtem)
          CALL DetermineTrimodalBounds(xtem, Lower)
          CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
          CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
          CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
          CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))          
        ENDIF
      ENDIF
      CALL WDialogShow(-1, -1, 0, SemiModeless)
      RowNumber = IFRow
      iUB = UB(IFRow)
      iLB = LB(IFRow)
      ix = X(IFRow)
      iRadio = ModalFlag(IFRow)

      END SUBROUTINE ShowBimodalDialog
!
!*****************************************************************************
!
      SUBROUTINE DealWithBimodalDialog

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE 
           
      INCLUDE 'PARAMS.INC'

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      LOGICAL, EXTERNAL :: OutOfBounds
      INTEGER ICol, NumColumns, ISET
      INTEGER Upper, Lower
      REAL    Zero, OneEighty, xtem, ttem
      REAL TempPrevub, TempPrevlb, TempPrevx

! Initialise variables          
      ICol = 0
      NumColumns = 3
      Upper = 1
      Lower = 2
      Zero = 0.0000
      OneEighty = 180.0000
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_ModalDialog)
      SELECT CASE (EventType) 
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_BiModalRadio)
              CALL WDialogClearField(IDF_ReportUpper2)
              CALL WDialogClearField(IDF_ReportLower2)
              CALL WDialogGetReal(IDF_ModalUpper, xtem)
              CALL WDialogGetReal(IDF_ModalLower, ttem)
              IF (xtem*ttem .LT. 0.00) THEN
                xtem = MAX(xtem, ttem)
                xtem = xtem - 180.00
                CALL WDialogPutReal(IDF_ReportLower1, xtem)
                ttem = ttem + 180.00
                CALL WDialogPutReal(IDF_ReportUpper1, ttem)
              ELSE
                CALL WDialogGetReal(IDF_ModalUpper, xtem)
                CALL WDialogPutReal(IDF_ReportLower1, -xtem)
                CALL WDialogGetReal(IDF_ModalLower, xtem)
                CALL WDialogPutReal(IDF_ReportUpper1, -xtem)
              ENDIF
              ModalFlag(RowNumber) = 2
            CASE (IDF_TriModalRadio)
              CALL WDialogGetReal(IDF_ModalUpper, xtem)
              CALL DetermineTrimodalBounds(xtem, Upper)               
              CALL WDialogGetReal(IDF_ModalLower, xtem)
              CALL DetermineTrimodalBounds(xtem, Lower)
              CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
              CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
              CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
              CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))            
              ModalFlag(RowNumber) = 3 
            CASE (IDF_Initial)
              CALL WDialogGetReal(IDF_Initial, xtem)       
              TempPrevx = xtem
              xtem = MAX(lb(RowNumber),xtem)
              X(RowNumber) = (MIN(ub(RowNumber),xtem))
              CALL WDialogPutReal(IDF_Initial, x(RowNumber), '(F12.5)')
            CASE (IDF_ModalLower)
              CALL WDialogGetReal(IDF_ModalLower, xtem)
              xtem = MIN(ub(RowNumber),xtem)
              TempPrevlb = LB(RowNumber)               
              lb(RowNumber) = xtem
              CALL WDialogPutReal(IDF_ModalLower,lb(RowNumber),'(F12.5)')
! How ranges are calculated depends on state of Modal RadioButton  
              CALL WDialogGetRadioButton(IDF_BimodalRadio, ISET)
              SELECT CASE (ISET) ! Bimodal radiobutton active
                CASE (1)
                  CALL WDialogClearField(IDF_ReportLower2)
                  CALL WDialogClearField(IDF_ReportUpper2)
                  CALL WDialogGetReal(IDF_ModalUpper, xtem)
                  CALL WDialogGetReal(IDF_ModalLower, ttem)
                  IF (xtem*ttem .LT. 0.00) THEN
                    xtem = MAX(xtem, ttem)
                    xtem = xtem - 180.00
                    CALL WDialogPutReal(IDF_ReportLower1, xtem)
                    ttem = ttem + 180.00
                    CALL WDialogPutReal(IDF_ReportUpper1, ttem)
                  ELSE
                    CALL WDialogGetReal(IDF_ModalUpper, xtem)
                    CALL WDialogPutReal(IDF_ReportLower1, (xtem * (-1)))
                    CALL WDialogGetReal(IDF_ModalLower, xtem)
                    CALL WDialogPutReal(IDF_ReportUpper1, (xtem * (-1)))
                  ENDIF
                  ModalFlag(RowNumber) = 2  
                CASE (2) !Trimodal radiobutton active           
                  CALL WDialogGetReal(IDF_ModalLower, xtem)
                  CALL DetermineTrimodalBounds(xtem, Lower)
                  CALL WDialogGetReal(IDF_ModalUpper, xtem)
                  CALL DetermineTrimodalBounds(xtem, Upper)
                  CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
                  CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
                  CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
                  CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))
                  ModalFlag(RowNumber) = 3
              END SELECT
            CASE (IDF_ModalUpper)
! Check the bounding - only update if parameter is set to vary
              CALL WDialogGetReal(IDF_ModalUpper,xtem)
              xtem = MAX(lb(RowNumber),xtem)
              TempPrevUb = UB(RowNumber)             
              ub(RowNumber) = xtem
              CALL WDialogPutReal(IDF_ModalUpper, ub(RowNumber), '(F12.5)')
!             How ranges are calculated depends on state of Modal RadioButton      
              CALL WDialogGetRadioButton(IDF_BimodalRadio, ISET)
                 SELECT CASE (ISET) ! Bimodal Radiobutton active
                   CASE (1)
                     CALL WDialogClearField(IDF_ReportLower2)
                     CALL WDialogClearField(IDF_ReportUpper2)
                     CALL WDialogGetReal(IDF_ModalUpper, xtem)
                     CALL WDialogGetReal(IDF_ModalLower, ttem)
                      IF (xtem*ttem .LT. 0.00) THEN
                        xtem = MAX(xtem, ttem)
                        xtem = xtem - 180.00
                        CALL WDialogPutReal(IDF_ReportLower1, xtem)
                        ttem = ttem + 180.00
                        CALL WDialogPutReal(IDF_ReportUpper1, ttem)
                      ELSE
                        CALL WDialogGetReal(IDF_ModalUpper, xtem)
                        CALL WDialogPutReal(IDF_ReportLower1, (xtem * (-1)))
                        CALL WDialogGetReal(IDF_ModalLower, xtem)
                        CALL WDialogPutReal(IDF_ReportUpper1, (xtem * (-1)))
                      ENDIF
                      ModalFlag(RowNumber) = 2
                   CASE (2) !Trimodal Radiobutton active
                     CALL WDialogGetReal(IDF_ModalUpper, xtem)
                     CALL DetermineTrimodalBounds(xtem, Upper)               
                     CALL WDialogGetReal(IDF_ModalLower, xtem)
                     CALL DetermineTrimodalBounds(xtem, Lower)
                     ModalFlag(RowNumber) = 3
                     CALL WDialogPutReal(IDF_ReportUpper1, Tempbounds(2,Upper))
                     CALL WDialogPutReal(IDF_ReportUpper2, Tempbounds(3,Upper))
                     CALL WDialogPutReal(IDF_ReportLower1, Tempbounds(2,Lower))
                     CALL WDialogPutReal(IDF_ReportLower2, Tempbounds(3,Lower))
                   END SELECT
            END SELECT
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK)
!             Record parameters in appropriate arrays
              CALL WDialogGetReal(IDF_Initial, X(RowNumber))
              CALL WDialogGetReal(IDF_ModalLower, lb(RowNumber))
              CALL WDialogGetReal(IDF_ModalUpper, ub(RowNumber))
!             Check that x is in bounds
              IF (OutOfBounds(RowNumber, X(RowNumber))) THEN
                CALL WarningMessage('Initial value does not fall within defined ranges')
                IF (WInfoDialog(ExitButtonCommon) .EQ. CommonOk) THEN
                  CALL PopActiveWindowID
                  RETURN
                ENDIF 
              ENDIF
              CALL WDialogHide
              CALL WDialogSelect(IDD_SA_Modal_Input2)
              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(255, 0, 0), WIN_RGB(256, 256, 256))  
              LimsChanged = .TRUE.
!           Return bounds to previous values
            CASE (IDCANCEL)
              UB(RowNumber) = iUB
              LB(RowNumber) = iLB
              X(RowNumber) = iX
              ModalFlag(RowNumber) = iRadio
              CALL WDialogHide
!           Return to "unimodal" mode. Modal torsion angle is no longer applied
            CASE (IDF_BiModalReset)
              ub(RowNumber) = OneEighty
              lb(RowNumber) = (-1) * OneEighty
              X(RowNumber) = iX
              ModalFlag(RowNumber) = 1 
              CALL WDialogHide
              CALL WDialogSelect(IDD_SA_Modal_Input2)
              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(256, 256, 256), WIN_RGB(256, 256, 256))                                              
          END SELECT
          CALL WDialogClearField(IDF_MogulText)
          prevub(RowNumber) = UB(RowNumber)
          prevlb(RowNumber) = LB(RowNumber)
          CALL WDialogSelect(IDD_SA_Modal_Input2)
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, RowNumber, X(RowNumber))
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, RowNumber, LB(RowNumber))
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, RowNumber, UB(RowNumber)) 
          CALL WGridPutCellCheckBox(IDF_parameter_grid_modal,5, RowNumber, UnChecked)                          
      END SELECT
      CALL PopActiveWindowID
          
      END SUBROUTINE DealWithBimodalDialog
!
!*****************************************************************************
!
      SUBROUTINE ThreeSixtyToOneEighty(Angle)

      IMPLICIT NONE
      
      REAL, INTENT (INOUT) :: Angle

      IF (Angle .GT.  180.0) Angle = Angle - 360.0
      IF (Angle .LT. -180.0) Angle = Angle + 360.0

      END SUBROUTINE ThreeSixtyToOneEighty
!
!*****************************************************************************
!
      SUBROUTINE OneEightyToThreeSixty(Angle)

      IMPLICIT NONE
      
      REAL, INTENT (INOUT) :: Angle

      IF (Angle .LT. 0.0) Angle = Angle + 360.0

      END SUBROUTINE OneEightyToThreeSixty
!
!*****************************************************************************
!
      SUBROUTINE DetermineTriModalBounds(xtem, BoundColumn)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: BoundColumn

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      REAL xtem,ttem

      TempBounds(1,BoundColumn) = xtem
      CALL OneEightyToThreeSixty(xtem) 
      ttem = xtem + 120.0
      IF (ttem .GE. 360.0) ttem = ttem - 360.0
      CALL ThreeSixtyToOneEighty(ttem)
      TempBounds(2,BoundColumn) = ttem                  
      ttem = xtem + 240.0
      IF (ttem .GE. 360.0) ttem = ttem - 360.0
      CALL ThreeSixtyToOneEighty(ttem) 
      TempBounds(3,BoundColumn) = ttem

      CALL ThreeSixtyToOneEighty(xtem)

      END SUBROUTINE DetermineTrimodalBounds
!
!*****************************************************************************
!
      SUBROUTINE CheckTrimodalBounds(OneEightyScale)

! Determines whether it is appropriate to use a -180 to 0 and 0 to 180 degree 
! scale.  A 0-360 degree scale may be more appropriate (OneEightyScale = .FALSE.)


      IMPLICIT NONE

      REAL, DIMENSION (3,2) :: TempBounds

      COMMON /TriModalBounds/  TempBounds

      INTEGER Upper, Lower
      INTEGER i
      LOGICAL OneEightyScale

      Upper = 1
      Lower = 2
      OneEightyScale = .TRUE.
      DO I = 1, 3
       IF (Tempbounds(I,Upper) * Tempbounds(I, Lower) .LT. 0.00) THEN
         IF (ABS(TempBounds(I,Upper)) .GT. 90.00) THEN
           OneEightyScale = .FALSE.
           EXIT
         ENDIF
       ENDIF
      ENDDO

      END SUBROUTINE CheckTriModalBounds
!
!*****************************************************************************
!
      SUBROUTINE CheckBimodalBounds(row, OneEightyScale)

! Determines whether it is appropriate to use a -180 to 0 and 0 to 180 degree 
! scale.  A 0-360 degree scale may be more appropriate (OneEightyScale = .FALSE.)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      INTEGER row
      LOGICAL OneEightyScale

      OneEightyScale = .TRUE.
      IF (UB(row) * LB(row) .LT. 0.0) THEN
        IF (ABS(UB(Row)) .GT. 90.0) THEN
          OneEightyScale = .FALSE.
        ENDIF
      ENDIF

      END SUBROUTINE CheckBiModalBounds 
!
!*****************************************************************************
!
      LOGICAL FUNCTION OutOfBounds(iPar, XIn)

! This Subroutine determines if a trial torsion angle value is within
! modal torsion angle ranges defined.

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iPar
      REAL,    INTENT (INOUT) :: XIn

      INCLUDE 'PARAMS.INC'

      REAL             x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(MVAR), RowNumber, iRadio, iX, iUB, iLB

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds 
           
      INTEGER I, Upper, Lower
      LOGICAL OneEightyScale
      REAL xtem, tempupper, templower, tempupper2, templower2

      Upper = 1
      Lower = 2
      OutOfBounds = .FALSE.
      SELECT CASE(ModalFlag(iPar))
        CASE (2) ! Bimodal ranges
          IF ((XIn .LT. -180.0) .OR. (XIn .GT. 180.0)) THEN
            OutOfBounds = .TRUE.
            RETURN
          ENDIF
          IF (UB(iPar) * LB(iPar) .LT. 0.0) THEN ! Range such as -10 to 10 defined                                                  
            TempLower = LB(iPar)                 ! so use 0 to 360 degree scale
            TempUpper = UB(iPar)
            TempLower2 = TempUpper - 180.00
            TempUpper2 = TempLower + 180.00

     !F       TempLower2 = TempLower + 180.00
     !F       TempUpper2 = TempUpper + 180.00

            CALL OneEightyToThreeSixty(TempLower)
            CALL OneEightyToThreeSixty(TempUpper)
            CALL OneEightyToThreeSixty(TempLower2)
            CALL OneEightyToThreeSixty(TempUpper2)
            xtem = XIn                                                                                     
            CALL OneEightytoThreeSixty(xtem)
            IF (((xtem .LT. MAX(TempLower, TempLower2)) .AND. &
                 (xtem .GT. MIN(TempLower, TempLower2))) .OR. &
                ((xtem .LT. MAX(TempUpper, TempUpper2)) .AND. &
                 (xtem .GT. MIN(TempUpper, TempUpper2)))) THEN
              OutOfBounds = .TRUE.                                       
            ENDIF
          ELSE ! Range such as 30 to 90 degs or -30 to -90 defined
            IF ((XIn .LT. LB(iPar)) .OR. (XIn .GT. UB(iPar))) THEN
              IF (((XIn .LT. -UB(iPar)) .OR. (XIn .GT. -LB(iPar)))) THEN ! Out of bounds            
                OutOfBounds = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        CASE (3) !trimodal ranges
          xtem = XIn
          CALL DetermineTriModalBounds(UB(iPar), Upper)
          CALL DetermineTriModalBounds(LB(iPar), Lower)
          IF ((xtem .LT. -180.0) .OR. (xtem .GT.180.0)) THEN
            OutOfBounds = .TRUE.
          ELSE                 
            CALL CheckTriModalBounds(OneEightyScale)
            IF (.NOT. OneEightyScale) THEN ! A range such as -10 to 10 has been defined
              CALL OneEightytoThreeSixty(xtem)    ! so use 0 to 360 scale
              DO I = 1, 3
                CALL OneEightyToThreeSixty(TempBounds(I,Upper))
                CALL OneEightyToThreeSixty(TempBounds(I,Lower))
              ENDDO
            ENDIF
!           Determine if XP is in any of the three torsion angle ranges              
            TempUpper = MAX(Tempbounds(1,Upper), Tempbounds(1,Lower))!!UB(H)
            TempLower = MIN(Tempbounds(1,Lower), Tempbounds(1,Upper))!!LB(H)
            IF((xtem .LT. TempLower) .OR. (xtem .GT. TempUpper)) THEN
              TempUpper = MAX(Tempbounds(2,Upper), Tempbounds(2,Lower))
              TempLower = MIN(Tempbounds(2,Upper), Tempbounds(2,Lower))
              IF((xtem .LT. TempLower) .OR. (xtem .GT. TempUpper)) THEN
                TempUpper = MAX(Tempbounds(3,Upper), Tempbounds(3,Lower))
                TempLower = MIN(Tempbounds(3,Upper), Tempbounds(3,Lower))
                IF((xtem .LT. TempLower) .OR. (xtem .GT. TempUpper)) THEN        
                  OutOfBounds = .TRUE.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
      END SELECT

      END FUNCTION OutOfBounds
!
!*****************************************************************************
!
