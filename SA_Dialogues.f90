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

! @@ Enable or disable the "Next" button, only partially taken care of in UpdateZmatrixSelection
      CALL UpdateZmatrixSelection
      CALL WizardWindowShow(IDD_SAW_Page1)
      PastPawley = .TRUE.
! Grey out 'Delete all peak fit ranges' button on toolbar
      CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOff)
! Grey out 'Fit peaks' button on toolbar
      CALL WMenuSetState(ID_FitPeaks,ItemEnabled,WintOff)
! Grey out 'Clear cell parameters' button on toolbar
      CALL WMenuSetState(ID_Delabc,ItemEnabled,WintOff)
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
      CALL WDialogFieldState(IDB_Delabc,Disabled)
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
!O      CALL WDialogSelect(IDD_ViewPawley)
!O      CALL WDialogFieldState(IDF_Sigma1,DialogReadOnly)
!O      CALL WDialogFieldState(IDF_Sigma2,DialogReadOnly)
!O      CALL WDialogFieldState(IDF_Gamma1,DialogReadOnly)
!O      CALL WDialogFieldState(IDF_Gamma2,DialogReadOnly)
! Grey out 'Remove background' button on toolbar
      CALL WMenuSetState(ID_Remove_Background,ItemEnabled,WintOff)
      CALL SetModeMenuState(-1,-1,0)
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

      INTEGER       iFlags
      INTEGER       zmread
      INTEGER       iFrg, iSelection
      CHARACTER(MaxPathLength) SDIFile, DirName, tFileName
      CHARACTER*80  FileName
      CHARACTER*150 FilterStr
      INTEGER       tNumZMatrices, tLen
      CHARACTER(80) tZmatrices(10)
      INTEGER       tNextzmNum
      INTEGER       tCounter
      CHARACTER*7   tExtension
      INTEGER, EXTERNAL :: Read_One_Zm
      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical

! The following variables are there to allow the dialogue fields in the
! window dealing with Z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

      INTEGER        IDFZMNumber,           IDFZMFile,                &
                     IDBZMDelete,           IDBZMBrowse,              &
                     IDBZMView,             IDBZMEdit,                &
                     IDFZMpars
      COMMON /IDFZM/ IDFZMNumber(1:maxfrg), IDFZMFile(1:maxfrg),      &
                     IDBZMDelete(1:maxfrg), IDBZMBrowse(1:maxfrg),    &
                     IDBZMView(1:maxfrg),   IDBZMEdit(1:maxfrg),      &
                     IDFZMpars(1:maxfrg)

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
! Go to the next stage of the SA input
! Grey out 'Load DASH Pawley file' button on toolbar
              CALL WMenuSetState(ID_import_dpj_file,ItemEnabled,WintOff)
              CALL WDialogSelect(IDD_SAW_Page1)
              DO iFrg = 1, maxfrg
                IF (gotzmfile(iFrg)) THEN
! Get the number of copies to use. If zero, set gotzmfile to .FALSE.
                  CALL WDialogGetInteger(IDFzmNumber(iFrg),zmNumberOfCopies(iFrg))
                  IF (zmNumberOfCopies(iFrg) .EQ. 0) gotzmfile(iFrg) = .FALSE.
                ENDIF
              ENDDO
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
            CASE (IDB_SA_Project_Browse)
              CALL SDIFileBrowse
            CASE (IDB_SA_Project_Open)
              CALL WDialogGetString(IDF_SA_Project_Name,SDIFile)
              CALL SDIFileOpen(SDIFile)
            CASE (IDB_SA_Project_Import)
! JCC Import .. convert a mol/pdb/mol2 file into a Z-matrix
              CALL ImportZmatrix('')
            CASE (IDB_zmDelete1, IDB_zmDelete2, IDB_zmDelete3, IDB_zmDelete4, IDB_zmDelete5, IDB_zmDelete6)
              IF (Confirm('Do you want to clear this Z-matrix?')) THEN
                iFrg = 1
                DO WHILE (IDBZMDelete(ifrg) .NE. EventInfo%VALUE1)
                  iFrg = iFrg + 1
                ENDDO
                gotzmfile(iFrg) = .FALSE.
              ENDIF ! Delete this Z-matrix
            CASE (IDB_zmBrowse1, IDB_zmBrowse2, IDB_zmBrowse3, IDB_zmBrowse4, IDB_zmBrowse5, IDB_zmBrowse6)
              iFrg = 1
              DO WHILE (IDBZMBrowse(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              iFlags = LoadDialog + PromptOn + DirChange + AppendExt
              FilterStr = "All files (*.*)|*.*|"//&
                          "Z-matrix files (*.zmatrix)|*.zmatrix|"//&
                          "Molecular model files|*.cif;*.pdb;*.mol2;*.ml2;*.mol;*.mdl;*.res;*.cssr|"
              iSelection = 2
              CALL WSelectFile(FilterStr, iFlags, tFileName,'Load Z-matrix file',iSelection)
! Did the user press cancel?
              IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) GOTO 999
! I don't think the following answer is allowed by Winteracter
              IF (LEN_TRIM(tFileName) .EQ. 0) THEN
                gotzmfile(iFrg) = .FALSE.
                GOTO 999
              ENDIF
              CALL SplitPath(tFileName,DirName,FileName)
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
                  gotzmfile(iFrg) = .TRUE.
! Initialise 'Number of' field to 1
                  CALL WDialogPutInteger(IDFzmNumber(iFrg),1)
! JCC traps for Z-matrix reading
                ELSE 
                  gotzmfile(iFrg) = .FALSE. 
                  CALL FileErrorPopup(frag_file(iFrg),zmread)
                ENDIF ! If the read on the Z-matrix was ok
              ELSE
                CALL zmConvert(tFileName,tNumZMatrices,tZmatrices)
                IF (tNumZMatrices .EQ. 0) RETURN
                tNextzmNum  = 1
   10           CONTINUE
                frag_file(iFrg) = DirName(1:LEN_TRIM(DirName))//tZmatrices(tNextzmNum)
                zmread = Read_One_ZM(iFrg)
                IF (zmread .EQ. 0) THEN ! successful read
                  gotzmfile(iFrg) = .TRUE.
! Initialise 'Number of' field to 1
                  CALL WDialogPutInteger(IDFzmNumber(iFrg),1)
! Find next free slot ("iFrg")
                  tCounter = 1
                  DO WHILE ((gotzmfile(iFrg)) .AND. (tCounter .LT. maxfrg))
                    CALL INC(tCounter)
                    CALL INC(iFrg)
                    IF (iFrg .GT. maxfrg) iFrg = 1
                  ENDDO
                ELSE
                  gotzmfile(iFrg) = .FALSE.
                  CALL FileErrorPopup(frag_file(iFrg),zmread)
! Slot still free, so iFrg still OK.
                ENDIF
! More Z-matrices to read?
                CALL INC(tNextzmNum)
                IF (tNextzmNum .GT. tNumZMatrices) GOTO 999
! If no free slot found, exit
                IF (gotzmfile(iFrg)) THEN
                  CALL InfoMessage('File contained more Z-matrices than available slots.')
                  GOTO 999
                ENDIF
! Read next Z-matrix.
                GOTO 10
              ENDIF
! View individual Z-matrices in e.g. Mercury
            CASE (IDB_zmView1, IDB_zmView2, IDB_zmView3, IDB_zmView4, IDB_zmView5, IDB_zmView6)
              iFrg = 1
              DO WHILE (IDBZMView(iFrg) .NE. EventInfo%VALUE1)
                iFrg = iFrg + 1
              ENDDO
              CALL ViewZmatrix(iFrg)
! Edit individual Z-matrices
            CASE (IDB_zmEdit1, IDB_zmEdit2, IDB_zmEdit3, IDB_zmEdit4, IDB_zmEdit5, IDB_zmEdit6)
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
      USE SAMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER I, BondNr
      REAL*8 CART(1:3,1:MAXATM)

      CALL WDialogSelect(IDD_zmEdit)
      CurrentlyEditedFrag = iFrg
! Make temporary copy
      CALL zmCopy(iFrg,0)
      zmAtomDeleted = .FALSE.
      CALL zmCopyTemp2Dialog
! In order to be able to delete atoms from the Z-matrix at random, we need their
! Cartesian co-ordinates.
      natcry = natoms(iFrg)
      CALL MAKEXYZ_2(natcry,BLEN(1,iFrg),ALPH(1,iFrg),BET(1,iFrg),IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
      DO I = 1, natcry
        axyzo(I,1) = SNGL(CART(1,I))
        axyzo(I,2) = SNGL(CART(2,I))
        axyzo(I,3) = SNGL(CART(3,I))
        aelem(I) = zmElementCSD(I,iFrg)
        atomlabel(I) = OriginalLabel(I,iFrg)
      ENDDO
      nbocry = NumberOfBonds(iFrg)
      DO BondNr = 1, nbocry
        btype(BondNr)  = BondType(BondNr,iFrg)
        bond(BondNr,1) = Bonds(1,BondNr,iFrg)
        bond(BondNr,2) = Bonds(2,BondNr,iFrg)
      ENDDO
! Bonds have already been calculated and will be updated automatically: there is no option
! to add atoms, so we can only remove atoms and their bonds.
! Only problem: changing a hydrogen into, say, a carbon
! might give rise to new bonds that will not be taken into account.
! Oh, and removing an atom can change a bond type, e.g. from "aromatic" to "double"
      CALL WDialogShow(-1,-1,0,SemiModeLess)

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

      INTEGER iFrg, iOption, iColumn, iAtomNr, iDummy, iBondNr, iRow, iCol
      REAL    tReal
      LOGICAL ThisOne
      INTEGER, EXTERNAL :: zmSave, zmSaveAs, WriteMol2, zmRebuild
      INTEGER tLength, I, iBondNr2
      CHARACTER(MaxPathLength) temp_file

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Set)
              CALL WDialogGetReal(IDF_BisoOccValue,tReal)
              CALL WDialogGetMenu(IDF_BisoOccMenu,iOption)
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
              CALL WDialogGetMenu(IDF_WhichAtomMenu,iOption)
              DO iAtomNr = 1, natoms(iFrg)
! Biso / occupancies
                SELECT CASE (iOption)
                  CASE (1) ! All atoms
                    ThisOne = .TRUE.
                  CASE (2) ! All non-hydrogens
                    ThisOne = (zmElementCSD(izmbid(iAtomNr,iFrg),iFrg) .NE. 2)
                  CASE (3) ! All hydrogens
                    ThisOne = (zmElementCSD(izmbid(iAtomNr,iFrg),iFrg) .EQ. 2)
                END SELECT
                IF (ThisOne) CALL WGridPutCellReal(IDF_AtomPropGrid,iColumn,iAtomNr,tReal,'(F5.3)')
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
                atomlabel(iAtomNr) = OriginalLabel(iAtomNr,iFrg)
                aelem(iAtomNr) = zmElementCSD(iAtomNr,iFrg)
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
              IF (WriteMol2(temp_file,.TRUE.,iFrg) .EQ. 1) CALL ViewStructure(temp_file)
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
                iAtomNr = izmbid(iRow,iFrg)
! It is very likely that we will end up with one of the atoms having an 'orignal ID' that
! is greater than the current number of atoms. This would give boundary overflows.
! We can retain the original order (minus one atom) but that involves subtracting 1
! from atom IDs following the one we have deleted
                DO i = 1, natoms(iFrg)
                  IF (izmoid(i,iFrg) .GT. izmoid(iAtomNr,iFrg)) izmoid(i,iFrg) = izmoid(i,iFrg) - 1
                ENDDO
! Remove any bonds this atom was involved in
                IF (NumberOfBonds(iFrg) .GT. 0) THEN
                  iBondNr = 1
                  DO WHILE (iBondNr .LE. NumberOfBonds(iFrg))
                    IF ((Bonds(1,iBondNr,iFrg) .EQ. iAtomNr) .OR. (Bonds(2,iBondNr,iFrg) .EQ. iAtomNr)) THEN
                      DO iBondNr2 = iBondNr, NumberOfBonds(iFrg)-1
                        BondType(iBondNr2,iFrg) = BondType(iBondNr2+1,iFrg)
                        Bonds(1,iBondNr2,iFrg)  = Bonds(1,iBondNr2+1,iFrg)
                        Bonds(2,iBondNr2,iFrg)  = Bonds(2,iBondNr2+1,iFrg)
                      ENDDO
                      NumberOfBonds(iFrg) = NumberOfBonds(iFrg) - 1
                    ELSE
                      CALL INC(iBondNr)
                    ENDIF
                  ENDDO
                  IF (NumberOfBonds(iFrg) .GT. 0) THEN
                    DO iBondNr = 1, NumberOfBonds(iFrg)
                      IF (Bonds(1,iBondNr,iFrg) .GT. iAtomNr) Bonds(1,iBondNr,iFrg) = Bonds(1,iBondNr,iFrg) - 1
                      IF (Bonds(2,iBondNr,iFrg) .GT. iAtomNr) Bonds(2,iBondNr,iFrg) = Bonds(2,iBondNr,iFrg) - 1
                    ENDDO
                  ENDIF
                ENDIF
! If not last atom in list, shuffle remaining
                IF (iAtomNr .NE. NATOMS(iFrg)) THEN 
                  DO I = iAtomNr, NATOMS(iFrg)-1
                    asym(I,iFrg) = asym(I+1,iFrg)
                    zmElementCSD(I,iFrg) = zmElementCSD(I+1,iFrg)
                    tiso(I,iFrg) = tiso(I+1,iFrg)
                    occ(I,iFrg)  = occ(I+1,iFrg)
                    OriginalLabel(I,iFrg) = OriginalLabel(I+1,iFrg)
                    izmoid(I,iFrg) = izmoid(I+1,iFrg)
                  ENDDO
                ENDIF
                natcry = NATOMS(iFrg)
! If not last atom in list, shuffle remaining
                IF (iAtomNr .NE. natcry) THEN 
                  DO I = iAtomNr, natcry-1
                    axyzo(I,1)   = axyzo(I+1,1)
                    axyzo(I,2)   = axyzo(I+1,2)
                    axyzo(I,3)   = axyzo(I+1,3)
                  ENDDO
                ENDIF
                natcry = natcry - 1
                nbocry = NumberOfBonds(iFrg)
                DO iBondNr = 1, nbocry
                  btype(iBondNr)  = BondType(iBondNr,iFrg)
                  bond(iBondNr,1) = Bonds(1,iBondNr,iFrg)
                  bond(iBondNr,2) = Bonds(2,iBondNr,iFrg)
                ENDDO
! If this was the pivot atom, use centre of mass
                IF (icomflg(iFrg) .EQ. iAtomNr) icomflg(iFrg) = 0 
                IF (natoms(iFrg) .NE. 0) THEN
                  CALL WGridSetCell(IDF_AtomPropGrid,1,1)
                  natoms(iFrg) = natoms(iFrg) - 1
                ENDIF
                DO i = 1, natoms(iFrg)
                  izmbid(izmoid(i,iFrg),iFrg) = i ! the back mapping
                ENDDO
                CALL zmCopyTemp2Dialog
              ENDIF
              CALL WGridPutCellCheckBox(IDF_AtomPropGrid,2,irow,Unchecked)
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

      IMPLICIT NONE

      CALL zmCopyDialog2Temp
      CALL zmRotCopyTemp2Dialog
      CALL WDialogSelect(IDD_zmEditRotations)
      CALL WDialogShow(-1,-1,0,SemiModeLess)

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

      INTEGER I, iFrg, iOption, iOpt1State, iOpt2State, iOpt3State, iAtomNr
      INTEGER, EXTERNAL :: WriteMol2
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, Get_UseCrystallographicCoM
      REAL, EXTERNAL :: Degrees2Radians
      REAL    Alpha, Beta, Gamma, Q(0:3)
      REAL    taxyzo(1:MAXATM_2,1:3)
      CHARACTER(50) temp_file
      REAL    RotMat(1:3,1:3)
      REAL    COM(1:3), tX, tY, tZ
      REAL*8  DQ(0:3), DRotMat(1:3,1:3)
      LOGICAL tUseSingleAxis

      iFrg = 0
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK)
              CALL zmRotCopyDialog2Temp
              CALL WDialogHide
            CASE (IDCANCEL)
              CALL WDialogHide
            CASE (IDB_Convert) ! Convert Euler angles to quaternions
              CALL WDialogGetReal(IDF_Alpha,Alpha)
              CALL WDialogGetReal(IDF_Beta,Beta)
              CALL WDialogGetReal(IDF_Gamma,Gamma)
              Q(0) = COS(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha+Gamma))
              Q(1) = SIN(0.5*Degrees2Radians(Beta)) * COS(0.5*Degrees2Radians(Alpha-Gamma))
              Q(2) = SIN(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha-Gamma))
              Q(3) = COS(0.5*Degrees2Radians(Beta)) * SIN(0.5*Degrees2Radians(Alpha+Gamma))
              CALL WDialogPutReal(IDF_Q0,Q(0))
              CALL WDialogPutReal(IDF_Q1,Q(1))
              CALL WDialogPutReal(IDF_Q2,Q(2))
              CALL WDialogPutReal(IDF_Q3,Q(3))
            CASE (IDB_View)
              natcry = NATOMS(iFrg)
              DO iAtomNr = 1, natcry
                taxyzo(iAtomNr,1) = axyzo(iAtomNr,1)
                taxyzo(iAtomNr,2) = axyzo(iAtomNr,2)
                taxyzo(iAtomNr,3) = axyzo(iAtomNr,3)
              ENDDO
! Subtract origin from co-ordinates
              CALL WDialogGetRadioButton(IDF_RotOrgCOM,iOption)
              SELECT CASE (iOption)
                CASE (1) ! C.O.M.
! If user set centre of mass flag to 0, then use the molecule's centre of mass
                  COM = 0.0
                  IF (Get_UseCrystallographicCoM()) THEN
                    CALL zmCreate_AtomicWeightings(iFrg)
                    DO iAtomNr = 1, natcry
                      COM(1) = COM(1) + AtomicWeighting(iAtomNr,iFrg)*axyzo(iAtomNr,1)
                      COM(2) = COM(2) + AtomicWeighting(iAtomNr,iFrg)*axyzo(iAtomNr,2)
                      COM(3) = COM(3) + AtomicWeighting(iAtomNr,iFrg)*axyzo(iAtomNr,3)
                    ENDDO
                  ELSE
                    DO iAtomNr = 1, natcry
                      COM(1) = COM(1) + axyzo(iAtomNr,1)
                      COM(2) = COM(2) + axyzo(iAtomNr,2)
                      COM(3) = COM(3) + axyzo(iAtomNr,3)
                    ENDDO
                    COM = COM / FLOAT(natcry)
                  ENDIF
! Otherwise, use atom number ICFRG
                CASE (2) ! Use atom nr.
                  CALL WDialogGetInteger(IDF_RotOrgAtomNr,iAtomNr)
                  COM(1) = axyzo(izmbid(iAtomNr,iFrg),1)
                  COM(2) = axyzo(izmbid(iAtomNr,iFrg),2)
                  COM(3) = axyzo(izmbid(iAtomNr,iFrg),3)
              END SELECT
              DO iAtomNr = 1, natcry
                axyzo(iAtomNr,1) = axyzo(iAtomNr,1) - COM(1)
                axyzo(iAtomNr,2) = axyzo(iAtomNr,2) - COM(2)
                axyzo(iAtomNr,3) = axyzo(iAtomNr,3) - COM(3)
              ENDDO
! Apply initial orientation
              CALL WDialogGetReal(IDF_Q0,Q(0))
              CALL WDialogGetReal(IDF_Q1,Q(1))
              CALL WDialogGetReal(IDF_Q2,Q(2))
              CALL WDialogGetReal(IDF_Q3,Q(3))
              DQ = DBLE(Q)
              CALL ROTMAK(DQ,DRotMat)
              RotMat = SNGL(DRotMat)
              DO I = 1, natcry
                tX = axyzo(I,1) * RotMat(1,1) + axyzo(I,2) * RotMat(1,2) + axyzo(I,3) * RotMat(1,3)
                tY = axyzo(I,1) * RotMat(2,1) + axyzo(I,2) * RotMat(2,2) + axyzo(I,3) * RotMat(2,3)
                tZ = axyzo(I,1) * RotMat(3,1) + axyzo(I,2) * RotMat(3,2) + axyzo(I,3) * RotMat(3,3)
                axyzo(I,1) = tX
                axyzo(I,2) = tY
                axyzo(I,3) = tZ
              ENDDO
              DO iAtomNr = 1, natcry
                atomlabel(iAtomNr) = OriginalLabel(iAtomNr,iFrg)
                aelem(iAtomNr) = zmElementCSD(iAtomNr,iFrg)
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
              IF (WriteMol2(temp_file,.TRUE.,iFrg) .EQ. 1) CALL ViewStructure(temp_file)
              CALL IOSDeleteFile(temp_file)
              DO iAtomNr = 1, natcry
                axyzo(iAtomNr,1) = taxyzo(iAtomNr,1)
                axyzo(iAtomNr,2) = taxyzo(iAtomNr,2)
                axyzo(iAtomNr,3) = taxyzo(iAtomNr,3)
              ENDDO
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_RotOrgCOM, IDF_RotOrgAtom)
              CALL WDialogGetRadioButton(IDF_RotOrgCOM,iOption)
              CALL WDialogFieldStateLogical(IDF_RotOrgAtomNr,iOption .EQ. 2)
            CASE (IDF_UseSingleAxis, IDF_RotAxAtom, IDF_RotAxFrac, IDF_RotAxPln)
              tUseSingleAxis = WDialogGetCheckBoxLogical(IDF_UseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_GROUP3,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_LABEL8,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Alpha,     tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Beta,      tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Gamma,     tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDB_Convert,   tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_LABEL4,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_LABEL5,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_LABEL6,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_LABEL7,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Q0,        tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Q1,        tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Q2,        tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_Q3,        tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_GROUP2,    tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxAtom, tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxFrac, tUseSingleAxis)
              CALL WDialogFieldStateLogical(IDF_RotAxPln,  tUseSingleAxis)
              iOpt1State = Disabled
              iOpt2State = Disabled
              iOpt3State = Disabled
              IF (tUseSingleAxis) THEN
                CALL WDialogGetRadioButton(IDF_RotAxAtom,iOption)
                SELECT CASE (iOption)
                  CASE (1)
                    iOpt1State = Enabled
                  CASE (2)
                    iOpt2State = Enabled
                  CASE (3)
                    iOpt3State = Enabled
                END SELECT
              ENDIF
              CALL WDialogFieldState(IDF_AtomNr,       iOpt1State)
              CALL WDialogFieldState(IDF_LABELa,       iOpt2State)
              CALL WDialogFieldState(IDF_LABELb,       iOpt2State)
              CALL WDialogFieldState(IDF_LABELc,       iOpt2State)
              CALL WDialogFieldState(IDF_a,            iOpt2State)
              CALL WDialogFieldState(IDF_b,            iOpt2State)
              CALL WDialogFieldState(IDF_c,            iOpt2State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm1, iOpt3State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm2, iOpt3State)
              CALL WDialogFieldState(IDF_RotAxPlnAtm3, iOpt3State)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithEditZMatrixRotationsWindow
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
      natoms(iFrg2)    = natoms(iFrg1)
      icomflg(iFrg2)   = icomflg(iFrg1)
      UseQuaternions(iFrg2)         = UseQuaternions(iFrg1)
      zmSingleRotAxDef(iFrg2)       = zmSingleRotAxDef(iFrg1)
      zmSingleRotAxAtm(iFrg2)       = zmSingleRotAxAtm(iFrg1)
      zmSingleRotAxFrac(:,iFrg2)    = zmSingleRotAxFrac(:,iFrg1)
      zmSingleRotAxAtms(:,iFrg2)    = zmSingleRotAxAtms(:,iFrg1)
      zmInitialQs(:,iFrg2)          = zmInitialQs(:,iFrg1)
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
        zmElementCSD(iAtomNr,iFrg2)  = zmElementCSD(iAtomNr,iFrg1)
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
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE SAMVAR

      IMPLICIT NONE      

      INTEGER iFrg
      INTEGER, EXTERNAL :: Read_One_ZM, WriteMol2
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
        atomlabel(iAtomNr) = OriginalLabel(iAtomNr,iFrg)
        aelem(iAtomNr) = zmElementCSD(iAtomNr,iFrg)
      ENDDO
      IF (WriteMol2('Rebuild_temp.mol2',.FALSE.,iFrg) .NE. 1) GOTO 999 ! Writing mol2 file failed
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
! filename
      CALL WDialogPutString(IDF_FileName,frag_file(iFrg))
! Fill grid with atom properties
! Set number of rows
      CALL WGridRows(IDF_AtomPropGrid,natoms(iFrg))
      DO iRow = 1, natoms(iFrg)
        iAtomNr = izmbid(iRow,iFrg)
! Show the number of the atom in the zeroth column
        WRITE(RowLabelStr,'(I3)') iRow
        CALL WGridLabelRow(IDF_AtomPropGrid,iRow,RowLabelStr)
! atom labels
        CALL WGridPutCellString(IDF_AtomPropGrid,1,iRow,OriginalLabel(iAtomNr,iFrg))
! atom elements
        CALL WGridPutCellString(IDF_AtomPropGrid,3,iRow,ElementStr(zmElementCSD(iAtomNr,iFrg)))
! Biso
        CALL WGridPutCellReal(IDF_AtomPropGrid,4,iRow,tiso(iAtomNr,iFrg),'(F5.3)')
! occupancies
        CALL WGridPutCellReal(IDF_AtomPropGrid,5,iRow,occ(iAtomNr,iFrg),'(F5.3)')
      ENDDO
! If only a single atom left, grey out "Rotations..." and "Re-order"
      CALL WDialogFieldStateLogical(IDB_Rotations,natoms(iFrg) .GT. 1)
      CALL WDialogFieldStateLogical(IDB_ReOrder,natoms(iFrg) .GT. 1)
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

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      iFrg = 0
      CALL WDialogPutReal(IDF_Q0,SNGL(zmInitialQs(0,iFrg)))
      CALL WDialogPutReal(IDF_Q1,SNGL(zmInitialQs(1,iFrg)))
      CALL WDialogPutReal(IDF_Q2,SNGL(zmInitialQs(2,iFrg)))
      CALL WDialogPutReal(IDF_Q3,SNGL(zmInitialQs(3,iFrg)))
      CALL WDialogFieldStateLogical(IDF_RotOrgAtomNr,icomflg(iFrg) .NE. 0)
      IF (icomflg(iFrg) .EQ. 0) THEN ! Use centre of mass
! Set radio button
        CALL WDialogPutRadioButton(IDF_RotOrgCOM)
      ELSE ! use atom number
! Set radio button
        CALL WDialogPutRadioButton(IDF_RotOrgAtom)
! Set atom number
        CALL WDialogPutInteger(IDF_RotOrgAtomNr,izmoid(icomflg(iFrg),iFrg))
      ENDIF
      CALL WDialogPutCheckBoxLogical(IDF_UseSingleAxis, .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_GROUP3,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Alpha,          .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Beta,           .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Gamma,          .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDB_Convert,        .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_LABEL4,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_LABEL5,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_LABEL6,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_LABEL7,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Q0,             .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Q1,             .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Q2,             .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_Q3,             .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_GROUP2,         .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_RotAxAtom,      .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_RotAxFrac,      .NOT. UseQuaternions(iFrg))
      CALL WDialogFieldStateLogical(IDF_RotAxPln,       .NOT. UseQuaternions(iFrg))
      CALL WDialogPutInteger(IDF_AtomNr,izmoid(zmSingleRotAxAtm(iFrg),iFrg))
      CALL WDialogPutReal(IDF_a,zmSingleRotAxFrac(1,iFrg))
      CALL WDialogPutReal(IDF_b,zmSingleRotAxFrac(2,iFrg))
      CALL WDialogPutReal(IDF_c,zmSingleRotAxFrac(3,iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm1,izmoid(zmSingleRotAxAtms(1,iFrg),iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm2,izmoid(zmSingleRotAxAtms(2,iFrg),iFrg))
      CALL WDialogPutInteger(IDF_RotAxPlnAtm3,izmoid(zmSingleRotAxAtms(3,iFrg),iFrg))
      iOpt1State = Disabled
      iOpt2State = Disabled
      iOpt3State = Disabled
      SELECT CASE (zmSingleRotAxDef(iFrg))
        CASE (1)
          CALL WDialogPutRadioButton(IDF_RotAxAtom)
          iOpt1State = Enabled
        CASE (2)
          CALL WDialogPutRadioButton(IDF_RotAxFrac)
          iOpt2State = Enabled
        CASE (3)
          CALL WDialogPutRadioButton(IDF_RotAxPln)
          iOpt3State = Enabled
      END SELECT
      IF (UseQuaternions(iFrg)) THEN
        iOpt1State = Disabled
        iOpt2State = Disabled
        iOpt3State = Disabled
      ENDIF
      CALL WDialogFieldState(IDF_AtomNr,       iOpt1State)
      CALL WDialogFieldState(IDF_LABELa,       iOpt2State)
      CALL WDialogFieldState(IDF_LABELb,       iOpt2State)
      CALL WDialogFieldState(IDF_LABELc,       iOpt2State)
      CALL WDialogFieldState(IDF_a,            iOpt2State)
      CALL WDialogFieldState(IDF_b,            iOpt2State)
      CALL WDialogFieldState(IDF_c,            iOpt2State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm1, iOpt3State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm2, iOpt3State)
      CALL WDialogFieldState(IDF_RotAxPlnAtm3, iOpt3State)
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

      INTEGER iFrg, iRow, iAtomNr
      INTEGER, EXTERNAL :: ElmSymbol2CSD

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEdit)
      iFrg = 0
! filename
      CALL WDialogGetString(IDF_FileName,frag_file(iFrg))
! Fill grid with atom properties
!U! Set number of rows
!U      CALL WGridRows(IDF_AtomPropGrid,natoms(iFrg))
      DO iRow = 1, natoms(iFrg)
        iAtomNr = izmbid(iRow,iFrg)
! atom labels
        CALL WGridGetCellString(IDF_AtomPropGrid,1,iRow,OriginalLabel(iAtomNr,iFrg))
! atom elements
        CALL WGridGetCellString(IDF_AtomPropGrid,3,iRow,asym(iAtomNr,iFrg))
        zmElementCSD(iAtomNr,iFrg) = ElmSymbol2CSD(asym(iAtomNr,iFrg)(1:2))
! Biso
        CALL WGridGetCellReal(IDF_AtomPropGrid,4,iRow,tiso(iAtomNr,iFrg))
! occupancies
        CALL WGridGetCellReal(IDF_AtomPropGrid,5,iRow,occ(iAtomNr,iFrg))
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
      
      INTEGER iFrg, iOption, tInteger
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      REAL    tReal

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_zmEditRotations)
      iFrg = 0
      CALL WDialogGetReal(IDF_Q0,tReal)
      zmInitialQs(0,iFrg) = DBLE(tReal)
      CALL WDialogGetReal(IDF_Q1,tReal)
      zmInitialQs(1,iFrg) = DBLE(tReal)
      CALL WDialogGetReal(IDF_Q2,tReal)
      zmInitialQs(2,iFrg) = DBLE(tReal)
      CALL WDialogGetReal(IDF_Q3,tReal)
      zmInitialQs(3,iFrg) = DBLE(tReal)
      CALL WDialogGetRadioButton(IDF_RotOrgCOM,iOption)
      SELECT CASE (iOption)
        CASE (1) ! C.O.M.
          icomflg(iFrg) = 0
        CASE (2) ! Atom number
          CALL WDialogGetInteger(IDF_RotOrgAtomNr,tInteger)
          icomflg(iFrg) = izmbid(tInteger,iFrg)
      END SELECT
      UseQuaternions(iFrg) = .NOT. WDialogGetCheckBoxLogical(IDF_UseSingleAxis)
      CALL WDialogGetInteger(IDF_AtomNr,tInteger)
      zmSingleRotAxAtm(iFrg) = izmbid(tInteger,iFrg)
      CALL WDialogGetReal(IDF_a,zmSingleRotAxFrac(1,iFrg))
      CALL WDialogGetReal(IDF_b,zmSingleRotAxFrac(2,iFrg))
      CALL WDialogGetReal(IDF_c,zmSingleRotAxFrac(3,iFrg))
      CALL WDialogGetInteger(IDF_RotAxPlnAtm1,tInteger)
      zmSingleRotAxAtms(1,iFrg) = izmbid(tInteger,iFrg)
      CALL WDialogGetInteger(IDF_RotAxPlnAtm2,tInteger)
      zmSingleRotAxAtms(2,iFrg) = izmbid(tInteger,iFrg)
      CALL WDialogGetInteger(IDF_RotAxPlnAtm3,tInteger)
      zmSingleRotAxAtms(3,iFrg) = izmbid(tInteger,iFrg)
      CALL WDialogGetRadioButton(IDF_RotAxAtom,zmSingleRotAxDef(iFrg))
      CALL zmDoAdmin(iFrg)
      CALL PopActiveWindowID

      END SUBROUTINE zmRotCopyDialog2Temp
!
!*****************************************************************************
!
      SUBROUTINE zmRelabel(iFrg)

! This routine re-labels atoms in Z-matrix number iFrg
! The new labels consist of element + sequential number

      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER iAtomNr, iLen, iOrig
      CHARACTER*3 LastNumberSoFarStr

      DO iAtomNr = 1, natoms(iFrg)
        iOrig = izmoid(iAtomNr,iFrg)
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
      SUBROUTINE zmRelabelAll

! This routine re-labels atoms in all Z-matrices
! The new labels consist of element + sequential number

      USE ZMVAR

      DO iFrg = 1, maxfrg
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
          IF ((zmElementCSD(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. zmElementCSD(izmbid(iAtomNr+1,iFrg),iFrg))) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, never swap if first is Carbon or second is Hydrogen
          ELSE IF ((zmElementCSD(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. 1) .OR. (zmElementCSD(izmbid(iAtomNr+1,iFrg),iFrg) .EQ. 2)) THEN
            ShouldBeSwapped = .FALSE.
! Otherwise, always swap if second is Carbon or first is Hydrogen
          ELSE IF ((zmElementCSD(izmbid(iAtomNr+1,iFrg),iFrg) .EQ. 1) .OR. (zmElementCSD(izmbid(iAtomNr  ,iFrg),iFrg) .EQ. 2)) THEN
            ShouldBeSwapped = .TRUE.
! Otherwise, swap if first > second
          ELSE
            ShouldBeSwapped = ((asym(izmbid(iAtomNr  ,iFrg),iFrg) .GT. asym(izmbid(iAtomNr+1,iFrg),iFrg)))
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
!
! RETURNS : 0 for success
!
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
        CALL PushActiveWindowID
        CALL WDialogSelect(IDD_zmEdit)
        CALL WDialogPutString(IDF_FileName,frag_file(iFrg))
        CALL PopActiveWindowID
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
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER, INTENT (IN   ) :: iFrg

      INTEGER tFileHandle, i
      INTEGER, EXTERNAL :: zmRebuild
      
! Save the Z-matrix
      zmSave = 1 ! Failure
! If an atom has been deleted, try to rebuild the Z-matrix
      IF (zmRebuild() .NE. 0) THEN
        CALL ErrorMessage('Could not rebuild Z-matrix.')
        RETURN
      ENDIF
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
              CALL WDialogFieldState(IDF_PO_a,tFieldState)
              CALL WDialogFieldState(IDF_PO_b,tFieldState)
              CALL WDialogFieldState(IDF_PO_c,tFieldState)
              CALL WDialogFieldState(IDF_LABELa,tFieldState)
              CALL WDialogFieldState(IDF_LABELb,tFieldState)
              CALL WDialogFieldState(IDF_LABELc,tFieldState)
          END SELECT
      END SELECT
  999 CALL UpdateZmatrixSelection
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowAdditionalSAParams
!
!*****************************************************************************
!
      SUBROUTINE ShowWizardWindowParameterBounds

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE      

      INCLUDE 'PARAMS.INC'

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      INTEGER IV, iCheck

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Modal_input2)
      DO IV = 1, NVAR
        CALL WGridGetCellReal(IDF_parameter_grid_modal,1,IV,prevx(IV))
        CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,4,IV,iCheck)
        IF (iCheck .EQ. UnChecked) THEN
          CALL WGridGetCellReal(IDF_parameter_grid_modal,2,IV,prevlb(IV))
          CALL WGridGetCellReal(IDF_parameter_grid_modal,3,IV,prevub(IV))
        ENDIF
      ENDDO
      LimsChanged = .FALSE.
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

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,lb,ub,vm
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)


      DOUBLE PRECISION T0, rt
      COMMON /saparl/  T0, rt

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      INTEGER                ModalFlag,       RowNumber
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber
      SAVE   /ModalTorsions/      
      
      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical
      LOGICAL, EXTERNAL :: NearlyEqual
      REAL    xtem
      INTEGER JPOS, NMOVES, IFCOl, IFRow, ICHK
      REAL    rpos
      INTEGER ipos, tMaxRuns, tFieldState
      INTEGER i


! We are now on window number 2
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_Modal_input2)

! Disable modal button for everything but torsion angles
      DO i = 1,nvar
        IF (kzmpar2(i) .NE. 3) THEN
          CALL WGridStateCell(IDF_parameter_grid_modal,5,i,DialogReadOnly)
        ENDIF
      END DO

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
                    ModalFlag(I) = 1
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
            CASE (IDF_parameter_grid_modal)
              CALL WGridPos(EventInfo%X,IFCol,IFRow)
              SELECT CASE (IFCol)
                CASE (1) ! parameter
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal,IFCol,IFRow,xtem)
                    xtem = MAX(xtem,prevlb(IFRow))
                    xtem = MIN(xtem,prevub(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevx(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal,1,IFRow,xtem,'(F12.5)')
                      prevx(IFRow) = xtem
                    ENDIF
                  ENDIF
                CASE (2) ! lower bound
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal,IFCol,IFRow,xtem)
                    xtem = MIN(xtem,prevub(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevlb(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal,2,IFRow,xtem,'(F12.5)')
                      prevlb(IFRow) = xtem
                      lb(IFRow) = DBLE(xtem)
                    ENDIF
                    xtem = MAX(prevlb(IFRow),prevx(IFRow))
                    CALL WGridPutCellReal(IDF_parameter_grid_modal,1,IFRow,xtem,'(F12.5)')
                    prevx(IFRow) = xtem
                  ENDIF
                CASE (3) ! upper bound
! JCC Check the bounding - only update if parameter is set to vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,4,IFRow,ICHK)
                  IF (ICHK .EQ. UnChecked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal,IFCol,IFRow,xtem)
                    xtem = MAX(xtem,prevlb(IFRow))
                    IF (.NOT. NearlyEqual(xtem,prevub(IFRow))) THEN
                      LimsChanged = .TRUE.
                      CALL WGridPutCellReal(IDF_parameter_grid_modal,3,IFRow,xtem,'(F12.5)')
                      prevub(IFRow) = xtem
                      ub(IFRow) = DBLE(xtem)
                    ENDIF
                    xtem = MIN(prevub(IFRow),prevx(IFRow))
                    CALL WGridPutCellReal(IDF_parameter_grid_modal,1,IFRow,xtem,'(F12.5)')
                    prevx(IFRow) = xtem
                  ENDIF
                CASE (4) ! fix or vary
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal,IFCol,IFRow,ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal,1,IFRow,xtem)
                    lb(IFRow) = DBLE(xtem-1.0D-5)
                    ub(IFRow) = DBLE(xtem+1.0D-5)
                    CALL WGridStateCell(IDF_parameter_grid_modal,1,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal,2,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal,3,IFRow,DialogReadOnly)
                    CALL WGridStateCell(IDF_parameter_grid_modal,5,IFRow,Disabled)
                  ELSE
                    lb(IFRow) = prevlb(IFRow)
                    ub(IFRow) = prevub(IFRow)
                    CALL WGridStateCell(IDF_parameter_grid_modal,1,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid_modal,2,IFRow,Enabled)
                    CALL WGridStateCell(IDF_parameter_grid_modal,3,IFRow,Enabled)
                    IF (kzmpar2(IFRow) .EQ. 3) THEN
                      CALL WGridStateCell(IDF_parameter_grid_modal,5,IFRow,Enabled)
                    ENDIF
                  ENDIF
                  CALL WGridPutCellReal(IDF_parameter_grid_modal,2,IFRow,SNGL(lb(IFRow)),'(F12.5)')
                  CALL WGridPutCellReal(IDF_parameter_grid_modal,3,IFRow,SNGL(ub(IFRow)),'(F12.5)')
                  LimsChanged = .TRUE.
                CASE (5) !Modal Torsion Angle Button
                  CALL WGridGetCellCheckBox(IDF_parameter_grid_modal, IFCol, IFRow, ICHK)
                  IF (ICHK .EQ. Checked) THEN
                    CALL WGridGetCellReal(IDF_parameter_grid_modal, 1, IFRow, xtem)                 
                    CALL ShowBiModalDialog(IFRow, xtem)
                  ENDIF
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

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER IHANDLE, JPOS, KPOS
      REAL    rpos
      INTEGER ipos
      INTEGER, EXTERNAL :: WriteSAParametersToFile
      INTEGER tMaxRuns, tFieldState
      LOGICAL, EXTERNAL :: Get_AutoAlign


! We are now on window number 3
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3)
      SELECT CASE (EventType)
! Interact with the main window and look at the Pawley refinement...
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the 2nd window
!!O              CALL WizardWindowShow(IDD_SA_input2)
              CALL WizardWindowShow(IDD_SA_Modal_input2)
            CASE (IDB_SA3_finish) ! 'Solve >' button
! We've finished the SA input
              CALL WizardWindowHide
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
            CASE (IDB_Configuration)
              CALL PushActiveWindowID
              CALL WDialogSelect(IDD_Configuration)
              CALL WDialogShow(-1,-1,0,Modeless)
              CALL PopActiveWindowID
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

      SUBROUTINE ShowBimodalDialog(IFrow, Xinitial)
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,lb,ub,vm
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB
      SAVE   /ModalTorsions/

      CHARACTER*36 parlabel(mvar)

      INTEGER, INTENT(INOUT):: IFrow
      INTEGER ICol, NumColumns
      INTEGER i,j,k, dof, copy, frag
      INTEGER Upper, Lower
      REAL    Zero, OneEighty, xtem
      REAL Xinitial


! Initialise variables          
      ICol = 0
      NumColumns = 3
      Upper = 1
      Lower = 2
      Zero = 0.0000
      OneEighty = 180.0000

!     Given the number of the parameter want to know
!     which zmatrix, fragment, copy it belongs to.
      dof = 0
      frag = 0
      copy = 0
      DO i = 1, maxDOF
        DO j = 1, maxcopies
          DO k = 1, nfrag
            IF (IFRow .EQ. zm2par(i,j,k)) THEN
              dof = i
              copy = j
              frag = k
              EXIT
            ENDIF
          ENDDO
          IF (copy .ne. 0) EXIT
        ENDDO
        IF (frag .ne. 0) EXIT
      ENDDO
 
      CALL WDialogSelect(IDD_ModalDialog)
      parlabel(IFrow) = czmpar(i,k) 

!     Clear Fields
      CALL WDialogClearField(IDF_TorsionName)
      CALL WDialogClearField(IDF_Initial)
      CALL WDialogClearField(IDF_ModalUpper)
      CALL WDialogClearField(IDF_ModalLower)
      CALL WDialogClearField(IDF_ReportLower1)
      CALL WDialogClearField(IDF_ReportLower2)
      CALL WDialogClearField(IDF_ReportUpper1)
      CALL WDialogClearField(IDF_ReportUpper2)

!     Initialise fields 
      CALL WDialogPutString(IDF_TorsionName, parlabel(IFRow))
      CALL WDialogPutReal(IDF_Initial, Xinitial, '(F12.5)')
      IF (ModalFlag(IfRow) .EQ. 1) THEN ! Not been set before
        CALL WDialogPutRadioButton(IDF_BiModalRadio) 
        IF (XInitial .GE. 0.00) THEN
          CALL WDialogPutReal(IDF_ModalLower,Zero,'(F12.5)')
          CALL WDialogPutReal(IDF_ModalUpper,OneEighty,'(F12.5)')
          CALL WDialogPutReal(IDF_ReportLower1,((-1)*OneEighty),'(F12.5)')
          CALL WDialogPutReal(IDF_ReportUpper1,Zero,'(F12.5)')
        ELSE
          CALL WDialogPutReal(IDF_ModalLower,((-1)*OneEighty),'(F12.5)')
          CALL WDialogPutReal(IDF_ModalUpper,Zero,'(F12.5)')
          CALL WDialogPutReal(IDF_ReportLower1,Zero,'(F12.5)')
          CALL WDialogPutReal(IDF_ReportUpper1,((-1)*OneEighty),'(F12.5)')
        ENDIF
      ELSE
        CALL WDialogPutReal(IDF_ModalLower,SNGL(lb(IFRow)),'(F12.5)')
        CALL WDialogPutReal(IDF_ModalUpper,SNGL(ub(IFRow)),'(F12.5)')
        IF (ModalFlag(IFRow) .EQ. 2) THEN
          CALL WDialogPutRadioButton(IDF_BiModalRadio)
          IF ((UB(IFRow) * LB(IFRow)) .LT. 0.00) THEN
            CALL WDialogGetReal(IDF_ModalUpper, xtem)
            CALL WDialogPutReal(IDF_ReportLower1, (xtem - 180.00))
            CALL WDialogGetReal(IDF_ModalLower, xtem)
            CALL WDialogPutReal(IDF_ReportUpper1, (xtem + 180.00))
          ELSE
            CALL WDialogGetReal(IDF_ModalUpper, xtem)
            CALL WDialogPutReal(IDF_ReportLower1, (xtem * (-1)))
            CALL WDialogGetReal(IDF_ModalLower, xtem)
            CALL WDialogPutReal(IDF_ReportUpper1, (xtem * (-1)))
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
      CALL WDialogShow(-1, -1, IDD_ModalDialog, SemiModeless)
      RowNumber = IFRow
      iUB = UB(IFRow)
      iLB = LB(IFRow)
      ix = X(IFRow)
      iRadio = ModalFlag(IFRow)
      END SUBROUTINE

!
!*****************************************************************************
!
      SUBROUTINE DealWithBimodalDialog()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,lb,ub,vm
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      REAL             prevx,       prevlb,       prevub
      LOGICAL                                                   LimsChanged
      COMMON /pvalues/ prevx(mvar), prevlb(mvar), prevub(mvar), LimsChanged

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                       iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB
      SAVE   /ModalTorsions/

      INTEGER ICol, NumColumns, ISET
      INTEGER Upper, Lower
      REAL    Zero, OneEighty, xtem, ttem
      DOUBLE PRECISION TempDouble
      REAL TempPrevub, TempPrevlb, TempPrevx
      LOGICAL OutOfBounds

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
                CALL WDialogPutReal(IDF_ReportLower1, (xtem * (-1)))
                CALL WDialogGetReal(IDF_ModalLower, xtem)
                CALL WDialogPutReal(IDF_ReportUpper1, (xtem * (-1)))
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
              xtem = MAX(SNGL(lb(RowNumber)),xtem)
              X(RowNumber)=DBLE(MIN(SNGL(ub(RowNumber)),xtem))
              CALL WDialogPutReal(IDF_Initial, SNGL(x(RowNumber)), '(F12.5)')
            CASE (IDF_ModalLower)
               CALL WDialogGetReal(IDF_ModalLower, xtem)
               xtem = MIN(SNGL(ub(RowNumber)),xtem)
               TempPrevlb = LB(RowNumber)               
               lb(RowNumber) = DBLE(xtem)
               CALL WDialogPutReal(IDF_ModalLower,SNGL(lb(RowNumber)),'(F12.5)')
!              How ranges are calculated depends on state of Modal RadioButton  
               CALL WDialogGetRadioButton(IDF_BimodalRadio, ISET)
                 SELECT CASE (ISET) !bimodal radiobutton active
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
 ! JCC Check the bounding - only update if parameter is set to vary
              CALL WDialogGetReal(IDF_ModalUpper,xtem)
              xtem = MAX(SNGL(lb(RowNumber)),xtem)
              TempPrevUb = UB(RowNumber)             
              ub(RowNumber) = DBLE(xtem)
              CALL WDialogPutReal(IDF_ModalUpper,SNGL(ub(RowNumber)),'(F12.5)')
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
              CALL WDialogGetDouble(IDF_Initial, tempdouble)
              X(RowNumber) = tempdouble
              CALL WDialogGetDouble(IDF_ModalLower, tempdouble)
              lb(RowNumber) = tempdouble                                                                               
              CALL WDialogGetDouble(IDF_ModalUpper, tempdouble)
              ub(RowNumber) = tempdouble
!             Check that x is in bounds
              CALL CheckXInBounds(RowNumber, X(RowNumber), OutOfBounds)
              IF(OutofBounds) THEN
               CALL WarningMessage('Initial value does not fall within defined ranges')
                 IF (WInfoDialog(ExitButtonCommon) .EQ. CommonOk) THEN
                 RETURN
               ENDIF 
              ENDIF
              CALL WDialogHide()
              CALL WDialogSelect(IDD_SA_Modal_Input2)
              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(255, 0, 0), WIN_RGB(256, 256, 256))  
              LimsChanged = .TRUE.
!           Return bounds to previous values
            CASE (IDCANCEL)
              UB(RowNumber) = iUB
              LB(RowNumber) = iLB
              X(RowNumber) = iX
              ModalFlag(RowNumber) = iRadio
              CALL WDialogHide()
!           Return to "unimodal" mode. Modal torsion angle is no longer applied
            CASE (IDF_BiModalReset)
              ub(RowNumber) = OneEighty
              lb(RowNumber) = (-1) * OneEighty
              X(RowNumber) = iX
              ModalFlag(RowNumber) = 1 
              CALL WDialogHide()
              CALL WDialogSelect(IDD_SA_Modal_Input2)
              CALL WGridColourRow(IDF_parameter_grid_modal, RowNumber, WIN_RGB(256, 256, 256), WIN_RGB(256, 256, 256))                                              
          END SELECT
          prevub(RowNumber) = UB(RowNumber)
          prevlb(RowNumber) = LB(RowNumber)
          CALL WDialogSelect(IDD_SA_Modal_Input2)
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, RowNumber, SNGL(X(RowNumber)))
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 2, RowNumber, SNGL(LB(RowNumber)))
          CALL WGridPutCellReal(IDF_parameter_grid_modal, 3, RowNumber, SNGL(UB(RowNumber))) 
          CALL WGridPutCellCheckBox(IDF_parameter_grid_modal,5, RowNumber, UnChecked)                          
      END SELECT
 
      CALL PopActiveWindowID
          
      END SUBROUTINE DealWithBimodalDialog

!
!*****************************************************************************
!
      SUBROUTINE ThreeSixtyToOneEighty(Angle)

      IMPLICIT NONE
      
      REAL Angle
      REAL TempAngle

      IF ((Angle .GE. 0.00) .AND. (Angle .LE. 180.00)) RETURN

      IF ((Angle .GT. 180.00) .AND. (Angle .LE. 360.00)) THEN
        TempAngle = ((-1) * 180.00 + (Angle - 180.00))
        Angle = TempAngle
      ENDIF

      IF ((Angle .LT. -180.00) .AND. (Angle .GE. -360.00)) THEN
        Angle = 360.00 - Angle
      ENDIF
      RETURN
        

      END SUBROUTINE ThreeSixtyToOneEighty
!
!*****************************************************************************
!
      SUBROUTINE OneEightyToThreeSixty(Angle)

      IMPLICIT NONE
      
      REAL Angle
      REAL TempAngle

      IF ((Angle .GE. 0.00) .AND. (Angle .LE. 180.00)) RETURN
      IF ((Angle .GT. 180.00) .AND. (Angle .LE. 360.00)) RETURN

      IF ((Angle .LT. 0.00) .AND. (Angle .GE. -180.00)) THEN
        TempAngle = 360.00 + Angle
        Angle = TempAngle
      ENDIF

      RETURN
        

      END SUBROUTINE OneEightyToThreeSixty

!
!*****************************************************************************
!
      SUBROUTINE DetermineTriModalBounds(xtem, BoundColumn)

      IMPLICIT NONE

      REAL xtem, ttem, zero
      INTEGER BoundColumn

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds

      zero = 0.000     
      TempBounds(1,BoundColumn) = xtem
      CALL OneEightyToThreeSixty(xtem) 
      ttem = xtem + 120.00
      IF (ttem .GE. 360.00) THEN
        Ttem = ttem - 360.00
      ENDIF
      CALL ThreeSixtyToOneEighty(ttem)
      TempBounds(2,BoundColumn) = ttem                  
      ttem = xtem + 240.00
      IF (ttem .GE. 360.00) THEN
        Ttem = ttem - 360.00
      ENDIF
      CALL ThreeSixtyToOneEighty(ttem) 
      TempBounds(3,BoundColumn) = ttem

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

      DO I = 1,3
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

      SUBROUTINE CheckBimodalBounds(row,OneEightyScale)

! Determines whether it is appropriate to use a -180 to 0 and 0 to 180 degree 
! scale.  A 0-360 degree scale may be more appropriate (OneEightyScale = .FALSE.)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,       lb,       ub,       vm
      COMMON /values/  x(MVAR), lb(MVAR), ub(MVAR), vm(MVAR)

      INTEGER row
      LOGICAL OneEightyScale

      OneEightyScale = .TRUE.

      IF (UB(row) * LB(row) .LT. 0.00) THEN
        IF (ABS(UB(Row)) .GT. 90.00) THEN
          OneEightyScale = .FALSE.
        ENDIF
      ENDIF

      END SUBROUTINE CheckBiModalBounds 
      
!
!*****************************************************************************
!

      SUBROUTINE CheckXInBounds(npar, XIn, OutofBounds)

! This Subroutine determines if a trial torsion angle value is within
! modal torsion angle ranges defined.

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR

      IMPLICIT NONE      

      INCLUDE 'PARAMS.INC'

      DOUBLE PRECISION x,lb,ub,vm
      COMMON /values/ x(mvar),lb(mvar),ub(mvar),vm(mvar)

      INTEGER                ModalFlag,       RowNumber      
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNUmber
      SAVE   /ModalTorsions/    

      REAL, DIMENSION (3,2) :: TempBounds
      COMMON /TriModalBounds/  TempBounds 
           
      INTEGER npar, I, Upper, Lower

      LOGICAL OneEightyScale
      LOGICAL OutOfBounds
      REAL xtem, tempupper, templower, tempupper2, templower2
      DOUBLE PRECISION, INTENT (INOUT) :: XIn

      Upper = 1
      Lower = 2

      OutOfBounds = .FALSE.
      SELECT  CASE(ModalFlag(npar))
        CASE (2) ! bimodal ranges
          IF (UB(npar) * LB(npar) .LT. 0.00) THEN ! range such as -170 to 170 defined                                                  
            TempUpper = SNGL(UB(npar))         ! so use 0-360 degree scale
            TempLower = SNGL(LB(npar))
            TempLower2 = TempUpper - 180.00
            TempUpper2 = TempLower + 180.00
            CALL OneEightyToThreeSixty(TempUpper)
            CALL OneEightyToThreeSixty(TempLower)
            CALL OneEightyToThreeSixty(TempUpper2)
            CALL OneEightyToThreeSixty(TempLower2)
            xtem = XIn                                                                                     
            IF ((xtem .LT. -180.00) .OR. (xtem .GT. 180.00)) THEN
              OutOfBounds = .TRUE.
            ELSE
              CALL OneEightytoThreeSixty(xtem)
              IF (((xtem .LT. MAX(TempLower, TempLower2)) .AND. (xtem .GT. MIN(TempLower, TempLower2))) &
              .OR. ((xtem .LT. MAX(TempUpper, TempUpper2)) .AND. (xtem .GT. MIN(TempUpper, TempUpper2)))) THEN
                OutOfBounds = .TRUE.                                       
              ENDIF
            ENDIF

          ELSEIF (UB(npar) * LB(npar) .GE. 0.00) THEN ! range such as 30-90 degs or -30- -90 defined
              IF ((XIn .LT. -180.00) .OR. (XIn .GT. 180.00)) THEN
                OutOfBounds = .TRUE.     
              ELSE
                IF ((XIn .LT. LB(npar)) .OR. (XIn .GT. UB(npar))) THEN
                  IF (((XIn .LT. (-1)*UB(npar)) .OR. (XIn .GT. (-1)*LB(npar)))) THEN !out of bounds            
                    OutOfBounds = .TRUE.
                  ENDIF
                ENDIF
              ENDIF
          ENDIF

        CASE(3) !trimodal ranges
          xtem = XIn
          CALL DetermineTriModalBounds(SNGL(UB(npar)), Upper)
          CALL DetermineTriModalBounds(SNGL(LB(npar)), Lower)
          IF ((xtem .LT. -180.00) .OR. (xtem .GT.180.00)) THEN
            OutOfBounds = .TRUE.
          ELSE                 
            CALL CheckTriModalBounds(OneEightyScale)
            IF (OneEightyScale .EQ. .FALSE.) THEN ! A range such as -170 to 170 has been defined
              CALL OneEightytoThreeSixty(xtem)    ! so use 0-360 scale
              DO I = 1,3
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

      RETURN 

      END SUBROUTINE CheckXInBounds