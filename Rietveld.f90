! Available:
! Dialog2RRVAR() and RRVAR2Dialog()
! Params2RRVAR() and RRVAR2Params()
!
!
!*****************************************************************************
!
      SUBROUTINE ShowWizardWindowRietveld(Curr_SA_Run)
!
! The window containing the Rietveld Refinement needs a lot of initialisation,
! so here is a special routine to open that window.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR
      USE SOLVAR
      USE REFVAR
      USE PO_VAR

      IMPLICIT NONE 

      INTEGER, INTENT (IN   ) :: Curr_SA_Run 

      INCLUDE 'GLBVAR.INC'

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER KK, i, J
      INTEGER iFrg
      INTEGER iRow, iCol, iField
      REAL ChiSqd, ChiProSqd 
      INTEGER tFieldState

      CALL WDialogSelect(IDD_Rietveld2)
      LOG_HYDROGENS = .TRUE.
      CALL CREATE_FOB(.FALSE.)
! Load all values of all bonds, angles etc. into RRVAR variables
      KK = 0
! Loop over all the fragments
      DO iFrg = 1, nFrag
        ! Position centre of mass inside unit cell
        RR_tran(1, iFrg) = BestValuesDoF(KK+1, Curr_SA_Run)
        RR_tran(2, iFrg) = BestValuesDoF(KK+2, Curr_SA_Run)
        RR_tran(3, iFrg) = BestValuesDoF(KK+3, Curr_SA_Run)
        KK = KK + 3
! If more than one atom then proceed
        IF (natoms(iFrg) .GT. 1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
          IF (UseQuaternions(iFrg)) THEN
            RR_rot(1, iFrg) = BestValuesDoF(KK+1, Curr_SA_Run)
            RR_rot(2, iFrg) = BestValuesDoF(KK+2, Curr_SA_Run)
            RR_rot(3, iFrg) = BestValuesDoF(KK+3, Curr_SA_Run)
            RR_rot(4, iFrg) = BestValuesDoF(KK+4, Curr_SA_Run)
            KK = KK + 4
          ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
            RR_rot(1, iFrg) = BestValuesDoF(KK+1, Curr_SA_Run)
            RR_rot(2, iFrg) = BestValuesDoF(KK+2, Curr_SA_Run)
            KK = KK + 2
          ENDIF
        ENDIF
        DO I = 1, natoms(iFrg)
          IF (IOPTB(i, iFrg) .EQ. 1) THEN
            KK = KK + 1
            RR_blen(i, iFrg) = BestValuesDoF(KK, Curr_SA_Run)
          ELSE
            RR_blen(i, iFrg) = blen(i,iFrg)
          ENDIF
          IF (IOPTA(i, iFrg) .EQ. 1) THEN
            KK = KK + 1
            RR_alph(i, iFrg) = BestValuesDoF(KK, Curr_SA_Run)
          ELSE
            RR_alph(i, iFrg) = alph(i,iFrg)
          ENDIF
          IF (IOPTT(i, iFrg) .EQ. 1) THEN
            KK = KK + 1
            RR_bet(i, iFrg) = BestValuesDoF(KK, Curr_SA_Run)
          ELSE
            RR_bet(i, iFrg) = bet(i, iFrg)
          ENDIF
        ENDDO
      ENDDO
      RR_ITF = 1.0
      IF (PrefParExists) THEN
        KK = KK + 1
        RR_PO = BestValuesDoF(KK, Curr_SA_Run)
      ELSE
        RR_PO = 1.0
      ENDIF
      RR_iopttran = 0
      RR_ioptrot = 0
      CALL WDialogPutInteger(IDI_Num1, 0)
      RR_ioptb = 0
      CALL WDialogPutInteger(IDI_Num4, 0)
      RR_iopta = 0
      CALL WDialogPutInteger(IDI_Num3, 0)
      RR_ioptt = 0
      CALL WDialogPutInteger(IDI_Num2, 0)
      RR_ioptITF = 1
      RR_ioptPO = 0
      ! Fill RR_Show_bond etc.
      CALL Set_Show_bond
      CALL Set_Show_angle
      CALL Set_Show_torsion
! Fill and display dialogue
! First set labels and number of rows
      iRow = 1
      iCol = 1
      iField = IDF_RR_ZmatrixGrid
      DO iFrg = 1, nFrag
        ! Translations
        CALL WGridLabelRow(iField, iRow, 'x')
        iRow = iRow + 1
        CALL WGridLabelRow(iField, iRow, 'y')
        iRow = iRow + 1
        CALL WGridLabelRow(iField, iRow, 'z')
        iRow = iRow + 1
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          CALL WGridLabelRow(iField, iRow, 'Q0')
          iRow = iRow + 1
          CALL WGridLabelRow(iField, iRow, 'Q1')
          iRow = iRow + 1
          IF (UseQuaternions(iFrg)) THEN
            CALL WGridLabelRow(iField, iRow, 'Q2')
            iRow = iRow + 1
            CALL WGridLabelRow(iField, iRow, 'Q3')
            iRow = iRow + 1
          ENDIF
        ENDIF
      ENDDO
      CALL WGridRows(iField, iRow-1)
      CALL Set_Show_bond
      CALL Set_Show_angle
      CALL Set_Show_torsion
      CALL RRVAR2Dialog
      ! Save PO state. This is necessary because this state can be changed during Rietveld and
      ! hence must be restored when the Rietveld window is closed.
      SA_PrefParExists = PrefParExists
      SA_PO_Direction(1:3) = PO_Direction(1:3)
      IF (PrefParExists) THEN
        CALL WDialogFieldState(IDC_PO, Enabled)
        CALL WDialogFieldState(IDR_PO, Enabled)
        CALL WDialogSelect(IDD_RR_PO_Dialog)
        CALL WDialogPutInteger(IDF_PO_a, PO_Direction(1))
        CALL WDialogPutInteger(IDF_PO_b, PO_Direction(2))
        CALL WDialogPutInteger(IDF_PO_c, PO_Direction(3))
        tFieldState = Enabled
      ELSE
        CALL WDialogFieldState(IDC_PO, Disabled)
        CALL WDialogFieldState(IDR_PO, Disabled)
        CALL WDialogSelect(IDD_RR_PO_Dialog)
        CALL WDialogPutInteger(IDF_PO_a, 0)
        CALL WDialogPutInteger(IDF_PO_b, 0)
        CALL WDialogPutInteger(IDF_PO_c, 1)
        tFieldState = Disabled
      ENDIF
      CALL WDialogPutCheckBoxLogical(IDF_Use_PO, PrefParExists)
      CALL WDialogFieldState(IDF_PO_a, tFieldState)
      CALL WDialogFieldState(IDF_PO_b, tFieldState)
      CALL WDialogFieldState(IDF_PO_c, tFieldState)
      CALL WDialogFieldState(IDF_LABELa, tFieldState)
      CALL WDialogFieldState(IDF_LABELb, tFieldState)
      CALL WDialogFieldState(IDF_LABELc, tFieldState)
      CALL WDialogSelect(IDD_Rietveld2)
      ! Initialise PO
      IF (PrefParExists) CALL PO_PRECFC(RR_PO)
      ! Initialise ITF
      CALL CreateFobITF
      ! Initialise XATO(1:3,1:150)
      CALL RR_MAKEFRAC
      ! Store initial crystal structure for comparison
      DO I = 1, NATOM
        DO J = 1, 3
          RR_XATO_Orig(J,I) = XATO(J,I)
        ENDDO
      ENDDO
      CALL MakRHm
      CALL RR_VALCHI(ChiSqd)
      CALL VALCHIPRO(ChiProSqd)
      CALL WDialogPutReal(IDR_INTCHI, ChiSqd, "(F9.2)")
      CALL WDialogPutReal(IDR_PROCHI, ChiProSqd, "(F9.2)")
      IPTYPE = 2
      CALL Profile_Plot
      CALL WizardWindowShow(IDD_Rietveld2)

      END SUBROUTINE ShowWizardWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE Set_Show_bond

      USE DRUID_HEADER
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      IF (WDialogGetCheckBoxLogical(IDC_HideH4)) THEN
        DO iFrg = 1, nFrag
          DO I = 2, natoms(iFrg)
            RR_Show_bond(I,iFrg) = (zmElementCSD(I,iFrg) .NE. 2) .AND. &
                                   (zmElementCSD(iz1(I,iFrg),iFrg) .NE. 2)
          ENDDO
        ENDDO
      ELSE
        RR_Show_bond = .TRUE.
      ENDIF
      ! Now update the dialogue
      ! Determine number of rows
      iRow = 1
      iField = IDF_RR_BondGrid
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_Show_bond(i, iFrg)) THEN
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridRows(iField, iRow-1)
      ! Fill the rows
      iRow = 1
      iCol = 1
      Num = 0
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_Show_bond(i, iFrg)) THEN
            CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))// &
              ':'//OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg))))
            CALL WGridPutCellReal(iField, iCol, iRow, RR_blen(i,iFrg), "(F7.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptb(i,iFrg))
            IF (RR_ioptb(i,iFrg) .EQ. 1) Num = Num + 1
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridSetCell(iField, 1, 1)
      CALL WDialogPutInteger(IDI_Num4,Num)
      CALL PopActiveWindowID

      END SUBROUTINE Set_Show_bond
!
!*****************************************************************************
!
      SUBROUTINE Set_Show_angle

      USE DRUID_HEADER
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      IF (WDialogGetCheckBoxLogical(IDC_HideH3)) THEN
        DO iFrg = 1, nFrag
          DO I = 3, natoms(iFrg)
            RR_Show_angle(I,iFrg) = (zmElementCSD(I,iFrg) .NE. 2) .AND. &
                                    (zmElementCSD(iz1(I,iFrg),iFrg) .NE. 2) .AND. &
                                    (zmElementCSD(iz2(I,iFrg),iFrg) .NE. 2)
          ENDDO
        ENDDO
      ELSE
        RR_Show_angle = .TRUE.
      ENDIF
      ! Now update the dialogue
      ! Determine number of rows
      iRow = 1
      iField = IDF_RR_AngleGrid
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_Show_angle(i, iFrg)) THEN
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridRows(iField, iRow-1)
      ! Fill the rows
      iRow = 1
      iCol = 1
      Num = 0
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_Show_angle(i, iFrg)) THEN
            CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg))))
            CALL WGridPutCellReal(iField, iCol, iRow, RR_alph(i,iFrg), "(F9.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_iopta(i,iFrg))
            IF (RR_iopta(i,iFrg) .EQ. 1) Num = Num + 1
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridSetCell(iField,1,1)
      CALL WDialogPutInteger(IDI_Num3,Num)
      CALL PopActiveWindowID

      END SUBROUTINE Set_Show_angle
!
!*****************************************************************************
!
      SUBROUTINE Set_Show_torsion

      USE DRUID_HEADER
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      IF (WDialogGetCheckBoxLogical(IDC_HideH2)) THEN
        DO iFrg = 1, nFrag
          DO I = 4, natoms(iFrg)
            RR_Show_torsion(I,iFrg) = (ioptt(I,iFrg) .EQ. 1)
          ENDDO
        ENDDO
      ELSE
        RR_Show_torsion = .TRUE.
      ENDIF
      ! Now update the dialogue
      ! Determine number of rows
      iRow = 1
      iField = IDF_RR_TorsionGrid
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_Show_torsion(i, iFrg)) THEN
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridRows(iField, iRow-1)
      ! Fill the rows
      iRow = 1
      iCol = 1
      Num = 0
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_Show_torsion(i, iFrg)) THEN
            CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                  OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                  OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg)))//':'// &
                                  OriginalLabel(iz3(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz3(i,iFrg),iFrg))))
            CALL WGridPutCellReal(iField, iCol, iRow, RR_bet(i,iFrg), "(F10.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptt(i,iFrg))
            IF (RR_ioptt(i,iFrg) .EQ. 1) Num = Num + 1
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WGridSetCell(iField, 1, 1)
      CALL WDialogPutInteger(IDI_Num2, Num)
      CALL PopActiveWindowID

      END SUBROUTINE Set_Show_torsion
!
!*****************************************************************************
!
      SUBROUTINE DealWithWindowRietveld

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE RRVAR
      USE SOLVAR
      USE PO_VAR

      IMPLICIT NONE      

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER I, J
      REAL ChiSqd, ChiProSqd 
      INTEGER iValues(1:100), NVALUES
      INTEGER Num
      CHARACTER*(15) file_name

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Refine)
              CALL RietveldRefinement
            CASE (IDB_Calculate)
              CALL Dialog2RRVAR
              ! Initialise PO
              IF (PrefParExists) CALL PO_PRECFC(RR_PO)
              ! Initialise ITF
              CALL CreateFobITF
              ! Initialise XATO(1:3,1:150)
              CALL RR_MAKEFRAC
              CALL RR_VALCHI(ChiSqd)
              CALL VALCHIPRO(ChiProSqd)
              CALL Profile_Plot
              CALL WDialogPutReal(IDR_INTCHI, ChiSqd, "(F9.2)")
              CALL WDialogPutReal(IDR_PROCHI, ChiProSqd, "(F9.2)")
            CASE (IDCANCEL, IDCLOSE)
      ! Restore PO state. This is necessary because this state can be changed during Rietveld and
      ! hence must be restored when the Rietveld window is closed.
              PrefParExists = SA_PrefParExists
              PO_Direction(1:3) = SA_PO_Direction(1:3)
              CALL EndWizardPastPawley
            CASE (IDB_SaveAs)
              CALL Dialog2RRVAR
              CALL RR_MAKEFRAC
              CALL RR_SaveAs
            CASE (IDB_View)
              CALL Dialog2RRVAR
              CALL RR_MAKEFRAC
! Calls subroutine which opens Mercury window with .pdb file
              ! Fill XAtmCoords(1:3,1:150,0)
              DO I = 1, NATOM
                DO J = 1, 3
                  XAtmCoords(J,I,0) = XATO(J,I)
                ENDDO
              ENDDO
              CALL SA_STRUCTURE_OUTPUT_PDB(0, file_name)
              CALL ViewStructure(file_name)
            CASE (IDB_Compare)
              CALL Dialog2RRVAR
              CALL RR_MAKEFRAC
              CALL RR_Compare
            CASE (IDB_Relabel)
              ! Update memory
              CALL zmRelabelAll
              ! Update current dialogue window
              CALL Set_Show_torsion
              CALL Set_Show_angle
              CALL Set_Show_bond
            CASE (IDB_Clear1)
              iValues = 0
              CALL WGridPutCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, 100)
              CALL WDialogPutInteger(IDI_Num1,0)
            CASE (IDB_Clear2)
              iValues = 0
              CALL WGridPutCheckBox(IDF_RR_TorsionGrid, 2, iValues, 100)
              CALL WDialogPutInteger(IDI_Num2,0)
            CASE (IDB_Clear3)
              iValues = 0
              CALL WGridPutCheckBox(IDF_RR_AngleGrid, 2, iValues, 100)
              CALL WDialogPutInteger(IDI_Num3,0)
            CASE (IDB_Clear4)
              iValues = 0
              CALL WGridPutCheckBox(IDF_RR_BondGrid, 2, iValues, 100)
              CALL WDialogPutInteger(IDI_Num4,0)
            CASE (IDB_Set1)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, NVALUES)
              CALL WGridGetCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num1, NVALUES)
            CASE (IDB_Set2)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              CALL WGridGetCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num2, NVALUES)
            CASE (IDB_Set3)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              CALL WGridGetCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num3, NVALUES)
            CASE (IDB_Set4)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
              CALL WGridGetCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num4, NVALUES)
            CASE (IDB_PO_Settings)
              CALL WDialogSelect(IDD_RR_PO_Dialog)
              CALL WDialogShow(-1, -1, 0, SemiModeLess)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDC_HideH2)
              CALL Set_Show_torsion
            CASE (IDC_HideH3)
              CALL Set_Show_angle
            CASE (IDC_HideH4)
              CALL Set_Show_bond
            CASE (IDF_RR_ZmatrixGrid)
              NVALUES = 100
              CALL WGridGetCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num1,Num)
            CASE (IDF_RR_TorsionGrid)
              NVALUES = 100
              CALL WGridGetCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num2,Num)
            CASE (IDF_RR_AngleGrid)
              NVALUES = 100
              CALL WGridGetCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num3,Num)
            CASE (IDF_RR_BondGrid)
              NVALUES = 100
              CALL WGridGetCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num4,Num)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE DealWithRR_PO_Settings

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE      

      LOGICAL, EXTERNAL :: Confirm, WDialogGetCheckBoxLogical
      INTEGER tFieldState
      REAL ChiSqd, ChiProSqd 

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_RR_PO_Dialog)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK, IDCANCEL)
              CALL WDialogHide
              PrefParExists = WDialogGetCheckBoxLogical(IDF_Use_PO)
              !C Initialise preferred orientation and recalculate pattern + chi-sqrds
              !C Initialise PO
              IF (PrefParExists) THEN
                CALL WDialogGetInteger(IDF_PO_a, PO_Direction(1))
                CALL WDialogGetInteger(IDF_PO_b, PO_Direction(2))
                CALL WDialogGetInteger(IDF_PO_c, PO_Direction(3))
                CALL WDialogSelect(IDD_Rietveld2)
                CALL WDialogGetReal(IDR_PO, RR_PO)
                CALL FillSymmetry_2
                CALL PO_PRECFC(RR_PO)
              ENDIF
              CALL RR_VALCHI(ChiSqd)
              CALL VALCHIPRO(ChiProSqd)
              !C Update the main Rietveld window
              CALL WDialogSelect(IDD_Rietveld2)
              IF (PrefParExists) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
                CALL WDialogPutCheckBox(IDC_PO, 0)
              ENDIF
              CALL WDialogFieldState(IDC_PO, tFieldState)
              CALL WDialogFieldState(IDR_PO, tFieldState)
              CALL WDialogPutReal(IDR_INTCHI, ChiSqd, "(F9.2)")
              CALL WDialogPutReal(IDR_PROCHI, ChiProSqd, "(F9.2)")
              CALL Profile_Plot
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

      END SUBROUTINE DealWithRR_PO_Settings
!
!*****************************************************************************
!
      SUBROUTINE RietveldRefinement 

      USE WINTERACTER
      USE DRUID_HEADER
      USE RRVAR
      USE PO_VAR

      IMPLICIT NONE

      REAL ChiSqd, ChiProSqd 

      CALL Dialog2RRVAR
      ! Initialise PO
      IF (PrefParExists) CALL PO_PRECFC(RR_PO)
      ! Initialise ITF
      CALL CreateFobITF
      ! Initialise XATO(1:3,1:150)
      CALL RR_MAKEFRAC
      CALL RRVAR2Params
      IF (RR_npar .EQ. 0) THEN
        CALL InfoMessage('No refinable parameters selected.')
        RETURN
      ENDIF
      CALL WCursorShape(CurHourGlass)
      CALL RR_SIMOPT(RR_Params, RR_InitSteps, RR_npar, ChiSqd)
      CALL Params2RRVAR
      CALL RRVAR2Dialog
      CALL VALCHIPRO(ChiProSqd)
      CALL WCursorShape(CurCrossHair)
      CALL Profile_Plot
      CALL WDialogPutReal(IDR_INTCHI, ChiSqd, "(F9.2)")
      CALL WDialogPutReal(IDR_PROCHI, ChiProSqd, "(F9.2)")

      END SUBROUTINE RietveldRefinement
!
!*****************************************************************************
!
      SUBROUTINE Dialog2RRVAR

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iRow, iCol, iField, iFrg

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      iRow = 1
      iCol = 1
      iField = IDF_RR_ZmatrixGrid
      DO iFrg = 1, nFrag
        ! Translations
        CALL WGridGetCellReal(iField, iCol, iRow, RR_tran(1,iFrg))
        CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(1,iFrg))
        iRow = iRow + 1
        CALL WGridGetCellReal(iField, iCol, iRow, RR_tran(2,iFrg))
        CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(2,iFrg))
        iRow = iRow + 1
        CALL WGridGetCellReal(iField, iCol, iRow, RR_tran(3,iFrg))
        CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(3,iFrg))
        iRow = iRow + 1
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          CALL WGridGetCellReal(iField, iCol, iRow, RR_rot(1,iFrg))
          CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(1,iFrg))
          iRow = iRow + 1
          CALL WGridGetCellReal(iField, iCol, iRow, RR_rot(2,iFrg))
          CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(2,iFrg))
          iRow = iRow + 1
          IF (UseQuaternions(iFrg)) THEN
            CALL WGridGetCellReal(iField, iCol, iRow, RR_rot(3,iFrg))
            CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(3,iFrg))
            iRow = iRow + 1
            CALL WGridGetCellReal(iField, iCol, iRow, RR_rot(4,iFrg))
            CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(4,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDIF
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_BondGrid
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_Show_bond(i, iFrg)) THEN
            CALL WGridGetCellReal(iField, iCol, iRow, RR_blen(i,iFrg))
            CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptb(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_AngleGrid
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_Show_angle(i, iFrg)) THEN
            CALL WGridGetCellReal(iField, iCol, iRow, RR_alph(i,iFrg))
            CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopta(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_TorsionGrid
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_Show_torsion(i, iFrg)) THEN
            CALL WGridGetCellReal(iField, iCol, iRow, RR_bet(i,iFrg))
            CALL WGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptt(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WDialogGetCheckBox(IDC_ITF, RR_ioptITF)
      CALL WDialogGetReal(IDR_ITF, RR_ITF)
      CALL WDialogGetCheckBox(IDC_PO, RR_ioptPO)
      CALL WDialogGetReal(IDR_PO, RR_PO)
      CALL PopActiveWindowID

      END SUBROUTINE Dialog2RRVAR
!
!*****************************************************************************
!
      SUBROUTINE RRVAR2Dialog

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iRow, iCol, iField, iFrg

! Fill and display dialogue
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      iRow = 1
      iCol = 1
      iField = IDF_RR_ZmatrixGrid
      DO iFrg = 1, nFrag
        ! Translations
        CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(1,iFrg), "(F7.5)")
        CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_iopttran(1,iFrg))
        iRow = iRow + 1
        CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(2,iFrg), "(F7.5)")
        CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_iopttran(2,iFrg))
        iRow = iRow + 1
        CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(3,iFrg), "(F7.5)")
        CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_iopttran(3,iFrg))
        iRow = iRow + 1
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(1,iFrg), "(F8.5)")
          CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(1,iFrg))
          iRow = iRow + 1
          CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(2,iFrg), "(F8.5)")
          CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(2,iFrg))
          iRow = iRow + 1
          IF (UseQuaternions(iFrg)) THEN
            CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(3,iFrg), "(F8.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(3,iFrg))
            iRow = iRow + 1
            CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(4,iFrg), "(F8.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(4,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDIF
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_BondGrid
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_Show_bond(i, iFrg)) THEN
            CALL WGridPutCellReal(iField, iCol, iRow, RR_blen(i,iFrg), "(F7.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptb(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_AngleGrid
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_Show_angle(i, iFrg)) THEN
            CALL WGridPutCellReal(iField, iCol, iRow, RR_alph(i,iFrg), "(F9.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_iopta(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_TorsionGrid
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_Show_torsion(i, iFrg)) THEN
            CALL WGridPutCellReal(iField, iCol, iRow, RR_bet(i,iFrg), "(F10.5)")
            CALL WGridPutCellCheckBox(iField, iCol+1, iRow, RR_ioptt(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL WDialogPutCheckBox(IDC_ITF,RR_ioptITF)
      CALL WDialogPutReal(IDR_ITF,RR_ITF)
      CALL WDialogPutCheckBox(IDC_PO,RR_ioptPO)
      CALL WDialogPutReal(IDR_PO,RR_PO)
      CALL PopActiveWindowID

      END SUBROUTINE RRVAR2Dialog
!
!*****************************************************************************
!
      SUBROUTINE Params2RRVAR

      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iFrg, iParam

      iParam = 1
      DO iFrg = 1, nFrag
        ! Translations
        IF (RR_iopttran(1,iFrg) .EQ. 1) THEN
          RR_tran(1,iFrg) = RR_Params(iParam)
          iParam = iParam + 1
        ENDIF
        IF (RR_iopttran(2,iFrg) .EQ. 1) THEN
          RR_tran(2,iFrg) = RR_Params(iParam)
          iParam = iParam + 1
        ENDIF
        IF (RR_iopttran(3,iFrg) .EQ. 1) THEN
          RR_tran(3,iFrg) = RR_Params(iParam)
          iParam = iParam + 1
        ENDIF
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          IF (RR_ioptrot(1,iFrg) .EQ. 1) THEN
            RR_rot(1,iFrg) = RR_Params(iParam)
            iParam = iParam + 1
          ENDIF
          IF (RR_ioptrot(2,iFrg) .EQ. 1) THEN
            RR_rot(2,iFrg) = RR_Params(iParam)
            iParam = iParam + 1
          ENDIF
          IF (UseQuaternions(iFrg)) THEN
            IF (RR_ioptrot(3,iFrg) .EQ. 1) THEN
              RR_rot(3,iFrg) = RR_Params(iParam)
              iParam = iParam + 1
            ENDIF
            IF (RR_ioptrot(4,iFrg) .EQ. 1) THEN
              RR_rot(4,iFrg) = RR_Params(iParam)
              iParam = iParam + 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_ioptb(i,iFrg) .EQ. 1) THEN
            RR_blen(i,iFrg) = RR_Params(iParam)
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_iopta(i,iFrg) .EQ. 1) THEN
            RR_alph(i,iFrg) = RR_Params(iParam)
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_ioptt(i,iFrg) .EQ. 1) THEN
            RR_bet(i,iFrg) = RR_Params(iParam)
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      IF (RR_ioptITF .EQ. 1) THEN
        RR_ITF = RR_Params(iParam)
        iParam = iParam + 1
      ENDIF
      IF (RR_ioptPO .EQ. 1) THEN
        RR_PO = RR_Params(iParam)
        iParam = iParam + 1
      ENDIF

      END SUBROUTINE Params2RRVAR
!
!*****************************************************************************
!
      SUBROUTINE RRVAR2Params

      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iFrg, iParam
      REAL Damping

      Damping = 0.1
      iParam = 1
      DO iFrg = 1, nFrag
        ! Translations
        IF (RR_iopttran(1,iFrg) .EQ. 1) THEN
          RR_Params(iParam) = RR_tran(1,iFrg)
          RR_InitSteps(iParam) = 0.1 * Damping
          iParam = iParam + 1
        ENDIF
        IF (RR_iopttran(2,iFrg) .EQ. 1) THEN
          RR_Params(iParam) = RR_tran(2,iFrg)
          RR_InitSteps(iParam) = 0.1 * Damping
          iParam = iParam + 1
        ENDIF
        IF (RR_iopttran(3,iFrg) .EQ. 1) THEN
          RR_Params(iParam) = RR_tran(3,iFrg)
          RR_InitSteps(iParam) = 0.1 * Damping
          iParam = iParam + 1
        ENDIF
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          IF (RR_ioptrot(1,iFrg) .EQ. 1) THEN
            RR_Params(iParam) = RR_rot(1,iFrg)
            RR_InitSteps(iParam) = 0.1 * Damping
            iParam = iParam + 1
          ENDIF
          IF (RR_ioptrot(2,iFrg) .EQ. 1) THEN
            RR_Params(iParam) = RR_rot(2,iFrg)
            RR_InitSteps(iParam) = 0.1 * Damping
            iParam = iParam + 1
          ENDIF
          IF (UseQuaternions(iFrg)) THEN
            IF (RR_ioptrot(3,iFrg) .EQ. 1) THEN
              RR_Params(iParam) = RR_rot(3,iFrg)
              RR_InitSteps(iParam) = 0.1 * Damping
              iParam = iParam + 1
            ENDIF
            IF (RR_ioptrot(4,iFrg) .EQ. 1) THEN
              RR_Params(iParam) = RR_rot(4,iFrg)
              RR_InitSteps(iParam) = 0.1 * Damping
              iParam = iParam + 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 2, natoms(iFrg)
          IF (RR_ioptb(i,iFrg) .EQ. 1) THEN
            RR_Params(iParam) = RR_blen(i,iFrg)
            RR_InitSteps(iParam) = 0.05 * Damping
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 3, natoms(iFrg)
          IF (RR_iopta(i,iFrg) .EQ. 1) THEN
            RR_Params(iParam) = RR_alph(i,iFrg)
            RR_InitSteps(iParam) = 1.0 * Damping
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      DO iFrg = 1, nFrag
        DO i = 4, natoms(iFrg)
          IF (RR_ioptt(i,iFrg) .EQ. 1) THEN
            RR_Params(iParam) = RR_bet(i,iFrg)
            RR_InitSteps(iParam) = 5.0 * Damping
            iParam = iParam + 1
          ENDIF
        ENDDO
      ENDDO
      IF (RR_ioptITF .EQ. 1) THEN
        RR_var2ITF = iParam
        RR_Params(iParam) = RR_ITF
        RR_InitSteps(iParam) = 0.1 * Damping
        iParam = iParam + 1
      ENDIF
      IF (RR_ioptPO .EQ. 1) THEN
        RR_Params(iParam) =  RR_PO
        RR_InitSteps(iParam) = 0.1 * Damping
        iParam = iParam + 1
      ENDIF
      RR_npar = iParam - 1

      END SUBROUTINE RRVAR2Params
!
!*****************************************************************************
!
! Fills REAL XATO(1:3,1:150) using variables from RRVAR
!
      SUBROUTINE RR_MAKEFRAC

      USE VARIABLES
      USE ZMVAR
      USE RRVAR
      USE SAMVAR

      IMPLICIT NONE

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MAXATM)

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      INTEGER KATOM, i, KI
      INTEGER iFrg
      REAL TRAN(1:3)
      REAL QQSUM, QDEN, QUATER(0:3), tQ(0:3), ROTA(1:3,1:3), Duonion(0:1)
      INTEGER JQ, ICFRG
      REAL XC, YC, ZC

      KATOM = 0
! Loop over all the fragments
      DO iFrg = 1, nFrag
        ! Position centre of mass inside unit cell
        RR_tran(1, iFrg) = RR_tran(1, iFrg) - INT(RR_tran(1, iFrg))
        RR_tran(2, iFrg) = RR_tran(2, iFrg) - INT(RR_tran(2, iFrg))
        RR_tran(3, iFrg) = RR_tran(3, iFrg) - INT(RR_tran(3 ,iFrg))
        CALL PremultiplyVectorByMatrix(f2cmat, RR_tran(1, iFrg), TRAN)
! If more than one atom then proceed
        IF (natoms(iFrg) .GT. 1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
          IF (UseQuaternions(iFrg)) THEN
            QQSUM = 0.0
            DO JQ = 1, 4
              QQSUM = QQSUM + RR_rot(JQ, iFrg)**2
            ENDDO
! QQSUM now holds the sum of the squares of the quaternions
            QDEN = 1.0 / SQRT(QQSUM)
            DO JQ = 0, 3
              QUATER(JQ) = QDEN * RR_rot(JQ+1, iFrg)
            ENDDO
! QUATER now holds the normalised quaternions
            CALL Quaternion2Matrix(QUATER, ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
          ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
            Duonion(0) = RR_rot(1, iFrg)
            Duonion(1) = RR_rot(2, iFrg)
            QDEN = 1.0 / SQRT(Duonion(0)**2 + Duonion(1)**2)
            Duonion(0) = Duonion(0) * QDEN 
            Duonion(1) = Duonion(1) * QDEN 
            QUATER(0) = Duonion(0) * zmSingleRotationQs(0, iFrg)
            QUATER(1) = Duonion(1) * zmSingleRotationQs(1, iFrg)
            QUATER(2) = Duonion(1) * zmSingleRotationQs(2, iFrg)
            QUATER(3) = Duonion(1) * zmSingleRotationQs(3, iFrg)
! QUATER now holds the normalised quaternion corresponding to the single rotation axis
! Now premultiply with the original molecular orientation (JvdS I don't understand why
! they must be premultiplied: I would say they should be postmultiplied)
            CALL QuaternionMultiply(zmInitialQs(0,iFrg), QUATER, tQ)  
            CALL Quaternion2Matrix(tQ, ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the single rotation axis
          ENDIF
        ENDIF
        CALL makexyz(natoms(iFrg),RR_blen(1,iFrg),RR_alph(1,iFrg),RR_bet(1,iFrg),        &
                     IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),axyzo)
! Determine origin for rotations
        ICFRG = ICOMFLG(iFrg)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
        IF (ICFRG.EQ.0) THEN
          XC = 0.0
          YC = 0.0
          ZC = 0.0
          IF (UseCCoM) THEN
            DO I = 1, natoms(iFrg)
              XC = XC + AtomicWeighting(I,iFrg)*axyzo(1,I)
              YC = YC + AtomicWeighting(I,iFrg)*axyzo(2,I)
              ZC = ZC + AtomicWeighting(I,iFrg)*axyzo(3,I)
            ENDDO
          ELSE
            DO I = 1, natoms(iFrg)
              XC = XC + axyzo(1,I)
              YC = YC + axyzo(2,I)
              ZC = ZC + axyzo(3,I)
            ENDDO
            XC = XC/FLOAT(natoms(iFrg))
            YC = YC/FLOAT(natoms(iFrg))
            ZC = ZC/FLOAT(natoms(iFrg))
          ENDIF
! Otherwise, use atom number ICFRG
        ELSE
          XC = axyzo(1,ICFRG)
          YC = axyzo(2,ICFRG)
          ZC = axyzo(3,ICFRG)
        ENDIF
! Subtract the origin from all atom positions
        DO I = 1, natoms(iFrg)
          axyzo(1,I) = axyzo(1,I) - XC
          axyzo(2,I) = axyzo(2,I) - YC
          axyzo(3,I) = axyzo(3,I) - ZC
        ENDDO
! Apply rotation and translation to the atoms of this Z-matrix
        CALL DO_ATOM_POS(TRAN, ROTA, axyzo, natoms(iFrg))
! When we are here, we have the actual co-ordinates of all the atoms in this Z-matrix
! in Cartesian (orthogonal) co-ordinates. We need fractional co-ordinates: convert.
        DO I = 1, natoms(iFrg)
          KI = KATOM + I
! Note that we must reorder the atoms such that the hydrogens are appended after the 
! non-hydrogens.
          CALL PremultiplyVectorByMatrix(c2fmat, axyzo(1,I), XATO(1,OrderedAtm(KI)))
        ENDDO
        KATOM = KATOM + natoms(iFrg)
      ENDDO

      END SUBROUTINE RR_MAKEFRAC
!
!*****************************************************************************
!
      SUBROUTINE RR_Compare

      USE ZMVAR
      USE ATMVAR
      USE RRVAR

      IMPLICIT NONE

! Writes out original and Rietveld refined crystal structure to pdb file for visual comparison
      INCLUDE 'PARAMS.INC'

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

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

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      INTEGER, EXTERNAL :: WritePDBCommon
      INTEGER iSol
      INTEGER pdbBond(1:maxbnd_2*maxfrg,1:2)
      INTEGER TotNumBonds, NumOfAtomsSoFar
      CHARACTER*4 LabelStr
      CHARACTER*2 ColourStr
      CHARACTER*2 SolStr
      INTEGER AtomLabelOption, AtomColourOption
      INTEGER I, iFrg, J, iiact, BondNr
      REAL    x_pdb(1:3)
      INTEGER iAtom
      INTEGER hFilePDB

      CALL PushActiveWindowID
      hFilePDB = 65
! Write the file headers first
      OPEN (UNIT=hFilePDB, FILE='Overlap_Temp.pdb', STATUS='unknown', ERR=999)
! Add in a Header record
      WRITE (hFilePDB, 1036, ERR=999)
 1036 FORMAT ('HEADER    PDB Solution File generated by DASH')
      IF (WritePDBCommon(hFilePDB) .NE. 0) GOTO 999
!C! Get atom label option from dialogue. Two options: 
!C! 1. "Element + solution #"
!C! 2. "Original atom labels"
!C      CALL WDialogGetRadioButton(IDF_UseSolutionNr,AtomLabelOption)
      AtomLabelOption = 2
!C! Get atom colour option from dialogue. Two options: 
!C! 1. "By solution number"
!C! 2. "By element"
!C      CALL WDialogGetRadioButton(IDF_ColourBySolution,AtomColourOption)
      AtomColourOption = 2

! Original structure
      iiact = 0
      SolStr = "_O"
! Note that elements are right-justified
      IF (AtomColourOption .EQ. 1) THEN ! Colour by solution
        ColourStr = ' S'  ! Sulphur        Yellow
      ENDIF
      iAtom = 0
      DO iFrg = 1, nFrag
        DO i = 1, natoms(iFrg)
          iiact = iiact + 1
          iAtom = iAtom + 1
          CALL PremultiplyVectorByMatrix(f2cpdb, RR_XATO_Orig(1,OrderedAtm(iAtom)), x_pdb)
! Note that elements are right-justified
          IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
            IF (ElSym(i,iFrg)(2:2) .EQ. ' ') THEN
              ColourStr(1:2) = ' '//ElSym(i,iFrg)(1:1)
            ELSE
              ColourStr = ElSym(i,iFrg)
            ENDIF
          ENDIF
          IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
            LabelStr = ElSym(i,iFrg)(1:LEN_TRIM(ElSym(i,iFrg)))//SolStr
          ELSE  ! Orignal atom labels
            LabelStr(1:4) = OriginalLabel(i,iFrg)(1:4)
          ENDIF
          WRITE (hFilePDB,1120,ERR=999) iiact, LabelStr(1:4), x_pdb(1), x_pdb(2), x_pdb(3), occ(i,iFrg), tiso(i,iFrg), ColourStr(1:2)
 1120     FORMAT ('HETATM',I5,' ',A4' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
        ENDDO ! loop over atoms
      ENDDO ! loop over Z-matrices
      ! Rietveld refined structure
      SolStr = "_N"
! Note that elements are right-justified
      IF (AtomColourOption .EQ. 1) THEN ! Colour by solution
        ColourStr = ' O'  ! Oxygen        Red
      ENDIF
      iAtom = 0
      DO iFrg = 1, nFrag
        DO i = 1, natoms(iFrg)
          iiact = iiact + 1
          iAtom = iAtom + 1
          CALL PremultiplyVectorByMatrix(f2cpdb, Xato(1,OrderedAtm(iAtom)), x_pdb)
! Note that elements are right-justified
          IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
            IF (ElSym(i,iFrg)(2:2) .EQ. ' ') THEN
              ColourStr(1:2) = ' '//ElSym(i,iFrg)(1:1)
            ELSE
              ColourStr = ElSym(i,iFrg)
            ENDIF
          ENDIF
          IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
            LabelStr = ElSym(i,iFrg)(1:LEN_TRIM(ElSym(i,iFrg)))//SolStr
          ELSE  ! Orignal atom labels
            LabelStr(1:4) = OriginalLabel(i,iFrg)(1:4)
          ENDIF
          WRITE (hFilePDB,1120,ERR=999) iiact, LabelStr(1:4), x_pdb(1), x_pdb(2), x_pdb(3), occ(i,iFrg), tiso(i,iFrg), ColourStr(1:2)
        ENDDO ! loop over atoms
      ENDDO ! loop over Z-matrices
! Per Z-matrix, determine the connectivity. This has to be done only once.
      TotNumBonds = 0
      NumOfAtomsSoFar = 0
      DO iFrg = 1, nFrag
        IF (NumberOfBonds(iFrg) .GT. 0) THEN
          DO J = 1, NumberOfBonds(iFrg)
            pdbBond(J+TotNumBonds,1) = Bonds(1,J,iFrg) + NumOfAtomsSoFar
            pdbBond(J+TotNumBonds,2) = Bonds(2,J,iFrg) + NumOfAtomsSoFar
          ENDDO
        ENDIF
        NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
        TotNumBonds = TotNumBonds + NumberOfBonds(iFrg)
      ENDDO ! loop over Z-matrices
      DO iSol = 1, 2
        DO BondNr = 1, TotNumBonds
          WRITE(hFilePDB,'(A6,I5,I5)',ERR=999) 'CONECT', (pdbBond(BondNr,1)+NATOM*(iSol-1)), (pdbBond(BondNr,2)+NATOM*(iSol-1))
        ENDDO
      ENDDO ! loop over "runs"
      WRITE (hFilePDB,"('END')",ERR=999)
      CLOSE (hFilePDB)
      CALL ViewStructure('Overlap_Temp.pdb')
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing temporary file.')
      CLOSE (hFilePDB)
      CALL PopActiveWindowID

      END SUBROUTINE RR_Compare
!
!*****************************************************************************
!
      SUBROUTINE RR_SaveAs
! Pop up file window, user must select file name + file type (through extension).

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:RR_maxatm)

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

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      CHARACTER*1, EXTERNAL :: ChrLowerCase
      INTEGER, EXTERNAL :: WritePDBCommon
      REAL, EXTERNAL :: UnitCellVolume
      LOGICAL, EXTERNAL :: Get_DivideByEsd
      INTEGER iFlags, iFType, hFile, ii, iiact, iTotal, iFrg, i, k, iOrig
      INTEGER iTem, j, iBond1, iBond2, NumOfAtomsSoFar, tLen, iRadSelection
      INTEGER tLen1, tLen2, NumOfAtmPerElm(1:MaxElm), iScat, K1, tElement
      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=255) :: FILTER
      REAL    x_pdb(1:3)
      CHARACTER*80 tString, tString1, tString2
      CHARACTER*2  LATT

      iFlags = SaveDialog + AppendExt + PromptOn
      FILTER = 'pdb (*.pdb)|*.pdb|'// &
               'cssr (*.cssr)|*.cssr|'// &
               'ccl (*.ccl)|*.ccl|'// &
               'cif (*.cif)|*.cif|'// &
               'SHELX (*.res)|*.res|'// &
               'Profile (*.pro)|*.pro|'
      tFileName = ''
      CALL WSelectFile(FILTER,iFlags,tFileName,'Save final structure',iFType)
      IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
        hFile = 10
        OPEN(UNIT=hFile,FILE=tFileName,ERR=999)
        SELECT CASE (iFType)
          CASE (1) ! pdb
! Add in a Header record
            WRITE (hFile,"('HEADER    PDB Solution File generated by DASH')",ERR=999)
            IF (PrefParExists) THEN
              WRITE (hFile,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'REMARK PO', (PO_Direction(ii),ii=1,3), RR_PO
            ENDIF
            IF (WritePDBCommon(hFile) .NE. 0) GOTO 999
            iiact = 0
            itotal = 0
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
! The PDB atom lines
                CALL PremultiplyVectorByMatrix(f2cpdb, Xato(1,ii), x_pdb)
! Note that elements are right-justified
! WebLab viewer even wants the elements in the atom names to be right justified.
                IF (ElSym(iOrig,iFrg)(2:2).EQ.' ') THEN
                  WRITE (hFile,1120,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:3), x_pdb(1), x_pdb(2), x_pdb(3), &
                                  occ(iOrig,iFrg), RR_ITF*tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:1)
 1120             FORMAT ('HETATM',I5,'  ',A3,' NON     1    ',3F8.3,2F6.2,'           ',A1,'  ')
                ELSE
                  WRITE (hFile,1130,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4), x_pdb(1), x_pdb(2), x_pdb(3), &
                                  occ(iOrig,iFrg), RR_ITF*tiso(iOrig,iFrg), ElSym(iOrig,iFrg)(1:2)
 1130             FORMAT ('HETATM',I5,' ',A4,' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
                ENDIF
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
! Per Z-matrix, write out the connectivity.
            NumOfAtomsSoFar = 0
            DO iFrg = 1, nFrag
              IF (NumberOfBonds(iFrg) .GT. 0) THEN
                DO J = 1, NumberOfBonds(iFrg)
! Due to the backmapping, it is possible that the original number of the first atom is greater than the
! original number of the second atom. Mercury can't always read pdb files where this is the case.
                  iBond1 = izmoid(Bonds(1,J,iFrg),iFrg) + NumOfAtomsSoFar
                  iBond2 = izmoid(Bonds(2,J,iFrg),iFrg) + NumOfAtomsSoFar
                  IF (iBond1 .GT. iBond2) THEN
                    iTem   = iBond1
                    iBond1 = iBond2
                    iBond2 = iTem
                  ENDIF
                  WRITE(hFile,'(A6,I5,I5)',ERR=999) 'CONECT', iBond1, iBond2
                ENDDO
              ENDIF
              NumOfAtomsSoFar = NumOfAtomsSoFar + natoms(iFrg)
            ENDDO ! loop over Z-matrices
            WRITE (hFile,"('END')",ERR=999)
          CASE (2) ! cssr
            WRITE (hFile,1000,ERR=999) (CellPar(ii),ii=1,3)
 1000       FORMAT (' REFERENCE STRUCTURE = 00000   A,B,C =',3F8.3)
            WRITE (hFile,1010,ERR=999) (CellPar(ii),ii=4,6), SGNumStr(NumberSGTable)(1:3)
 1010       FORMAT ('   ALPHA,BETA,GAMMA =',3F8.3,'    SPGR = ',A3)
            IF (PrefParExists) THEN
              WRITE (hFile,"(' ',I3,'   0 PO',3(1X,I3),1X,F9.5)",ERR=999) natom,(PO_Direction(ii),ii=1,3), RR_PO
            ELSE
              WRITE (hFile,"(' ',I3,'   0 DASH solution')",ERR=999) natom
            ENDIF
            WRITE (hFile,'(A)',ERR=999) '     1 '
            iiact = 0
            itotal = 0
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
! The CSSR atom lines
                WRITE (hFile,1110,ERR=999) iiact, OriginalLabel(iOrig,iFrg)(1:4),(Xato(k,ii),k=1,3), 0, 0, 0, 0, 0, 0, 0, 0, 0.0
 1110           FORMAT (I4,1X,A4,2X,3(F9.5,1X),8I4,1X,F7.3)
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
          CASE (3) ! ccl
            IF (PrefParExists) THEN
              WRITE (hFile,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'Z PO', (PO_Direction(ii),ii=1,3), RR_PO
            ENDIF
            WRITE (hFile,1100,ERR=999) (CellPar(ii),ii=1,6)
 1100       FORMAT ('C ',6F10.5)
            iiact = 0
            itotal = 0
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
                WRITE (hFile,1033,ERR=999) ElSym(iOrig,iFrg), (Xato(k,ii),k=1,3), RR_ITF*tiso(iOrig,iFrg), occ(iOrig,iFrg) 
 1033           FORMAT ('A ',A2,'  ',F10.5,1X,F10.5,1X,F10.5,1X,F4.2,1X,F4.2)
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
          CASE (4) ! cif
            WRITE (hFile,'("data_global")',ERR=999)
            tString = CrystalSystemString(LatBrav)
            tString(1:1) = ChrLowerCase(tString(1:1))
            tLen = LEN_TRIM(tString)
! Remove '-a' etc. from monoclinic
            IF (LatBrav .EQ. 2 .OR. LatBrav .EQ. 3 .OR. LatBrav .EQ. 4) tLen = tLen -2
            WRITE (hFile,'("_symmetry_cell_setting ",A)',ERR=999) tString(1:tLen)
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
            WRITE (hFile,"('_symmetry_space_group_name_H-M ',A)",ERR=999) tString(1:tLen)
            WRITE (hFile,'("loop_")',ERR=999)
            WRITE (hFile,'("  _symmetry_equiv_pos_as_xyz")',ERR=999)
            DO i = 1, npdbops
              tString = cpdbops(i)
              tLen = LEN_TRIM(tString)
              tString = "  '"//tString(1:tLen)//"'"
              tLen = tLen + 4
              WRITE (hFile,"(A)",ERR=999) tString(1:tLen)
            ENDDO
            WRITE (hFile, '("_cell_length_a    ", F8.4)', ERR=999) CellPar(1)
            WRITE (hFile, '("_cell_length_b    ", F8.4)', ERR=999) CellPar(2)
            WRITE (hFile, '("_cell_length_c    ", F8.4)', ERR=999) CellPar(3)
            WRITE (hFile, '("_cell_angle_alpha ", F8.4)', ERR=999) CellPar(4)
            WRITE (hFile, '("_cell_angle_beta  ", F8.4)', ERR=999) CellPar(5)
            WRITE (hFile, '("_cell_angle_gamma ", F8.4)', ERR=999) CellPar(6)
            WRITE (hFile, '("_cell_volume",F7.1)',ERR=999) UnitCellVolume(CellPar(1), &
                  CellPar(2), CellPar(3), CellPar(4), CellPar(5), CellPar(6))
            WRITE (hFile, '("_cell_formula_units_Z  ?")',ERR=999)
            SELECT CASE (JRadOption)
              CASE (1) ! X-ray lab data
                CALL PushActiveWindowID
                CALL WDialogSelect(IDD_Data_Properties)
                CALL WDialogGetMenu(IDF_Wavelength_Menu,iRadSelection)
                CALL PopActiveWindowID
                SELECT CASE (iRadSelection)
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
            WRITE (hFile, '("_diffrn_radiation_type ",A)', ERR=999) tString(1:tLen)
            WRITE (hFile, '("_diffrn_radiation_wavelength",F10.5)', ERR=999) ALambda
  !@@          WRITE (hFile,'("_refine_ls_goodness_of_fit_all ",F7.3)',ERR=999) SQRT(MAX(0.0,FOPT))
            IF (PrefParExists) THEN
              WRITE (hFile, '("_pd_proc_ls_pref_orient_corr")', ERR=999)               
              WRITE (hFile, '(";")',ERR=999)
              WRITE (hFile, '("  March-Dollase function")', ERR=999)
              WRITE (hFile, '("  Orientation =",3(1X,I3))', ERR=999) (PO_Direction(ii),ii=1,3)
              WRITE (hFile, '("  Magnitude   = ",F6.3)',ERR=999) RR_PO
              WRITE (hFile ,'(";")', ERR=999)
            ENDIF
            WRITE (hFile ,'("loop_")', ERR=999)
            WRITE (hFile, '("  _atom_site_label")', ERR=999)
            WRITE (hFile, '("  _atom_site_fract_x")', ERR=999)
            WRITE (hFile, '("  _atom_site_fract_y")', ERR=999)
            WRITE (hFile, '("  _atom_site_fract_z")', ERR=999)
            WRITE (hFile, '("  _atom_site_occupancy")', ERR=999)
            WRITE (hFile, '("  _atom_site_adp_type")', ERR=999)
            WRITE (hFile, '("  _atom_site_B_iso_or_equiv")', ERR=999)
            iiact = 0
            itotal = 0
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
                WRITE (hFile,1034,ERR=999) OriginalLabel(iOrig,iFrg), (Xato(k,ii),k=1,3), occ(iOrig,iFrg), RR_ITF*tiso(iOrig,iFrg) 
 1034           FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Biso ',F5.2)
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
          CASE (5) ! res
            WRITE (hFile,"('TITL Solution File generated by DASH')",ERR=999)
            IF (PrefParExists) THEN
              WRITE (hFile,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'REM  PO',(PO_Direction(ii),ii=1,3),RR_PO
            ENDIF
            WRITE (hFile,1032,ERR=999) ALambda, (CellPar(ii),ii=1,6)
 1032       FORMAT ('CELL ',F7.5,1X,6(F8.4,1X))
            WRITE (hFile,'("ZERR ",I2," 0.000 0.000 0.000 0.000 0.000 0.000")',ERR=999) npdbops
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
            WRITE (hFile,'(A)',ERR=999) 'LATT '//LATT
!C LATT N [1] 
!C Lattice type: 1=P, 2=I, 3=rhombohedral obverse on hexagonal axes, 4=F, 5=A, 6=B, 7=C. N must be made
!C negative if the structure is non-centrosymmetric. 
            IF (npdbops .GE. 2) THEN
              DO i = 2, npdbops ! " +X, +Y, +Z" must be left out
                tString = cpdbops(i)
                tLen = LEN_TRIM(tString)
                WRITE (hFile, "('SYMM ',A)", ERR=999) tString(1:tLen)
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
            WRITE (hFile, '(A)', ERR=999) tString1(1:tLen1)
            WRITE (hFile, '(A)', ERR=999) tString2(1:tLen2)
            iiact = 0
            itotal = 0
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
! Determine this atom's entry number in the scattering factor list
                tElement = zmElementCSD(iOrig,iFrg)
                iScat = 0
                DO k1 = 1, tElement
                  IF (NumOfAtmPerElm(k1) .NE. 0) iScat = iScat + 1
                ENDDO
                WRITE (hFile,1035,ERR=999) OriginalLabel(iOrig,iFrg), iScat, (Xato(k,ii),k=1,3), &
                                    occ(iOrig,iFrg), RR_ITF*tiso(iOrig,iFrg)/(8.0*(PI**2)) 
 1035           FORMAT (A5,1X,I2,1X,3(F10.5,1X),F5.3,1X,F5.3)
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
            WRITE (hFile, "('END ')", ERR=999)
          CASE (6) ! pro
!            WRITE(hFile,'(2(A,F7.2))',ERR=999) "Intensity chi-sqrd = ", tIntChiSqd, ", profile chi-sqrd = ", tProChiSqd
            IF (Get_DivideByEsd()) THEN
              WRITE(hFile, '(A)', ERR=999) "      2theta"//CHAR(9)//"        Yobs"//CHAR(9)//"   ESD(Yobs)"//CHAR(9)//"       Ycalc"//CHAR(9)//"(Yobs - Ycalc) * <ESD> / ESD(Yobs)"
              DO I = 1, NBIN
                WRITE(hFile, 12, ERR=999) XBIN(I), CHAR(9), YOBIN(I), CHAR(9), EBIN(I), CHAR(9), YCBIN(I), CHAR(9), (YOBIN(I)-YCBIN(I))*AVGESD/EBIN(I)
              ENDDO
            ELSE
              WRITE(hFile, '(A)', ERR=999) "      2theta"//CHAR(9)//"        Yobs"//CHAR(9)//"   ESD(Yobs)"//CHAR(9)//"       Ycalc"//CHAR(9)//"Yobs - Ycalc"
              DO I = 1, NBIN
                WRITE(hFile, 12, ERR=999) XBIN(I), CHAR(9), YOBIN(I), CHAR(9), EBIN(I), CHAR(9), YCBIN(I), CHAR(9), (YOBIN(I)-YCBIN(I))
              ENDDO
            ENDIF
   12       FORMAT(F12.4, 4(A,F12.4))
        END SELECT
        CLOSE(hFile)
      ENDIF
      RETURN
  999 CALL ErrorMessage('Error writing file.')
      CLOSE(hFile)

      END SUBROUTINE RR_SaveAs
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardRietveldRefinement

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR

      IMPLICIT NONE

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER, EXTERNAL :: XtalFileOpen, XtalFileBrowse 
      CHARACTER(MaxPathLength) SDIFile, XtalFile
      INTEGER Curr_SA_Run, iStat, IV

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page6)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the first Wizard window
              CALL EndWizardPastPawley
              CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
              CALL WDialogPutRadioButton(IDF_PW_Option6)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT)
              Curr_SA_Run = 1
              CALL WizardWindowHide
              CALL ShowWizardWindowRietveld(Curr_SA_Run)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDB_SDI_file_Browse)
              CALL SDIFileBrowse
            CASE (IDB_SDI_file_Open)
              CALL WDialogGetString(IDF_SDI_File_Name, SDIFile)
              CALL SDIFileOpen(SDIFile)
            CASE (IDB_xtal_file_Browse)
              iStat = XtalFileBrowse()
              CALL WDialogFieldStateLogical(IDNEXT, (iStat .EQ. 0))
              CALL WDialogFieldStateLogical(IDB_Restart, (iStat .EQ. 0))
            CASE (IDB_xtal_file_Open)
              CALL WDialogGetString(IDF_Xtal_File_Name, XtalFile)
              iStat = XtalFileOpen(XtalFile)
              CALL WDialogFieldStateLogical(IDNEXT, (iStat .EQ. 0))
              CALL WDialogFieldStateLogical(IDB_Restart, (iStat .EQ. 0))
            CASE (IDB_Restart)
! Fill SA Parameter Bounds Wizard Window with the values from this solution.
              CALL WDialogSelect(IDD_SA_Modal_input2)
              DO IV = 1, NVAR
                CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IV, BestValuesDoF(IV,1))
              ENDDO
! Untick "Randomise initial values"
              CALL WDialogPutCheckBoxLogical(IDF_RandomInitVal, .FALSE.)
              ! Change mode to structure solution
              CALL SelectMode(ID_Structure_Solution_Mode)
              CALL ShowWizardWindowParameterBounds
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardRietveldRefinement
!
!*****************************************************************************
!
