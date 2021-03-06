! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2002 Cambridge Crystallographic Data Centre
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
      USE dash_gui_resources
      USE ZMVAR
      USE RRVAR
      USE SOLVAR
      USE ATMVAR
      USE REFVAR
      USE PO_VAR
      USE TAVAR

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
      COMMON /POSNS / NATOM, XATO(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      INTEGER KK, i, J
      INTEGER iFrg
      INTEGER iRow, iCol, iField
      REAL ChiSqd, ChiProSqd 
      INTEGER tFieldState


      IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN


      CALL SelectDASHDialog(IDD_Rietveld2)


      ENDIF  ! iRietveldMethod


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
      IF ( PrefParExists ) THEN
        KK = KK + 1
        RR_PO = BestValuesDoF(KK, Curr_SA_Run)
      ELSE
        RR_PO = 1.0
      ENDIF


      IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN


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
! Are the three calls redundant?
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
        CALL SelectDASHDialog(IDD_RR_PO_Dialog)
        CALL WDialogPutInteger(IDF_PO_a, PO_Direction(1))
        CALL WDialogPutInteger(IDF_PO_b, PO_Direction(2))
        CALL WDialogPutInteger(IDF_PO_c, PO_Direction(3))
        tFieldState = Enabled
      ELSE
        CALL WDialogFieldState(IDC_PO, Disabled)
        CALL WDialogFieldState(IDR_PO, Disabled)
        CALL SelectDASHDialog(IDD_RR_PO_Dialog)
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
      CALL SelectDASHDialog(IDD_Rietveld2)
      ! Initialise PO
      IF (PrefParExists) CALL PO_PRECFC(RR_PO)
      ! Initialise ITF
      CALL CreateFobITF


      ENDIF  ! iRietveldMethod


      ! Initialise XATO(1:3,1:300)
      CALL RR_MAKEFRAC


      IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN


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


      ENDIF  ! iRietveldMethod

      END SUBROUTINE ShowWizardWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE Set_Show_bond

      USE WINTERACTER
      USE dash_gui_resources
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
      IF (DASHWDialogGetCheckBoxLogical(IDC_HideH4)) THEN
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
      ! WGridRows refuse to set 0 raw and sets ErrGridSize; 
      ! Set to 1 raw and clear it
      IF ( iRow .GT. 1) THEN
        CALL WGridRows(iField, iRow-1)
      ELSE
        CALL WGridRows(iField, 1)
        CALL WGridDeleteRows(iField, 1)
        CALL WGridLabelRow(iField, 1, '')
      ENDIF
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

      USE WINTERACTER
      USE dash_gui_resources
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
      IF (DASHWDialogGetCheckBoxLogical(IDC_HideH3)) THEN
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
      ! WGridRows refuse to set 0 raw and sets ErrGridSize; 
      ! Set to 1 raw and clear it
      IF ( iRow .GT. 1) THEN
        CALL WGridRows(iField, iRow-1)
      ELSE
        CALL WGridRows(iField, 1)
        CALL WGridDeleteRows(iField, 1)
        CALL WGridLabelRow(iField, 1, '')
      ENDIF
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

      USE WINTERACTER
      USE dash_gui_resources
      USE RRVAR
      USE ZMVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER iFrg, i
      INTEGER iRow, iCol, iField
      INTEGER Num

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
      IF (DASHWDialogGetCheckBoxLogical(IDC_HideH2)) THEN
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
      ! WGridRows refuse to set 0 raw and sets ErrGridSize; 
      ! Set to 1 raw and clear it
      IF ( iRow .GT. 1) THEN
        CALL WGridRows(iField, iRow-1)
      ELSE
        CALL WGridRows(iField, 1)
        CALL WGridDeleteRows(iField, 1)
        CALL WGridLabelRow(iField, 1, '')
      ENDIF
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
      USE dash_gui_resources
      USE VARIABLES
      USE RRVAR
      USE ATMVAR
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
      COMMON /POSNS / NATOM, XATO(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER I, J
      REAL ChiSqd, ChiProSqd 
      INTEGER iValues(1:100), NVALUES
      INTEGER Num
      CHARACTER*(15) file_name

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
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
              ! Initialise XATO(1:3,1:300)
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
!             CALL SA_STRUCTURE_OUTPUT_PDB(0, file_name)
              CALL SA_STRUCTURE_OUTPUT_NON_OVERLAP(0, file_name)
              CALL ViewStructure(file_name, .FALSE.)
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
              CALL DASHWGridGetCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num1, NVALUES)
            CASE (IDB_Set2)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              CALL DASHWGridGetCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num2, NVALUES)
            CASE (IDB_Set3)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              CALL DASHWGridGetCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num3, NVALUES)
            CASE (IDB_Set4)
              iValues = 1
              NVALUES = 100
              CALL WGridPutCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
              CALL DASHWGridGetCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
              CALL WDialogPutInteger(IDI_Num4, NVALUES)
            CASE (IDB_PO_Settings)
              CALL SelectDASHDialog(IDD_RR_PO_Dialog)
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
              CALL DASHWGridGetCheckBox(IDF_RR_ZmatrixGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num1,Num)
            CASE (IDF_RR_TorsionGrid)
              NVALUES = 100
              CALL DASHWGridGetCheckBox(IDF_RR_TorsionGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num2,Num)
            CASE (IDF_RR_AngleGrid)
              NVALUES = 100
              CALL DASHWGridGetCheckBox(IDF_RR_AngleGrid, 2, iValues, NVALUES)
              Num = 0
              DO i = 1, NVALUES
                IF (iValues(i) .EQ. 1) Num = Num + 1
              ENDDO
              CALL WDialogPutInteger(IDI_Num3,Num)
            CASE (IDF_RR_BondGrid)
              NVALUES = 100
              CALL DASHWGridGetCheckBox(IDF_RR_BondGrid, 2, iValues, NVALUES)
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
      USE dash_gui_resources
      USE VARIABLES
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE      

      LOGICAL, EXTERNAL :: Confirm, DASHWDialogGetCheckBoxLogical
      INTEGER tFieldState
      REAL ChiSqd, ChiProSqd 

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_RR_PO_Dialog)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK, IDCANCEL)
              CALL WDialogHide
              PrefParExists = DASHWDialogGetCheckBoxLogical(IDF_Use_PO)
              !C Initialise preferred orientation and recalculate pattern + chi-sqrds
              !C Initialise PO
              IF (PrefParExists) THEN
                CALL DASHWDialogGetInteger(IDF_PO_a, PO_Direction(1))
                CALL DASHWDialogGetInteger(IDF_PO_b, PO_Direction(2))
                CALL DASHWDialogGetInteger(IDF_PO_c, PO_Direction(3))
                CALL SelectDASHDialog(IDD_Rietveld2)
                CALL DASHWDialogGetReal(IDR_PO, RR_PO)
                CALL FillSymmetry_2
                CALL PO_PRECFC(RR_PO)
              ENDIF
              CALL RR_VALCHI(ChiSqd)
              CALL VALCHIPRO(ChiProSqd)
              !C Update the main Rietveld window
              CALL SelectDASHDialog(IDD_Rietveld2)
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
              IF (DASHWDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
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
      USE dash_gui_resources
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
      USE dash_gui_resources
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iRow, iCol, iField, iFrg

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
      iRow = 1
      iCol = 1
      iField = IDF_RR_ZmatrixGrid
      DO iFrg = 1, nFrag
        ! Translations
        CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_tran(1,iFrg))
        CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(1,iFrg))
        iRow = iRow + 1
        CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_tran(2,iFrg))
        CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(2,iFrg))
        iRow = iRow + 1
        CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_tran(3,iFrg))
        CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopttran(3,iFrg))
        iRow = iRow + 1
        ! Rotations
        IF (natoms(iFrg) .NE. 1) THEN
          CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_rot(1,iFrg))
          CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(1,iFrg))
          iRow = iRow + 1
          CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_rot(2,iFrg))
          CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(2,iFrg))
          iRow = iRow + 1
          IF (UseQuaternions(iFrg)) THEN
            CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_rot(3,iFrg))
            CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(3,iFrg))
            iRow = iRow + 1
            CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_rot(4,iFrg))
            CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptrot(4,iFrg))
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
            CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_blen(i,iFrg))
            CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptb(i,iFrg))
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
            CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_alph(i,iFrg))
            CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_iopta(i,iFrg))
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
            CALL DASHWGridGetCellReal(iField, iCol, iRow, RR_bet(i,iFrg))
            CALL DASHWGridGetCellCheckBox(iField, iCol+1, iRow, RR_ioptt(i,iFrg))
            iRow = iRow + 1
          ENDIF
        ENDDO
      ENDDO
      CALL DASHWDialogGetCheckBox(IDC_ITF, RR_ioptITF)
      CALL DASHWDialogGetReal(IDR_ITF, RR_ITF)
      CALL DASHWDialogGetCheckBox(IDC_PO, RR_ioptPO)
      CALL DASHWDialogGetReal(IDR_PO, RR_PO)
      CALL PopActiveWindowID

      END SUBROUTINE Dialog2RRVAR
!
!*****************************************************************************
!
      SUBROUTINE RRVAR2Dialog

      USE WINTERACTER
      USE dash_gui_resources
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER i, iRow, iCol, iField, iFrg

! Fill and display dialogue
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Rietveld2)
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
      USE ATMVAR

      IMPLICIT NONE

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER         NATOM
      REAL                   XATO
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, XATO(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
     &                KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
     &                SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

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
          IF ( UseCCoM ) THEN
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

      USE dash_gui_resources
      USE ZMVAR
      USE ATMVAR
      USE RRVAR

      IMPLICIT NONE

! Writes out original and Rietveld refined crystal structure to pdb file for visual comparison
      INCLUDE 'params.inc'

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
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: WritePDBCommon, WriteCIFCommon
      INTEGER iSol
      INTEGER pdbBond(1:maxbnd_2*maxfrg,1:2)
      INTEGER TotNumBonds, NumOfAtomsSoFar
      CHARACTER*5 LabelStr
      CHARACTER*2 ColourStr
      CHARACTER*2 SolStr
      INTEGER AtomLabelOption, AtomColourOption
      INTEGER I, iFrg, J, iiact, BondNr
      REAL    x_pdb(1:3)
      INTEGER iAtom, iEntry
      LOGICAL tUseCif
      INTEGER, PARAMETER :: chFile = 65
      REAL, PARAMETER :: TOUISO = 0.01266514796 
      CHARACTER*(*), PARAMETER :: PDBFileName = 'Overlap_Temp.pdb', CIFFileName =  'Overlap_Temp.cif'

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      tUseCif = DASHWDialogGetCheckBoxLogical(IDC_cif_for_viewer)
      IF ( tUseCif ) THEN
        OPEN (UNIT=chFile, FILE=CIFFileName, STATUS='unknown', ERR=999)
        WRITE (chFile, 1036, ERR=999) '# CIF '
      ELSE
! Write the file headers first
        OPEN (UNIT=chFile, FILE=PDBFileName, STATUS='unknown', ERR=999)
! Add in a Header record
        WRITE (chFile, 1036, ERR=999) 'HEADER    PDB '
        IF (WritePDBCommon(chFile) .NE. 0) GOTO 999
      ENDIF
 1036 FORMAT (A, 'Solution File generated by DASH')
!C! Get atom label option from dialogue. Two options: 
!C! 1. "Element + solution #"
!C! 2. "Original atom labels"
!C      CALL DASHWDialogGetRadioButton(IDF_UseSolutionNr,AtomLabelOption)
      AtomLabelOption = 2
!C! Get atom colour option from dialogue. Two options: 
!C! 1. "By solution number"
!C! 2. "By element"
!C      CALL DASHWDialogGetRadioButton(IDF_ColourBySolution,AtomColourOption)
      AtomColourOption = 2

      iiact = 0
      DO iEntry = 1, 2
        IF ( iEntry .EQ. 1 ) THEN
! Original structure
          SolStr = "_O"
          ColourStr = ' S'  ! Colour by solution: Sulphur        Yellow
        ELSE
! Rietveld refined structure
          SolStr = "_N"
          ColourStr = ' O'  ! Colour by solution: Oxygen        Red
        ENDIF
        IF ( tUseCif ) THEN
          WRITE (chFile,'(/"data_entry",A)',ERR=999) SolStr
          IF (WriteCIFCommon(chFile, 0.0, RR_PO, .TRUE.) .NE. 0) GOTO 999
! Note that elements are right-justified
        ENDIF
        iAtom = 0
        DO iFrg = 1, nFrag
          DO i = 1, natoms(iFrg)
            iiact = iiact + 1
            iAtom = iAtom + 1
            IF (AtomLabelOption .EQ. 1) THEN ! Element symbol + solution number
              LabelStr = TRIM(ElSym(i,iFrg))//SolStr
            ELSE  ! Orignal atom labels
              LabelStr = OriginalLabel(i,iFrg)
            ENDIF
            IF ( tUseCif ) THEN
              IF ( iEntry .EQ. 1 ) THEN
                WRITE (chFile,1034,ERR=999) LabelStr, RR_XATO_Orig(1:3,OrderedAtm(iAtom)), occ(i,iFrg), TOUISO * tiso(i,iFrg) 
              ELSE
                WRITE (chFile,1034,ERR=999) LabelStr, Xato(1:3,OrderedAtm(iAtom)), occ(i,iFrg), TOUISO * tiso(i,iFrg) 
              ENDIF
   1034       FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Uiso ',F6.4)
            ELSE
! Note that elements are right-justified
              IF (AtomColourOption .EQ. 2) THEN ! Colour by Element
                IF (ElSym(i,iFrg)(2:2) .EQ. ' ') THEN
                  ColourStr(1:2) = ' '//ElSym(i,iFrg)(1:1)
                ELSE
                  ColourStr = ElSym(i,iFrg)
                ENDIF
              ENDIF
              IF ( iEntry .EQ. 1 ) THEN
                CALL PremultiplyVectorByMatrix(f2cpdb, RR_XATO_Orig(1,OrderedAtm(iAtom)), x_pdb)
              ELSE
                CALL PremultiplyVectorByMatrix(f2cpdb, Xato(1,OrderedAtm(iAtom)), x_pdb)
              ENDIF
              WRITE (chFile,1120,ERR=999) iiact, LabelStr(1:4), x_pdb(1:3), occ(i,iFrg), tiso(i,iFrg), ColourStr(1:2)
   1120       FORMAT ('HETATM',I5,' ',A4' NON     1    ',3F8.3,2F6.2,'          ',A2,'  ')
            ENDIF
          ENDDO ! loop over atoms
        ENDDO ! loop over Z-matrices
      ENDDO ! loop over entries
      IF ( tUseCif ) GOTO 200
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
          WRITE(chFile,'(A6,I5,I5)',ERR=999) 'CONECT', (pdbBond(BondNr,1)+NATOM*(iSol-1)), (pdbBond(BondNr,2)+NATOM*(iSol-1))
        ENDDO
      ENDDO ! loop over "runs"
      WRITE (chFile,"('END')",ERR=999)
  200 CLOSE (chFile)
      IF ( tUseCif ) THEN
        CALL ViewStructure(CIFFileName, .FALSE.)
      ELSE
        CALL ViewStructure(PDBFileName, .FALSE.)
      ENDIF
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing temporary file.')
      CLOSE (chFile)
      CALL PopActiveWindowID

      END SUBROUTINE RR_Compare
!
!*****************************************************************************
!
      SUBROUTINE RR_SaveAs
! Pop up file window, user must select file name + file type (through extension).

      USE dash_gui_resources
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

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
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      REAL            f2cpdb
      COMMON /pdbcat/ f2cpdb(1:3,1:3)

      CHARACTER*1, EXTERNAL :: ChrLowerCase
      INTEGER, EXTERNAL :: WritePDBCommon, WriteCIFCommon
      REAL, EXTERNAL :: UnitCellVolume
      LOGICAL, EXTERNAL :: Get_DivideByEsd
      INTEGER iFlags, iFType, hFile, ii, iiact, iTotal, iFrg, i, k, iOrig
      INTEGER iTem, j, iBond1, iBond2, NumOfAtomsSoFar, tLen
      INTEGER tLen1, tLen2, NumOfAtmPerElm(1:MaxElm), iScat, K1, tElement
      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=255) :: FILTER
      REAL    x_pdb(1:3)
      CHARACTER*80 tString, tString1, tString2
      CHARACTER*2  LATT
!C    This is 1/( 8 * pi^2)          
      REAL, PARAMETER :: TOUISO = 0.01266514796 
                        

      iFlags = SaveDialog + AppendExt + PromptOn
      FILTER = 'pdb (*.pdb)|*.pdb|'// &
               'cssr (*.cssr)|*.cssr|'// &
               'ccl (*.ccl)|*.ccl|'// &
               'cif (*.cif)|*.cif|'// &
               'SHELX (*.res)|*.res|'// &
               'Profile (*.pro)|*.pro|'
      tFileName = ''
      CALL WSelectFile(FILTER, iFlags, tFileName, 'Save final structure', iFType)
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
            IF (WriteCIFCommon(hFile, 0.0, RR_PO, .TRUE.) .NE. 0) GOTO 999
            iiact = 0
            itotal = 0
            
            DO iFrg = 1, nFrag
              itotal = iiact
              DO i = 1, natoms(iFrg)
                iiact = iiact + 1
                iOrig = izmbid(i,iFrg)
                ii = OrderedAtm(itotal + iOrig)
                WRITE (hFile,1034,ERR=999) OriginalLabel(iOrig,iFrg), (Xato(k,ii),k=1,3), occ(iOrig,iFrg), TOUISO * RR_ITF * tiso(iOrig,iFrg) 
 1034           FORMAT ('  ',A5,1X,3(F10.5,1X),F5.3,' Uiso ',F6.4)
              ENDDO ! loop over atoms
            ENDDO ! loop over Z-matrices
          CASE (5) ! res
            WRITE (hFile,"('TITL Solution File generated by DASH')",ERR=999)
            IF (PrefParExists) THEN
              WRITE (hFile,'(A,3(1X,I3),1X,F9.5)',ERR=999) 'REM  PO',(PO_Direction(ii),ii=1,3),RR_PO
            ENDIF
            WRITE (hFile,1032,ERR=999) ALambda, (CellPar(ii),ii=1,6)
 1032       FORMAT ('CELL ',F7.5,1X,6(F8.4,1X))
            WRITE (hFile,'("ZERR ",I3," 0.000 0.000 0.000 0.000 0.000 0.000")',ERR=999) npdbops
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
      SUBROUTINE DealWithWizardChooseRietveldRefinementMethod

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page6a)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SAW_Page5)
            CASE (IDNEXT)
              CALL DASHWDialogGetRadioButton(IDF_RADIO1, iRietveldMethodOpt)
              SELECT CASE (iRietveldMethodOpt)
                CASE (2)
                  iRietveldMethod = FOR_TOPAS
                CASE (3)
                  iRietveldMethod = FOR_GSAS
                CASE (4)
                  iRietveldMethod = FOR_RIETAN
                CASE DEFAULT
                  iRietveldMethod = INTERNAL_RB
              END SELECT
              IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN
                ! Rigid-body Rietveld refinement in DASH
                CALL ShowWizardWindowRietveld(RR_SA_Sol)
              ELSE
                ! External
                ext_RR_stage = 1
                CALL CopyPattern2Backup()
                ext_RR_start_dialog_id = IDD_SAW_Page6a
                CALL WizardWindowShow(IDD_RR_External)
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardChooseRietveldRefinementMethod
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardRietveldRefinement

      USE WINTERACTER
      USE dash_gui_resources
      USE VARIABLES
      USE SOLVAR
      USE TAVAR

      IMPLICIT NONE

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER, EXTERNAL :: XtalFileOpen, XtalFileBrowse 
      LOGICAL, EXTERNAL :: FnUnitCellOK, SDIFileOpen
      CHARACTER(MaxPathLength) SDIFile, XtalFile
      INTEGER iStat, IV, iOpt
      LOGICAL IErrCode

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page6)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! Go back to the first Wizard window
              CALL EndWizardPastPawley
              CALL SelectDASHDialog(IDD_Polyfitter_Wizard_01)
              CALL WDialogPutRadioButton(IDF_PW_Option6)
              CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
            CASE (IDNEXT, IDB_Restart)
! As Xtal file is compulsory, it may be more meaningful by calling it:
! 'Rietveld refinement using external structure'
!
! Check if we have reasonable data left. If not, don't go ahead
              IF ( .NOT. FnUnitCellOK()) THEN
                CALL DASHWDialogGetString(IDF_SDI_File_Name, SDIFile)
                IF (LEN_TRIM(SDIFile) .LE. 0) THEN
                  CALL ErrorMessage('Invalid unit cell parameters.'//CHAR(13)//&
                                    'Have you opened a SDI file?')
                  GOTO 230
                ENDIF
                IF ( .NOT. SDIFileOpen(SDIFile) ) GOTO 230
                CALL SelectDASHDialog(IDD_SAW_Page6) ! SDIFileOpen may change current selection
              ENDIF
              CALL DASHWDialogGetString(IDF_Xtal_File_Name, XtalFile)
              IF (LEN_TRIM(XtalFile) .LE. 0) THEN
                CALL ErrorMessage('Have you chosen a crystal structure file?')
                GOTO 230
              ENDIF
! Redo XtalFileOpen to ensure:
!    1. unit cell from xtal is consistent 
!    2. SA_Parameter_Set called after .sdi
!    3. in case user typed xtal path but not click open
              IF (XtalFileOpen(XtalFile) .NE. 0) GOTO 230
              IF (EventInfo%VALUE1 .EQ. IDB_Restart) THEN
! Fill SA Parameter Bounds Wizard Window with the values from this solution.
                CALL SelectDASHDialog(IDD_SA_Modal_input2)
                DO IV = 1, NVAR
                  CALL WGridPutCellReal(IDF_parameter_grid_modal, 1, IV, BestValuesDoF(IV,1))
                ENDDO
! Untick "Randomise initial values"
                CALL WDialogPutCheckBoxLogical(IDF_RandomInitVal, .FALSE.)
                ! Change mode to structure solution
                CALL SelectMode(ID_Structure_Solution_Mode)
                CALL ShowWizardWindowParameterBounds
                GOTO 230
              ENDIF !IDB_Restart
! 
              CALL DASHWDialogGetRadioButton(IDF_RADIO1, iOpt)
              SELECT CASE (iOpt)
                CASE (2)
                  iRietveldMethod = FOR_TOPAS
                CASE (3)
                  iRietveldMethod = FOR_GSAS
                CASE (4)
                  iRietveldMethod = FOR_RIETAN
                CASE DEFAULT
                  iRietveldMethod = INTERNAL_RB
              END SELECT
              RR_SA_Sol = 1
              ! Update SPG related global variables, eg. NOPC, cpdbops etc.
              CALL FillSymmetry_2
              IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN
                ! Rigid-body Rietveld refinement in DASH
                CALL ShowWizardWindowRietveld(RR_SA_Sol)
              ELSE
                ! External
                ext_RR_stage = 1
                CALL CopyPattern2Backup()
                ext_RR_start_dialog_id = IDD_SAW_Page6
                CALL WizardWindowShow(IDD_RR_External)
              ENDIF
 230          CONTINUE
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizardPastPawley
            CASE (IDB_SDI_file_Browse)
              CALL SDIFileBrowse
            CASE (IDB_SDI_file_Open)
              CALL DASHWDialogGetString(IDF_SDI_File_Name, SDIFile)
              IErrCode = SDIFileOpen(SDIFile)
            CASE (IDB_xtal_file_Browse)
              iStat = XtalFileBrowse()
              CALL WDialogFieldStateLogical(IDNEXT, (iStat .EQ. 0))
              CALL WDialogFieldStateLogical(IDB_Restart, (iStat .EQ. 0))
            CASE (IDB_xtal_file_Open)
              CALL DASHWDialogGetString(IDF_Xtal_File_Name, XtalFile)
! XtalFileOpen return 0 even when filename string empty
              IF (LEN_TRIM(XtalFile) .GT. 0) THEN
                iStat = XtalFileOpen(XtalFile)
              ELSE
                iStat = 1
              ENDIF
              CALL WDialogFieldStateLogical(IDNEXT, (iStat .EQ. 0))
              CALL WDialogFieldStateLogical(IDB_Restart, (iStat .EQ. 0))
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardRietveldRefinement
!
!*****************************************************************************
!
