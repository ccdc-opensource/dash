!
!*****************************************************************************
!
      SUBROUTINE ShowWindowRietveld(Curr_SA_Run)
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

      IMPLICIT NONE 

      INTEGER, INTENT (IN   ) :: Curr_SA_Run

      INTEGER KK, JQ, JQS, i
      INTEGER iFrg
      INTEGER iFrgCopy
      INTEGER iRow, iCol
      REAL QQSUM, QDEN, QUATER(1:4)
      REAL Duonion(0:1)

! Load all values of all bonds, angles etc. into RRVAR variables
      KK = 0
! Loop over all the fragments
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            ! Position centre of mass inside unit cell
            RR_tran(1,iFrg,iFrgCopy) = BestValuesDoF(KK+1,Curr_SA_Run) - INT(BestValuesDoF(KK+1,Curr_SA_Run))
            RR_tran(2,iFrg,iFrgCopy) = BestValuesDoF(KK+2,Curr_SA_Run) - INT(BestValuesDoF(KK+2,Curr_SA_Run))
            RR_tran(3,iFrg,iFrgCopy) = BestValuesDoF(KK+3,Curr_SA_Run) - INT(BestValuesDoF(KK+3,Curr_SA_Run))
            KK = KK + 3
! If more than one atom then proceed
            IF (natoms(iFrg) .GT. 1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
              IF (UseQuaternions(iFrg)) THEN
                QQSUM = 0.0
                DO JQ = 1, 4
                  JQS = JQ + KK
                  QQSUM = QQSUM + BestValuesDoF(JQS,Curr_SA_Run)**2
                ENDDO
! QQSUM now holds the sum of the squares of the quaternions
                QDEN = 1.0 / SQRT(QQSUM)
                DO JQ = 1, 4
                  JQS = JQ + KK
                  QUATER(JQ) = QDEN * BestValuesDoF(JQS,Curr_SA_Run)
                ENDDO
! QUATER now holds the normalised quaternions
                RR_rot(1,iFrg,iFrgCopy) = QUATER(1)
                RR_rot(2,iFrg,iFrgCopy) = QUATER(2)
                RR_rot(3,iFrg,iFrgCopy) = QUATER(3)
                RR_rot(4,iFrg,iFrgCopy) = QUATER(4)
                KK = KK + 4
              ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
                Duonion(0) = BestValuesDoF(KK+1,Curr_SA_Run)
                Duonion(1) = BestValuesDoF(KK+2,Curr_SA_Run)
                QDEN = 1.0 / SQRT(Duonion(0)**2 + Duonion(1)**2)
                Duonion(0) = Duonion(0) * QDEN 
                Duonion(1) = Duonion(1) * QDEN 
                RR_rot(1,iFrg,iFrgCopy) = Duonion(0)
                RR_rot(2,iFrg,iFrgCopy) = Duonion(1)
                KK = KK + 2
              ENDIF
            ENDIF
            DO I = 1, natoms(iFrg)
              IF (IOPTB(i,iFrg) .EQ. 1) THEN
                KK = KK + 1
                RR_blen(i,iFrg,iFrgCopy) = BestValuesDoF(KK,Curr_SA_Run)
              ELSE
                RR_blen(i,iFrg,iFrgCopy) = blen(i,iFrg)
              ENDIF
              IF (IOPTA(i,iFrg) .EQ. 1) THEN
                KK = KK + 1
                RR_alph(i,iFrg,iFrgCopy) = BestValuesDoF(KK,Curr_SA_Run)
              ELSE
                RR_alph(i,iFrg,iFrgCopy) = alph(i,iFrg)
              ENDIF
              IF (IOPTT(i,iFrg) .EQ. 1) THEN
                KK = KK + 1
                RR_bet(i,iFrg,iFrgCopy) = BestValuesDoF(KK,Curr_SA_Run)
              ELSE
                RR_bet(i,iFrg,iFrgCopy) = bet(i,iFrg)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
! Fill and display dialogue
      CALL WDialogSelect(IDD_Rietveld2)
      iRow = 1
      iCol = 1
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            ! Translations
            CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'x')
            CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_tran(1,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'y')
            CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_tran(2,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'z')
            CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_tran(3,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            ! Rotations
            IF (natoms(iFrg) .EQ. 1) THEN
! Single atom: no rotations
            ELSE IF (UseQuaternions(iFrg)) THEN
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q0')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(1,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q1')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(2,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q2')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(3,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q3')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(4,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
            ELSE
! Molecule with rotation restricted to a single axis
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q0')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(1,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
              CALL WGridLabelRow(IDF_RR_ZmatrixGrid, iRow, 'Q1')
              CALL WGridPutCellReal(IDF_RR_ZmatrixGrid, iCol, iRow, RR_rot(2,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      CALL WGridRows(IDF_RR_ZmatrixGrid,iRow-1)
      iRow = 1
      iCol = 1
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 4, natoms(iFrg)
              CALL WGridLabelRow(IDF_RR_TorsionGrid, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                    OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                    OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg)))//':'// &
                                    OriginalLabel(iz3(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz3(i,iFrg),iFrg))))
              CALL WGridPutCellReal(IDF_RR_TorsionGrid, iCol, iRow, RR_alph(i,iFrg,iFrgCopy), "(F9.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      CALL WGridRows(IDF_RR_TorsionGrid,iRow-1)
      iRow = 1
      iCol = 1
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 3, natoms(iFrg)
              CALL WGridLabelRow(IDF_RR_AngleGrid, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                  OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                  OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg))))
              CALL WGridPutCellReal(IDF_RR_AngleGrid, iCol, iRow, RR_alph(i,iFrg,iFrgCopy), "(F9.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      CALL WGridRows(IDF_RR_AngleGrid,iRow-1)
      iRow = 1
      iCol = 1
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 2, natoms(iFrg)
              CALL WGridLabelRow(IDF_RR_BondGrid, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))// &
                ':'//OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg))))
              CALL WGridPutCellReal(IDF_RR_BondGrid, iCol, iRow, RR_blen(i,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      CALL WGridRows(IDF_RR_BondGrid,iRow-1)
      CALL WDialogShow(-1,-1,0,ModeLess)



      END SUBROUTINE ShowWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE DealWithWindowRietveld

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE      

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Rietveld2)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Refine)
              CALL RietveldRefinement

            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide
            CASE (IDB_View)



          END SELECT
        CASE (FieldChanged)
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE RietveldRefinement 



      END SUBROUTINE RietveldRefinement
!
!*****************************************************************************
!
