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
      INTEGER iRow, iCol, nRows, iField
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
      iField = IDF_RR_ZmatrixGrid
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            ! Translations
            CALL WGridLabelRow(iField, iRow, 'x')
            CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(1,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            CALL WGridLabelRow(iField, iRow, 'y')
            CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(2,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            CALL WGridLabelRow(iField, iRow, 'z')
            CALL WGridPutCellReal(iField, iCol, iRow, RR_tran(3,iFrg,iFrgCopy), "(F7.5)")
            iRow = iRow + 1
            ! Rotations
            IF (natoms(iFrg) .NE. 1) THEN
              CALL WGridLabelRow(iField, iRow, 'Q0')
              CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(1,iFrg,iFrgCopy), "(F8.5)")
              iRow = iRow + 1
              CALL WGridLabelRow(iField, iRow, 'Q1')
              CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(2,iFrg,iFrgCopy), "(F8.5)")
              iRow = iRow + 1
              IF (UseQuaternions(iFrg)) THEN
                CALL WGridLabelRow(iField, iRow, 'Q2')
                CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(3,iFrg,iFrgCopy), "(F8.5)")
                iRow = iRow + 1
                CALL WGridLabelRow(iField, iRow, 'Q3')
                CALL WGridPutCellReal(iField, iCol, iRow, RR_rot(4,iFrg,iFrgCopy), "(F8.5)")
                iRow = iRow + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      nRows = iRow-1
      CALL WGridRows(iField, nRows)
      iCol = 2
      DO iRow = 1, nRows
        CALL WGridPutCellCheckBox(iField, iCol, iRow, 0)
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_TorsionGrid
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 4, natoms(iFrg)
              CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                    OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                    OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg)))//':'// &
                                    OriginalLabel(iz3(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz3(i,iFrg),iFrg))))
              CALL WGridPutCellReal(iField, iCol, iRow, RR_alph(i,iFrg,iFrgCopy), "(F9.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      nRows = iRow-1
      CALL WGridRows(iField, nRows)
      iCol = 2
      DO iRow = 1, nRows
        CALL WGridPutCellCheckBox(iField, iCol, iRow, 0)
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_AngleGrid
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 3, natoms(iFrg)
              CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))//':'// &
                                  OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg)))//':'// &
                                  OriginalLabel(iz2(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz2(i,iFrg),iFrg))))
              CALL WGridPutCellReal(iField, iCol, iRow, RR_alph(i,iFrg,iFrgCopy), "(F9.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      nRows = iRow-1
      CALL WGridRows(iField, nRows)
      iCol = 2
      DO iRow = 1, nRows
        CALL WGridPutCellCheckBox(iField, iCol, iRow, 0)
      ENDDO
      iRow = 1
      iCol = 1
      iField = IDF_RR_BondGrid
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            DO i = 2, natoms(iFrg)
              CALL WGridLabelRow(iField, iRow, OriginalLabel(i,iFrg)(1:LEN_TRIM(OriginalLabel(i,iFrg)))// &
                ':'//OriginalLabel(iz1(i,iFrg),iFrg)(1:LEN_TRIM(OriginalLabel(iz1(i,iFrg),iFrg))))
              CALL WGridPutCellReal(iField, iCol, iRow, RR_blen(i,iFrg,iFrgCopy), "(F7.5)")
              iRow = iRow + 1
            ENDDO
          ENDDO
        ENDIF
      ENDDO
      nRows = iRow-1
      CALL WGridRows(iField, nRows)
      iCol = 2
      DO iRow = 1, nRows
        CALL WGridPutCellCheckBox(iField, iCol, iRow, 0)
      ENDDO
      ! Initialise RR_AtmCoords(1:3,1:150)
      CALL RR_MAKEFRAC
      CALL WDialogShow(-1,-1,0,ModeLess)

      END SUBROUTINE ShowWindowRietveld
!
!*****************************************************************************
!
      SUBROUTINE DealWithWindowRietveld

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE RRVAR
      USE SOLVAR

      IMPLICIT NONE      

      INTEGER         NATOM
      REAL                   X
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER I, J

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
! Calls subroutine which opens Mercury window with .pdb file
              ! Fill XAtmCoords(1:3,1:150,0)
              DO I = 1, NATOM
                DO J = 1, 3
                  XAtmCoords(J,I,0) = RR_AtmCoords(J,I)
                ENDDO
              ENDDO
              CALL SA_STRUCTURE_OUTPUT_PDB(0)
              CALL ViewStructure('SA_best.pdb')
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
! Fills REAL RR_AtmCoords(1:3,1:150) using variables from RRVAR
!
! For the refinement, this should be preceded by a step that translates the
! list of parameters to RR_tran, RR_rot etc.
      SUBROUTINE RR_MAKEFRAC

      USE VARIABLES
      USE ZMVAR
      USE RRVAR

      IMPLICIT NONE

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MAXATM)

      INTEGER KATOM, i, KI
      INTEGER iFrg, iFrgCopy
      REAL CKK1, CKK2, CKK3, TRAN(1:3)
      REAL QQSUM, QDEN, QUATER(0:3), tQ(0:3), ROTA(1:3,1:3), CART(1:3,1:MAXATM), Duonion(0:1)
      INTEGER JQ, ICFRG
      REAL tX, tY, tZ, XC, YC, ZC
      LOGICAL, EXTERNAL :: Get_UseCrystallographicCoM

      KATOM = 0
! Loop over all the fragments
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          DO iFrgCopy = 1, zmNumberOfCopies(iFrg)
            ! Position centre of mass inside unit cell
            RR_tran(1,iFrg,iFrgCopy) = RR_tran(1,iFrg,iFrgCopy) - INT(RR_tran(1,iFrg,iFrgCopy))
            RR_tran(2,iFrg,iFrgCopy) = RR_tran(2,iFrg,iFrgCopy) - INT(RR_tran(2,iFrg,iFrgCopy))
            RR_tran(3,iFrg,iFrgCopy) = RR_tran(3,iFrg,iFrgCopy) - INT(RR_tran(3,iFrg,iFrgCopy))
            CKK1 = RR_tran(1,iFrg,iFrgCopy)
            CKK2 = RR_tran(2,iFrg,iFrgCopy)
            CKK3 = RR_tran(3,iFrg,iFrgCopy)
            TRAN(1) = CKK1*SNGL(F2CMAT(1,1)) + CKK2*SNGL(F2CMAT(1,2)) + CKK3*SNGL(F2CMAT(1,3))
            TRAN(2) = CKK1*SNGL(F2CMAT(2,1)) + CKK2*SNGL(F2CMAT(2,2)) + CKK3*SNGL(F2CMAT(2,3))
            TRAN(3) = CKK1*SNGL(F2CMAT(3,1)) + CKK2*SNGL(F2CMAT(3,2)) + CKK3*SNGL(F2CMAT(3,3))
! If more than one atom then proceed
            IF (natoms(iFrg).GT.1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
              IF (UseQuaternions(iFrg)) THEN
                QQSUM = 0.0
                DO JQ = 1, 4
                  QQSUM = QQSUM + RR_rot(JQ,iFrg,iFrgCopy)**2
                ENDDO
! QQSUM now holds the sum of the squares of the quaternions
                QDEN = 1.0 / SQRT(QQSUM)
                DO JQ = 0, 3
                  QUATER(JQ) = QDEN * RR_rot(JQ+1,iFrg,iFrgCopy)
                ENDDO
! QUATER now holds the normalised quaternions
                CALL RR_ROTMAK(QUATER,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
              ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
                Duonion(0) = RR_rot(1,iFrg,iFrgCopy)
                Duonion(1) = RR_rot(2,iFrg,iFrgCopy)
                QDEN = 1.0 / SQRT(Duonion(0)**2 + Duonion(1)**2)
                Duonion(0) = Duonion(0) * QDEN 
                Duonion(1) = Duonion(1) * QDEN 
                QUATER(0) = Duonion(0) * zmSingleRotationQs(0,iFrg)
                QUATER(1) = Duonion(1) * zmSingleRotationQs(1,iFrg)
                QUATER(2) = Duonion(1) * zmSingleRotationQs(2,iFrg)
                QUATER(3) = Duonion(1) * zmSingleRotationQs(3,iFrg)
! QUATER now holds the normalised quaternion corresponding to the single rotation axis
! Now premultiply with the original molecular orientation (JvdS I don't understand why
! they must be premultiplied: I would say they should be postmultiplied)
                tQ(0) =   QUATER(0)*zmInitialQs(0,iFrg) - QUATER(1)*zmInitialQs(1,iFrg) &
                        - QUATER(2)*zmInitialQs(2,iFrg) - QUATER(3)*zmInitialQs(3,iFrg)
                tQ(1) =   QUATER(0)*zmInitialQs(1,iFrg) + QUATER(1)*zmInitialQs(0,iFrg) &
                        - QUATER(2)*zmInitialQs(3,iFrg) + QUATER(3)*zmInitialQs(2,iFrg)
                tQ(2) =   QUATER(0)*zmInitialQs(2,iFrg) + QUATER(1)*zmInitialQs(3,iFrg) &
                        + QUATER(2)*zmInitialQs(0,iFrg) - QUATER(3)*zmInitialQs(1,iFrg)
                tQ(3) =   QUATER(0)*zmInitialQs(3,iFrg) - QUATER(1)*zmInitialQs(2,iFrg) &
                        + QUATER(2)*zmInitialQs(1,iFrg) + QUATER(3)*zmInitialQs(0,iFrg)
                CALL RR_ROTMAK(tQ,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the single rotation axis
              ENDIF
            ENDIF
            CALL RR_MAKEXYZ(natoms(iFrg),RR_blen(1,iFrg,iFrgCopy),RR_alph(1,iFrg,iFrgCopy),RR_bet(1,iFrg,iFrgCopy),        &
                           IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
! Determine origin for rotations
            ICFRG = ICOMFLG(iFrg)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
            IF (ICFRG.EQ.0) THEN
              XC = 0.0
              YC = 0.0
              ZC = 0.0
              IF (Get_UseCrystallographicCoM()) THEN
                DO I = 1, natoms(iFrg)
                  XC = XC + AtomicWeighting(I,iFrg)*CART(1,I)
                  YC = YC + AtomicWeighting(I,iFrg)*CART(2,I)
                  ZC = ZC + AtomicWeighting(I,iFrg)*CART(3,I)
                ENDDO
              ELSE
                DO I = 1, natoms(iFrg)
                  XC = XC + CART(1,I)
                  YC = YC + CART(2,I)
                  ZC = ZC + CART(3,I)
                ENDDO
                XC = XC/FLOAT(natoms(iFrg))
                YC = YC/FLOAT(natoms(iFrg))
                ZC = ZC/FLOAT(natoms(iFrg))
              ENDIF
! Otherwise, use atom number ICFRG
            ELSE
              XC = CART(1,ICFRG)
              YC = CART(2,ICFRG)
              ZC = CART(3,ICFRG)
            ENDIF
! Subtract the origin from all atom positions
            DO I = 1, natoms(iFrg)
              CART(1,I) = CART(1,I) - XC
              CART(2,I) = CART(2,I) - YC
              CART(3,I) = CART(3,I) - ZC
            ENDDO
! Apply rotation and translation to the atoms of this Z-matrix
            CALL RR_DO_ATOM_POS(TRAN,ROTA,CART,natoms(iFrg))
! When we are here, we have the actual co-ordinates of all the atoms in this Z-matrix
! in Cartesian (orthogonal) co-ordinates. We need fractional co-ordinates: convert.
            DO I = 1, natoms(iFrg)
              tX = CART(1,I)*SNGL(c2fmat(1,1)) + CART(2,I)*SNGL(c2fmat(1,2)) + CART(3,I)*SNGL(c2fmat(1,3))
              tY = CART(1,I)*SNGL(c2fmat(2,1)) + CART(2,I)*SNGL(c2fmat(2,2)) + CART(3,I)*SNGL(c2fmat(2,3))
              tZ = CART(1,I)*SNGL(c2fmat(3,1)) + CART(2,I)*SNGL(c2fmat(3,2)) + CART(3,I)*SNGL(c2fmat(3,3))
              KI = KATOM + I
! Note that we must reorder the atoms such that the hydrogens are appended after the 
! non-hydrogens.
              RR_AtmCoords(1,OrderedAtm(KI)) = tX
              RR_AtmCoords(2,OrderedAtm(KI)) = tY
              RR_AtmCoords(3,OrderedAtm(KI)) = tZ
            ENDDO
            KATOM = KATOM + natoms(iFrg)
          ENDDO
        ENDIF
      ENDDO

      END SUBROUTINE RR_MAKEFRAC
!
!*****************************************************************************
!
      SUBROUTINE RR_ROTMAK(DC4,ROTA)
!
! Converts 4 quaternions to a 3x3 rotation matrix
!
      IMPLICIT NONE

      REAL DC4(0:3), ROTA(1:3,1:3)

      REAL EL(0:3,0:3)
      INTEGER I, J

      DO I = 0, 3
        DO J = I, 3
          EL(I,J) = 2.0 * DC4(I) * DC4(J)
        ENDDO
      ENDDO
      ROTA(1,1) = 0.5 * (EL(0,0) + EL(1,1) - EL(2,2) - EL(3,3))
      ROTA(2,2) = 0.5 * (EL(0,0) - EL(1,1) + EL(2,2) - EL(3,3))
      ROTA(3,3) = 0.5 * (EL(0,0) - EL(1,1) - EL(2,2) + EL(3,3))
      ROTA(1,2) = EL(1,2) + EL(0,3)
      ROTA(1,3) = EL(1,3) - EL(0,2)
      ROTA(2,1) = EL(1,2) - EL(0,3)
      ROTA(2,3) = EL(2,3) + EL(0,1)
      ROTA(3,1) = EL(1,3) + EL(0,2)
      ROTA(3,2) = EL(2,3) - EL(0,1)

      END SUBROUTINE RR_ROTMAK
!
!*****************************************************************************
!
      SUBROUTINE RR_DO_ATOM_POS(TRANS,ROTA,POS,NATOMS)

      IMPLICIT NONE

      REAL,  INTENT (IN   ) :: TRANS(3), ROTA(3,3)
      REAL,  INTENT (  OUT) :: POS(3,*)
      INTEGER, INTENT (IN   ) :: NATOMS

      REAL POSIN(3)
      INTEGER I, J

      DO J = 1, NATOMS
        DO I = 1, 3
          POSIN(I) = POS(I,J)
        ENDDO
        CALL RR_ROTCAR(POSIN,POS(1,J),ROTA)
        DO I = 1, 3
          POS(I,J) = POS(I,J) + TRANS(I)
        ENDDO
      ENDDO

      END SUBROUTINE RR_DO_ATOM_POS
!
!*****************************************************************************
!
      SUBROUTINE RR_ROTCAR(XORTO,XORTN,ROTA)

      IMPLICIT NONE

      REAL,  INTENT (IN   ) :: XORTO(3), ROTA(3,3)
      REAL,  INTENT (  OUT) :: XORTN(3)

      INTEGER I, J

      DO I = 1, 3
        XORTN(I) = 0.0
        DO J = 1, 3
          XORTN(I) = XORTN(I) + ROTA(I,J)*XORTO(J)
        ENDDO
      ENDDO

      END SUBROUTINE RR_ROTCAR
!
!*****************************************************************************
!
