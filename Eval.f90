!
!*****************************************************************************
!
      SUBROUTINE MAKEFRAC(CHROM)

      USE VARIABLES
      USE ZMVAR
      USE ATMVAR

      IMPLICIT NONE

      REAL CHROM(*)

      INCLUDE 'PARAMS.INC'

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
      COMMON /POSNS / NATOM, XATO(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17

      LOGICAL, EXTERNAL :: Get_UseCrystallographicCoM
      REAL CKK1, CKK2, CKK3
      REAL TRAN(1:3), ROTA(1:3,1:3), CART(1:3,1:MAXATM)
      REAL QUATER(0:3), QQSUM, QDEN
      REAL XC, YC, ZC
      INTEGER KK, KATOM, iFrg, NATS, KK1, KK2, KK3, JQ, JQS, I, ICFRG, KI
      REAL Duonion(0:1)
      REAL tQ(0:3)

      KK = 0
      KATOM = 0
! Loop over all the fragments
      DO iFrg = 1, maxfrg
        IF (gotzmfile(iFrg)) THEN
          NATS = NATOMS(iFrg)
          KK1 = KK + 1     ! x-translation
          KK2 = KK + 2     ! y-translation
          KK3 = KK + 3     ! z-translation
          CHROM(KK1) = CHROM(KK1) - INT(CHROM(KK1))  ! Position centre of mass inside unit cell
          CHROM(KK2) = CHROM(KK2) - INT(CHROM(KK2))
          CHROM(KK3) = CHROM(KK3) - INT(CHROM(KK3))
          CKK1 = CHROM(KK1)
          CKK2 = CHROM(KK2)
          CKK3 = CHROM(KK3)
          TRAN(1) = CKK1*F2CMAT(1,1) + CKK2*F2CMAT(1,2) + CKK3*F2CMAT(1,3)
          TRAN(2) = CKK1*F2CMAT(2,1) + CKK2*F2CMAT(2,2) + CKK3*F2CMAT(2,3)
          TRAN(3) = CKK1*F2CMAT(3,1) + CKK2*F2CMAT(3,2) + CKK3*F2CMAT(3,3)
          KK = KK + 3
! If more than one atom then proceed
          IF (NATS.GT.1) THEN
! If we have at least two atoms, there are two options:
! 1. Rotate the whole molecule freely, using quaternions
! 2. Specify the rotation axis (e.g. if molecule on mirror plane)
            IF (UseQuaternions(iFrg)) THEN
              QQSUM = 0.0
              DO JQ = 0, 3
                JQS = 1 + JQ + KK
                QQSUM = QQSUM + CHROM(JQS)**2
              ENDDO
! QQSUM now holds the sum of the squares of the quaternions
              QDEN = 1.0 / SQRT(QQSUM)
              DO JQ = 0, 3
                JQS = 1 + JQ + KK
                QUATER(JQ) = QDEN * CHROM(JQS)
              ENDDO
! QUATER now holds the normalised quaternions
              CALL ROTMAK(QUATER,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
              KK = KK + 4
            ELSE
! Single axis, so we use the 2D analogue of quaternions: a complex number of length 1.0
              Duonion(0) = CHROM(KK+1)
              Duonion(1) = CHROM(KK+2)
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
              CALL ROTMAK(tQ,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the single rotation axis
              KK = KK + 2
            ENDIF
          ENDIF
          DO I = 1, NATS
            IF (IOPTB(I,iFrg) .EQ. 1) THEN
              KK = KK + 1
              BLEN(I,iFrg) = CHROM(KK)
            ENDIF
            IF (IOPTA(I,iFrg) .EQ. 1) THEN
              KK = KK + 1
              ALPH(I,iFrg) = CHROM(KK)
            ENDIF
            IF (IOPTT(I,iFrg) .EQ. 1) THEN
              KK = KK + 1
              BET(I,iFrg) = CHROM(KK)
            ENDIF
          ENDDO
          CALL makexyz(NATS,BLEN(1,iFrg),ALPH(1,iFrg),BET(1,iFrg),        &
                       IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),CART)
! Determine origin for rotations
          ICFRG = ICOMFLG(iFrg)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
          IF (ICFRG .EQ. 0) THEN
            XC = 0.0
            YC = 0.0
            ZC = 0.0
            IF (Get_UseCrystallographicCoM()) THEN
              DO I = 1, NATS
                XC = XC + AtomicWeighting(I,iFrg)*CART(1,I)
                YC = YC + AtomicWeighting(I,iFrg)*CART(2,I)
                ZC = ZC + AtomicWeighting(I,iFrg)*CART(3,I)
              ENDDO
            ELSE
              DO I = 1, NATS
                XC = XC + CART(1,I)
                YC = YC + CART(2,I)
                ZC = ZC + CART(3,I)
              ENDDO
              XC = XC/FLOAT(NATS)
              YC = YC/FLOAT(NATS)
              ZC = ZC/FLOAT(NATS)
            ENDIF
! Otherwise, use atom number ICFRG
          ELSE
            XC = CART(1,ICFRG)
            YC = CART(2,ICFRG)
            ZC = CART(3,ICFRG)
          ENDIF
! Subtract the origin from all atom positions
          DO I = 1, NATS
            CART(1,I) = CART(1,I) - XC
            CART(2,I) = CART(2,I) - YC
            CART(3,I) = CART(3,I) - ZC
          ENDDO
! Apply rotation and translation to the atoms of this Z-matrix
          CALL DO_ATOM_POS(TRAN,ROTA,CART,NATS)
! When we are here, we have the actual co-ordinates of all the atoms in this Z-matrix
! in Cartesian (orthogonal) co-ordinates. We need fractional co-ordinates: convert.
          DO I = 1, NATS
            KI = KATOM + I
! Note that we must reorder the atoms such that the hydrogens are appended after the 
! non-hydrogens.
            XATO(1,OrderedAtm(KI)) = CART(1,I)*c2fmat(1,1) + CART(2,I)*c2fmat(1,2) + CART(3,I)*c2fmat(1,3)
            XATO(2,OrderedAtm(KI)) = CART(1,I)*c2fmat(2,1) + CART(2,I)*c2fmat(2,2) + CART(3,I)*c2fmat(2,3)
            XATO(3,OrderedAtm(KI)) = CART(1,I)*c2fmat(3,1) + CART(2,I)*c2fmat(3,2) + CART(3,I)*c2fmat(3,3)
          ENDDO
          KATOM = KATOM + NATS
        ENDIF
      ENDDO

      END SUBROUTINE MAKEFRAC
!
!*****************************************************************************
!
      SUBROUTINE ROTMAK(Q,ROTA)
!
! Converts 4 quaternions to a 3x3 rotation matrix
!
! JvdS, 14 Jan 2002.
! Reprogrammed according to Molecular Modelling by Leach, page 384.
! (Note the typos: q1 and q2 should read phi minus psi)

! q0 = Cos(0.5*beta) * Cos(0.5*(alpha+gamma))
! q1 = Sin(0.5*beta) * Cos(0.5*(alpha-gamma))
! q2 = Sin(0.5*beta) * Sin(0.5*(alpha-gamma))
! q3 = Cos(0.5*beta) * Sin(0.5*(alpha+gamma))
!
! 1. gamma about the z-axis
! 2. beta  about the x-axis
! 3. alpha about the z-axis
!
! quaternion A times quaternion B:
!
! {A0*1 + A1*i + A2*j + A3*k} * {B0*1 + B1*i + B2*j + B3*k} =
!   (A0*B0 - A1*B1 - A2*B2 - A3*B3)*1 +
!   (A0*B1 + A1*B0 + A2*B3 - A3*B2)*i +
!   (A0*B2 - A1*B3 + A2*B0 + A3*B1)*j +
!   (A0*B3 + A1*B2 - A2*B1 + A3*B0)*k
!
! Inverse of   1*q0 + i*q1 + j*q2 + k*q3   =   1*q0 - i*q1 - j*q2 - k*q3 (only if normalised)
!
! unity   q0 = 1
!         q1 = 0
!         q2 = 0
!         q3 = 0
!
! Rotation about z-axis => beta = 0 => q1 = q2 = 0
!
! 1 * 1 =  1    i * 1 =  i    j * 1 =  j    k * 1 =  k
! 1 * i =  i    i * i = -1    j * i = -k    k * i =  j
! 1 * j =  j    i * j =  k    j * j = -1    k * j = -i
! 1 * k =  k    i * k = -j    j * k =  i    k * k = -1
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Q(0:3)
      REAL, INTENT (  OUT) :: ROTA(1:3,1:3)

      ROTA(1,1) = 1.0 - 2.0*(Q(2)**2) - 2.0*(Q(3)**2); 
      ROTA(1,2) = 2.0*Q(1)*Q(2) - 2.0*Q(3)*Q(0);     
      ROTA(1,3) = 2.0*Q(1)*Q(3) + 2.0*Q(2)*Q(0);
      ROTA(2,1) = 2.0*Q(1)*Q(2) + 2.0*Q(3)*Q(0);     
      ROTA(2,2) = 1.0 - 2.0*(Q(1)**2) - 2.0*(Q(3)**2); 
      ROTA(2,3) = 2.0*Q(2)*Q(3) - 2.0*Q(1)*Q(0);
      ROTA(3,1) = 2.0*Q(1)*Q(3) - 2.0*Q(2)*Q(0);     
      ROTA(3,2) = 2.0*Q(2)*Q(3) + 2.0*Q(1)*Q(0);     
      ROTA(3,3) = 1.0 - 2.0*(Q(1)**2) - 2.0*(Q(2)**2);

      END SUBROUTINE ROTMAK
!
!*****************************************************************************
!
      SUBROUTINE DO_ATOM_POS(TRANS, ROTA, POS, NATOMS)

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: TRANS(3), ROTA(3,3)
      REAL,    INTENT (  OUT) :: POS(3,*)
      INTEGER, INTENT (IN   ) :: NATOMS

      REAL POSIN(3)
      INTEGER I, J
      INTEGER K, L

      DO J = 1, NATOMS
        DO I = 1, 3
          POSIN(I) = POS(I,J)
        ENDDO
        DO K = 1, 3
          POS(K,J) = 0.0
          DO L = 1, 3
            POS(K,J) = POS(K,J) + ROTA(K,L)*POSIN(L)
          ENDDO
        ENDDO
        DO I = 1, 3
          POS(I,J) = POS(I,J) + TRANS(I)
        ENDDO
      ENDDO

      END SUBROUTINE DO_ATOM_POS
!
!*****************************************************************************
!
