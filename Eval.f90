!*==MAKEFRAC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE MAKEFRAC(CHROM)

      USE ZMVAR

      PARAMETER (mvar=100)
      REAL*8 CHROM(*)
      REAL*8 CKK1, CKK2, CKK3
      REAL*8 TRAN(3), ROTA(3,3), POS(3,MAXATM), CART(MAXATM,3)
      REAL*8 QUATER(4), QQSUM, QDEN, QUATT(MVAR)
      REAL*8 XC, YC, ZC, ZERO, ONE, V1, V2, V3

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
      DATA ZERO, ONE/0.0D0, 1.0D0/
      LOGICAL UseCrystallographicCentreOfMass

      KK = 0
      KATOM = 0
!.. Loop over all the fragments
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
          NATS = NATOMS(ifrg)
          KK1 = KK + 1     ! x-translation
          KK2 = KK + 2     ! y-translation
          KK3 = KK + 3     ! z-translation
          CHROM(KK1) = CHROM(KK1) - INT(CHROM(KK1))  ! Position centre of mass inside unit cell
          CHROM(KK2) = CHROM(KK2) - INT(CHROM(KK2))
          CHROM(KK3) = CHROM(KK3) - INT(CHROM(KK3))
          CKK1 = CHROM(KK1)
          CKK2 = CHROM(KK2)
          CKK3 = CHROM(KK3)
          TRAN(1) = CKK1*F2CMAT(1,1)
          TRAN(2) = CKK1*F2CMAT(1,2) + CKK2*F2CMAT(2,2)
          TRAN(3) = CKK1*F2CMAT(1,3) + CKK2*F2CMAT(2,3) + CKK3*F2CMAT(3,3)
          KK = KK + 3
! If more than one atom then proceed
          IF (NATS.GT.1) THEN
            QQSUM = 0.
            DO JQ = 1, 4
              JQS = JQ + KK
              QUATT(JQS) = CHROM(JQS)
              QQSUM = QQSUM + QUATT(JQS)**2
            ENDDO
! QQSUM now holds the sum of the squares of the quaternions
            QDEN = 1./SQRT(QQSUM)
            DO JQ = 1, 4
              JQS = JQ + KK
              QUATT(JQS) = QDEN*QUATT(JQS)
              QUATER(JQ) = QUATT(JQS)
            ENDDO
! QUATER now holds the normalised quaternions
            CALL ROTMAK(QUATER,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
            KK = KK + 4
          ENDIF
          DO I = 1, NATS
            IF (IOPTB(I,IFRG).EQ.1) THEN
              KK = KK + 1
              BLEN(I,IFRG) = CHROM(KK)
            ENDIF
            IF (IOPTA(I,IFRG).EQ.1) THEN
              KK = KK + 1
              ALPH(I,IFRG) = CHROM(KK)
            ENDIF
            IF (IOPTT(I,IFRG).EQ.1) THEN
              KK = KK + 1
              BET(I,IFRG) = CHROM(KK)
            ENDIF
          ENDDO
          CALL MAKEXYZ(NATS,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),        &
       &               IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART(1,1),     &
       &               CART(1,2),CART(1,3))
! Determine origin for rotations
          ICFRG = ICOMFLG(ifrg)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
          IF (ICFRG.EQ.0) THEN
            XC = ZERO
            YC = ZERO
            ZC = ZERO
            UseCrystallographicCentreOfMass = .TRUE.
            IF (UseCrystallographicCentreOfMass) THEN
              DO I = 1, NATS
                XC = XC + AtomicWeighting(I,ifrg)*CART(I,1)
                YC = YC + AtomicWeighting(I,ifrg)*CART(I,2)
                ZC = ZC + AtomicWeighting(I,ifrg)*CART(I,3)
              ENDDO
            ELSE
              DO I = 1, NATS
                XC = XC + CART(I,1)
                YC = YC + CART(I,2)
                ZC = ZC + CART(I,3)
              ENDDO
              XC = XC/DBLE(NATS)
              YC = YC/DBLE(NATS)
              ZC = ZC/DBLE(NATS)
            ENDIF
! Otherwise, use atom number ICFRG
          ELSE
            XC = CART(ICFRG,1)
            YC = CART(ICFRG,2)
            ZC = CART(ICFRG,3)
          ENDIF
! Subtract the origin from all atom positions
          DO I = 1, NATS
            CART(I,1) = CART(I,1) - XC
            CART(I,2) = CART(I,2) - YC
            CART(I,3) = CART(I,3) - ZC
          ENDDO
          DO I = 1, NATS
            POS(1,I) = CART(I,1)
            POS(2,I) = CART(I,2)
            POS(3,I) = CART(I,3)   ! Last occurrence of CART in this file: we're switching to POS
          ENDDO
          CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
          V1 = ONE/F2CMAT(1,1)
          V2 = ONE/F2CMAT(2,2)
          V3 = ONE/F2CMAT(3,3)
          DO I = 1, NATS
            POS(1,I) = POS(1,I)*V1
            POS(2,I) = (POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
            POS(3,I) = (POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3))*V3
            KI = KATOM + I
            X(1,KI) = SNGL(POS(1,I))
            X(2,KI) = SNGL(POS(2,I))
            X(3,KI) = SNGL(POS(3,I))
          ENDDO
          KATOM = KATOM + NATS
        ENDIF
      ENDDO

      END SUBROUTINE MAKEFRAC
!*==MAKEFRAC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE MAKEFRAC_2(CHROM)

      USE ZMVAR

      IMPLICIT NONE

      INTEGER MVAR
      PARAMETER (mvar=100)
      REAL*8 CHROM(*)
      REAL*8 CKK1, CKK2, CKK3
      REAL*8 TRAN(3), ROTA(3,3), CART(1:3,MAXATM)
      REAL*8 QUATER(4), QQSUM, QDEN, QUATT(MVAR)
      REAL*8 XC, YC, ZC, ZERO, ONE, V1, V2, V3

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
      DATA ZERO, ONE/0.0D0, 1.0D0/
      LOGICAL UseCrystallographicCentreOfMass
      INTEGER KK, KATOM, ifrg, NATS, KK1, KK2, KK3, JQ, JQS, I, ICFRG, KI

      KK = 0
      KATOM = 0
!.. Loop over all the fragments
      DO ifrg = 1, maxfrg
        IF (gotzmfile(ifrg)) THEN
          NATS = NATOMS(ifrg)
          KK1 = KK + 1     ! x-translation
          KK2 = KK + 2     ! y-translation
          KK3 = KK + 3     ! z-translation
          CHROM(KK1) = CHROM(KK1) - INT(CHROM(KK1))  ! Position centre of mass inside unit cell
          CHROM(KK2) = CHROM(KK2) - INT(CHROM(KK2))
          CHROM(KK3) = CHROM(KK3) - INT(CHROM(KK3))
          CKK1 = CHROM(KK1)
          CKK2 = CHROM(KK2)
          CKK3 = CHROM(KK3)
          TRAN(1) = CKK1*F2CMAT(1,1)
          TRAN(2) = CKK1*F2CMAT(1,2) + CKK2*F2CMAT(2,2)
          TRAN(3) = CKK1*F2CMAT(1,3) + CKK2*F2CMAT(2,3) + CKK3*F2CMAT(3,3)
          KK = KK + 3
! If more than one atom then proceed
          IF (NATS.GT.1) THEN
            QQSUM = 0.
            DO JQ = 1, 4
              JQS = JQ + KK
              QUATT(JQS) = CHROM(JQS)
              QQSUM = QQSUM + QUATT(JQS)**2
            ENDDO
! QQSUM now holds the sum of the squares of the quaternions
            QDEN = 1./SQRT(QQSUM)
            DO JQ = 1, 4
              JQS = JQ + KK
              QUATT(JQS) = QDEN*QUATT(JQS)
              QUATER(JQ) = QUATT(JQS)
            ENDDO
! QUATER now holds the normalised quaternions
            CALL ROTMAK(QUATER,ROTA)
! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
            KK = KK + 4
          ENDIF
          DO I = 1, NATS
            IF (IOPTB(I,IFRG).EQ.1) THEN
              KK = KK + 1
              BLEN(I,IFRG) = CHROM(KK)
            ENDIF
            IF (IOPTA(I,IFRG).EQ.1) THEN
              KK = KK + 1
              ALPH(I,IFRG) = CHROM(KK)
            ENDIF
            IF (IOPTT(I,IFRG).EQ.1) THEN
              KK = KK + 1
              BET(I,IFRG) = CHROM(KK)
            ENDIF
          ENDDO
          CALL MAKEXYZ_2(NATS,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),        &
                       IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART)
! Determine origin for rotations
          ICFRG = ICOMFLG(ifrg)
! If user set centre of mass flag to 0, then use the molecule's centre of mass
          IF (ICFRG.EQ.0) THEN
            XC = ZERO
            YC = ZERO
            ZC = ZERO
            UseCrystallographicCentreOfMass = .TRUE.
            IF (UseCrystallographicCentreOfMass) THEN
              DO I = 1, NATS
                XC = XC + AtomicWeighting(I,ifrg)*CART(1,I)
                YC = YC + AtomicWeighting(I,ifrg)*CART(2,I)
                ZC = ZC + AtomicWeighting(I,ifrg)*CART(3,I)
              ENDDO
            ELSE
              DO I = 1, NATS
                XC = XC + CART(1,I)
                YC = YC + CART(2,I)
                ZC = ZC + CART(3,I)
              ENDDO
              XC = XC/DBLE(NATS)
              YC = YC/DBLE(NATS)
              ZC = ZC/DBLE(NATS)
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
          CALL DO_ATOM_POS(TRAN,ROTA,CART,NATS)
          V1 = ONE/F2CMAT(1,1)
          V2 = ONE/F2CMAT(2,2)
          V3 = ONE/F2CMAT(3,3)
          DO I = 1, NATS
            CART(1,I) = CART(1,I)*V1
            CART(2,I) = (CART(2,I)-CART(1,I)*F2CMAT(1,2))*V2
            CART(3,I) = (CART(3,I)-CART(1,I)*F2CMAT(1,3)-CART(2,I)*F2CMAT(2,3))*V3
            KI = KATOM + I
            X(1,KI) = SNGL(CART(1,I))
            X(2,KI) = SNGL(CART(2,I))
            X(3,KI) = SNGL(CART(3,I))
          ENDDO
          KATOM = KATOM + NATS
        ENDIF
      ENDDO

      END SUBROUTINE MAKEFRAC_2
!*==ROTMAK.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
!
      SUBROUTINE ROTMAK(DC4,ROTA)
! Converts 4 quaternions to a 3x3 rotation matrix
      REAL*8 DC4(4), EL(4,4), ROTA(3,3)

      DO I = 1, 4
        DO J = I, 4
          EL(I,J) = 2.*DC4(I)*DC4(J)
        ENDDO
      ENDDO
      ROTA(1,1) = 1.0 - (EL(2,2)+EL(3,3))
      ROTA(2,2) = 1.0 - (EL(1,1)+EL(3,3))
      ROTA(3,3) = 1.0 - (EL(1,1)+EL(2,2))
      ROTA(1,2) = EL(1,2) - EL(3,4)
      ROTA(1,3) = EL(1,3) + EL(2,4)
      ROTA(2,3) = EL(2,3) - EL(1,4)
      ROTA(2,1) = EL(1,2) + EL(3,4)
      ROTA(3,1) = EL(1,3) - EL(2,4)
      ROTA(3,2) = EL(2,3) + EL(1,4)

      END SUBROUTINE ROTMAK
