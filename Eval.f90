!O!*==MAKEFRAC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!O!
!O      SUBROUTINE MAKEFRAC(CHROM)
!O
!O      USE ZMVAR
!O
!O      PARAMETER (mvar=100)
!O      REAL*8 CHROM(*)
!O      REAL*8 CKK1, CKK2, CKK3
!O      REAL*8 TRAN(3), ROTA(3,3), POS(3,MAXATM), CART(MAXATM,3)
!O      REAL*8 QUATER(4), QQSUM, QDEN, QUATT(MVAR)
!O      REAL*8 XC, YC, ZC, ZERO, ONE, V1, V2, V3
!O
!O      INTEGER         NATOM
!O      REAL                   X
!O      INTEGER                          KX
!O      REAL                                        AMULT,      TF
!O      INTEGER         KTF
!O      REAL                      SITE
!O      INTEGER                              KSITE,      ISGEN
!O      REAL            SDX,        SDTF,      SDSITE
!O      INTEGER                                             KOM17
!O      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
!O     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
!O     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
!O      DATA ZERO, ONE/0.0D0, 1.0D0/
!O      LOGICAL UseCrystallographicCentreOfMass
!O
!O      KK = 0
!O      KATOM = 0
!O!.. Loop over all the fragments
!O      DO ifrg = 1, maxfrg
!O        IF (gotzmfile(ifrg)) THEN
!O          NATS = NATOMS(ifrg)
!O          KK1 = KK + 1     ! x-translation
!O          KK2 = KK + 2     ! y-translation
!O          KK3 = KK + 3     ! z-translation
!O          CHROM(KK1) = CHROM(KK1) - INT(CHROM(KK1))  ! Position centre of mass inside unit cell
!O          CHROM(KK2) = CHROM(KK2) - INT(CHROM(KK2))
!O          CHROM(KK3) = CHROM(KK3) - INT(CHROM(KK3))
!O          CKK1 = CHROM(KK1)
!O          CKK2 = CHROM(KK2)
!O          CKK3 = CHROM(KK3)
!O          TRAN(1) = CKK1*F2CMAT(1,1)
!O          TRAN(2) = CKK1*F2CMAT(1,2) + CKK2*F2CMAT(2,2)
!O          TRAN(3) = CKK1*F2CMAT(1,3) + CKK2*F2CMAT(2,3) + CKK3*F2CMAT(3,3)
!O          KK = KK + 3
!O! If more than one atom then proceed
!O          IF (NATS.GT.1) THEN
!O            QQSUM = 0.
!O            DO JQ = 1, 4
!O              JQS = JQ + KK
!O              QUATT(JQS) = CHROM(JQS)
!O              QQSUM = QQSUM + QUATT(JQS)**2
!O            ENDDO
!O! QQSUM now holds the sum of the squares of the quaternions
!O            QDEN = 1./SQRT(QQSUM)
!O            DO JQ = 1, 4
!O              JQS = JQ + KK
!O              QUATT(JQS) = QDEN*QUATT(JQS)
!O              QUATER(JQ) = QUATT(JQS)
!O            ENDDO
!O! QUATER now holds the normalised quaternions
!O            CALL ROTMAK(QUATER,ROTA)
!O! ROTA now holds the 3x3 rotation matrix corresponding to the quaternions
!O            KK = KK + 4
!O          ENDIF
!O          DO I = 1, NATS
!O            IF (IOPTB(I,IFRG).EQ.1) THEN
!O              KK = KK + 1
!O              BLEN(I,IFRG) = CHROM(KK)
!O            ENDIF
!O            IF (IOPTA(I,IFRG).EQ.1) THEN
!O              KK = KK + 1
!O              ALPH(I,IFRG) = CHROM(KK)
!O            ENDIF
!O            IF (IOPTT(I,IFRG).EQ.1) THEN
!O              KK = KK + 1
!O              BET(I,IFRG) = CHROM(KK)
!O            ENDIF
!O          ENDDO
!O          CALL MAKEXYZ(NATS,BLEN(1,ifrg),ALPH(1,ifrg),BET(1,ifrg),        &
!O       &               IZ1(1,ifrg),IZ2(1,ifrg),IZ3(1,ifrg),CART(1,1),     &
!O       &               CART(1,2),CART(1,3))
!O! Determine origin for rotations
!O          ICFRG = ICOMFLG(ifrg)
!O! If user set centre of mass flag to 0, then use the molecule's centre of mass
!O          IF (ICFRG.EQ.0) THEN
!O            XC = ZERO
!O            YC = ZERO
!O            ZC = ZERO
!O            UseCrystallographicCentreOfMass = .TRUE.
!O            IF (UseCrystallographicCentreOfMass) THEN
!O              DO I = 1, NATS
!O                XC = XC + AtomicWeighting(I,ifrg)*CART(I,1)
!O                YC = YC + AtomicWeighting(I,ifrg)*CART(I,2)
!O                ZC = ZC + AtomicWeighting(I,ifrg)*CART(I,3)
!O              ENDDO
!O            ELSE
!O              DO I = 1, NATS
!O                XC = XC + CART(I,1)
!O                YC = YC + CART(I,2)
!O                ZC = ZC + CART(I,3)
!O              ENDDO
!O              XC = XC/DBLE(NATS)
!O              YC = YC/DBLE(NATS)
!O              ZC = ZC/DBLE(NATS)
!O            ENDIF
!O! Otherwise, use atom number ICFRG
!O          ELSE
!O            XC = CART(ICFRG,1)
!O            YC = CART(ICFRG,2)
!O            ZC = CART(ICFRG,3)
!O          ENDIF
!O! Subtract the origin from all atom positions
!O          DO I = 1, NATS
!O            CART(I,1) = CART(I,1) - XC
!O            CART(I,2) = CART(I,2) - YC
!O            CART(I,3) = CART(I,3) - ZC
!O          ENDDO
!O          DO I = 1, NATS
!O            POS(1,I) = CART(I,1)
!O            POS(2,I) = CART(I,2)
!O            POS(3,I) = CART(I,3)   ! Last occurrence of CART in this file: we're switching to POS
!O          ENDDO
!O          CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
!O          V1 = ONE/F2CMAT(1,1)
!O          V2 = ONE/F2CMAT(2,2)
!O          V3 = ONE/F2CMAT(3,3)
!O          DO I = 1, NATS
!O            POS(1,I) = POS(1,I)*V1
!O            POS(2,I) = (POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
!O            POS(3,I) = (POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3))*V3
!O            KI = KATOM + I
!O            X(1,KI) = SNGL(POS(1,I))
!O            X(2,KI) = SNGL(POS(2,I))
!O            X(3,KI) = SNGL(POS(3,I))
!O          ENDDO
!O          KATOM = KATOM + NATS
!O        ENDIF
!O      ENDDO
!O
!O      END SUBROUTINE MAKEFRAC
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
