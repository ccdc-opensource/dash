!*==MAKEFRAC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      SUBROUTINE MAKEFRAC(CHROM,NGENES)
                                       !,NATS,ICOM)
!     -------------------------------------------
!
      INCLUDE 'PARAMS.INC'
      INCLUDE 'IZMcheck.inc'
!
      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen, alph, bet, f2cmat
      CHARACTER*3 asym
      INTEGER ioptb, iopta, ioptt, iz1, iz2, iz3
      COMMON /frgcom/ nfrag, lfrag(maxfrg)
      COMMON /zmcomi/ ntatm, natoms(maxfrg), ioptb(maxatm,maxfrg),      &
     &                iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),       &
     &                iz1(maxatm,maxfrg), iz2(maxatm,maxfrg),           &
     &                iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg), alph(maxatm,maxfrg),         &
     &                bet(maxatm,maxfrg), f2cmat(3,3)
      COMMON /zmcomc/ asym(maxatm,maxfrg)
      COMMON /zmcomg/ icomflg(maxfrg)
!
      PARAMETER (mvar=100)
      LOGICAL log_shad
      COMMON /shadl / log_shad(mvar)
      COMMON /shadi / kshad(mvar)
!      DOUBLE PRECISION BLEN,ALPH,BET,F2CMAT
      REAL*8 CHROM(*)
      REAL*8 CKK1, CKK2, CKK3
      REAL*8 TRAN(3), ROTA(3,3), POS(3,MAXATM), CART(MAXATM,3)
      REAL*8 QUATER(4), QQSUM, QDEN, QUATT(MVAR)
      REAL*8 XC, YC, ZC, ZERO, ONE, XNORM, V1, V2, V3
!
!      COMMON  /ZMCOMI/ NATOMS,IOPTT(MAXATM),IZ1(MAXATM),IZ2(MAXATM),IZ3(MAXATM)
!      COMMON  /ZMCOMR/ BLEN(MAXATM),ALPH(MAXATM),BET(MAXATM),F2CMAT(3,3)
!      COMMON  /ZMCOMC/ ASYM(MAXATM)
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      DATA ZERO, ONE/0.0D0, 1.0D0/
!
      KK = 0
      KATOM = 0
!.. Loop over all the fragments
      IFRG = 0
      DO JJJ = 1, NFRAG
!
        DO WHILE (ifrg.LE.CheckSize)
          ifrg = ifrg + 1
          IF (IZMCheck(ifrg).EQ.1) EXIT       ! the loop since we have a fragment we are using
        ENDDO
!
        NATS = NATOMS(IFRG)
        KK1 = KK + 1
        KK2 = KK + 2
        KK3 = KK + 3
        CHROM(KK1) = CHROM(KK1) - INT(CHROM(KK1))
        CHROM(KK2) = CHROM(KK2) - INT(CHROM(KK2))
        CHROM(KK3) = CHROM(KK3) - INT(CHROM(KK3))
        IF (LOG_SHAD(KK1)) THEN
          CKK1 = CHROM(KK1) + CHROM(KSHAD(KK1))
        ELSE
          CKK1 = CHROM(KK1)
        ENDIF
        IF (LOG_SHAD(KK2)) THEN
          CKK2 = CHROM(KK2) + CHROM(KSHAD(KK2))
        ELSE
          CKK2 = CHROM(KK2)
        ENDIF
        IF (LOG_SHAD(KK3)) THEN
          CKK3 = CHROM(KK3) + CHROM(KSHAD(KK3))
        ELSE
          CKK3 = CHROM(KK3)
        ENDIF
        TRAN(1) = CKK1*F2CMAT(1,1)
        TRAN(2) = CKK1*F2CMAT(1,2) + CKK2*F2CMAT(2,2)
        TRAN(3) = CKK1*F2CMAT(1,3) + CKK2*F2CMAT(2,3) + CKK3*F2CMAT(3,3)
        KK = KK + 3
!.. If more than one atom then proceed
        IF (NATS.GT.1) THEN
          QQSUM = 0.
          DO JQ = 1, 4
            JQS = JQ + KK
            IF (LOG_SHAD(JQS)) THEN
              QUATT(JQS) = CHROM(JQS) + CHROM(KSHAD(JQS))
            ELSE
              QUATT(JQS) = CHROM(JQS)
            ENDIF
            QQSUM = QQSUM + QUATT(JQS)**2
          ENDDO
          QDEN = 1./SQRT(QQSUM)
          DO JQ = 1, 4
            JQS = JQ + KK
            QUATT(JQS) = QDEN*QUATT(JQS)
            QUATER(JQ) = QUATT(JQS)
          ENDDO
          CALL ROTMAK(QUATER,ROTA)
          KK = KK + 4
        ENDIF
        DO I = 1, NATS
          IF (IOPTB(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              BLEN(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              BLEN(I,IFRG) = CHROM(KK)
            ENDIF
          ENDIF
          IF (IOPTA(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              ALPH(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              ALPH(I,IFRG) = CHROM(KK)
            ENDIF
          ENDIF
          IF (IOPTT(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              BET(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              BET(I,IFRG) = CHROM(KK)
            ENDIF
          ENDIF
        ENDDO
        CALL MAKEXYZ(NATS,BLEN(1,IFRG),ALPH(1,IFRG),BET(1,IFRG),        &
     &               IZ1(1,IFRG),IZ2(1,IFRG),IZ3(1,IFRG),CART(1,1),     &
     &               CART(1,2),CART(1,3))
        ICFRG = ICOMFLG(IFRG)
        IF (ICFRG.EQ.0) THEN
          XC = ZERO
          YC = ZERO
          ZC = ZERO
          DO I = 1, NATS
            XC = XC + CART(I,1)
            YC = YC + CART(I,2)
            ZC = ZC + CART(I,3)
          ENDDO
          XNORM = ONE/DFLOAT(NATS)
          XC = XC*XNORM
          YC = YC*XNORM
          ZC = ZC*XNORM
        ELSE
          XC = CART(ICFRG,1)
          YC = CART(ICFRG,2)
          ZC = CART(ICFRG,3)
        ENDIF
        DO I = 1, NATS
          CART(I,1) = CART(I,1) - XC
          CART(I,2) = CART(I,2) - YC
          CART(I,3) = CART(I,3) - ZC
        ENDDO
        DO I = 1, NATS
          POS(1,I) = CART(I,1)
          POS(2,I) = CART(I,2)
          POS(3,I) = CART(I,3)
        ENDDO
        CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
        V1 = ONE/F2CMAT(1,1)
        V2 = ONE/F2CMAT(2,2)
        V3 = ONE/F2CMAT(3,3)
        DO I = 1, NATS
          POS(1,I) = POS(1,I)*V1
          POS(2,I) = (POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
          POS(3,I) = (POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3)&
     &               )*V3
          KI = KATOM + I
          X(1,KI) = SNGL(POS(1,I))
          X(2,KI) = SNGL(POS(2,I))
          X(3,KI) = SNGL(POS(3,I))
        ENDDO
        KATOM = KATOM + NATS
      ENDDO
      END SUBROUTINE MAKEFRAC
!*==MAKEFRAC_PRT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
!
      SUBROUTINE MAKEFRAC_PRT(CHROM,NGENES,IWRUN)
                                                 !,NATS,ICOM)
!     -------------------------------------------
!
      INCLUDE 'PARAMS.INC'
!
      REAL tiso, occ
      COMMON /zmcomo/ tiso(maxatm,maxfrg), occ(maxatm,maxfrg)
      DOUBLE PRECISION blen, alph, bet, f2cmat
      CHARACTER*3 asym
      INTEGER ioptb, iopta, ioptt, iz1, iz2, iz3
      INTEGER IWRUN
      COMMON /frgcom/ nfrag, lfrag(maxfrg)
      COMMON /zmcomi/ ntatm, natoms(maxfrg), ioptb(maxatm,maxfrg),      &
     &                iopta(maxatm,maxfrg), ioptt(maxatm,maxfrg),       &
     &                iz1(maxatm,maxfrg), iz2(maxatm,maxfrg),           &
     &                iz3(maxatm,maxfrg)
      COMMON /zmcomr/ blen(maxatm,maxfrg), alph(maxatm,maxfrg),         &
     &                bet(maxatm,maxfrg), f2cmat(3,3)
      COMMON /zmcomc/ asym(maxatm,maxfrg)
      COMMON /zmcomg/ icomflg(maxfrg)
!
      PARAMETER (mvar=100)
      COMMON /shadl / log_shad(mvar)
      COMMON /shadi / kshad(mvar)
!      DOUBLE PRECISION BLEN,ALPH,BET,F2CMAT
      REAL*8 CHROM(*)
      REAL*8 TRAN(3), ROTA(3,3), POS(3,MAXATM), CART(MAXATM,3)
      REAL*8 QUATER(4), QQSUM, QDEN, QUATT(MVAR)
      REAL*8 XC, YC, ZC, ZERO, ONE, XNORM, V1, V2, V3
!
!      COMMON  /ZMCOMI/ NATOMS,IOPTT(MAXATM),IZ1(MAXATM),IZ2(MAXATM),IZ3(MAXATM)
!      COMMON  /ZMCOMR/ BLEN(MAXATM),ALPH(MAXATM),BET(MAXATM),F2CMAT(3,3)
!      COMMON  /ZMCOMC/ ASYM(MAXATM)
      COMMON /POSNS / NATOM, X(3,150), KX(3,150), AMULT(150), TF(150),  &
     &                KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
     &                SDX(3,150), SDTF(150), SDSITE(150), KOM17
      DATA ZERO, ONE/0.0D0, 1.0D0/
!
      KK = 0
      KATOM = 0
!.. Loop over all the fragments
      DO IFRG = 1, NFRAG
        NATS = NATOMS(IFRG)
!
        IF (IWRUN.GT.0) THEN
          IF (NATS.EQ.1) THEN
!
            WRITE (IWRUN,6009) IFRG
!
 6009       FORMAT ('  Fragment number ',I2,' is a single atom')
!
          ELSE
!
            WRITE (IWRUN,6010) IFRG, NATS
 6010       FORMAT ('  Fragment number ',I2,' contains ',I3,' atoms')
!
          ENDIF
        ENDIF
!
        KK1 = KK + 1
        KK2 = KK + 2
        KK3 = KK + 3
!       TRAN(1)=CHROM(KK1)*F2CMAT(1,1)
!       TRAN(2)=CHROM(KK1)*F2CMAT(1,2)+CHROM(KK2)*F2CMAT(2,2)
!       TRAN(3)=CHROM(KK1)*F2CMAT(1,3)+CHROM(KK2)*F2CMAT(2,3)+
!     *        CHROM(KK3)*F2CMAT(3,3)
        IF (LOG_SHAD(KK1)) THEN
          CKK1 = CHROM(KK1) + CHROM(KSHAD(KK1))
        ELSE
          CKK1 = CHROM(KK1)
        ENDIF
        IF (LOG_SHAD(KK2)) THEN
          CKK2 = CHROM(KK2) + CHROM(KSHAD(KK2))
        ELSE
          CKK2 = CHROM(KK2)
        ENDIF
        IF (LOG_SHAD(KK3)) THEN
          CKK3 = CHROM(KK3) + CHROM(KSHAD(KK3))
        ELSE
          CKK3 = CHROM(KK3)
        ENDIF
        TRAN(1) = CKK1*F2CMAT(1,1)
        TRAN(2) = CKK1*F2CMAT(1,2) + CKK2*F2CMAT(2,2)
        TRAN(3) = CKK1*F2CMAT(1,3) + CKK2*F2CMAT(2,3) + CKK3*F2CMAT(3,3)
!
        IF (IWRUN.GT.0) THEN
          WRITE (IWRUN,6020) CKK1, CKK2, CKK3
 6020     FORMAT ('   and is positioned at ',3F10.5)
        ENDIF
!
        KK = KK + 3
!.. If more than one atom then proceed
        IF (NATS.GT.1) THEN
          QQSUM = 0.
          DO JQ = 1, 4
            JQS = JQ + KK
            IF (LOG_SHAD(JQS)) THEN
              QUATT(JQS) = CHROM(JQS) + CHROM(KSHAD(JQS))
            ELSE
              QUATT(JQS) = CHROM(JQS)
            ENDIF
            QQSUM = QQSUM + QUATT(JQS)**2
          ENDDO
          QDEN = 1./SQRT(QQSUM)
          DO JQ = 1, 4
            JQS = JQ + KK
            QUATT(JQS) = QDEN*QUATT(JQS)
            QUATER(JQ) = QUATT(JQS)
          ENDDO
          CALL ROTMAK(QUATER,ROTA)
          KK = KK + 4
        ENDIF
!
        IF (NATS.GT.1) THEN
          IF (IWRUN.GT.0) THEN
            WRITE (IWRUN,6030) (SNGL(QUATER(I)),I=1,4)
 6030       FORMAT ('   with orientation 4-vector ',4F10.5)
          ENDIF
        ENDIF
!
        DO I = 1, NATS
          IF (IOPTB(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              BLEN(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              BLEN(I,IFRG) = CHROM(KK)
            ENDIF
          ENDIF
          IF (IOPTA(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              ALPH(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              ALPH(I,IFRG) = CHROM(KK)
            ENDIF
          ENDIF
          IF (IOPTT(I,IFRG).EQ.1) THEN
            KK = KK + 1
            IF (LOG_SHAD(KK)) THEN
              BET(I,IFRG) = CHROM(KSHAD(KK)) + CHROM(KK)
            ELSE
              BET(I,IFRG) = CHROM(KK)
            ENDIF
            IF (IWRUN.GT.0) THEN
              WRITE (IWRUN,6040) I, SNGL(BET(I,IFRG))
 6040         FORMAT ('   torsion number ',i2,' is ',f10.3)
            ENDIF
          ENDIF
        ENDDO
        CALL MAKEXYZ(NATS,BLEN(1,IFRG),ALPH(1,IFRG),BET(1,IFRG),        &
     &               IZ1(1,IFRG),IZ2(1,IFRG),IZ3(1,IFRG),CART(1,1),     &
     &               CART(1,2),CART(1,3))
!
        ICFRG = ICOMFLG(IFRG)
        IF (ICFRG.EQ.0) THEN
          XC = ZERO
          YC = ZERO
          ZC = ZERO
          DO I = 1, NATS
            XC = XC + CART(I,1)
            YC = YC + CART(I,2)
            ZC = ZC + CART(I,3)
          ENDDO
          XNORM = ONE/DFLOAT(NATS)
          XC = XC*XNORM
          YC = YC*XNORM
          ZC = ZC*XNORM
        ELSE
          XC = CART(ICFRG,1)
          YC = CART(ICFRG,2)
          ZC = CART(ICFRG,3)
        ENDIF
        DO I = 1, NATS
          CART(I,1) = CART(I,1) - XC
          CART(I,2) = CART(I,2) - YC
          CART(I,3) = CART(I,3) - ZC
        ENDDO
        DO I = 1, NATS
          POS(1,I) = CART(I,1)
          POS(2,I) = CART(I,2)
          POS(3,I) = CART(I,3)
        ENDDO
        CALL DO_ATOM_POS(TRAN,ROTA,POS,NATS)
        V1 = ONE/F2CMAT(1,1)
        V2 = ONE/F2CMAT(2,2)
        V3 = ONE/F2CMAT(3,3)
        DO I = 1, NATS
          POS(1,I) = POS(1,I)*V1
          POS(2,I) = (POS(2,I)-POS(1,I)*F2CMAT(1,2))*V2
          POS(3,I) = (POS(3,I)-POS(1,I)*F2CMAT(1,3)-POS(2,I)*F2CMAT(2,3)&
     &               )*V3
          KI = KATOM + I
          X(1,KI) = SNGL(POS(1,I))
          X(2,KI) = SNGL(POS(2,I))
          X(3,KI) = SNGL(POS(3,I))
        ENDDO
        KATOM = KATOM + NATS
      ENDDO
      END SUBROUTINE MAKEFRAC_PRT
!*==ROTMAK.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!----------<<<<<<<<<<<<<<<==========+++++++++==========>>>>>>>>>>>>>>>----------
!
      SUBROUTINE ROTMAK(DC4,ROTA)
!
      REAL*8 DC4(4), EL(4,4), ROTA(3,3)
!
      DO I = 1, 4
        DO J = I, 4
          EL(I,J) = 2.*DC4(I)*DC4(J)
        ENDDO
      ENDDO
!
      ROTA(1,1) = 1.0 - (EL(2,2)+EL(3,3))
      ROTA(2,2) = 1.0 - (EL(1,1)+EL(3,3))
      ROTA(3,3) = 1.0 - (EL(1,1)+EL(2,2))
      ROTA(1,2) = EL(1,2) - EL(3,4)
      ROTA(1,3) = EL(1,3) + EL(2,4)
      ROTA(2,3) = EL(2,3) - EL(1,4)
      ROTA(2,1) = EL(1,2) + EL(3,4)
      ROTA(3,1) = EL(1,3) - EL(2,4)
      ROTA(3,2) = EL(2,3) + EL(1,4)
!
      RETURN
      END SUBROUTINE ROTMAK
