!*==TRICLINI2.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!    -------------------------------------------------------
!    |		     T R I C L I N I C 2		   |				   |
!    -------------------------------------------------------
      SUBROUTINE TRICLINI2
      USE DICVAR
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Local variables
!
      REAL :: Abd, Alet0, Alet1, Alet2, Alet3, Alet4, Alet5, Ambd1, Ambd2, Atest, Btest, Ctest, Dmoim,&
     &        Dmoir1, Dplur1, Dtest, Emma1, Emma2, Emma3, Emma4, Emmi1, Emmi2, Emmi3, Emmi4, Eplu1,    &
     &        Eplu2, Eplu3, Etest, Etesty, Fplu1, Fplu2, Fplu3, Ftest, Xabc, Zzo
      REAL, INTRINSIC :: AMAX1, AMIN1
      INTEGER :: I, Ia, Ia2, Ia3, Ia4, Ib, Ib2, Ib3, Ib4, Ic, Ic2, Ic3, Ic4, Id, Id2, Id3, Id4,   &
     &           Ie, Ie2, Ie3, Ie4, Imf, Imf2, Imf3, Imf4, J, Jj, Jjl, Jjl1, Na, Nb, Nc, Nd, Ne,  &
     &           Nf
!
!*** End of declarations rewritten by SPAG
!
      Na = (amax-amin)/pasa
      Nb = (bmax-bmin)/pasb
      Nc = (cmax-cmin)/pasc
      Nd = (dmax-dmin)/pasd
      Ne = (emax-emin)/pase
      Nf = (fmax-fmin)/pasf
      IF ( Na.LT.0 .OR. Nb.LT.0 .OR. Nc.LT.0 .OR. Nd.LT.0 .OR. Ne.LT.0 .OR. Nf.LT.0 ) GOTO 99999
      IF ( iiaa.EQ.0 ) GOTO 200
      Ia = 0
 100  amoi1 = amin + Ia*pasa
      aplu1 = amoi1 + pasa
      kamoi1 = coeff*amoi1
      kaplu1 = coeff*aplu1
      IF ( md34.EQ.1 ) THEN
        dmoi1 = amoi1 - s3 - errd
        dplu1 = aplu1 - s3 + errd
        Dmoim = AMIN1(ABS(dmoi1),ABS(dplu1))
        IF ( Dmoim.GT.dmax .AND. dmoi1.GE.0. ) GOTO 99999
        IF ( Dmoim.GT.dmax ) GOTO 2400
        IF ( dmoi1.LT.(-dmax) ) dmoi1 = -dmax
        IF ( dplu1.GT.dmax ) dplu1 = dmax
        Zzo = zco
        IF ( Dmoim*Dmoim.GE.zco*aplu1*bplu1 .AND. dmoi1.GE.0. ) GOTO 99999
        IF ( Dmoim*Dmoim.GE.zco*aplu1*bplu1 ) GOTO 2400
        kdmoi1 = coeff*dmoi1
        kdplu1 = coeff*dplu1
      ELSEIF ( md7.NE.0 ) THEN
        bmoi1 = s7 - aplu1 - errb
        IF ( bmoi1.GT.bmax ) GOTO 2400
        bplu1 = s7 - amoi1 + errb
        IF ( bplu1.LE.0 ) GOTO 99999
        IF ( bmoi1.LE.0 ) bmoi1 = bmin
        IF ( bmoi1.GT.aplu1 .OR. (dmoi1-1E-8)**2.GE.zco*aplu1*bplu1 ) GOTO 2400
        kbmoi1 = coeff*bmoi1
        kbplu1 = coeff*bplu1
      ENDIF
 200  IF ( iibb.EQ.0 ) GOTO 400
      Ib = 0
 300  bmoi1 = bmin + Ib*pasb
      bplu1 = bmoi1 + pasb
      kbmoi1 = coeff*bmoi1
      kbplu1 = coeff*bplu1
      IF ( md12.EQ.1 ) THEN
        dmoi1 = (bmoi1-s1) - errd
        dplu1 = (bplu1-s1) + errd
        Dmoim = AMIN1(ABS(dmoi1),ABS(dplu1))
        IF ( Dmoim.GT.dmax .AND. dmoi1.GE.0. ) GOTO 99999
        IF ( Dmoim*Dmoim.GE.zco*aplu1*bplu1 .AND. dmoi1.GE.0. ) GOTO 99999
      ELSE
        IF ( md56.EQ.0 ) GOTO 400
        dmoi1 = (s5-aplu1-bplu1) - errd
        dplu1 = (s5-amoi1-bmoi1) + errd
        Dmoim = AMIN1(ABS(dmoi1),ABS(dplu1))
        IF ( Dmoim.GT.dmax .AND. dplu1.LE.0. ) GOTO 2400
        IF ( Dmoim*Dmoim.GE.zco*aplu1*bplu1 .AND. dplu1.LE.0. ) GOTO 2400
      ENDIF
      IF ( Dmoim.GT.dmax ) GOTO 2200
      IF ( dmoi1.LT.(-dmax) ) dmoi1 = -dmax
      IF ( dplu1.GT.dmax ) dplu1 = dmax
      Zzo = zco
      IF ( Dmoim*Dmoim.GE.zco*aplu1*bplu1 ) GOTO 2200
      kdmoi1 = coeff*dmoi1
      kdplu1 = coeff*dplu1
 400  IF ( iicc.EQ.0 ) GOTO 600
      Ic = 0
 500  cmoi1 = cmin + Ic*pasc
      IF ( cmoi1.GT.(cmax+1E-8) ) GOTO 2100
      cplu1 = cmoi1 + pasc
      kcmoi1 = coeff*cmoi1
      kcplu1 = coeff*cplu1
      IF ( md89.NE.0 ) THEN
        emoi1 = (bmoi1+cmoi1-s8) - erre
        Eplu1 = (bplu1+cplu1-s8) + erre
        Emma1 = AMIN1(ABS(emoi1),ABS(Eplu1))
        IF ( Emma1.GT.emax .AND. emoi1.GE.0 ) GOTO 2200
        IF ( Emma1.GT.emax ) GOTO 2000
        IF ( emoi1.LT.(-emax) ) emoi1 = -emax
        IF ( Eplu1.GT.emax ) Eplu1 = emax
        Zzo = zco
        IF ( emoi1.LT.0. ) Zzo = 4.
        Emmi1 = AMIN1(ABS(emoi1),ABS(Eplu1))
        IF ( Emmi1*Emmi1.GE.Zzo*bplu1*cplu1 .AND. emoi1.GE.0 ) GOTO 2200
        IF ( Emmi1*Emmi1.GE.Zzo*bplu1*cplu1 ) GOTO 2000
        kemoi1 = coeff*emoi1
        keplu1 = coeff*Eplu1
      ENDIF
 600  IF ( iidd.EQ.0 ) GOTO 800
      Id = 0
 700  dmoi1 = dmin + Id*pasd
      IF ( dmoi1.GT.(dmax+1E-8) ) GOTO 1900
      dplu1 = dmoi1 + pasd
      IF ( (dmoi1-1E-8)**2.GT.zco*aplu1*bplu1 ) GOTO 1900
      kdmoi1 = coeff*dmoi1
      kdplu1 = coeff*dplu1
 800  IF ( iiee.EQ.0 ) GOTO 1000
      Ie = -Ne
 900  emoi1 = emin + Ie*pase
      Eplu1 = emoi1 + pase
      IF ( emoi1.LT.0. ) THEN
        IF ( ABS(Eplu1-1E-8).GT.emax ) GOTO 1700
        IF ( ABS(Eplu1-1E-8).GT.SQRT(4.*bplu1*cplu1) ) GOTO 1700
      ELSE
        IF ( emoi1.GT.(emax+1E-8) ) GOTO 1800
        IF ( (emoi1-1E-8).GT.SQRT(zco*bplu1*cplu1) ) GOTO 1800
      ENDIF
      kemoi1 = coeff*emoi1
      keplu1 = coeff*Eplu1
 1000 IF ( iiff.NE.0 ) THEN
        mt = -1
        Dmoir1 = AMIN1(ABS(dmoi1),ABS(dplu1))
        Dplur1 = AMAX1(ABS(dmoi1),ABS(dplu1))
        DO Imf = 0, Nf
          fmoi1 = fmin + Imf*pasf
          IF ( fmoi1.GT.(fmax-1E-8) ) GOTO 1600
          Fplu1 = fmoi1 + pasf
          IF ( (fmoi1-1E-8)**2.GT.zco*aplu1*cplu1 ) GOTO 1600
          IF ( Fplu1**2.GE.4.*amoi1*cmoi1 .OR. Dplur1**2.GT.4.*amoi1*bmoi1 ) THEN
            IF ( (2.*Eplu1*aplu1-dmoi1*fmoi1+SQRT(16.*aplu1*aplu1*bplu1*cplu1-4.*amoi1*bmoi1*fmoi1**2-4.*amoi1*    &
     &           cmoi1*Dmoir1**2+(Dplur1*Fplu1)**2)).LT.0. ) GOTO 1540
            IF ( (dplu1*Fplu1-2.*emoi1*amoi1+SQRT(16.*aplu1*aplu1*bplu1*cplu1-4.*amoi1*bmoi1*fmoi1**2-4.*amoi1*    &
     &           cmoi1*Dmoir1**2+(Dplur1*Fplu1)**2)).LT.0. ) GOTO 1600
          ELSE
            Xabc = 2*aplu1*Eplu1 - dmoi1*fmoi1
            Xabc = Xabc + SQRT(16*aplu1**2*bplu1*cplu1-4*aplu1*bplu1*fmoi1**2-4*aplu1*cplu1*Dmoir1**2+             &
     &             (Dplur1*fmoi1)**2)
            IF ( (Xabc-1E-8).LT.0. ) GOTO 1540
            IF ( SQRT(bplu1/cplu1)*Fplu1.GE.dmoi1 .AND. SQRT(bplu1/cplu1)*fmoi1.LE.dplu1 ) THEN
              Alet5 = -2*emoi1*aplu1 + 4*SQRT(cplu1*bplu1)*aplu1
            ELSE
              Alet0 = 16*aplu1*aplu1*bplu1*cplu1
              Alet1 = Dplur1*fmoi1 - 2*emoi1*amoi1 + SQRT(Alet0-4*aplu1*bplu1*fmoi1**2-4*aplu1*cplu1*Dmoir1**2+    &
     &                (Dplur1*fmoi1)**2)
              Ambd1 = 0.
              IF ( 4*aplu1*cplu1.GE.Fplu1**2 ) Ambd1 = SQRT(Alet0-4*aplu1*bplu1*Fplu1**2-4*aplu1*cplu1*Dmoir1**2+  &
     &             (Dplur1*Fplu1)**2)
              Alet2 = Dplur1*Fplu1 - 2*emoi1*amoi1 + Ambd1
              Ambd2 = 0.
              IF ( 4*aplu1*bplu1.LT.Dplur1**2 ) Ambd2 = SQRT(Alet0-4*aplu1*bplu1*fmoi1**2-4*aplu1*cplu1*Dmoir1**2+ &
     &             (Dplur1*fmoi1)**2)
              Alet3 = Dplur1*fmoi1 - 2*emoi1*amoi1 + Ambd2
              Abd = Ambd1*Ambd2
              Alet4 = Dplur1*Fplu1 - 2*emoi1*amoi1
              IF ( Abd.NE.0. ) Alet4 = Alet4 + SQRT(Alet0-4*aplu1*bplu1*Fplu1**2-4*aplu1*cplu1*Dmoir1**2+          &
     &                                 (Dplur1*Fplu1)**2)
              Alet5 = AMAX1(Alet1,Alet2,Alet3,Alet4)
            ENDIF
            IF ( (Alet5-1E-8).LT.0. ) GOTO 1540
          ENDIF
          kfmoi1 = coeff*fmoi1
          kfplu1 = coeff*Fplu1
          DO I = 1, kt
            Atest = ABS(amoi1-aas(I))
            Btest = ABS(bmoi1-bbs(I))
            Ctest = ABS(cmoi1-ccs(I))
            Dtest = ABS(dmoi1-dds(I))
            Etest = ABS(emoi1-ees(I))
            Etesty = ABS(ABS(emoi1)-ABS(ees(I)))
            Ftest = ABS(fmoi1-ffs(I))
            IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND. Etest.LT.cxe .AND.    &
     &           Ftest.LT.cxf ) GOTO 1540
            IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND. Etesty.LT.cya .AND.   &
     &           Ftest.LT.cya ) GOTO 1540
          ENDDO
          CALL VOLUME(amoi1,bmoi1,cmoi1,dmoi1,emoi1,fmoi1,aplu1,bplu1,cplu1,dplu1,Eplu1,Fplu1)
          IF ( ly.EQ.-1 ) GOTO 1540
          IF ( mt.EQ.-1 ) THEN
            CALL TRIHKL(kamoi1,kbmoi1,kcmoi1,kdmoi1,kemoi1,kfmoi1,kaplu1,kbplu1,kcplu1,kdplu1,keplu1,kfplu1)
            IF ( mt.EQ.-1 ) GOTO 1540
            DO I = n6, n
              IF ( irj(I,1).EQ.0 ) THEN
                mt = -1
                GOTO 1540
              ENDIF
            ENDDO
          ELSE
            CALL TRIHKLB(kamoi1,kbmoi1,kcmoi1,kdmoi1,kemoi1,kfmoi1,kaplu1,kbplu1,kcplu1,kdplu1,keplu1,kfplu1)
            IF ( nt.EQ.-1 ) GOTO 1540
            DO I = 1, n
              Jjl1 = irj(I,1)
              Jjl = irj(I,1) + irjb(I)
              irj(I,1) = Jjl
              IF ( Jjl.EQ.0 ) GOTO 1540
              IF ( irjb(I).NE.0 ) THEN
                DO J = Jjl1 + 1, Jjl
                  Jj = J - Jjl1
                  ih(I,J,1) = ihb(I,Jj)
                  ik(I,J,1) = ikb(I,Jj)
                  il(I,J,1) = ilb(I,Jj)
                ENDDO
              ENDIF
            ENDDO
          ENDIF
          ndich(1) = ndich(1) + 1
          DO I = 1, n - 1
            J = I + 1
            IF ( irj(I,1).EQ.1 .AND. irj(J,1).EQ.1 .AND. ih(I,1,1).EQ.ih(J,1,1) .AND. ik(I,1,1).EQ.ik(J,1,1) .AND. &
     &           il(I,1,1).EQ.il(J,1,1) ) GOTO 1540
          ENDDO
          IF ( iiaa.EQ.0 ) GOTO 1040
          Ia2 = 0
 1020     amoi2 = amoi1 + Ia2*pasa2
          aplu2 = amoi2 + pasa2
          kamoi2 = coeff*amoi2
          kaplu2 = coeff*aplu2
          IF ( md34.EQ.1 ) THEN
            dmoi2 = amoi2 - s3 - errd
            dplu2 = aplu2 - s3 + errd
            Dmoim = AMIN1(ABS(dmoi2),ABS(dplu2))
            IF ( Dmoim.GT.dmax .AND. dmoi2.GE.0. ) GOTO 1540
            IF ( Dmoim.GT.dmax ) GOTO 1520
            IF ( dmoi2.LT.(-dmax) ) dmoi2 = -dmax
            IF ( dplu2.GT.dmax ) dplu2 = dmax
            Zzo = zco
            IF ( Dmoim*Dmoim.GE.zco*aplu2*bplu2 .AND. dmoi2.GE.0. ) GOTO 1540
            IF ( Dmoim*Dmoim.GE.zco*aplu2*bplu2 ) GOTO 1520
            kdmoi2 = coeff*dmoi2
            kdplu2 = coeff*dplu2
          ELSEIF ( md7.NE.0 ) THEN
            bmoi2 = s7 - aplu2 - errb
            IF ( bmoi2.GT.bmax ) GOTO 1520
            bplu2 = s7 - amoi2 + errb
            IF ( bplu2.LE.0 ) GOTO 1540
            IF ( bmoi2.LE.0 ) bmoi2 = bmin
            IF ( bmoi2.GT.aplu2 .OR. (dmoi2-1E-8)**2.GE.zco*aplu2*bplu2 ) GOTO 1520
            kbmoi2 = coeff*bmoi2
            kbplu2 = coeff*bplu2
          ENDIF
 1040     IF ( iibb.EQ.0 ) GOTO 1080
          Ib2 = 0
 1060     bmoi2 = bmoi1 + Ib2*pasb2
          bplu2 = bmoi2 + pasb2
          kbmoi2 = coeff*bmoi2
          kbplu2 = coeff*bplu2
          IF ( md12.EQ.1 ) THEN
            dmoi2 = (bmoi2-s1) - errd
            dplu2 = (bplu2-s1) + errd
            Dmoim = AMIN1(ABS(dmoi2),ABS(dplu2))
            IF ( Dmoim.GT.dmax .AND. dmoi2.GE.0. ) GOTO 1540
            IF ( Dmoim*Dmoim.GE.zco*aplu2*bplu2 .AND. dmoi2.GE.0. ) GOTO 1540
          ELSE
            IF ( md56.EQ.0 ) GOTO 1080
            dmoi2 = (s5-aplu2-bplu2) - errd
            dplu2 = (s5-amoi2-bmoi2) + errd
            Dmoim = AMIN1(ABS(dmoi2),ABS(dplu2))
            IF ( Dmoim.GT.dmax .AND. dplu2.LE.0. ) GOTO 1520
            IF ( Dmoim*Dmoim.GE.zco*aplu2*bplu2 .AND. dplu2.LE.0. ) GOTO 1520
          ENDIF
          IF ( Dmoim.GT.dmax ) GOTO 1480
          IF ( dmoi2.LT.(-dmax) ) dmoi2 = -dmax
          IF ( dplu2.GT.dmax ) dplu2 = dmax
          Zzo = zco
          IF ( Dmoim*Dmoim.GE.zco*aplu2*bplu2 ) GOTO 1480
          kdmoi2 = coeff*dmoi2
          kdplu2 = coeff*dplu2
 1080     IF ( iicc.EQ.0 ) GOTO 1120
          Ic2 = 0
 1100     cmoi2 = cmoi1 + Ic2*pasc2
          cplu2 = cmoi2 + pasc2
          kcmoi2 = coeff*cmoi2
          kcplu2 = coeff*cplu2
          IF ( md89.NE.0 ) THEN
            emoi2 = (bmoi2+cmoi2-s8) - erre
            Eplu2 = (bplu2+cplu2-s8) + erre
            Emma2 = AMIN1(ABS(emoi2),ABS(Eplu2))
            IF ( Emma2.GT.emax .AND. emoi2.GT.0 ) GOTO 1480
            IF ( Emma2.GT.emax ) GOTO 1440
            IF ( emoi2.LT.(-emax) ) emoi2 = -emax
            IF ( Eplu2.GT.emax ) Eplu2 = emax
            Zzo = zco
            IF ( emoi2.LT.0. ) Zzo = 4.
            Emmi2 = AMIN1(ABS(emoi2),ABS(Eplu2))
            IF ( Emmi2*Emmi2.GE.Zzo*bplu2*cplu2 .AND. emoi2.GT.0 ) GOTO 1480
            IF ( Emmi2*Emmi2.GE.Zzo*bplu2*cplu2 ) GOTO 1440
            kemoi2 = coeff*emoi2
            keplu2 = coeff*Eplu2
          ENDIF
 1120     IF ( iidd.EQ.0 ) GOTO 1160
          Id2 = 0
 1140     dmoi2 = dmoi1 + Id2*pasd2
          dplu2 = dmoi2 + pasd2
          IF ( (dmoi2-1E-8)**2.GT.zco*aplu2*bplu2 ) GOTO 1420
          kdmoi2 = coeff*dmoi2
          kdplu2 = coeff*dplu2
 1160     IF ( iiee.EQ.0 ) GOTO 1200
          Ie2 = 0
 1180     emoi2 = emoi1 + Ie2*pase2
          Eplu2 = emoi2 + pase2
          IF ( emoi2.LT.0. ) THEN
            IF ( ABS(Eplu2-1E-8).GT.SQRT(4.*bplu2*cplu2) ) GOTO 1380
          ELSEIF ( (emoi2-1E-8).GT.SQRT(zco*bplu2*cplu2) ) THEN
            GOTO 1400
          ENDIF
          kemoi2 = coeff*emoi2
          keplu2 = coeff*Eplu2
 1200     IF ( iiff.NE.0 ) THEN
            DO Imf2 = 0, 1
              fmoi2 = fmoi1 + Imf2*pasf2
              DO I = 1, kt
                Atest = ABS(amoi2-aas(I))
                Btest = ABS(bmoi2-bbs(I))
                Ctest = ABS(cmoi2-ccs(I))
                Dtest = ABS(dmoi2-dds(I))
                Etest = ABS(emoi2-ees(I))
                Etesty = ABS(ABS(emoi2)-ABS(ees(I)))
                Ftest = ABS(fmoi2-ffs(I))
                IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND. Etest.LT.cxe .AND.&
     &               Ftest.LT.cxf ) GOTO 1340
                IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND.                   &
     &               Etesty.LT.cya .AND. Ftest.LT.cya ) GOTO 1340
              ENDDO
              Fplu2 = fmoi2 + pasf2
              IF ( (fmoi2-1E-8)**2.GT.zco*aplu2*cplu2 ) GOTO 1360
              kfmoi2 = coeff*fmoi2
              kfplu2 = coeff*Fplu2
              CALL VOLUME(amoi2,bmoi2,cmoi2,dmoi2,emoi2,fmoi2,aplu2,bplu2,cplu2,dplu2,Eplu2,Fplu2)
              IF ( ly.EQ.-1 ) GOTO 1340
              CALL TRIHKL1(kamoi2,kbmoi2,kcmoi2,kdmoi2,kemoi2,kfmoi2,kaplu2,kbplu2,kcplu2,kdplu2,keplu2,kfplu2,2)
              IF ( nt.EQ.-1 ) GOTO 1340
              ndich(2) = ndich(2) + 1
              DO I = 1, n - 1
                J = I + 1
                IF ( irj(I,2).EQ.1 .AND. irj(J,2).EQ.1 .AND. ih(I,1,2).EQ.ih(J,1,2) .AND. ik(I,1,2).EQ.ik(J,1,2)   &
     &               .AND. il(I,1,2).EQ.il(J,1,2) ) GOTO 1340
              ENDDO
              IF ( iiaa.EQ.0 ) GOTO 1210
              Ia3 = 0
 1205         amoi3 = amoi2 + Ia3*pasa4
              aplu3 = amoi3 + pasa4
              kamoi3 = coeff*amoi3
              kaplu3 = coeff*aplu3
              IF ( md34.EQ.1 ) THEN
                dmoi3 = amoi3 - s3 - errd
                dplu3 = aplu3 - s3 + errd
                Dmoim = AMIN1(ABS(dmoi3),ABS(dplu3))
                IF ( Dmoim.GT.dmax .AND. dmoi3.GE.0. ) GOTO 1340
                IF ( Dmoim.GT.dmax ) GOTO 1335
                IF ( dmoi3.LT.(-dmax) ) dmoi3 = -dmax
                IF ( dplu3.GT.dmax ) dplu3 = dmax
                Zzo = zco
                IF ( Dmoim*Dmoim.GE.zco*aplu3*bplu3 .AND. dmoi3.GE.0. ) GOTO 1340
                IF ( Dmoim*Dmoim.GE.zco*aplu3*bplu3 ) GOTO 1335
                kdmoi3 = coeff*dmoi3
                kdplu3 = coeff*dplu3
              ELSEIF ( md7.NE.0 ) THEN
                bmoi3 = s7 - aplu3 - errb
                IF ( bmoi3.GT.bmax ) GOTO 1335
                bplu3 = s7 - amoi3 + errb
                IF ( bplu3.LE.0 ) GOTO 1340
                IF ( bmoi3.LE.0 ) bmoi3 = bmin
                IF ( bmoi3.GT.aplu3 .OR. (dmoi3-1E-8)**2.GE.zco*aplu3*bplu3 ) GOTO 1335
                kbmoi3 = coeff*bmoi3
                kbplu3 = coeff*bplu3
              ENDIF
 1210         IF ( iibb.EQ.0 ) GOTO 1220
              Ib3 = 0
 1215         bmoi3 = bmoi2 + Ib3*pasb4
              bplu3 = bmoi3 + pasb4
              kbmoi3 = coeff*bmoi3
              kbplu3 = coeff*bplu3
              IF ( md12.EQ.1 ) THEN
                dmoi3 = (bmoi3-s1) - errd
                dplu3 = (bplu3-s1) + errd
                Dmoim = AMIN1(ABS(dmoi3),ABS(dplu3))
                IF ( Dmoim.GT.dmax .AND. dmoi3.GE.0. ) GOTO 1340
                IF ( Dmoim*Dmoim.GE.zco*aplu3*bplu3 .AND. dmoi3.GE.0. ) GOTO 1340
              ELSE
                IF ( md56.EQ.0 ) GOTO 1220
                dmoi3 = (s5-aplu3-bplu3) - errd
                dplu3 = (s5-amoi3-bmoi3) + errd
                Dmoim = AMIN1(ABS(dmoi3),ABS(dplu3))
                IF ( Dmoim.GT.dmax .AND. dplu3.LE.0. ) GOTO 1335
                IF ( Dmoim*Dmoim.GE.zco*aplu3*bplu3 .AND. dplu3.LE.0 ) GOTO 1335
              ENDIF
              IF ( Dmoim.GT.dmax ) GOTO 1325
              IF ( dmoi3.LT.(-dmax) ) dmoi3 = -dmax
              IF ( dplu3.GT.dmax ) dplu3 = dmax
              Zzo = zco
              IF ( Dmoim*Dmoim.GE.zco*aplu3*bplu3 ) GOTO 1325
              kdmoi3 = coeff*dmoi3
              kdplu3 = coeff*dplu3
 1220         IF ( iicc.EQ.0 ) GOTO 1230
              Ic3 = 0
 1225         cmoi3 = cmoi2 + Ic3*pasc4
              cplu3 = cmoi3 + pasc4
              kcmoi3 = coeff*cmoi3
              kcplu3 = coeff*cplu3
              IF ( md89.NE.0 ) THEN
                emoi3 = (bmoi3+cmoi3-s8) - erre
                Eplu3 = (bplu3+cplu3-s8) + erre
                Emma3 = AMIN1(ABS(emoi3),ABS(Eplu3))
                IF ( Emma3.GT.emax .AND. emoi3.GT.0 ) GOTO 1325
                IF ( Emma3.GT.emax ) GOTO 1315
                IF ( emoi3.LT.(-emax) ) emoi3 = -emax
                IF ( Eplu3.GT.emax ) Eplu3 = emax
                Zzo = zco
                IF ( emoi3.LT.0. ) Zzo = 4.
                Emmi3 = AMIN1(ABS(emoi3),ABS(Eplu3))
                IF ( Emmi3*Emmi3.GE.Zzo*bplu3*cplu3 .AND. emoi3.GT.0 ) GOTO 1325
                IF ( Emmi3*Emmi3.GE.Zzo*bplu3*cplu3 ) GOTO 1315
                kemoi3 = coeff*emoi3
                keplu3 = coeff*Eplu3
              ENDIF
 1230         IF ( iidd.EQ.0 ) GOTO 1240
              Id3 = 0
 1235         dmoi3 = dmoi2 + Id3*pasd4
              dplu3 = dmoi3 + pasd4
              IF ( (dmoi3-1E-8)**2.GT.zco*aplu3*bplu3 ) GOTO 1310
              kdmoi3 = coeff*dmoi3
              kdplu3 = coeff*dplu3
 1240         IF ( iiee.EQ.0 ) GOTO 1250
              Ie3 = 0
 1245         emoi3 = emoi2 + Ie3*pase4
              Eplu3 = emoi3 + pase4
              IF ( emoi3.LT.0. ) THEN
                IF ( ABS(Eplu3-1E-8).GT.SQRT(4.*bplu3*cplu3) ) GOTO 1300
              ELSEIF ( (emoi3-1E-8).GT.SQRT(zco*bplu3*cplu3) ) THEN
                GOTO 1305
              ENDIF
              kemoi3 = coeff*emoi3
              keplu3 = coeff*Eplu3
 1250         IF ( iiff.NE.0 ) THEN
                DO Imf3 = 0, 1
                  fmoi3 = fmoi2 + Imf3*pasf4
                  DO I = 1, kt
                    Atest = ABS(amoi3-aas(I))
                    Btest = ABS(bmoi3-bbs(I))
                    Ctest = ABS(cmoi3-ccs(I))
                    Dtest = ABS(dmoi3-dds(I))
                    Etest = ABS(emoi3-ees(I))
                    Etesty = ABS(ABS(emoi3)-ABS(ees(I)))
                    Ftest = ABS(fmoi3-ffs(I))
                    IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND.               &
     &                   Etest.LT.cxe .AND. Ftest.LT.cxf ) GOTO 1292
                    IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND.               &
     &                   Etesty.LT.cya .AND. Ftest.LT.cya ) GOTO 1292
                  ENDDO
                  Fplu3 = fmoi3 + pasf4
                  IF ( (fmoi3-1E-8)**2.GT.zco*aplu3*cplu3 ) GOTO 1295
                  kfmoi3 = coeff*fmoi3
                  kfplu3 = coeff*Fplu3
                  CALL VOLUME(amoi3,bmoi3,cmoi3,dmoi3,emoi3,fmoi3,aplu3,bplu3,cplu3,dplu3,Eplu3,Fplu3)
                  IF ( ly.EQ.-1 ) GOTO 1292
                  CALL TRIHKL1(kamoi3,kbmoi3,kcmoi3,kdmoi3,kemoi3,kfmoi3,kaplu3,kbplu3,kcplu3,kdplu3,keplu3,kfplu3,&
     &                         3)
                  IF ( nt.EQ.-1 ) GOTO 1292
                  ndich(3) = ndich(3) + 1
                  DO I = 1, n - 1
                    J = I + 1
                    IF ( irj(I,3).EQ.1 .AND. irj(J,3).EQ.1 .AND. ih(I,1,3).EQ.ih(J,1,3) .AND. ik(I,1,3)            &
     &                   .EQ.ik(J,1,3) .AND. il(I,1,3).EQ.il(J,1,3) ) GOTO 1292
                  ENDDO
                  IF ( iiaa.EQ.0 ) GOTO 1254
                  Ia4 = 0
 1252             amoi4 = amoi3 + Ia4*pasa8
                  aplu4 = amoi4 + pasa8
                  kamoi4 = coeff*amoi4
                  kaplu4 = coeff*aplu4
                  IF ( md34.EQ.1 ) THEN
                    dmoi4 = amoi4 - s3 - errd
                    dplu4 = aplu4 - s3 + errd
                    Dmoim = AMIN1(ABS(dmoi4),ABS(dplu4))
                    IF ( Dmoim.GT.dmax .AND. dmoi4.GE.0. ) GOTO 1292
                    IF ( Dmoim.GT.dmax ) GOTO 1290
                    IF ( dmoi4.LT.(-dmax) ) dmoi4 = -dmax
                    IF ( dplu4.GT.dmax ) dplu4 = dmax
                    Zzo = zco
                    IF ( Dmoim*Dmoim.GE.zco*aplu4*bplu4 .AND. dmoi4.GE.0. ) GOTO 1292
                    IF ( Dmoim*Dmoim.GE.zco*aplu4*bplu4 ) GOTO 1290
                    kdmoi4 = coeff*dmoi4
                    kdplu4 = coeff*dplu4
                  ELSEIF ( md7.NE.0 ) THEN
                    bmoi4 = s7 - aplu4 - errb
                    IF ( bmoi4.GT.bmax ) GOTO 1290
                    bplu4 = s7 - amoi4 + errb
                    IF ( bplu4.LE.0 ) GOTO 1292
                    IF ( bmoi4.LE.0 ) bmoi4 = bmin
                    IF ( bmoi4.GT.aplu4 .OR. (dmoi4-1E-8)**2.GE.zco*aplu4*bplu4 ) GOTO 1290
                    kbmoi4 = coeff*bmoi4
                    kbplu4 = coeff*bplu4
                  ENDIF
 1254             IF ( iibb.EQ.0 ) GOTO 1258
                  Ib4 = 0
 1256             bmoi4 = bmoi3 + Ib4*pasb8
                  bplu4 = bmoi4 + pasb8
                  kbmoi4 = coeff*bmoi4
                  kbplu4 = coeff*bplu4
                  IF ( md12.EQ.1 ) THEN
                    dmoi4 = (bmoi4-s1) - errd
                    dplu4 = (bplu4-s1) + errd
                    Dmoim = AMIN1(ABS(dmoi4),ABS(dplu4))
                    IF ( Dmoim.GT.dmax .AND. dmoi4.GE.0. ) GOTO 1292
                    IF ( Dmoim*Dmoim.GE.zco*aplu4*bplu4 .AND. dmoi4.GE.0. ) GOTO 1292
                  ELSE
                    IF ( md56.EQ.0 ) GOTO 1258
                    dmoi4 = (s5-aplu4-bplu4) - errd
                    dplu4 = (s5-amoi4-bmoi4) + errd
                    Dmoim = AMIN1(ABS(dmoi4),ABS(dplu4))
                    IF ( Dmoim.GT.dmax .AND. dplu4.LE.0. ) GOTO 1290
                    IF ( Dmoim*Dmoim.GE.zco*aplu4*bplu4 .AND. dplu4.LE.0. ) GOTO 1290
                  ENDIF
                  IF ( Dmoim.GT.dmax ) GOTO 1286
                  IF ( dmoi4.LT.(-dmax) ) dmoi4 = -dmax
                  IF ( dplu4.GT.dmax ) dplu4 = dmax
                  Zzo = zco
                  IF ( Dmoim*Dmoim.GE.zco*aplu4*bplu4 ) GOTO 1286
                  kdmoi4 = coeff*dmoi4
                  kdplu4 = coeff*dplu4
 1258             IF ( iicc.EQ.0 ) GOTO 1262
                  Ic4 = 0
 1260             cmoi4 = cmoi3 + Ic4*pasc8
                  cplu4 = cmoi4 + pasc8
                  kcmoi4 = coeff*cmoi4
                  kcplu4 = coeff*cplu4
                  IF ( md89.NE.0 ) THEN
                    emoi4 = (bmoi4+cmoi4-s8) - erre
                    eplu4 = (bplu4+cplu4-s8) + erre
                    Emma4 = AMIN1(ABS(emoi4),ABS(eplu4))
                    IF ( Emma4.GT.emax .AND. emoi4.GT.0 ) GOTO 1286
                    IF ( Emma4.GT.emax ) GOTO 1282
                    IF ( emoi4.LT.(-emax) ) emoi4 = -emax
                    IF ( eplu4.GT.emax ) eplu4 = emax
                    Zzo = zco
                    IF ( emoi4.LT.0. ) Zzo = 4.
                    Emmi4 = AMIN1(ABS(emoi4),ABS(eplu4))
                    IF ( Emmi4*Emmi4.GE.Zzo*bplu4*cplu4 .AND. emoi4.GT.0 ) GOTO 1286
                    IF ( Emmi4*Emmi4.GE.Zzo*bplu4*cplu4 ) GOTO 1282
                    kemoi4 = coeff*emoi4
                    keplu4 = coeff*eplu4
                  ENDIF
 1262             IF ( iidd.EQ.0 ) GOTO 1266
                  Id4 = 0
 1264             dmoi4 = dmoi3 + Id4*pasd8
                  dplu4 = dmoi4 + pasd8
                  IF ( (dmoi4-1E-8)**2.GT.zco*aplu4*bplu4 ) GOTO 1280
                  kdmoi4 = coeff*dmoi4
                  kdplu4 = coeff*dplu4
 1266             IF ( iiee.EQ.0 ) GOTO 1270
                  Ie4 = 0
 1268             emoi4 = emoi3 + Ie4*pase8
                  eplu4 = emoi4 + pase8
                  IF ( emoi4.LT.0. ) THEN
                    IF ( ABS(eplu4-1E-8).GT.SQRT(4.*bplu4*cplu4) ) GOTO 1276
                  ELSEIF ( (emoi4-1E-8).GT.SQRT(zco*bplu4*cplu4) ) THEN
                    GOTO 1278
                  ENDIF
                  kemoi4 = coeff*emoi4
                  keplu4 = coeff*eplu4
 1270             IF ( iiff.NE.0 ) THEN
                    DO Imf4 = 0, 1
                      fmoi4 = fmoi3 + Imf4*pasf8
                      DO I = 1, kt
                        Atest = ABS(amoi4-aas(I))
                        Btest = ABS(bmoi4-bbs(I))
                        Ctest = ABS(cmoi4-ccs(I))
                        Dtest = ABS(dmoi4-dds(I))
                        Etest = ABS(emoi4-ees(I))
                        Etesty = ABS(ABS(emoi4)-ABS(ees(I)))
                        Ftest = ABS(fmoi4-ffs(I))
                        IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND.           &
     &                       Etest.LT.cxe .AND. Ftest.LT.cxf ) GOTO 1272
                        IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND.           &
     &                       Etesty.LT.cya .AND. Ftest.LT.cya ) GOTO 1272
                      ENDDO
                      fplu4 = fmoi4 + pasf8
                      IF ( (fmoi4-1E-8)**2.GT.zco*aplu4*cplu4 ) GOTO 1274
                      kfmoi4 = coeff*fmoi4
                      kfplu4 = coeff*fplu4
                      CALL VOLUME(amoi4,bmoi4,cmoi4,dmoi4,emoi4,fmoi4,aplu4,bplu4,cplu4,dplu4,eplu4,fplu4)
                      IF ( ly.NE.-1 ) THEN
                        CALL TRIHKL1(kamoi4,kbmoi4,kcmoi4,kdmoi4,kemoi4,kfmoi4,kaplu4,kbplu4,kcplu4,kdplu4,keplu4, &
     &                               kfplu4,4)
                        IF ( nt.NE.-1 ) THEN
                          ndich(4) = ndich(4) + 1
                          DO I = 1, n - 1
                            J = I + 1
                            IF ( irj(I,4).EQ.1 .AND. irj(J,4).EQ.1 .AND. ih(I,1,4).EQ.ih(J,1,4) .AND. ik(I,1,4)    &
     &                           .EQ.ik(J,1,4) .AND. il(I,1,4).EQ.il(J,1,4) ) GOTO 1272
                          ENDDO
                          CALL TRICLINI3
                          IF (DICVOL_Error .NE. 0) RETURN
                        ENDIF
                      ENDIF
 1272                 IF ( iiff.EQ.0 ) GOTO 1274
                    ENDDO
                  ENDIF
 1274             IF ( iiee.EQ.0 ) GOTO 1278
 1276             Ie4 = Ie4 + 1
                  IF ( Ie4.LE.1 ) GOTO 1268
 1278             IF ( iidd.NE.0 ) THEN
                    Id4 = Id4 + 1
                    IF ( Id4.LE.1 ) GOTO 1264
                  ENDIF
 1280             IF ( iicc.EQ.0 ) GOTO 1284
 1282             Ic4 = Ic4 + 1
                  IF ( Ic4.LE.1 ) GOTO 1260
 1284             IF ( iibb.EQ.0 ) GOTO 1288
 1286             Ib4 = Ib4 + 1
                  IF ( Ib4.LE.1 ) GOTO 1256
 1288             IF ( iiaa.EQ.0 ) GOTO 1292
 1290             Ia4 = Ia4 + 1
                  IF ( Ia4.LE.1 ) GOTO 1252
 1292             IF ( iiff.EQ.0 ) GOTO 1295
                ENDDO
              ENDIF
 1295         IF ( iiee.EQ.0 ) GOTO 1305
 1300         Ie3 = Ie3 + 1
              IF ( Ie3.LE.1 ) GOTO 1245
 1305         IF ( iidd.NE.0 ) THEN
                Id3 = Id3 + 1
                IF ( Id3.LE.1 ) GOTO 1235
              ENDIF
 1310         IF ( iicc.EQ.0 ) GOTO 1320
 1315         Ic3 = Ic3 + 1
              IF ( Ic3.LE.1 ) GOTO 1225
 1320         IF ( iibb.EQ.0 ) GOTO 1330
 1325         Ib3 = Ib3 + 1
              IF ( Ib3.LE.1 ) GOTO 1215
 1330         IF ( iiaa.EQ.0 ) GOTO 1340
 1335         Ia3 = Ia3 + 1
              IF ( Ia3.LE.1 ) GOTO 1205
 1340         IF ( iiff.EQ.0 ) GOTO 1360
            ENDDO
          ENDIF
 1360     IF ( iiee.EQ.0 ) GOTO 1400
 1380     Ie2 = Ie2 + 1
          IF ( Ie2.LE.1 ) GOTO 1180
 1400     IF ( iidd.NE.0 ) THEN
            Id2 = Id2 + 1
            IF ( Id2.LE.1 ) GOTO 1140
          ENDIF
 1420     IF ( iicc.EQ.0 ) GOTO 1460
 1440     Ic2 = Ic2 + 1
          IF ( Ic2.LE.1 ) GOTO 1100
 1460     IF ( iibb.EQ.0 ) GOTO 1500
 1480     Ib2 = Ib2 + 1
          IF ( Ib2.LE.1 ) GOTO 1060
 1500     IF ( iiaa.EQ.0 ) GOTO 1540
 1520     Ia2 = Ia2 + 1
          IF ( Ia2.LE.1 ) GOTO 1020
 1540     IF ( iiff.EQ.0 ) GOTO 1600
        ENDDO
      ENDIF
 1600 IF ( iiee.EQ.0 ) GOTO 1800
 1700 Ie = Ie + 1
      IF ( Ie.LE.Ne ) GOTO 900
 1800 IF ( iidd.NE.0 ) THEN
        Id = Id + 1
        IF ( Id.LE.Nd ) GOTO 700
      ENDIF
 1900 IF ( iicc.EQ.0 ) GOTO 2100
 2000 Ic = Ic + 1
      IF ( Ic.LE.Nc ) GOTO 500
 2100 IF ( iibb.EQ.0 ) GOTO 2300
 2200 Ib = Ib + 1
      IF ( Ib.LE.Nb ) GOTO 300
 2300 IF ( iiaa.EQ.0 ) GOTO 99999
 2400 Ia = Ia + 1
      IF ( Ia.LE.Na ) GOTO 100
99999 END SUBROUTINE TRICLINI2
