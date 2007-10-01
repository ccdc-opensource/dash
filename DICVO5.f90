!*==TRICLINI3.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!     ----------------------------------------------------
!     | 	     T R I C L I N I C 3		 |
!     ----------------------------------------------------
      SUBROUTINE TRICLINI3
      USE DICVAR
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Local variables
!
      REAL, INTRINSIC :: AMIN1
      REAL :: Amoi8, Aplu8, Artest, Atest, Bmoi8, Bplu8, Brtest, Btest, Cmoi8, Cplu8, Crtest, Ctest,   &
     &        Dd, Dmoi8, Dmoim, Dplu8, Drstest, Drtest, Dtest, Ee, Emma5, Emma6, Emma7, Emmi5, Emmi6, &
     &        Emmi7, Emoi8, Eplu5, Eplu6, Eplu7, Eplu8, Erstest, Ertest, Etest, Etesty, Ff, Fmoi8,     &
     &        Fplu5, Fplu6, Fplu7, Fplu8, Frstest, Frtest, Ftest, Sinal, Sinbe, Singa, Sv, Vap, Vet,  &
     &        Zzo
      INTEGER :: I, Ia5, Ia6, Ia7, Iap7, Ib5, Ib6, Ib7, Ibp7, Ic5, Ic6, Ic7, Icp7, Id5, Id6, Id7,  &
     &           Idp7, Ie5, Ie6, Ie7, Iep7, Ifp7, Imf5, Imf6, Imf7, Ind, J, Kamoi8, Kaplu8,      &
     &           Kbmoi8, Kbplu8, Kcmoi8, Kcplu8, Kdmoi8, Kdplu8, Kemoi8, Keplu8, Kfmoi8, Kfplu8, Nap,   &
     &           Nbp, Ncp, Ndp, Nep, Nfp, Nrind
      INTEGER, INTRINSIC :: NINT

      IF ( iiaa.EQ.0 ) GOTO 200
      Ia5 = 0
 100  amoi5 = amoi4 + Ia5*pasa16
      aplu5 = amoi5 + pasa16
      kamoi5 = coeff*amoi5
      kaplu5 = coeff*aplu5
      IF ( md34.EQ.1 ) THEN
        dmoi5 = amoi5 - s3 - errd
        dplu5 = aplu5 - s3 + errd
        Dmoim = AMIN1(ABS(dmoi5),ABS(dplu5))
        IF ( Dmoim.GT.dmax .AND. dmoi5.GE.0. ) GOTO 99999
        IF ( Dmoim.GT.dmax ) GOTO 2400
        IF ( dmoi5.LT.(-dmax) ) dmoi5 = -dmax
        IF ( dplu5.GT.dmax ) dplu5 = dmax
        Zzo = zco
        IF ( Dmoim*Dmoim.GE.zco*aplu5*bplu5 .AND. dmoi5.GE.0. ) GOTO 99999
        IF ( Dmoim*Dmoim.GE.zco*aplu5*bplu5 ) GOTO 2400
        kdmoi5 = coeff*dmoi5
        kdplu5 = coeff*dplu5
      ELSEIF ( md7.NE.0 ) THEN
        bmoi5 = s7 - aplu5 - errb
        IF ( bmoi5.GT.bmax ) GOTO 2400
        bplu5 = s7 - amoi5 + errb
        IF ( bplu5.LE.0 ) GOTO 99999
        IF ( bmoi5.LE.0 ) bmoi5 = bmin
        IF ( bmoi5.GT.aplu5 .OR. (dmoi5-1E-8)**2.GE.zco*aplu5*bplu5 ) GOTO 2400
        kbmoi5 = coeff*bmoi5
        kbplu5 = coeff*bplu5
      ENDIF
 200  IF ( iibb.EQ.0 ) GOTO 400
      Ib5 = 0
 300  bmoi5 = bmoi4 + Ib5*pasb16
      bplu5 = bmoi5 + pasb16
      kbmoi5 = coeff*bmoi5
      kbplu5 = coeff*bplu5
      IF ( md12.EQ.1 ) THEN
        dmoi5 = (bmoi5-s1) - errd
        dplu5 = (bplu5-s1) + errd
        Dmoim = AMIN1(ABS(dmoi5),ABS(dplu5))
        IF ( Dmoim.GT.dmax .AND. dmoi5.GE.0. ) GOTO 99999
        IF ( Dmoim*Dmoim.GE.zco*aplu5*bplu5 .AND. dmoi5.GE.0. ) GOTO 99999
      ELSE
        IF ( md56.EQ.0 ) GOTO 400
        dmoi5 = (s5-aplu5-bplu5) - errd
        dplu5 = (s5-amoi5-bmoi5) + errd
        Dmoim = AMIN1(ABS(dmoi5),ABS(dplu5))
        IF ( Dmoim.GT.dmax .AND. dplu5.LE.0. ) GOTO 2400
        IF ( Dmoim*Dmoim.GE.zco*aplu5*bplu5 .AND. dplu5.LE.0. ) GOTO 2400
      ENDIF
      IF ( Dmoim.GT.dmax ) GOTO 2200
      IF ( dmoi5.LT.(-dmax) ) dmoi5 = -dmax
      IF ( dplu5.GT.dmax ) dplu5 = dmax
      zco = zco
      IF ( Dmoim*Dmoim.GE.zco*aplu5*bplu5 ) GOTO 2200
      kdmoi5 = coeff*dmoi5
      kdplu5 = coeff*dplu5
 400  IF ( iicc.EQ.0 ) GOTO 600
      Ic5 = 0
 500  cmoi5 = cmoi4 + Ic5*pasc16
      cplu5 = cmoi5 + pasc16
      kcmoi5 = coeff*cmoi5
      kcplu5 = coeff*cplu5
      IF ( md89.NE.0 ) THEN
        emoi5 = (bmoi5+cmoi5-s8) - erre
        Eplu5 = (bplu5+cplu5-s8) + erre
        Emma5 = AMIN1(ABS(emoi5),ABS(Eplu5))
        IF ( Emma5.GT.emax .AND. emoi5.GT.0 ) GOTO 2200
        IF ( Emma5.GT.emax ) GOTO 2000
        IF ( emoi5.LT.(-emax) ) emoi5 = -emax
        IF ( Eplu5.GT.emax ) Eplu5 = emax
        Zzo = zco
        IF ( emoi5.LT.0. ) Zzo = 4.
        Emmi5 = AMIN1(ABS(emoi5),ABS(Eplu5))
        IF ( Emmi5*Emmi5.GE.Zzo*bplu5*cplu5 .AND. emoi5.GT.0 ) GOTO 2200
        IF ( Emmi5*Emmi5.GE.Zzo*bplu5*cplu5 ) GOTO 2000
        kemoi5 = coeff*emoi5
        keplu5 = coeff*Eplu5
      ENDIF
 600  IF ( iidd.EQ.0 ) GOTO 800
      Id5 = 0
 700  dmoi5 = dmoi4 + Id5*pasd16
      dplu5 = dmoi5 + pasd16
      IF ( (dmoi5-1E-8)**2.GT.zco*aplu5*bplu5 ) GOTO 1900
      kdmoi5 = coeff*dmoi5
      kdplu5 = coeff*dplu5
 800  IF ( iiee.EQ.0 ) GOTO 1000
      Ie5 = 0
 900  emoi5 = emoi4 + Ie5*pase16
      Eplu5 = emoi5 + pase16
      IF ( emoi5.LT.0. ) THEN
        IF ( ABS(Eplu5-1E-8).GT.SQRT(4.*bplu5*cplu5) ) GOTO 1700
      ELSEIF ( (emoi5-1E-8).GT.SQRT(zco*bplu5*cplu5) ) THEN
        GOTO 1800
      ENDIF
      kemoi5 = coeff*emoi5
      keplu5 = coeff*Eplu5
 1000 IF ( iiff.NE.0 ) THEN
        DO Imf5 = 0, 1
          fmoi5 = fmoi4 + Imf5*pasf16
          DO I = 1, kt
            Atest = ABS(amoi5-aas(I))
            Btest = ABS(bmoi5-bbs(I))
            Ctest = ABS(cmoi5-ccs(I))
            Dtest = ABS(dmoi5-dds(I))
            Etest = ABS(emoi5-ees(I))
            Etesty = ABS(ABS(emoi5)-ABS(ees(I)))
            Ftest = ABS(fmoi5-ffs(I))
            IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND. Etest.LT.cxe .AND.    &
     &           Ftest.LT.cxf ) GOTO 1520
            IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND. Etesty.LT.cya .AND.   &
     &           Ftest.LT.cya ) GOTO 1520
          ENDDO
          Fplu5 = fmoi5 + pasf16
          IF ( (fmoi5-1E-8)**2.GT.zco*aplu5*cplu5 ) GOTO 1600
          kfmoi5 = coeff*fmoi5
          kfplu5 = coeff*Fplu5
          CALL VOLUME(amoi5,bmoi5,cmoi5,dmoi5,emoi5,fmoi5,aplu5,bplu5,cplu5,dplu5,Eplu5,Fplu5)
          IF ( ly.EQ.-1 ) GOTO 1520
          CALL TRIHKL1(kamoi5,kbmoi5,kcmoi5,kdmoi5,kemoi5,kfmoi5,kaplu5,kbplu5,kcplu5,kdplu5,keplu5,kfplu5,5)
          IF ( nt.EQ.-1 ) GOTO 1520
          ndich(5) = ndich(5) + 1
          DO I = 1, n - 1
            J = I + 1
            IF ( irj(I,5).EQ.1 .AND. irj(J,5).EQ.1 .AND. ih(I,1,5).EQ.ih(J,1,5) .AND. ik(I,1,5).EQ.ik(J,1,5) .AND. &
     &           il(I,1,5).EQ.il(J,1,5) ) GOTO 1520
          ENDDO
          IF ( iiaa.EQ.0 ) GOTO 1040
          Ia6 = 0
 1020     amoi6 = amoi5 + Ia6*pasa32
          aplu6 = amoi6 + pasa32
          kamoi6 = coeff*amoi6
          kaplu6 = coeff*aplu6
          IF ( md34.EQ.1 ) THEN
            dmoi6 = amoi6 - s3 - errd
            dplu6 = aplu6 - s3 + errd
            Dmoim = AMIN1(ABS(dmoi6),ABS(dplu6))
            IF ( Dmoim.GT.dmax .AND. dmoi6.GE.0. ) GOTO 1520
            IF ( Dmoim.GT.dmax ) GOTO 1500
            IF ( dmoi6.LT.(-dmax) ) dmoi6 = -dmax
            IF ( dplu6.GT.dmax ) dplu6 = dmax
            Zzo = zco
            IF ( Dmoim*Dmoim.GE.zco*aplu6*bplu6 .AND. dmoi6.GE.0. ) GOTO 1520
            IF ( Dmoim*Dmoim.GE.zco*aplu6*bplu6 ) GOTO 1500
            kdmoi6 = coeff*dmoi6
            kdplu6 = coeff*dplu6
          ELSEIF ( md7.NE.0 ) THEN
            bmoi6 = s7 - aplu6 - errb
            IF ( bmoi6.GT.bmax ) GOTO 1500
            bplu6 = s7 - amoi6 + errb
            IF ( bplu6.LE.0 ) GOTO 1520
            IF ( bmoi6.LE.0 ) bmoi6 = bmin
            IF ( bmoi6.GT.aplu6 .OR. (dmoi6-1E-8)**2.GE.zco*aplu6*bplu6 ) GOTO 1500
            kbmoi6 = coeff*bmoi6
            kbplu6 = coeff*bplu6
          ENDIF
 1040     IF ( iibb.EQ.0 ) GOTO 1080
          Ib6 = 0
 1060     bmoi6 = bmoi5 + Ib6*pasb32
          bplu6 = bmoi6 + pasb32
          kbmoi6 = coeff*bmoi6
          kbplu6 = coeff*bplu6
          IF ( md12.EQ.1 ) THEN
            dmoi6 = (bmoi6-s1) - errd
            dplu6 = (bplu6-s1) + errd
            Dmoim = AMIN1(ABS(dmoi6),ABS(dplu6))
            IF ( Dmoim.GT.dmax .AND. dmoi6.GE.0. ) GOTO 1520
            IF ( Dmoim*Dmoim.GE.zco*aplu6*bplu6 .AND. dmoi6.GE.0. ) GOTO 1520
          ELSE
            IF ( md56.EQ.0 ) GOTO 1080
            dmoi6 = (s5-aplu6-bplu6) - errd
            dplu6 = (s5-amoi6-bmoi6) + errd
            Dmoim = AMIN1(ABS(dmoi6),ABS(dplu6))
            IF ( Dmoim.GT.dmax .AND. dplu6.LE.0. ) GOTO 1500
            IF ( Dmoim*Dmoim.GE.zco*aplu6*bplu6 .AND. dplu6.LE.0. ) GOTO 1500
          ENDIF
          IF ( Dmoim.GT.dmax ) GOTO 1460
          IF ( dmoi6.LT.(-dmax) ) dmoi6 = -dmax
          IF ( dplu6.GT.dmax ) dplu6 = dmax
          Zzo = zco
          IF ( Dmoim*Dmoim.GE.zco*aplu6*bplu6 ) GOTO 1460
          kdmoi6 = coeff*dmoi6
          kdplu6 = coeff*dplu6
 1080     IF ( iicc.EQ.0 ) GOTO 1120
          Ic6 = 0
 1100     cmoi6 = cmoi5 + Ic6*pasc32
          cplu6 = cmoi6 + pasc32
          kcmoi6 = coeff*cmoi6
          kcplu6 = coeff*cplu6
          IF ( md89.NE.0 ) THEN
            emoi6 = (bmoi6+cmoi6-s8) - erre
            Eplu6 = (bplu6+cplu6-s8) + erre
            Emma6 = AMIN1(ABS(emoi6),ABS(Eplu6))
            IF ( Emma6.GT.emax .AND. emoi6.GT.0 ) GOTO 1460
            IF ( Emma6.GT.emax ) GOTO 1420
            IF ( emoi6.LT.(-emax) ) emoi6 = -emax
            IF ( Eplu6.GT.emax ) Eplu6 = emax
            Zzo = zco
            IF ( emoi6.LT.0. ) Zzo = 4.
            Emmi6 = AMIN1(ABS(emoi6),ABS(Eplu6))
            IF ( Emmi6*Emmi6.GE.Zzo*bplu6*cplu6 .AND. emoi6.GT.0 ) GOTO 1460
            IF ( Emmi6*Emmi6.GE.Zzo*bplu6*cplu6 ) GOTO 1420
            kemoi6 = coeff*emoi6
            keplu6 = coeff*Eplu6
          ENDIF
 1120     IF ( iidd.EQ.0 ) GOTO 1160
          Id6 = 0
 1140     dmoi6 = dmoi5 + Id6*pasd32
          dplu6 = dmoi6 + pasd32
          IF ( (dmoi6-1E-8)**2.GT.zco*aplu6*bplu6 ) GOTO 1400
          kdmoi6 = coeff*dmoi6
          kdplu6 = coeff*dplu6
 1160     IF ( iiee.EQ.0 ) GOTO 1200
          Ie6 = 0
 1180     emoi6 = emoi5 + Ie6*pase32
          Eplu6 = emoi6 + pase32
          IF ( emoi6.LT.0. ) THEN
            IF ( ABS(Eplu6-1E-8).GT.SQRT(4.*bplu6*cplu6) ) GOTO 1360
          ELSEIF ( (emoi6-1E-8).GT.SQRT(zco*bplu6*cplu6) ) THEN
            GOTO 1380
          ENDIF
          kemoi6 = coeff*emoi6
          keplu6 = coeff*Eplu6
 1200     IF ( iiff.NE.0 ) THEN
            DO Imf6 = 0, 1
              fmoi6 = fmoi5 + Imf6*pasf32
              DO I = 1, kt
                Atest = ABS(amoi6-aas(I))
                Btest = ABS(bmoi6-bbs(I))
                Ctest = ABS(cmoi6-ccs(I))
                Dtest = ABS(dmoi6-dds(I))
                Etest = ABS(emoi6-ees(I))
                Etesty = ABS(ABS(emoi6)-ABS(ees(I)))
                Ftest = ABS(fmoi6-ffs(I))
                IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND. Etest.LT.cxe .AND.&
     &               Ftest.LT.cxf ) GOTO 1325
                IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND.                   &
     &               Etesty.LT.cya .AND. Ftest.LT.cya ) GOTO 1325
              ENDDO
              Fplu6 = fmoi6 + pasf32
              IF ( (fmoi6-1E-8)**2.GT.zco*aplu6*cplu6 ) GOTO 1340
              kfmoi6 = coeff*fmoi6
              kfplu6 = coeff*Fplu6
              CALL VOLUME(amoi6,bmoi6,cmoi6,dmoi6,emoi6,fmoi6,aplu6,bplu6,cplu6,dplu6,Eplu6,Fplu6)
              IF ( ly.EQ.-1 ) GOTO 1325
              CALL TRIHKL1(kamoi6,kbmoi6,kcmoi6,kdmoi6,kemoi6,kfmoi6,kaplu6,kbplu6,kcplu6,kdplu6,keplu6,kfplu6,6)
              IF ( nt.EQ.-1 ) GOTO 1325
              ndich(6) = ndich(6) + 1
              DO I = 1, n - 1
                J = I + 1
                IF ( irj(I,6).EQ.1 .AND. irj(J,6).EQ.1 .AND. ih(I,1,6).EQ.ih(J,1,6) .AND. ik(I,1,6).EQ.ik(J,1,6)   &
     &               .AND. il(I,1,6).EQ.il(J,1,6) ) GOTO 1325
              ENDDO
              IF ( iiaa.EQ.0 ) GOTO 1210
              Ia7 = 0
 1205         amoi7 = amoi6 + Ia7*pasa64
              aplu7 = amoi7 + pasa64
              kamoi7 = coeff*amoi7
              kaplu7 = coeff*aplu7
              IF ( md34.EQ.1 ) THEN
                dmoi7 = amoi7 - s3 - errd
                dplu7 = aplu7 - s3 + errd
                Dmoim = AMIN1(ABS(dmoi7),ABS(dplu7))
                IF ( Dmoim.GT.dmax .AND. dmoi7.GE.0. ) GOTO 1325
                IF ( Dmoim.GT.dmax ) GOTO 1320
                IF ( dmoi7.LT.(-dmax) ) dmoi7 = -dmax
                IF ( dplu7.GT.dmax ) dplu7 = dmax
                Zzo = zco
                IF ( Dmoim*Dmoim.GE.zco*aplu7*bplu7 .AND. dmoi7.GE.0. ) GOTO 1325
                IF ( Dmoim*Dmoim.GE.zco*aplu7*bplu7 ) GOTO 1320
                kdmoi7 = coeff*dmoi7
                kdplu7 = coeff*dplu7
              ELSEIF ( md7.NE.0 ) THEN
                bmoi7 = s7 - aplu7 - errb
                IF ( bmoi7.GT.bmax ) GOTO 1320
                bplu7 = s7 - amoi7 + errb
                IF ( bplu7.LE.0 ) GOTO 1325
                IF ( bmoi7.LE.0 ) bmoi7 = bmin
                IF ( bmoi7.GT.aplu7 .OR. (dmoi7-1E-8)**2.GE.zco*aplu7*bplu7 ) GOTO 1320
                kbmoi7 = coeff*bmoi7
                kbplu7 = coeff*bplu7
              ENDIF
 1210         IF ( iibb.EQ.0 ) GOTO 1220
              Ib7 = 0
 1215         bmoi7 = bmoi6 + Ib7*pasb64
              bplu7 = bmoi7 + pasb64
              kbmoi7 = coeff*bmoi7
              kbplu7 = coeff*bplu7
              IF ( md12.EQ.1 ) THEN
                dmoi7 = (bmoi7-s1) - errd
                dplu7 = (bplu7-s1) + errd
                Dmoim = AMIN1(ABS(dmoi7),ABS(dplu7))
                IF ( Dmoim.GT.dmax .AND. dmoi7.GE.0. ) GOTO 1325
                IF ( Dmoim*Dmoim.GE.zco*aplu7*bplu7 .AND. dmoi7.GE.0. ) GOTO 1325
              ELSE
                IF ( md56.EQ.0 ) GOTO 1220
                dmoi7 = (s5-aplu7-bplu7) - errd
                dplu7 = (s5-amoi7-bmoi7) + errd
                Dmoim = AMIN1(ABS(dmoi7),ABS(dplu7))
                IF ( Dmoim.GT.dmax .AND. dplu7.LE.0. ) GOTO 1320
                IF ( Dmoim*Dmoim.GE.zco*aplu7*bplu7 .AND. dplu7.LE.0. ) GOTO 1320
              ENDIF
              IF ( Dmoim.GT.dmax ) GOTO 1310
              IF ( dmoi7.LT.(-dmax) ) dmoi7 = -dmax
              IF ( dplu7.GT.dmax ) dplu7 = dmax
              Zzo = zco
              IF ( Dmoim*Dmoim.GE.zco*aplu7*bplu7 ) GOTO 1310
              kdmoi7 = coeff*dmoi7
              kdplu7 = coeff*dplu7
 1220         IF ( iicc.EQ.0 ) GOTO 1230
              Ic7 = 0
 1225         cmoi7 = cmoi6 + Ic7*pasc64
              cplu7 = cmoi7 + pasc64
              kcmoi7 = coeff*cmoi7
              kcplu7 = coeff*cplu7
              IF ( md89.NE.0 ) THEN
                emoi7 = (bmoi7+cmoi7-s8) - erre
                Eplu7 = (bplu7+cplu7-s8) + erre
                Emma7 = AMIN1(ABS(emoi7),ABS(Eplu7))
                IF ( Emma7.GT.emax .AND. emoi7.GT.0 ) GOTO 1310
                IF ( Emma7.GT.emax ) GOTO 1300
                IF ( emoi7.LT.(-emax) ) emoi7 = -emax
                IF ( Eplu7.GT.emax ) Eplu7 = emax
                Zzo = zco
                IF ( emoi7.LT.0. ) Zzo = 4.
                Emmi7 = AMIN1(ABS(emoi7),ABS(Eplu7))
                IF ( Emmi7*Emmi7.GE.Zzo*bplu7*cplu7 .AND. emoi7.GT.0 ) GOTO 1310
                IF ( Emmi7*Emmi7.GE.Zzo*bplu7*cplu7 ) GOTO 1300
                kemoi7 = coeff*emoi7
                keplu7 = coeff*Eplu7
              ENDIF
 1230         IF ( iidd.EQ.0 ) GOTO 1240
              Id7 = 0
 1235         dmoi7 = dmoi6 + Id7*pasd64
              dplu7 = dmoi7 + pasd64
              IF ( (dmoi7-1E-8)**2.GT.zco*aplu7*bplu7 ) GOTO 1295
              kdmoi7 = coeff*dmoi7
              kdplu7 = coeff*dplu7
 1240         IF ( iiee.EQ.0 ) GOTO 1250
              Ie7 = 0
 1245         emoi7 = emoi6 + Ie7*pase64
              Eplu7 = emoi7 + pase64
              IF ( emoi7.LT.0. ) THEN
                IF ( ABS(Eplu7-1E-8).GT.SQRT(4.*bplu7*cplu7) ) GOTO 1285
              ELSEIF ( (emoi7-1E-8).GT.SQRT(zco*bplu7*cplu7) ) THEN
                GOTO 1290
              ENDIF
              kemoi7 = coeff*emoi7
              keplu7 = coeff*Eplu7
 1250         IF ( iiff.NE.0 ) THEN
                DO Imf7 = 0, 1
                  fmoi7 = fmoi6 + Imf7*pasf64
                  DO I = 1, kt
                    Atest = ABS(amoi7-aas(I))
                    Btest = ABS(bmoi7-bbs(I))
                    Ctest = ABS(cmoi7-ccs(I))
                    Dtest = ABS(dmoi7-dds(I))
                    Etest = ABS(emoi7-ees(I))
                    Etesty = ABS(ABS(emoi7)-ABS(ees(I)))
                    Ftest = ABS(fmoi7-ffs(I))
                    IF ( Atest.LT.cxa .AND. Btest.LT.cxb .AND. Ctest.LT.cxc .AND. Dtest.LT.cxd .AND.               &
     &                   Etest.LT.cxe .AND. Ftest.LT.cxf ) GOTO 1278
                    IF ( Atest.LT.cya .AND. Btest.LT.cya .AND. Ctest.LT.cya .AND. Dtest.LT.cya .AND.               &
     &                   Etesty.LT.cya .AND. Ftest.LT.cya ) GOTO 1278
                  ENDDO
                  Fplu7 = fmoi7 + pasf64
                  IF ( (fmoi7-1E-8)**2.GT.zco*aplu7*cplu7 ) GOTO 1280
                  kfmoi7 = coeff*fmoi7
                  kfplu7 = coeff*Fplu7
                  CALL VOLUME(amoi7,bmoi7,cmoi7,dmoi7,emoi7,fmoi7,aplu7,bplu7,cplu7,dplu7,Eplu7,Fplu7)
                  IF ( ly.EQ.-1 ) GOTO 1278
                  CALL TRIHKL1(kamoi7,kbmoi7,kcmoi7,kdmoi7,kemoi7,kfmoi7,kaplu7,kbplu7,kcplu7,kdplu7,keplu7,kfplu7,&
     &                         7)
                  IF ( nt.EQ.-1 ) GOTO 1278
                  ndich(7) = ndich(7) + 1
                  DO I = 1, n - 1
                    J = I + 1
                    IF ( irj(I,7).EQ.1 .AND. irj(J,7).EQ.1 .AND. ih(I,1,7).EQ.ih(J,1,7) .AND. ik(I,1,7)            &
     &                   .EQ.ik(J,1,7) .AND. il(I,1,7).EQ.il(J,1,7) ) GOTO 1278
                  ENDDO
                  Amoi8 = amoi7
                  Aplu8 = aplu7
                  Kamoi8 = coeff*Amoi8
                  Kaplu8 = coeff*Aplu8
                  Bmoi8 = bmoi7
                  Bplu8 = bplu7
                  Kbmoi8 = coeff*Bmoi8
                  Kbplu8 = coeff*Bplu8
                  Cmoi8 = cmoi7
                  Cplu8 = cplu7
                  Kcmoi8 = coeff*Cmoi8
                  Kcplu8 = coeff*Cplu8
                  Dmoi8 = dmoi7
                  Dplu8 = dplu7
                  Kdmoi8 = coeff*Dmoi8
                  Kdplu8 = coeff*Dplu8
                  Emoi8 = emoi7
                  Eplu8 = Eplu7
                  Kemoi8 = coeff*Emoi8
                  Keplu8 = coeff*Eplu8
                  Fmoi8 = fmoi7
                  Fplu8 = Fplu7
                  Kfmoi8 = coeff*Fmoi8
                  Kfplu8 = coeff*Fplu8
                  IF ( jjaa.EQ.1 ) GOTO 1254
                  IF ( (aplu7-amoi7).LT.pasa64 ) GOTO 1254
                  Nap = NINT((aplu7-amoi7)/pasa64)
                  Iap7 = 0
 1252             Amoi8 = amoi7 + Iap7*pasa64
                  Aplu8 = Amoi8 + pasa64
                  Kamoi8 = coeff*Amoi8
                  Kaplu8 = coeff*Aplu8
 1254             IF ( jjbb.EQ.1 ) GOTO 1258
                  IF ( (bplu7-bmoi7).LT.pasb64 ) GOTO 1258
                  Nbp = NINT((bplu7-bmoi7)/pasb64)
                  Ibp7 = 0
 1256             Bmoi8 = bmoi7 + Ibp7*pasb64
                  Bplu8 = Bmoi8 + pasb64
                  Kbmoi8 = coeff*Bmoi8
                  Kbplu8 = coeff*Bplu8
 1258             IF ( jjcc.EQ.1 ) GOTO 1262
                  IF ( (cplu7-cmoi7).LT.pasc64 ) GOTO 1262
                  Ncp = NINT((cplu7-cmoi7)/pasc64)
                  Icp7 = 0
 1260             Cmoi8 = cmoi7 + Icp7*pasc64
                  Cplu8 = Cmoi8 + pasc64
                  Kcmoi8 = coeff*Cmoi8
                  Kcplu8 = coeff*Cplu8
 1262             IF ( jjdd.EQ.1 ) GOTO 1266
                  IF ( (dplu7-dmoi7).LT.pasd64 ) GOTO 1266
                  Ndp = NINT((dplu7-dmoi7)/pasd64)
                  Idp7 = 0
 1264             Dmoi8 = dmoi7 + Idp7*pasd64
                  Dplu8 = Dmoi8 + pasd64
                  Kdmoi8 = coeff*Dmoi8
                  Kdplu8 = coeff*Dplu8
 1266             IF ( jjee.EQ.1 ) GOTO 1270
                  IF ( (Eplu7-emoi7).LT.pase64 ) GOTO 1270
                  Nep = NINT((Eplu7-emoi7)/pase64)
                  Iep7 = 0
 1268             Emoi8 = emoi7 + Iep7*pase64
                  Eplu8 = Emoi8 + pase64
                  Kemoi8 = coeff*Emoi8
                  Keplu8 = coeff*Eplu8
 1270             IF ( jjff.EQ.1 ) GOTO 1274
                  IF ( (Fplu7-fmoi7).LT.pasf64 ) GOTO 1274
                  Nfp = NINT((Fplu7-fmoi7)/pasf64)
                  Ifp7 = 0
 1272             Fmoi8 = fmoi7 + Ifp7*pasf64
                  Fplu8 = Fmoi8 + pasf64
                  Kfmoi8 = coeff*Fmoi8
                  Kfplu8 = coeff*Fplu8
 1274             CALL TRIHKL1(Kamoi8,Kbmoi8,Kcmoi8,Kdmoi8,Kemoi8,Kfmoi8,Kaplu8,Kbplu8,Kcplu8,Kdplu8,Keplu8,Kfplu8,&
     &                         8)
                  IF ( nt.NE.-1 ) THEN
                    ndich(8) = ndich(8) + 1
                    DO I = 1, n - 1
                      J = I + 1
                      IF ( irj(I,8).EQ.1 .AND. irj(J,8).EQ.1 .AND. ih(I,1,8).EQ.ih(J,1,8) .AND. ik(I,1,8)          &
     &                     .EQ.ik(J,1,8) .AND. il(I,1,8).EQ.il(J,1,8) ) GOTO 1276
                    ENDDO
                    aa = (Amoi8+Aplu8)/2.
                    bb = (Bmoi8+Bplu8)/2.
                    cc = (Cmoi8+Cplu8)/2.
                    Dd = (Dmoi8+Dplu8)/2.
                    Ee = (Emoi8+Eplu8)/2.
                    Ff = (Fmoi8+Fplu8)/2.
                    ktp = ktp + 1
                    kt = 1 + MOD(1,ktp)*kt
                    aas(kt) = aa
                    bbs(kt) = bb
                    ccs(kt) = cc
                    dds(kt) = Dd
                    ees(kt) = Ee
                    ffs(kt) = Ff
                    Vet = .5*SQRT(4*aa*bb*cc+Dd*Ee*Ff-aa*Ee*Ee-bb*Ff*Ff-cc*Dd*Dd)
                    vr2 = 1/Vet**2
                    Sv = SQRT(vr2)
                    Vap = Sv
                    Sinal = SQRT(1-Ee*Ee/4/bb/cc)
                    Sinbe = SQRT(1-Ff*Ff/4/aa/cc)
                    Singa = SQRT(1-Dd*Dd/4/aa/bb)
                    ar = SQRT(bb*cc)*Sinal/Vet
                    br = SQRT(aa*cc)*Sinbe/Vet
                    cr = SQRT(aa*bb)*Singa/Vet
                    dr = pideg*ACOS((Dd*Ff/4/aa-Ee/2)/Sinbe/Singa/SQRT(bb*cc))
                    er = pideg*ACOS((Ee*Dd/4/bb-Ff/2)/Sinal/Singa/SQRT(aa*cc))
                    fr = pideg*ACOS((Ee*Ff/4/cc-Dd/2)/Sinal/Sinbe/SQRT(aa*bb))
                    IF ( kt.NE.1 ) THEN
                      DO J = 1, kt - 1
                        Artest = ABS(ar-arx(J))
                        Brtest = ABS(br-brx(J))
                        Crtest = ABS(cr-crx(J))
                        Drtest = ABS(dr-drx(J))
                        Ertest = ABS(er-erx(J))
                        Frtest = ABS(fr-frx(J))
                        Drstest = ABS(180.-dr-drx(J))
                        Erstest = ABS(180.-er-erx(J))
                        Frstest = ABS(180.-fr-frx(J))
                        IF ( Artest.LT..1 .AND. Brtest.LT..1 .AND. Crtest.LT..1 .AND.                              &
     &                       (Drtest.LT..5 .OR. Drstest.LT..5) .AND. (Ertest.LT..5 .OR. Erstest.LT..5) .AND.       &
     &                       (Frtest.LT..5 .OR. Frstest.LT..5) ) GOTO 1278
                      ENDDO
                    ENDIF
                    Ind = 6
                    Nrind = 6
                    CALL AFFPAR(Ind,Nrind,Vap)
                    arx(kt) = ar
                    brx(kt) = br
                    crx(kt) = cr
                    drx(kt) = dr
                    erx(kt) = er
                    frx(kt) = fr
                    IF ( fwolff.EQ.-1000. ) THEN
                      ktp = ktp - 1
                      IF ( kt.EQ.1 ) THEN
                        aas(1) = 1000.
                        bbs(1) = 1000.
                        ccs(1) = 1000.
                        dds(1) = 1000.
                        ees(1) = 1000.
                        ffs(1) = 1000.
                      ELSE
                        kt = kt - 1
                      ENDIF
                      GOTO 1276
                    ENDIF
                    vr2 = v*v
!                    WRITE (iw,99001) (ndich(I),I=1,8)
!99001 FORMAT (//,2X,'ITERATION NUMBER AT EACH DICHOTOMY : ',8I5,//)
                  ENDIF
 1276             IF ( jjff.NE.1 ) THEN
                    Ifp7 = Ifp7 + 1
                    IF ( Ifp7.LE.Nfp ) GOTO 1272
                  ENDIF
                  IF ( jjee.NE.1 ) THEN
                    Iep7 = Iep7 + 1
                    IF ( Iep7.LE.Nep ) GOTO 1268
                  ENDIF
                  IF ( jjdd.NE.1 ) THEN
                    Idp7 = Idp7 + 1
                    IF ( Idp7.LE.Ndp ) GOTO 1264
                  ENDIF
                  IF ( jjcc.NE.1 ) THEN
                    Icp7 = Icp7 + 1
                    IF ( Icp7.LE.Ncp ) GOTO 1260
                  ENDIF
                  IF ( jjbb.NE.1 ) THEN
                    Ibp7 = Ibp7 + 1
                    IF ( Ibp7.LE.Nbp ) GOTO 1256
                  ENDIF
                  IF ( jjaa.NE.1 ) THEN
                    Iap7 = Iap7 + 1
                    IF ( Iap7.LE.Nap ) GOTO 1252
                  ENDIF
 1278             IF ( iiff.EQ.0 ) GOTO 1280
                ENDDO
              ENDIF
 1280         IF ( iiee.EQ.0 ) GOTO 1290
 1285         Ie7 = Ie7 + 1
              IF ( Ie7.LE.1 ) GOTO 1245
 1290         IF ( iidd.NE.0 ) THEN
                Id7 = Id7 + 1
                IF ( Id7.LE.1 ) GOTO 1235
              ENDIF
 1295         IF ( iicc.EQ.0 ) GOTO 1305
 1300         Ic7 = Ic7 + 1
              IF ( Ic7.LE.1 ) GOTO 1225
 1305         IF ( iibb.EQ.0 ) GOTO 1315
 1310         Ib7 = Ib7 + 1
              IF ( Ib7.LE.1 ) GOTO 1215
 1315         IF ( iiaa.EQ.0 ) GOTO 1325
 1320         Ia7 = Ia7 + 1
              IF ( Ia7.LE.1 ) GOTO 1205
 1325         IF ( iiff.EQ.0 ) GOTO 1340
            ENDDO
          ENDIF
 1340     IF ( iiee.EQ.0 ) GOTO 1380
 1360     Ie6 = Ie6 + 1
          IF ( Ie6.LE.1 ) GOTO 1180
 1380     IF ( iidd.NE.0 ) THEN
            Id6 = Id6 + 1
            IF ( Id6.LE.1 ) GOTO 1140
          ENDIF
 1400     IF ( iicc.EQ.0 ) GOTO 1440
 1420     Ic6 = Ic6 + 1
          IF ( Ic6.LE.1 ) GOTO 1100
 1440     IF ( iibb.EQ.0 ) GOTO 1480
 1460     Ib6 = Ib6 + 1
          IF ( Ib6.LE.1 ) GOTO 1060
 1480     IF ( iiaa.EQ.0 ) GOTO 1520
 1500     Ia6 = Ia6 + 1
          IF ( Ia6.LE.1 ) GOTO 1020
 1520     IF ( iiff.EQ.0 ) GOTO 1600
        ENDDO
      ENDIF
 1600 IF ( iiee.EQ.0 ) GOTO 1800
 1700 Ie5 = Ie5 + 1
      IF ( Ie5.LE.1 ) GOTO 900
 1800 IF ( iidd.NE.0 ) THEN
        Id5 = Id5 + 1
        IF ( Id5.LE.1 ) GOTO 700
      ENDIF
 1900 IF ( iicc.EQ.0 ) GOTO 2100
 2000 Ic5 = Ic5 + 1
      IF ( Ic5.LE.1 ) GOTO 500
 2100 IF ( iibb.EQ.0 ) GOTO 2300
 2200 Ib5 = Ib5 + 1
      IF ( Ib5.LE.1 ) GOTO 300
 2300 IF ( iiaa.EQ.0 ) GOTO 99999
 2400 Ia5 = Ia5 + 1
      IF ( Ia5.LE.1 ) GOTO 100
99999 END SUBROUTINE TRICLINI3
