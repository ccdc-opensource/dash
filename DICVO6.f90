!*==TRIHKL.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE TRIHKL(Kamoi,Kbmoi,Kcmoi,Kdmoi,Kemoi,Kfmoi,Kaplu,Kbplu,Kcplu,Kdplu,Keplu,Kfplu)
      USE DICVAR
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi, Kfplu
      INTENT (IN) Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi, Kfplu
!
! Local variables
!
      INTEGER :: I, Jj, Jjb, K, Kqqm, Kqqp, M, Mcarhm, Mcarhp, Mcarkm, Mcarkp, Mcarlm, Mcarlp, Mco1m,&
     &           Mco1p, Mco2m, Mco2p, Mco3m, Mco3p, Mdk, Mdk2, Mdl, Mdl2, Mxl
!
!*** End of declarations rewritten by SPAG
!
      mt = 0
      DO I = 1, n
        irj(I,1) = 0
        irjb(I) = 0
      ENDDO
      DO M = 0, mh
        Mdk = -mk*MOD(1,1+M)
        Mcarhp = M*M*Kaplu
        Mcarhm = M*M*Kamoi
        DO K = Mdk, mk
          IF ( (M+ABS(K)).LE.6 ) THEN
            Mdl = -ml
            IF ( (MOD(1,1+M)+MOD(1,1+K*K)).EQ.0 ) Mdl = 0
            Mcarkp = K*K*Kbplu
            Mcarkm = K*K*Kbmoi
            DO Mxl = Mdl, ml
              IF ( (M+ABS(K)+ABS(Mxl)).LE.6 ) THEN
                Mcarlp = Mxl*Mxl*Kcplu
                Mcarlm = Mxl*Mxl*Kcmoi
                IF ( M*K.LT.0 ) THEN
                  Mco1p = Kdmoi*M*K
                  Mco1m = Kdplu*M*K
                ELSE
                  Mco1p = Kdplu*M*K
                  Mco1m = Kdmoi*M*K
                ENDIF
                IF ( K*Mxl.LT.0 ) THEN
                  Mco2p = Kemoi*K*Mxl
                  Mco2m = Keplu*K*Mxl
                ELSE
                  Mco2p = Keplu*K*Mxl
                  Mco2m = Kemoi*K*Mxl
                ENDIF
                IF ( M*Mxl.LT.0 ) THEN
                  Mco3p = Kfmoi*M*Mxl
                  Mco3m = Kfplu*M*Mxl
                ELSE
                  Mco3p = Kfplu*M*Mxl
                  Mco3m = Kfmoi*M*Mxl
                ENDIF
                Kqqm = Mcarhm + Mcarkm + Mcarlm + Mco1m + Mco2m + Mco3m
                IF ( (Kqqm-kepsq(n5)).LE.kq(n5) ) THEN
                  Kqqp = Mcarhp + Mcarkp + Mcarlp + Mco1p + Mco2p + Mco3p
                  IF ( (Kqqp+kepsq(1)).GE.kq(1) ) THEN
                    DO I = 1, n5
                      IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
                        IF ( (Kqqp+kepsq(I)).GE.kq(I) ) THEN
                          irj(I,1) = irj(I,1) + 1
                          Jj = irj(I,1)
                          ih(I,Jj,1) = M
                          ik(I,Jj,1) = K
                          il(I,Jj,1) = Mxl
                          IF ( M*Mxl.EQ.0 ) THEN
                            irjb(I) = irjb(I) + 1
                            Jjb = irjb(I)
                            ihb(I,Jjb) = M
                            ikb(I,Jjb) = K
                            ilb(I,Jjb) = Mxl
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, n5
        IF ( irj(I,1).EQ.0 ) THEN
          mt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 0, mh2
        Mdk2 = -mk2*MOD(1,1+M)
        Mcarhp = M*M*Kaplu
        Mcarhm = M*M*Kamoi
        DO K = Mdk2, mk2
          IF ( (M+ABS(K)).LE.6 ) THEN
            Mdl2 = -ml2
            IF ( (MOD(1,1+M)+MOD(1,1+K*K)).EQ.0 ) Mdl2 = 0
            Mcarkp = K*K*Kbplu
            Mcarkm = K*K*Kbmoi
            DO Mxl = Mdl2, ml2
              IF ( (M+ABS(K)+ABS(Mxl)).LE.6 ) THEN
                Mcarlp = Mxl*Mxl*Kcplu
                Mcarlm = Mxl*Mxl*Kcmoi
                IF ( M*K.LT.0 ) THEN
                  Mco1p = Kdmoi*M*K
                  Mco1m = Kdplu*M*K
                ELSE
                  Mco1p = Kdplu*M*K
                  Mco1m = Kdmoi*M*K
                ENDIF
                IF ( K*Mxl.LT.0 ) THEN
                  Mco2p = Kemoi*K*Mxl
                  Mco2m = Keplu*K*Mxl
                ELSE
                  Mco2p = Keplu*K*Mxl
                  Mco2m = Kemoi*K*Mxl
                ENDIF
                IF ( M*Mxl.LT.0 ) THEN
                  Mco3p = Kfmoi*M*Mxl
                  Mco3m = Kfplu*M*Mxl
                ELSE
                  Mco3p = Kfplu*M*Mxl
                  Mco3m = Kfmoi*M*Mxl
                ENDIF
                Kqqm = Mcarhm + Mcarkm + Mcarlm + Mco1m + Mco2m + Mco3m
                IF ( (Kqqm-kepsq(n)).LE.kq(n) ) THEN
                  Kqqp = Mcarhp + Mcarkp + Mcarlp + Mco1p + Mco2p + Mco3p
                  IF ( (Kqqp+kepsq(n6)).GE.kq(n6) ) THEN
                    DO I = n6, n
                      IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
                        IF ( (Kqqp+kepsq(I)).GE.kq(I) ) THEN
                          irj(I,1) = irj(I,1) + 1
                          Jj = irj(I,1)
                          ih(I,Jj,1) = M
                          ik(I,Jj,1) = K
                          il(I,Jj,1) = Mxl
                          IF ( M*Mxl.EQ.0 ) THEN
                            irjb(I) = irjb(I) + 1
                            Jjb = irjb(I)
                            ihb(I,Jjb) = M
                            ikb(I,Jjb) = K
                            ilb(I,Jjb) = Mxl
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
99999 END SUBROUTINE TRIHKL
!*==TRIHKLB.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE TRIHKLB(Kamoi,Kbmoi,Kcmoi,Kdmoi,Kemoi,Kfmoi,Kaplu,Kbplu,Kcplu,Kdplu,Keplu,Kfplu)
      USE DICVAR
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi, Kfplu
      INTENT (IN) Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi, Kfplu
!
! Local variables
!
      INTEGER :: I, Jj, K, Kqqm, Kqqp, M, Mcarhm, Mcarhp, Mcarkm, Mcarkp, Mcarlm, Mcarlp, Mco1m,      &
     &           Mco1p, Mco2m, Mco2p, Mco3m, Mco3p, Mdk, Mdk2, Mdl, Mdl2, Mxl
!
!*** End of declarations rewritten by SPAG
!
      nt = 0
      DO I = 1, n
        irj(I,1) = 0
      ENDDO
      DO M = 1, mh
        Mdk = -mk*MOD(1,1+M)
        Mcarhp = M*M*Kaplu
        Mcarhm = M*M*Kamoi
        DO K = Mdk, mk
          IF ( (M+ABS(K)).LE.6 ) THEN
            Mdl = -ml
            IF ( (MOD(1,1+M)+MOD(1,1+K*K)).EQ.0 ) Mdl = 0
            Mcarkp = K*K*Kbplu
            Mcarkm = K*K*Kbmoi
            DO Mxl = Mdl, ml
              IF ( (M+ABS(K)+ABS(Mxl)).LE.6 ) THEN
                IF ( Mxl.NE.0 ) THEN
                  Mcarlp = Mxl*Mxl*Kcplu
                  Mcarlm = Mxl*Mxl*Kcmoi
                  IF ( M*K.LT.0 ) THEN
                    Mco1p = Kdmoi*M*K
                    Mco1m = Kdplu*M*K
                  ELSE
                    Mco1p = Kdplu*M*K
                    Mco1m = Kdmoi*M*K
                  ENDIF
                  IF ( K*Mxl.LT.0 ) THEN
                    Mco2p = Kemoi*K*Mxl
                    Mco2m = Keplu*K*Mxl
                  ELSE
                    Mco2p = Keplu*K*Mxl
                    Mco2m = Kemoi*K*Mxl
                  ENDIF
                  IF ( M*Mxl.LT.0 ) THEN
                    Mco3p = Kfmoi*M*Mxl
                    Mco3m = Kfplu*M*Mxl
                  ELSE
                    Mco3p = Kfplu*M*Mxl
                    Mco3m = Kfmoi*M*Mxl
                  ENDIF
                  Kqqm = Mcarhm + Mcarkm + Mcarlm + Mco1m + Mco2m + Mco3m
                  IF ( (Kqqm-kepsq(n5)).LE.kq(n5) ) THEN
                    Kqqp = Mcarhp + Mcarkp + Mcarlp + Mco1p + Mco2p + Mco3p
                    IF ( (Kqqp+kepsq(1)).GE.kq(1) ) THEN
                      DO I = 1, n5
                        IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
                          IF ( (Kqqp+kepsq(I)).GE.kq(I) ) THEN
                            irj(I,1) = irj(I,1) + 1
                            Jj = irj(I,1)
                            ih(I,Jj,1) = M
                            ik(I,Jj,1) = K
                            il(I,Jj,1) = Mxl
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      DO I = 1, n5
        IF ( (irj(I,1)+irjb(I)).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 1, mh2
        Mdk2 = -mk2*MOD(1,1+M)
        Mcarhp = M*M*Kaplu
        Mcarhm = M*M*Kamoi
        DO K = Mdk2, mk2
          IF ( (M+ABS(K)).LE.6 ) THEN
            Mdl2 = -ml2
            IF ( (MOD(1,1+M)+MOD(1,1+K*K)).EQ.0 ) Mdl2 = 0
            Mcarkp = K*K*Kbplu
            Mcarkm = K*K*Kbmoi
            DO Mxl = Mdl2, ml2
              IF ( (M+ABS(K)+ABS(Mxl)).LE.6 ) THEN
                IF ( Mxl.NE.0 ) THEN
                  Mcarlp = Mxl*Mxl*Kcplu
                  Mcarlm = Mxl*Mxl*Kcmoi
                  IF ( M*K.LT.0 ) THEN
                    Mco1p = Kdmoi*M*K
                    Mco1m = Kdplu*M*K
                  ELSE
                    Mco1p = Kdplu*M*K
                    Mco1m = Kdmoi*M*K
                  ENDIF
                  IF ( K*Mxl.LT.0 ) THEN
                    Mco2p = Kemoi*K*Mxl
                    Mco2m = Keplu*K*Mxl
                  ELSE
                    Mco2p = Keplu*K*Mxl
                    Mco2m = Kemoi*K*Mxl
                  ENDIF
                  IF ( M*Mxl.LT.0 ) THEN
                    Mco3p = Kfmoi*M*Mxl
                    Mco3m = Kfplu*M*Mxl
                  ELSE
                    Mco3p = Kfplu*M*Mxl
                    Mco3m = Kfmoi*M*Mxl
                  ENDIF
                  Kqqm = Mcarhm + Mcarkm + Mcarlm + Mco1m + Mco2m + Mco3m
                  IF ( (Kqqm-kepsq(n)).LE.kq(n) ) THEN
                    Kqqp = Mcarhp + Mcarkp + Mcarlp + Mco1p + Mco2p + Mco3p
                    IF ( (Kqqp+kepsq(n6)).GE.kq(n6) ) THEN
                      DO I = n6, n
                        IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
                          IF ( (Kqqp+kepsq(I)).GE.kq(I) ) THEN
                            irj(I,1) = irj(I,1) + 1
                            Jj = irj(I,1)
                            ih(I,Jj,1) = M
                            ik(I,Jj,1) = K
                            il(I,Jj,1) = Mxl
                          ENDIF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
99999 END SUBROUTINE TRIHKLB
!*==TRIHKL1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE TRIHKL1(Kamoi,Kbmoi,Kcmoi,Kdmoi,Kemoi,Kfmoi,Kaplu,Kbplu,Kcplu,Kdplu,Keplu,Kfplu,K1)
      USE DICVAR
      IMPLICIT NONE
!
!*** Start of declarations rewritten by SPAG
!
! Dummy arguments
!
      INTEGER :: K1, Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi, Kfplu
      INTENT (IN) K1, Kamoi, Kaplu, Kbmoi, Kbplu, Kcmoi, Kcplu, Kdmoi, Kdplu, Kemoi, Keplu, Kfmoi,     &
     &            Kfplu
!
! Local variables
!
      INTEGER :: I, J, Jj, K, Kj, Kk, Kqqm, Kqqp, M, Mcarhm, Mcarhp, Mcarkm, Mcarkp, Mcarlm, Mcarlp,&
     &           Mco1m, Mco1p, Mco2m, Mco2p, Mco3m, Mco3p, Mxl
!
!*** End of declarations rewritten by SPAG
!
      nt = 0
      Kk = K1 - 1
      DO I = 1, n
        Jj = irj(I,Kk)
        irj(I,K1) = 0
        DO J = 1, Jj
          M = ih(I,J,Kk)
          K = ik(I,J,Kk)
          Mxl = il(I,J,Kk)
          Mcarhp = M*M*Kaplu
          Mcarhm = M*M*Kamoi
          Mcarkp = K*K*Kbplu
          Mcarkm = K*K*Kbmoi
          Mcarlp = Mxl*Mxl*Kcplu
          Mcarlm = Mxl*Mxl*Kcmoi
          IF ( M*K.LT.0 ) THEN
            Mco1p = Kdmoi*M*K
            Mco1m = Kdplu*M*K
          ELSE
            Mco1p = Kdplu*M*K
            Mco1m = Kdmoi*M*K
          ENDIF
          IF ( K*Mxl.LT.0 ) THEN
            Mco2p = Kemoi*K*Mxl
            Mco2m = Keplu*K*Mxl
          ELSE
            Mco2p = Keplu*K*Mxl
            Mco2m = Kemoi*K*Mxl
          ENDIF
          IF ( M*Mxl.LT.0 ) THEN
            Mco3p = Kfmoi*M*Mxl
            Mco3m = Kfplu*M*Mxl
          ELSE
            Mco3p = Kfplu*M*Mxl
            Mco3m = Kfmoi*M*Mxl
          ENDIF
          Kqqm = Mcarhm + Mcarkm + Mcarlm + Mco1m + Mco2m + Mco3m
          IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
            Kqqp = Mcarhp + Mcarkp + Mcarlp + Mco1p + Mco2p + Mco3p
            IF ( (Kqqp+kepsq(I)).GE.kq(I) ) THEN
              irj(I,K1) = irj(I,K1) + 1
              Kj = irj(I,K1)
              ih(I,Kj,K1) = M
              ik(I,Kj,K1) = K
              il(I,Kj,K1) = Mxl
            ENDIF
          ENDIF
        ENDDO
        IF ( irj(I,K1).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
99999 END SUBROUTINE TRIHKL1
!*==VOLUME.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE VOLUME(Amoi,Bmoi,Cmoi,Dmoi,Emoi,Fmoi,Aplu,Bplu,Cplu,Dplu,Eplu,Fplu)
      USE DICVAR
      IMPLICIT NONE

! Dummy arguments
!
      REAL :: Amoi, Aplu, Bmoi, Bplu, Cmoi, Cplu, Dmoi, Dplu, Emoi, Eplu, Fmoi, Fplu
      INTENT (IN) Amoi, Aplu, Bmoi, Bplu, Cmoi, Cplu, Dmoi, Dplu, Emoi, Eplu, Fmoi, Fplu
!
! Local variables
!
      REAL, INTRINSIC :: AMAX1, AMIN1
      REAL :: Ed1, Ed2, Ed3, Ed4, Ef1, Ef2, Ef3, Ef4, Emoir, Eplur, Fd1, Fd2, Fd3, Fd4, Fm1, Fm2,  &
     &        Fm3, Fm4, Fm5, Fm6, Fm7, Fm8, Fp1, Fp2, Fp3, Fp4, Fp5, Fp6, Fp7, Fp8, O, P1, Q1,    &
     &        Vmoi, Vplu, Vtestm, Vtestp, X, Y, Z
      REAL :: FX1, FX2

      FX1(X,Y,O,P1,Q1) = .25*(4.*O*P1*Q1-O*X*X-P1*Y*Y+X*X*Y*Y/4./Q1)
      FX2(Z,X,Y,O,P1,Q1) = .25*(4.*O*P1*Q1-O*X*X-P1*Y*Y-Q1*Z*Z+X*Y*Z)
      ly = 0
      Emoir = AMIN1(ABS(Emoi),ABS(Eplu))
      Eplur = AMAX1(ABS(Emoi),ABS(Eplu))
      IF ( Fplu**2.GE.4.*Amoi*Cmoi .OR. Dplu**2.GE.4.*Amoi*Bmoi .OR. Eplur**2.GE.4.*Bmoi*Cmoi ) THEN
        Vmoi = .25*(4.*Amoi*Bmoi*Cmoi+Dmoi*Emoir*Fmoi-Aplu*Eplur**2-Bplu*Fplu**2-Cplu*Dplu**2)
        Vplu = .25*(4.*Aplu*Bplu*Cplu+Dplu*Eplur*Fplu-Amoi*Emoir**2-Bmoi*Fmoi**2-Cmoi*Dmoi**2)
        IF ( Vplu.LE.0. ) THEN
          ly = -1
        ELSE
          Vmoi = AMAX1(6.25E-8,Vmoi)
          Vtestp = 1/Vmoi
          Vtestm = 1/Vplu
          vmoii = SQRT(Vmoi)
          IF ( Vtestm.GT.vmax2 ) THEN
            ly = -1
          ELSEIF ( Vtestp.LT.vmin2 ) THEN
            ly = -1
          ELSEIF ( SQRT(Vtestm).GT.(1.02*SQRT(vr2)) ) THEN
            ly = -1
          ENDIF
        ENDIF
      ELSE
        Ef1 = 0.
        Ef2 = 0.
        Ef3 = 0.
        Ef4 = 0.
        Ed1 = 0.
        Ed2 = 0.
        Ed3 = 0.
        Ed4 = 0.
        Fd1 = 0.
        Fd2 = 0.
        Fd3 = 0.
        Fd4 = 0.
        Fp1 = FX2(Dmoi,Emoi,Fmoi,Aplu,Bplu,Cplu)
        Fp2 = FX2(Dmoi,Emoi,Fplu,Aplu,Bplu,Cplu)
        Fp3 = FX2(Dmoi,Eplu,Fmoi,Aplu,Bplu,Cplu)
        Fp4 = FX2(Dmoi,Eplu,Fplu,Aplu,Bplu,Cplu)
        Fp5 = FX2(Dplu,Emoi,Fmoi,Aplu,Bplu,Cplu)
        Fp6 = FX2(Dplu,Emoi,Fplu,Aplu,Bplu,Cplu)
        Fp7 = FX2(Dplu,Eplu,Fmoi,Aplu,Bplu,Cplu)
        Fp8 = FX2(Dplu,Eplu,Fplu,Aplu,Bplu,Cplu)
        Fm1 = FX2(Dmoi,Emoi,Fmoi,Amoi,Bmoi,Cmoi)
        Fm2 = FX2(Dmoi,Emoi,Fplu,Amoi,Bmoi,Cmoi)
        Fm3 = FX2(Dmoi,Eplu,Fmoi,Amoi,Bmoi,Cmoi)
        Fm4 = FX2(Dmoi,Eplu,Fplu,Amoi,Bmoi,Cmoi)
        Fm5 = FX2(Dplu,Emoi,Fmoi,Amoi,Bmoi,Cmoi)
        Fm6 = FX2(Dplu,Emoi,Fplu,Amoi,Bmoi,Cmoi)
        Fm7 = FX2(Dplu,Eplu,Fmoi,Amoi,Bmoi,Cmoi)
        Fm8 = FX2(Dplu,Eplu,Fplu,Amoi,Bmoi,Cmoi)
        IF ( Dmoi.LE.Emoi*Fmoi/2./Cplu .AND. Dplu.GE.Emoi*Fmoi/2./Cplu ) Ef1 = FX1(Emoi,Fmoi,Aplu,Bplu,Cplu)
        IF ( Dmoi.LE.Eplu*Fmoi/2./Cplu .AND. Dplu.GE.Eplu*Fmoi/2./Cplu ) Ef2 = FX1(Eplu,Fmoi,Aplu,Bplu,Cplu)
        IF ( Dmoi.LE.Emoi*Fplu/2./Cplu .AND. Dplu.GE.Emoi*Fplu/2./Cplu ) Ef3 = FX1(Emoi,Fplu,Aplu,Bplu,Cplu)
        IF ( Dmoi.LE.Eplu*Fplu/2./Cplu .AND. Dplu.GE.Eplu*Fplu/2./Cplu ) Ef4 = FX1(Eplu,Fplu,Aplu,Bplu,Cplu)
        IF ( Emoi.LE.Fmoi*Dmoi/2./Aplu .AND. Eplu.GE.Fmoi*Dmoi/2./Aplu ) Fd1 = FX1(Fmoi,Dmoi,Bplu,Cplu,Aplu)
        IF ( Emoi.LE.Fplu*Dmoi/2./Aplu .AND. Eplu.GE.Fplu*Dmoi/2./Aplu ) Fd2 = FX1(Fplu,Dmoi,Bplu,Cplu,Aplu)
        IF ( Emoi.LE.Fmoi*Dplu/2./Aplu .AND. Eplu.GE.Fmoi*Dplu/2./Aplu ) Fd3 = FX1(Fmoi,Dplu,Bplu,Cplu,Aplu)
        IF ( Emoi.LE.Fplu*Dplu/2./Aplu .AND. Eplu.GE.Fplu*Dplu/2./Aplu ) Fd4 = FX1(Fplu,Dplu,Bplu,Cplu,Aplu)
        IF ( Fmoi.LE.Emoi*Dmoi/2./Bplu .AND. Fplu.GE.Emoi*Dmoi/2./Bplu ) Ed1 = FX1(Emoi,Dmoi,Aplu,Cplu,Bplu)
        IF ( Fmoi.LE.Eplu*Dmoi/2./Bplu .AND. Fplu.GE.Eplu*Dmoi/2./Bplu ) Ed2 = FX1(Eplu,Dmoi,Aplu,Cplu,Bplu)
        IF ( Fmoi.LE.Emoi*Dplu/2./Bplu .AND. Fplu.GE.Emoi*Dplu/2./Bplu ) Ed3 = FX1(Emoi,Dplu,Aplu,Cplu,Bplu)
        IF ( Fmoi.LE.Eplu*Dplu/2./Bplu .AND. Fplu.GE.Eplu*Dplu/2./Bplu ) Ed4 = FX1(Eplu,Dplu,Aplu,Cplu,Bplu)
        Vplu = AMAX1(Ef1,Ef2,Ef3,Ef4,Fd1,Fd2,Fd3,Fd4,Ed1,Ed2,Ed3,Ed4,Fp1,Fp2,Fp3,Fp4,Fp5,Fp6,Fp7,Fp8)
        Vmoi = AMIN1(Fm1,Fm2,Fm3,Fm4,Fm5,Fm6,Fm7,Fm8)
        IF ( Vplu.LE.0. ) THEN
          ly = -1
        ELSE
          Vmoi = AMAX1(6.25E-8,Vmoi)
          Vtestp = 1/Vmoi
          Vtestm = 1/Vplu
          vmoii = SQRT(Vmoi)
          IF ( Vtestm.GT.vmax2 ) THEN
            ly = -1
          ELSEIF ( Vtestp.LT.vmin2 ) THEN
            ly = -1
          ELSEIF ( SQRT(Vtestm).GT.(1.02*SQRT(vr2)) ) THEN
            ly = -1
          ENDIF
        ENDIF
      ENDIF
      END SUBROUTINE VOLUME
!*==RELSPE.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!
!
      SUBROUTINE RELSPE

      USE DICVAR

      IMPLICIT NONE
!
! Local variables
!
      REAL :: Dax, Epr, Epr1, Eri2, Eri22, Qab, Rmn
      INTEGER :: I, J, J1, J2, Jh, Jk, Nh1, Nk1

      DO Nh1 = 1, 4
        DO Nk1 = 1, 4
          IF ( (Nh1+ABS(Nk1)).GT.6 ) GOTO 100
          Epr1 = Nh1*Nh1*(amax-amin) + Nk1*Nk1*(bmax-bmin)
          Qab = amn*Nh1*Nh1 + bmn*Nk1*Nk1
          J1 = 0
          DO I = 2, n
            IF ( (q(I)-epsq(I)).LT.Qab .AND. (q(I+1)+epsq(I+1)).GT.Qab ) THEN
              J1 = I
              J2 = I + 1
              GOTO 20
            ENDIF
          ENDDO
 20       IF ( J1.EQ.0 ) GOTO 100
 40       Epr = Epr1 + epsq(J1) + epsq(J2)
          IF ( (ABS(2.*Qab-q(J1)-q(J2)).LT.Epr) .AND. (J1.GE.5 .OR. (ABS(Nh1)+ABS(Nk1)).LE.3) ) THEN
            jrd = jrd + 1
            Dax = (q(J2)-q(J1))/2./Nh1/Nk1
            rmin(jrd) = Dax - (epsq(J1)+epsq(J2))/2.
            IF ( rmin(jrd).LT.0. ) rmin(jrd) = 0.
            rmax(jrd) = Dax + (epsq(J1)+epsq(J2))/2.
            Rmn = (rmax(jrd)+rmin(jrd))/2.
            J = 0
            DO I = 1, n
              DO Jh = 0, mh2
                Eri22 = Jh*Jh*(amax-amin)/2. + epsq(I)
                DO Jk = -mk2, mk2
                  IF ( (Jh+ABS(Jk)).LE.6 ) THEN
                    Eri2 = Eri22 + Jk*Jk*(bmax-bmin)/2. + Jh*Jk*(rmax(jrd)-rmin(jrd))/2.
                    IF ( ABS(amn*Jh*Jh+bmn*Jk*Jk+Rmn*Jh*Jk-q(I)).LT.Eri2 ) GOTO 50
                  ENDIF
                ENDDO
              ENDDO
              J = J + 1
              IF ( J.NE.1 ) THEN
                rcmax(jrd) = q(I)
                GOTO 60
              ENDIF
 50         ENDDO
            IF ( J.LT.2 ) rcmax(jrd) = q(n)
 60         iidd = 0
          ELSEIF ( (2.*Qab-q(J1)-q(J2)).GE.0. ) THEN
            J2 = J2 + 1
            IF ( J2.LE.n ) GOTO 40
            GOTO 100
          ENDIF
          J1 = J1 - 1
          IF ( J1.GE.3 ) GOTO 40
 100    ENDDO
      ENDDO
      END SUBROUTINE RELSPE
