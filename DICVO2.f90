!*==MODHKL1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE MODHKL2(Kram,Krap,Krcm,Krcp,Kracm,Kracp,Krbm,Krbp,Kamcp,Kapcm,Csm,Csp,Amoi,Aplu,Cmoi,Cplu,Kracms, &
     &                   Kracps,K1)
 
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      REAL :: Amoi, Aplu, Cmoi, Cplu, Csm, Csp
      INTEGER :: K1, Kamcp, Kapcm, Kracm, Kracms, Kracp, Kracps, Kram, Krap, Krbm, Krbp, Krcm, Krcp
      INTENT (IN) Amoi, Aplu, Cmoi, Cplu, Csm, Csp, K1, Kamcp, Kapcm, Kracm, Kracms, Kracp, Kracps,   &
     &            Kram, Krap, Krbm, Krbp, Krcm, Krcp
!
! Local variables
!
      REAL :: Dd
      REAL :: F
      INTEGER :: I, J, Jj, K, Kj, Kk, Kqqm, Kqqp, La1, Lc0, M, Mcarh, Mcark, Mcarl, Mm1, Mm2, Mm3,&
     &           Mp1, Mp2, Mp3, Mprohl, Mxl, Ne, Ng, Nx, Ny, Nz

      F(Nx,Ny,Nz,Dd,Ne,Ng) = Nx + Ny + Nz - Dd*Ne*Ng
      nt = 0
      Kk = K1 - 1
      DO I = 1, n
        Jj = irj(I,Kk)
        irj(I,K1) = 0
        DO J = 1, Jj
          M = ih(I,J,Kk)
          K = ik(I,J,Kk)
          Mxl = il(I,J,Kk)
          Mcarh = M*M
          Mcark = K*K
          Mcarl = Mxl*Mxl
          Mprohl = M*Mxl
          Mm1 = Mcarh*Kram
          Mp1 = Mcarh*Krap
          Mm2 = Mcark*Krbm
          Mp2 = Mcark*Krbp
          Mm3 = Mcarl*Krcm
          Mp3 = Mcarl*Krcp
          IF ( Mprohl.LT.0 ) THEN
            Lc0 = 0
            La1 = 0
            IF ( (M*Cplu/(Amoi*Mxl)).LE.Csp ) THEN
              Lc0 = 1
              IF ( (M*Cplu/(Mxl*Aplu)).GE.Csp ) THEN
                Kqqm = (coeff*(Mxl/Cplu)**2*(1-Csp**2)) + Mm2
                GOTO 40
              ENDIF
            ENDIF
            IF ( (M*Cmoi/(Mxl*Amoi)).LE.Csp ) THEN
              IF ( (M*Cmoi/(Mxl*Aplu)).GE.Csp ) THEN
                Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                GOTO 40
              ENDIF
            ENDIF
            IF ( (Mxl*Aplu/(M*Cmoi)).LE.Csp ) THEN
              La1 = 1
              IF ( (Mxl*Aplu/(M*Cplu)).GE.Csp ) THEN
                Kqqm = (coeff*(M/Aplu)**2*(1-Csp**2)) + Mm2
                GOTO 20
              ENDIF
            ENDIF
            IF ( (Mxl*Amoi/(M*Cmoi)).LE.Csp .AND. (Mxl*Amoi/(M*Cplu)).GE.Csp ) THEN
              Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
            ELSEIF ( Csm.LT.(Mxl*Aplu/(M*Cmoi)) ) THEN
              Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
              Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
              GOTO 60
            ELSEIF ( Csm.LE.(Mxl*Amoi/(M*Cplu)) ) THEN
              Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
              Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
              GOTO 60
            ELSEIF ( Lc0.EQ.0 ) THEN
              Kqqm = F(Mp1,Mm2,Mm3,Csp,Kamcp,Mprohl)
              GOTO 40
            ELSE
              Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
              IF ( La1.EQ.0 ) THEN
                Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                GOTO 60
              ELSE
                Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                GOTO 40
              ENDIF
            ENDIF
          ELSE
            Kqqm = Mm1 + Mm2 + Mm3 - (Mprohl*Kracm)
            Kqqp = Mp1 + Mp2 + Mp3 - (Mprohl*Kracp)
            GOTO 60
          ENDIF
 20       IF ( (1./Cmoi+1./Cplu).GE.((2.*M*Csm)/(Mxl*Amoi)) ) THEN
            Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
          ELSE
            Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
          ENDIF
          GOTO 60
 40       IF ( (1./Amoi+1./Aplu).GE.(2.*Mxl*Csm/(M*Cmoi)) ) THEN
            Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
          ELSE
            Kqqp = F(Mm1,Mp2,Mp3,Csm,Kapcm,Mprohl)
          ENDIF
 60       IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
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
99999 END SUBROUTINE MODHKL2



!*==CUBIQU.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!     ------------------------------------------------------------------
!     |                         C U B I Q U E                          |
!     ------------------------------------------------------------------
      SUBROUTINE CUBIQU(Na)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Na
      INTENT (IN) Na
!
! Local variables
!
      REAL :: Aare,     &
     &        Aw, Ram7, Rap7, Vap, Vtestm, Vtestp
      INTEGER :: I, Ia, Ia2, Ia3, Ia4, Ia5, Ia6, Ia7, Ind, Kram1, Kram2, Kram3, Kram4,       &
     &           Kram5, Kram6, Kram7, Krap1, Krap2, Krap3, Krap4, Krap5, Krap6, Krap7, Nrind

      DO I = 1, 7
        Ndich(I) = 0
      ENDDO
      aa = 1000.
      DO Ia = 1, Na
        Aw = Ia - 1
        Amoi1 = Amin + (Aw*pas)
        IF ( Amoi1.LE.Amax ) THEN
          Aplu1 = Amoi1 + pas
          Vtestp = Aplu1**3
          Vtestm = Amoi1**3
          IF ( Vtestp.GE.vinf .AND. Vtestm.LE.vsup ) THEN
            IF ( v.GE.Vtestm ) THEN
              mh = Aplu1/d(5)
              mh2 = Aplu1/d(n)
              Kram1 = coeff/(Amoi1*Amoi1)
              Krap1 = coeff/(Aplu1*Aplu1)
              CALL CUDHKL(Kram1,Krap1)
              IF ( nt.NE.-1 ) THEN
                DO I = 6, n
                  IF ( irj(I,1).EQ.0 ) GOTO 100
                ENDDO
                Ndich(1) = Ndich(1) + 1
                DO Ia2 = 1, 2
                  Aw = Ia2 - 1
                  Amoi2 = Amoi1 + (Aw*pas2)
                  IF ( ABS(Amoi2-aa).GE.0.25 ) THEN
                    Aplu2 = Amoi2 + pas2
                    Vtestm = Amoi2*Amoi2*Amoi2
                    IF ( vsup.LT.Vtestm ) GOTO 100
                    Vtestp = Aplu2*Aplu2*Aplu2
                    IF ( vinf.LE.Vtestp ) THEN
                      Kram2 = coeff/(Amoi2*Amoi2)
                      Krap2 = coeff/(Aplu2*Aplu2)
                      CALL CUDHKL1(Kram2,Krap2,2)
                      IF ( nt.NE.-1 ) THEN
                        Ndich(2) = Ndich(2) + 1
                        DO Ia3 = 1, 2
                          Aw = Ia3 - 1
                          Amoi3 = Amoi2 + (Aw*pas4)
                          IF ( ABS(Amoi3-aa).GE.0.25 ) THEN
                            Aplu3 = Amoi3 + pas4
                            Vtestm = Amoi3*Amoi3*Amoi3
                            IF ( vsup.LT.Vtestm ) GOTO 10
                            Vtestp = Aplu3*Aplu3*Aplu3
                            IF ( vinf.LE.Vtestp ) THEN
                              Kram3 = coeff/(Amoi3*Amoi3)
                              Krap3 = coeff/(Aplu3*Aplu3)
                              CALL CUDHKL1(Kram3,Krap3,3)
                              IF ( nt.NE.-1 ) THEN
                                Ndich(3) = Ndich(3) + 1
                                DO Ia4 = 1, 2
                                  Aw = Ia4 - 1
                                  Amoi4 = Amoi3 + (Aw*pas8)
                                  IF ( ABS(Amoi4-aa).GE.0.25 ) THEN
                                    Aplu4 = Amoi4 + pas8
                                    Vtestm = Amoi4*Amoi4*Amoi4
                                    IF ( vsup.LT.Vtestm ) GOTO 8
                                    Vtestp = Aplu4*Aplu4*Aplu4
                                    IF ( vinf.LE.Vtestp ) THEN
                                      Kram4 = coeff/(Amoi4*Amoi4)
                                      Krap4 = coeff/(Aplu4*Aplu4)
                                      CALL CUDHKL1(Kram4,Krap4,4)
                                      IF ( nt.NE.-1 ) THEN
                                        Ndich(4) = Ndich(4) + 1
                                        DO Ia5 = 1, 2
                                         Aw = Ia5 - 1
                                         Amoi5 = Amoi4 + (Aw*pas16)
                                         IF ( ABS(Amoi5-aa).GE.0.25 ) THEN
                                         Aplu5 = Amoi5 + pas16
                                         Vtestm = Amoi5*Amoi5*Amoi5
                                         IF ( vsup.LT.Vtestm ) GOTO 6
                                         Vtestp = Aplu5*Aplu5*Aplu5
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Kram5 = coeff/(Amoi5*Amoi5)
                                         Krap5 = coeff/(Aplu5*Aplu5)
                                         CALL CUDHKL1(Kram5,Krap5,5)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(5) = Ndich(5) + 1
                                         DO Ia6 = 1, 2
                                         Aw = Ia6 - 1
                                         Amoi6 = Amoi5 + (Aw*pas32)
                                         IF ( ABS(Amoi6-aa).GE.0.25 ) THEN
                                         Aplu6 = Amoi6 + pas32
                                         Vtestm = Amoi6*Amoi6*Amoi6
                                         IF ( vsup.LT.Vtestm ) GOTO 4
                                         Vtestp = Aplu6*Aplu6*Aplu6
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Kram6 = coeff/(Amoi6*Amoi6)
                                         Krap6 = coeff/(Aplu6*Aplu6)
                                         CALL CUDHKL1(Kram6,Krap6,6)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(6) = Ndich(6) + 1
                                         DO Ia7 = 1, 2
                                         Aw = Ia7 - 1
                                         Amoi7 = Amoi6 + (Aw*pas64)
                                         IF ( ABS(Amoi7-aa).GE.0.25 ) THEN
                                         Aplu7 = Amoi7 + pas64
                                         Vtestm = Amoi7*Amoi7*Amoi7
                                         IF ( vsup.LT.Vtestm ) GOTO 2
                                         Vtestp = Aplu7*Aplu7*Aplu7
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Ram7 = 1./(Amoi7*Amoi7)
                                         Rap7 = 1./(Aplu7*Aplu7)
                                         Kram7 = coeff*Ram7
                                         Krap7 = coeff*Rap7
                                         CALL CUDHKL1(Kram7,Krap7,7)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(7) = Ndich(7) + 1
                                         Aare = aa
                                         aa = Amoi7 + (pas64/2.)
                                         Vap = aa*aa*aa
                                    !     DO I = 1, n
                                   !        Jj = irj(I,7)
                                   !        DO J = 1, Jj
                                    !         Carl = ih(I,J,7)**2 + ik(I,J,7)**2 + il(I,J,7)**2
                                     !      ENDDO
                                      !   ENDDO
                                         Ind = 1
                                         Nrind = 1
                                         CALL AFFPAR(Ind,Nrind,Vap)
                                         IF (DICVOL_Error .NE. 0) RETURN
                                         IF ( fwolff.NE.-1000. ) THEN
                                         ELSE
                                         aa = Aare
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
 2                                       ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
 4                                      ENDDO
                                      ENDIF
                                    ENDIF
                                  ENDIF
 6                              ENDDO
                              ENDIF
                            ENDIF
                          ENDIF
 8                      ENDDO
                      ENDIF
                    ENDIF
                  ENDIF
 10             ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
 100  ENDDO
!      WRITE (iw,99003) (Ndich(I),I=1,7)
!99003 FORMAT (//,2X,'ITERATION NUMBER AT EACH DICHOTOMY LEVEL :',7I5)
      END SUBROUTINE CUBIQU
!*==CUDHKL.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE CUDHKL(Kram,Krap)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Kram, Krap
      INTENT (IN) Kram, Krap
!
! Local variables
!
      INTEGER :: I, Jj, K, Kqqm, Kqqp, M, Mcarh, Mcark, Mcarl, Mxl

      nt = 0
      DO I = 1, n
        irj(I,1) = 0
      ENDDO
      DO M = 1, mh
        Mcarh = M*M
        DO K = 0, M
          Mcark = K*K + Mcarh
          DO Mxl = 0, K
            Mcarl = Mxl*Mxl + Mcark
            Kqqm = Mcarl*Krap
            IF ( (Kqqm-kepsq(5)).GT.kq(5) ) GOTO 50
            Kqqp = Mcarl*Kram
            IF ( (Kqqp+kepsq(1)).GE.kq(1) ) THEN
              DO I = 1, 5
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
          ENDDO
 50     ENDDO
      ENDDO
      DO I = 1, 5
        IF ( irj(I,1).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 1, mh2
        Mcarh = M*M
        DO K = 0, M
          Mcark = K*K + Mcarh
          DO Mxl = 0, K
            Mcarl = Mxl*Mxl + Mcark
            Kqqp = Mcarl*Kram
            IF ( (Kqqp+kepsq(6)).GE.kq(6) ) THEN
              Kqqm = Mcarl*Krap
              IF ( (Kqqm-kepsq(n)).GT.kq(n) ) GOTO 100
              DO I = 6, n
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
          ENDDO
 100    ENDDO
      ENDDO
99999 END SUBROUTINE CUDHKL
!*==CUDHKL1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE CUDHKL1(Kram,Krap,K1)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: K1, Kram, Krap
      INTENT (IN) K1, Kram, Krap
!
! Local variables
!
      INTEGER :: I, J, Jj, K, Kj, Kk, Kqqm, Kqqp, M, Mcarl, Mxl

      nt = 0
      Kk = K1 - 1
      DO I = 1, n
        Jj = irj(I,Kk)
        irj(I,K1) = 0
        DO J = 1, Jj
          M = ih(I,J,Kk)
          K = ik(I,J,Kk)
          Mxl = il(I,J,Kk)
          Mcarl = M*M + K*K + Mxl*Mxl
          Kqqm = Mcarl*Krap
          Kqqp = Mcarl*Kram
          IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
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
99999 END SUBROUTINE CUDHKL1
!*==TETHEX.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!     ------------------------------------------------------------------
!     |          T E T R A G O N A L   E T   H E X A G O N A L         |
!     ------------------------------------------------------------------
      SUBROUTINE TETHEX(Nc,Ichoix)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Ichoix, Nc
      INTENT (IN) Nc
!
! Local variables
!
      REAL :: Aare,       &
     &         Atest, Aw, Ccre,   &
     &        Const, Contr, Contr1, Covol, Ctest, Cw,&
     &        Ra, Ram7, Rap7, Rcm7, Rcp7, Sam, Vap, Vtestm, Vtestp
      INTEGER :: I, Ia, Ia2, Ia3, Ia4, Ia5, Ia6, Ia7, Ic, Ic2, Ic3, Ic4, Ic5, Ic6, Ic7, Ind,  &
     &           Kram1, Kram2, Kram3, Kram4, Kram5, Kram6, Kram7, Krap1, Krap2, Krap3, Krap4,      &
     &           Krap5, Krap6, Krap7, Krcm1, Krcm2, Krcm3, Krcm4, Krcm5, Krcm6, Krcm7, Krcp1, Krcp2,   &
     &           Krcp3, Krcp4, Krcp5, Krcp6, Krcp7, Nrind

      DO I = 1, 7
        Ndich(I) = 0
      ENDDO
      aa = 1000.
      cc = 1000.
      Ra = 4./3.
      Const = coeff
      Contr = d(1) - epsil(1)
      Contr1 = Contr
      Covol = 1.
      IF ( Ichoix.GT.0 ) THEN
        Const = Ra*coeff
        Covol = 0.8660254
        Contr = Contr/Covol
      ENDIF
      DO Ia = 1, Nc
        Aw = Ia - 1
        Amoi1 = Cmin + (Aw*pas)
        Aplu1 = Amoi1 + pas
        mh = (Aplu1/d(5))
        mh2 = (Aplu1/d(n))
        Kram1 = Const/(Amoi1*Amoi1)
        Krap1 = Const/(Aplu1*Aplu1)
        DO Ic = 1, Nc
          Cw = Ic - 1
          Cmoi1 = Cmin + (Cw*pas)
          Cplu1 = Cmoi1 + pas
          IF ( Aplu1.GE.Contr .OR. Cplu1.GE.Contr1 ) THEN
            Vtestm = Amoi1*Amoi1*Cmoi1*Covol
            IF ( v.LT.Vtestm ) GOTO 100
            IF ( vsup.LT.Vtestm ) GOTO 100
            Vtestp = Aplu1*Aplu1*Cplu1*Covol
            IF ( vinf.LE.Vtestp ) THEN
              ml = (Cplu1/d(5))
              ml2 = (Cplu1/d(n))
              Krcm1 = coeff/(Cmoi1*Cmoi1)
              Krcp1 = coeff/(Cplu1*Cplu1)
              CALL TEDHKL(Kram1,Krap1,Krcm1,Krcp1,Ichoix)
              IF ( nt.NE.-1 ) THEN
                DO I = 6, n
                  IF ( irj(I,1).EQ.0 ) GOTO 50
                ENDDO
                Ndich(1) = Ndich(1) + 1
                DO Ia2 = 1, 2
                  Aw = Ia2 - 1
                  Amoi2 = Amoi1 + (Aw*pas2)
                  Aplu2 = Amoi2 + pas2
                  Kram2 = Const/(Amoi2*Amoi2)
                  Krap2 = Const/(Aplu2*Aplu2)
                  DO Ic2 = 1, 2
                    Cw = Ic2 - 1
                    Cmoi2 = Cmoi1 + (Cw*pas2)
                    Atest = ABS(Amoi2-aa)
                    Ctest = ABS(Cmoi2-cc)
                    IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                      Vtestm = Amoi2*Amoi2*Cmoi2*Covol
                      IF ( vsup.LT.Vtestm ) GOTO 12
                      Cplu2 = Cmoi2 + pas2
                      Vtestp = Aplu2*Aplu2*Cplu2*Covol
                      IF ( vinf.LE.Vtestp ) THEN
                        Krcm2 = coeff/(Cmoi2*Cmoi2)
                        Krcp2 = coeff/(Cplu2*Cplu2)
                        CALL TEDHKL1(Kram2,Krap2,Krcm2,Krcp2,Ichoix,2)
                        IF ( nt.NE.-1 ) THEN
                          Ndich(2) = Ndich(2) + 1
                          DO Ia3 = 1, 2
                            Aw = Ia3 - 1
                            Amoi3 = Amoi2 + (Aw*pas4)
                            Aplu3 = Amoi3 + pas4
                            Kram3 = Const/(Amoi3*Amoi3)
                            Krap3 = Const/(Aplu3*Aplu3)
                            DO Ic3 = 1, 2
                              Cw = Ic3 - 1
                              Cmoi3 = Cmoi2 + (Cw*pas4)
                              Atest = ABS(Amoi3-aa)
                              Ctest = ABS(Cmoi3-cc)
                              IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                Vtestm = Amoi3*Amoi3*Cmoi3*Covol
                                IF ( vsup.LT.Vtestm ) GOTO 10
                                Cplu3 = Cmoi3 + pas4
                                Vtestp = Aplu3*Aplu3*Cplu3*Covol
                                IF ( vinf.LE.Vtestp ) THEN
                                  Krcm3 = coeff/(Cmoi3*Cmoi3)
                                  Krcp3 = coeff/(Cplu3*Cplu3)
                                  CALL TEDHKL1(Kram3,Krap3,Krcm3,Krcp3,Ichoix,3)
                                  IF ( nt.NE.-1 ) THEN
                                    Ndich(3) = Ndich(3) + 1
                                    DO Ia4 = 1, 2
                                      Aw = Ia4 - 1
                                      Amoi4 = Amoi3 + (Aw*pas8)
                                      Aplu4 = Amoi4 + pas8
                                      Kram4 = Const/(Amoi4*Amoi4)
                                      Krap4 = Const/(Aplu4*Aplu4)
                                      DO Ic4 = 1, 2
                                        Cw = Ic4 - 1
                                        Cmoi4 = Cmoi3 + (Cw*pas8)
                                        Atest = ABS(Amoi4-aa)
                                        Ctest = ABS(Cmoi4-cc)
                                        IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Vtestm = Amoi4*Amoi4*Cmoi4*Covol
                                         IF ( vsup.LT.Vtestm ) GOTO 8
                                         Cplu4 = Cmoi4 + pas8
                                         Vtestp = Aplu4*Aplu4*Cplu4*Covol
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm4 = coeff/(Cmoi4*Cmoi4)
                                         Krcp4 = coeff/(Cplu4*Cplu4)
                                         CALL TEDHKL1(Kram4,Krap4,Krcm4,Krcp4,Ichoix,4)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(4) = Ndich(4) + 1
                                         DO Ia5 = 1, 2
                                         Aw = Ia5 - 1
                                         Amoi5 = Amoi4 + (Aw*pas16)
                                         Aplu5 = Amoi5 + pas16
                                         Kram5 = Const/(Amoi5*Amoi5)
                                         Krap5 = Const/(Aplu5*Aplu5)
                                         DO Ic5 = 1, 2
                                         Cw = Ic5 - 1
                                         Cmoi5 = Cmoi4 + (Cw*pas16)
                                         Atest = ABS(Amoi5-aa)
                                         Ctest = ABS(Cmoi5-cc)
                                         IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Vtestm = Amoi5*Amoi5*Cmoi5*Covol
                                         IF ( vsup.LT.Vtestm ) GOTO 6
                                         Cplu5 = Cmoi5 + pas16
                                         Vtestp = Aplu5*Aplu5*Cplu5*Covol
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm5 = coeff/(Cmoi5*Cmoi5)
                                         Krcp5 = coeff/(Cplu5*Cplu5)
                                         CALL TEDHKL1(Kram5,Krap5,Krcm5,Krcp5,Ichoix,5)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(5) = Ndich(5) + 1
                                         DO Ia6 = 1, 2
                                         Aw = Ia6 - 1
                                         Amoi6 = Amoi5 + (Aw*pas32)
                                         Aplu6 = Amoi6 + pas32
                                         Kram6 = Const/(Amoi6*Amoi6)
                                         Krap6 = Const/(Aplu6*Aplu6)
                                         DO Ic6 = 1, 2
                                         Cw = Ic6 - 1
                                         Cmoi6 = Cmoi5 + (Cw*pas32)
                                         Atest = ABS(Amoi6-aa)
                                         Ctest = ABS(Cmoi6-cc)
                                         IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Vtestm = Amoi6*Amoi6*Cmoi6*Covol
                                         IF ( vsup.LT.Vtestm ) GOTO 4
                                         Cplu6 = Cmoi6 + pas32
                                         Vtestp = Aplu6*Aplu6*Cplu6*Covol
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm6 = coeff/(Cmoi6*Cmoi6)
                                         Krcp6 = coeff/(Cplu6*Cplu6)
                                         CALL TEDHKL1(Kram6,Krap6,Krcm6,Krcp6,Ichoix,6)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(6) = Ndich(6) + 1
                                         DO Ia7 = 1, 2
                                         Aw = Ia7 - 1
                                         Amoi7 = Amoi6 + (Aw*pas64)
                                         Aplu7 = Amoi7 + pas64
                                         Ram7 = 1./(Amoi7*Amoi7)
                                         Rap7 = 1./(Aplu7*Aplu7)
                                         Kram7 = Const*Ram7
                                         Krap7 = Const*Rap7
                                         DO Ic7 = 1, 2
                                         Cw = Ic7 - 1
                                         Cmoi7 = Cmoi6 + (Cw*pas64)
                                         Atest = ABS(Amoi7-aa)
                                         Ctest = ABS(Cmoi7-cc)
                                         IF ( Atest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Vtestm = Amoi7*Amoi7*Cmoi7*Covol
                                         IF ( vsup.LT.Vtestm ) GOTO 2
                                         Cplu7 = Cmoi7 + pas64
                                         Vtestp = Aplu7*Aplu7*Cplu7*Covol
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Rcm7 = 1./(Cmoi7*Cmoi7)
                                         Rcp7 = 1./(Cplu7*Cplu7)
                                         Krcm7 = coeff*Rcm7
                                         Krcp7 = coeff*Rcp7
                                         CALL TEDHKL1(Kram7,Krap7,Krcm7,Krcp7,Ichoix,7)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(7) = Ndich(7) + 1
                                         Aare = aa
                                         Ccre = cc
                                         aa = Amoi7 + (pas64/2.)
                                         cc = Cmoi7 + (pas64/2.)
                                         Vap = aa*aa*cc
                                         IF ( Ichoix.EQ.1 ) Vap = Vap*SQRT(3.)/2.
                                         Sam = 0.
                                         IF ( Ichoix.EQ.1 ) Sam = 1.
                                    !     DO I = 1, n
                                    !     Jj = irj(I,7)
                                     !      DO J = 1, Jj
                                    !         Nphk = Sam*ih(I,J,7)*ik(I,J,7)
                                     !      ENDDO
                                    !     ENDDO
                                         Ind = 2
                                         Nrind = 2
                                         IF ( Ichoix.GT.0 ) Ind = 3
                                         CALL AFFPAR(Ind,Nrind,Vap)
                                         IF (DICVOL_Error .NE. 0) RETURN
                                         IF ( fwolff.NE.-1000. ) THEN
                                         ELSE
                                         aa = Aare
                                         cc = Ccre
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 2                                       ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 4                                       ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 6                                       ENDDO
                                         ENDIF
                                         ENDIF
                                        ENDIF
                                      ENDDO
 8                                  ENDDO
                                  ENDIF
                                ENDIF
                              ENDIF
                            ENDDO
 10                       ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDDO
 12             ENDDO
              ENDIF
            ENDIF
          ENDIF
 50     ENDDO
 100  ENDDO
!      WRITE (iw,99003) (Ndich(I),I=1,7)
!99003 FORMAT (//,2X,'ITERATION NUMBER AT EACH DICHOTOMY LEVEL :',7I5)
      END SUBROUTINE TETHEX
!*==TEDHKL.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE TEDHKL(Kram,Krap,Krcm,Krcp,Ichoix)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Ichoix, Kram, Krap, Krcm, Krcp
      INTENT (IN) Ichoix, Kram, Krap, Krcm, Krcp
!
! Local variables
!
      INTEGER :: I, Jj, K, Kqqm, Kqqp, Ksom, M, Mcarh, Mcark, Mcarl, Mxl

      nt = 0
      DO I = 1, n
        irj(I,1) = 0
      ENDDO
      DO M = 0, mh
        Mcarh = M*M
        DO K = 0, M
          Mcark = Mcarh + K*K
          DO Mxl = 0, ml
            Mcarl = Mxl*Mxl
            IF ( Ichoix.LE.0 ) THEN
              Kqqm = Mcark*Krap + Mcarl*Krcp
              Kqqp = Mcark*Kram + Mcarl*Krcm
            ELSE
              Ksom = Mcark + M*K
              Kqqm = Ksom*Krap + Mcarl*Krcp
              Kqqp = Ksom*Kram + Mcarl*Krcm
            ENDIF
            IF ( (Kqqm-kepsq(5)).GT.kq(5) ) GOTO 50
            IF ( (Kqqp+kepsq(1)).GE.kq(1) ) THEN
              DO I = 1, 5
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
          ENDDO
 50     ENDDO
      ENDDO
      DO I = 1, 5
        IF ( irj(I,1).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 0, mh2
        Mcarh = M*M
        DO K = 0, M
          Mcark = Mcarh + K*K
          DO Mxl = 0, ml2
            Mcarl = Mxl*Mxl
            IF ( Ichoix.LE.0 ) THEN
              Kqqm = (Mcark*Krap) + (Mcarl*Krcp)
              Kqqp = (Mcark*Kram) + (Mcarl*Krcm)
            ELSE
              Ksom = Mcark + M*K
              Kqqm = (Ksom*Krap) + (Mcarl*Krcp)
              Kqqp = (Ksom*Kram) + (Mcarl*Krcm)
            ENDIF
            IF ( (Kqqm-kepsq(n)).GT.kq(n) ) GOTO 100
            IF ( (Kqqp+kepsq(6)).GE.kq(6) ) THEN
              DO I = 6, n
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
          ENDDO
 100    ENDDO
      ENDDO
99999 END SUBROUTINE TEDHKL
!*==TEDHKL1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE TEDHKL1(Kram,Krap,Krcm,Krcp,Ichoix,K1)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Ichoix, K1, Kram, Krap, Krcm, Krcp
      INTENT (IN) Ichoix, K1, Kram, Krap, Krcm, Krcp
!
! Local variables
!
      INTEGER :: I, J, Jj, K, Kj, Kk, Kqqm, Kqqp, Ksom, M, Mcark, Mcarl, Mxl

      nt = 0
      Kk = K1 - 1
      DO I = 1, n
        Jj = irj(I,Kk)
        irj(I,K1) = 0
        DO J = 1, Jj
          M = ih(I,J,Kk)
          K = ik(I,J,Kk)
          Mxl = il(I,J,Kk)
          Mcark = M*M + K*K
          Mcarl = Mxl*Mxl
          IF ( Ichoix.LE.0 ) THEN
            Kqqm = (Mcark*Krap) + (Mcarl*Krcp)
            Kqqp = (Mcark*Kram) + (Mcarl*Krcm)
          ELSE
            Ksom = Mcark + M*K
            Kqqm = Ksom*Krap + Mcarl*Krcp
            Kqqp = Ksom*Kram + Mcarl*Krcm
          ENDIF
          IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
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
99999 END SUBROUTINE TEDHKL1
!*==ORTHOR.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!     ------------------------------------------------------------------
!     |                   O R T H O R H O M B I Q U E                  |
!     ------------------------------------------------------------------
      SUBROUTINE ORTHOR(Na,Nb,Nc)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Na, Nb, Nc
      INTENT (IN) Na, Nb, Nc
!
! Local variables
!
      REAL :: Aare,  Atest, Aw, Bbre,  &
     &        Btest, Bw, Ccre, Ctest, Cw, &
     &        Ram7, Rap7, Rbm7, Rbp7, Rcm7, Rcp7, Vap, Vtestm, Vtestp
      INTEGER :: I, Ia, Ia2, Ia3, Ia4, Ia5, Ia6, Ia7, Ib, Ib2, Ib3, Ib4, Ib5, Ib6, Ib7, Ic, Ic2,  &
     &           Ic3, Ic4, Ic5, Ic6, Ic7, Ind, Kram1, Kram2, Kram3, Kram4, Kram5, Kram6,      &
     &           Kram7, Krap1, Krap2, Krap3, Krap4, Krap5, Krap6, Krap7, Krbm1, Krbm2, Krbm3, Krbm4,   &
     &           Krbm5, Krbm6, Krbm7, Krbp1, Krbp2, Krbp3, Krbp4, Krbp5, Krbp6, Krbp7, Krcm1, Krcm2,   &
     &           Krcm3, Krcm4, Krcm5, Krcm6, Krcm7, Krcp1, Krcp2, Krcp3, Krcp4, Krcp5, Krcp6, Krcp7,   &
     &           Nrind

      DO I = 1, 7
        Ndich(I) = 0
      ENDDO
      aa = 1000.
      bb = 1000.
      cc = 1000.
      DO Ia = 1, Na
        Aw = Ia - 1
        Amoi1 = Amin + (Aw*pas)
        Aplu1 = Amoi1 + pas
        mh = (Aplu1/d(5))
        mh2 = (Aplu1/d(n))
        Kram1 = coeff/(Amoi1*Amoi1)
        Krap1 = coeff/(Aplu1*Aplu1)
        DO Ib = 1, Nb
          Bw = Ib - 1
          Bmoi1 = Bmin + (Bw*pas)
          IF ( Aplu1.LT.Bmoi1 ) GOTO 100
          Bplu1 = Bmoi1 + pas
          mk = (Bplu1/d(5))
          mk2 = (Bplu1/d(n))
          Krbm1 = coeff/(Bmoi1*Bmoi1)
          Krbp1 = coeff/(Bplu1*Bplu1)
          DO Ic = 1, Nc
            Cw = Ic - 1
            Cmoi1 = Cmin + (Cw*pas)
            Cplu1 = Cmoi1 + pas
            IF ( Bplu1.LT.Cmoi1 ) GOTO 50
            IF ( Cplu1.GT.Cmin ) THEN
              Vtestm = Amoi1*Bmoi1*Cmoi1
              IF ( v.LT.Vtestm ) GOTO 50
              IF ( vsup.LT.Vtestm ) GOTO 50
              Vtestp = Aplu1*Bplu1*Cplu1
              IF ( vinf.LE.Vtestp ) THEN
                ml = (Cplu1/d(5))
                ml2 = (Cplu1/d(n))
                Krcm1 = coeff/(Cmoi1*Cmoi1)
                Krcp1 = coeff/(Cplu1*Cplu1)
                CALL ORDHKL(Kram1,Krap1,Krbm1,Krbp1,Krcm1,Krcp1)
                IF ( nt.NE.-1 ) THEN
                  DO I = 6, n
                    IF ( irj(I,1).EQ.0 ) GOTO 20
                  ENDDO
                  Ndich(1) = Ndich(1) + 1
                  DO Ia2 = 1, 2
                    Aw = Ia2 - 1
                    Amoi2 = Amoi1 + (Aw*pas2)
                    Aplu2 = Amoi2 + pas2
                    Kram2 = coeff/(Amoi2*Amoi2)
                    Krap2 = coeff/(Aplu2*Aplu2)
                    DO Ib2 = 1, 2
                      Bw = Ib2 - 1
                      Bmoi2 = Bmoi1 + (Bw*pas2)
                      Bplu2 = Bmoi2 + pas2
                      Krbm2 = coeff/(Bmoi2*Bmoi2)
                      Krbp2 = coeff/(Bplu2*Bplu2)
                      DO Ic2 = 1, 2
                        Cw = Ic2 - 1
                        Cmoi2 = Cmoi1 + (Cw*pas2)
                        Atest = ABS(Amoi2-aa)
                        Btest = ABS(Bmoi2-bb)
                        Ctest = ABS(Cmoi2-cc)
                        IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                          Cplu2 = Cmoi2 + pas2
                          Vtestm = Amoi2*Bmoi2*Cmoi2
                          IF ( vsup.LT.Vtestm ) GOTO 14
                          Vtestp = Aplu2*Bplu2*Cplu2
                          IF ( vinf.LE.Vtestp ) THEN
                            Krcm2 = coeff/(Cmoi2*Cmoi2)
                            Krcp2 = coeff/(Cplu2*Cplu2)
                            CALL ORDHKL1(Kram2,Krap2,Krbm2,Krbp2,Krcm2,Krcp2,2)
                            IF ( nt.NE.-1 ) THEN
                              Ndich(2) = Ndich(2) + 1
                              DO Ia3 = 1, 2
                                Aw = Ia3 - 1
                                Amoi3 = Amoi2 + (Aw*pas4)
                                Aplu3 = Amoi3 + pas4
                                Kram3 = coeff/(Amoi3*Amoi3)
                                Krap3 = coeff/(Aplu3*Aplu3)
                                DO Ib3 = 1, 2
                                  Bw = Ib3 - 1
                                  Bmoi3 = Bmoi2 + (Bw*pas4)
                                  Bplu3 = Bmoi3 + pas4
                                  Krbm3 = coeff/(Bmoi3*Bmoi3)
                                  Krbp3 = coeff/(Bplu3*Bplu3)
                                  DO Ic3 = 1, 2
                                    Cw = Ic3 - 1
                                    Cmoi3 = Cmoi2 + (Cw*pas4)
                                    Atest = ABS(Amoi3-aa)
                                    Btest = ABS(Bmoi3-bb)
                                    Ctest = ABS(Cmoi3-cc)
                                    IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                      Cplu3 = Cmoi3 + pas4
                                      Vtestm = Amoi3*Bmoi3*Cmoi3
                                      IF ( vsup.LT.Vtestm ) GOTO 12
                                      Vtestp = Aplu3*Bplu3*Cplu3
                                      IF ( vinf.LE.Vtestp ) THEN
                                        Krcm3 = coeff/(Cmoi3*Cmoi3)
                                        Krcp3 = coeff/(Cplu3*Cplu3)
                                        CALL ORDHKL1(Kram3,Krap3,Krbm3,Krbp3,Krcm3,Krcp3,3)
                                        IF ( nt.NE.-1 ) THEN
                                         Ndich(3) = Ndich(3) + 1
                                         DO Ia4 = 1, 2
                                         Aw = Ia4 - 1
                                         Amoi4 = Amoi3 + (Aw*pas8)
                                         Aplu4 = Amoi4 + pas8
                                         Kram4 = coeff/(Amoi4*Amoi4)
                                         Krap4 = coeff/(Aplu4*Aplu4)
                                         DO Ib4 = 1, 2
                                         Bw = Ib4 - 1
                                         Bmoi4 = Bmoi3 + (Bw*pas8)
                                         Bplu4 = Bmoi4 + pas8
                                         Krbm4 = coeff/(Bmoi4*Bmoi4)
                                         Krbp4 = coeff/(Bplu4*Bplu4)
                                         DO Ic4 = 1, 2
                                         Cw = Ic4 - 1
                                         Cmoi4 = Cmoi3 + (Cw*pas8)
                                         Atest = ABS(Amoi4-aa)
                                         Btest = ABS(Bmoi4-bb)
                                         Ctest = ABS(Cmoi4-cc)
                                         IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Cplu4 = Cmoi4 + pas8
                                         Vtestm = Amoi4*Bmoi4*Cmoi4
                                         IF ( vsup.LT.Vtestm ) GOTO 10
                                         Vtestp = Aplu4*Bplu4*Cplu4
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm4 = coeff/(Cmoi4*Cmoi4)
                                         Krcp4 = coeff/(Cplu4*Cplu4)
                                         CALL ORDHKL1(Kram4,Krap4,Krbm4,Krbp4,Krcm4,Krcp4,4)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(4) = Ndich(4) + 1
                                         DO Ia5 = 1, 2
                                         Aw = Ia5 - 1
                                         Amoi5 = Amoi4 + (Aw*pas16)
                                         Aplu5 = Amoi5 + pas16
                                         Kram5 = coeff/(Amoi5*Amoi5)
                                         Krap5 = coeff/(Aplu5*Aplu5)
                                         DO Ib5 = 1, 2
                                         Bw = Ib5 - 1
                                         Bmoi5 = Bmoi4 + (Bw*pas16)
                                         IF ( Bmoi5.GT.Aplu5 ) GOTO 8
                                         Bplu5 = Bmoi5 + pas16
                                         Krbm5 = coeff/(Bmoi5*Bmoi5)
                                         Krbp5 = coeff/(Bplu5*Bplu5)
                                         DO Ic5 = 1, 2
                                         Cw = Ic5 - 1
                                         Cmoi5 = Cmoi4 + (Cw*pas16)
                                         IF ( Cmoi5.GT.Bplu5 ) GOTO 6
                                         Atest = ABS(Amoi5-aa)
                                         Btest = ABS(Bmoi5-bb)
                                         Ctest = ABS(Cmoi5-cc)
                                         IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Cplu5 = Cmoi5 + pas16
                                         Vtestm = Amoi5*Bmoi5*Cmoi5
                                         IF ( vsup.LT.Vtestm ) GOTO 6
                                         Vtestp = Aplu5*Bplu5*Cplu5
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm5 = coeff/(Cmoi5*Cmoi5)
                                         Krcp5 = coeff/(Cplu5*Cplu5)
                                         CALL ORDHKL1(Kram5,Krap5,Krbm5,Krbp5,Krcm5,Krcp5,5)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(5) = Ndich(5) + 1
                                         DO Ia6 = 1, 2
                                         Aw = Ia6 - 1
                                         Amoi6 = Amoi5 + (Aw*pas32)
                                         Aplu6 = Amoi6 + pas32
                                         Kram6 = coeff/(Amoi6*Amoi6)
                                         Krap6 = coeff/(Aplu6*Aplu6)
                                         DO Ib6 = 1, 2
                                         Bw = Ib6 - 1
                                         Bmoi6 = Bmoi5 + (Bw*pas32)
                                         Bplu6 = Bmoi6 + pas32
                                         Krbm6 = coeff/(Bmoi6*Bmoi6)
                                         Krbp6 = coeff/(Bplu6*Bplu6)
                                         DO Ic6 = 1, 2
                                         Cw = Ic6 - 1
                                         Cmoi6 = Cmoi5 + (Cw*pas32)
                                         Atest = ABS(Amoi6-aa)
                                         Btest = ABS(Bmoi6-bb)
                                         Ctest = ABS(Cmoi6-cc)
                                         IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Cplu6 = Cmoi6 + pas32
                                         Vtestm = Amoi6*Bmoi6*Cmoi6
                                         IF ( vsup.LT.Vtestm ) GOTO 4
                                         Vtestp = Aplu6*Bplu6*Cplu6
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm6 = coeff/(Cmoi6*Cmoi6)
                                         Krcp6 = coeff/(Cplu6*Cplu6)
                                         CALL ORDHKL1(Kram6,Krap6,Krbm6,Krbp6,Krcm6,Krcp6,6)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(6) = Ndich(6) + 1
                                         DO Ia7 = 1, 2
                                         Aw = Ia7 - 1
                                         Amoi7 = Amoi6 + (Aw*pas64)
                                         Aplu7 = Amoi7 + pas64
                                         Ram7 = 1./(Amoi7*Amoi7)
                                         Rap7 = 1./(Aplu7*Aplu7)
                                         Kram7 = coeff*Ram7
                                         Krap7 = coeff*Rap7
                                         DO Ib7 = 1, 2
                                         Bw = Ib7 - 1
                                         Bmoi7 = Bmoi6 + (Bw*pas64)
                                         Bplu7 = Bmoi7 + pas64
                                         Rbm7 = 1./(Bmoi7*Bmoi7)
                                         Rbp7 = 1./(Bplu7*Bplu7)
                                         Krbm7 = coeff*Rbm7
                                         Krbp7 = coeff*Rbp7
                                         DO Ic7 = 1, 2
                                         Cw = Ic7 - 1
                                         Cmoi7 = Cmoi6 + (Cw*pas64)
                                         Atest = ABS(Amoi7-aa)
                                         Btest = ABS(Bmoi7-bb)
                                         Ctest = ABS(Cmoi7-cc)
                                         IF ( Atest.GE.0.25 .OR. Btest.GE.0.25 .OR. Ctest.GE.0.25 ) THEN
                                         Cplu7 = Cmoi7 + pas64
                                         Vtestm = Amoi7*Bmoi7*Cmoi7
                                         IF ( vsup.LT.Vtestm ) GOTO 2
                                         Vtestp = Aplu7*Bplu7*Cplu7
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Rcm7 = 1./(Cmoi7*Cmoi7)
                                         Rcp7 = 1./(Cplu7*Cplu7)
                                         Krcm7 = coeff*Rcm7
                                         Krcp7 = coeff*Rcp7
                                         CALL ORDHKL1(Kram7,Krap7,Krbm7,Krbp7,Krcm7,Krcp7,7)
                                         IF ( nt.NE.-1 ) THEN
                                         Ndich(7) = Ndich(7) + 1
                                         Aare = aa
                                         Bbre = bb
                                         Ccre = cc
                                         aa = Amoi7 + (pas64/2.)
                                         bb = Bmoi7 + (pas64/2.)
                                         cc = Cmoi7 + (pas64/2.)
                                         Vap = aa*bb*cc
                                         Ind = 4
                                         Nrind = 3
                                         CALL AFFPAR(Ind,Nrind,Vap)
                                         IF (DICVOL_Error .NE. 0) RETURN
                                         IF ( fwolff.NE.-1000. ) THEN
                                         ELSE
                                         aa = Aare
                                         bb = Bbre
                                         cc = Ccre
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 2                                       ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 4                                       ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 6                                       ENDDO
 8                                       ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDDO
 10                                      ENDDO
                                         ENDDO
                                        ENDIF
                                      ENDIF
                                    ENDIF
                                  ENDDO
 12                             ENDDO
                              ENDDO
                            ENDIF
                          ENDIF
                        ENDIF
                      ENDDO
 14                 ENDDO
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
 20       ENDDO
 50     ENDDO
 100  ENDDO
!      WRITE (iw,99003) (Ndich(I),I=1,7)
!99003 FORMAT (//,2X,'ITERATION NUMBER AT EACH DICHOTOMY LEVEL :',7I5)
      END SUBROUTINE ORTHOR
!*==ORDHKL.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE ORDHKL(Kram,Krap,Krbm,Krbp,Krcm,Krcp)

      USE DICVAR

      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Kram, Krap, Krbm, Krbp, Krcm, Krcp
      INTENT (IN) Kram, Krap, Krbm, Krbp, Krcm, Krcp
!
! Local variables
!
      INTEGER :: I, Jj, K, Kborin, Kborsu, Kqqm, Kqqp, M, M1, M2, Mcarh, Mcark, Mcarl, Mp1, Mp2, Mxl

      nt = 0
      DO I = 1, n
        irj(I,1) = 0
      ENDDO
      DO M = 0, mh
        Mcarh = M*M
        M1 = Mcarh*Kram
        Mp1 = Mcarh*Krap
        DO K = 0, mk
          Mcark = K*K
          M2 = (Mcark*Krbm) + M1
          Mp2 = (Mcark*Krbp) + Mp1
          Kborin = Mp2 - kepsq(5)
          IF ( Kborin.GT.kq(5) ) GOTO 100
          DO Mxl = 0, ml
            Mcarl = Mxl*Mxl
            Kqqm = (Mcarl*Krcp) + Mp2
            Kborin = Kqqm - kepsq(5)
            IF ( Kborin.GT.kq(5) ) GOTO 50
            Kqqp = (Mcarl*Krcm) + M2
            Kborsu = Kqqp + kepsq(1)
            IF ( kq(1).LE.Kborsu ) THEN
              DO I = 1, 5
                Kborin = Kqqm - kepsq(I)
                IF ( Kborin.LE.kq(I) ) THEN
                  Kborsu = Kqqp + kepsq(I)
                  IF ( Kborsu.GE.kq(I) ) THEN
                    irj(I,1) = irj(I,1) + 1
                    Jj = irj(I,1)
                    ih(I,Jj,1) = M
                    ik(I,Jj,1) = K
                    il(I,Jj,1) = Mxl
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
 50     ENDDO
 100  ENDDO
      DO I = 1, 5
        IF ( irj(I,1).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 0, mh2
        Mcarh = M*M
        M1 = Mcarh*Kram
        Mp1 = Mcarh*Krap
        DO K = 0, mk2
          Mcark = K*K
          M2 = (Mcark*Krbm) + M1
          Mp2 = (Mcark*Krbp) + Mp1
          Kborin = Mp2 - kepsq(n)
          IF ( Kborin.GT.kq(n) ) GOTO 200
          DO Mxl = 0, ml2
            Mcarl = Mxl*Mxl
            Kqqp = (Mcarl*Krcm) + M2
            Kborsu = Kqqp + kepsq(6)
            IF ( Kborsu.GE.kq(6) ) THEN
              Kqqm = (Mcarl*Krcp) + Mp2
              Kborin = Kqqm - kepsq(n)
              IF ( Kborin.GT.kq(n) ) GOTO 150
              DO I = 6, n
                Kborin = Kqqm - kepsq(I)
                IF ( Kborin.LE.kq(I) ) THEN
                  Kborsu = Kqqp + kepsq(I)
                  IF ( Kborsu.GE.kq(I) ) THEN
                    irj(I,1) = irj(I,1) + 1
                    Jj = irj(I,1)
                    ih(I,Jj,1) = M
                    ik(I,Jj,1) = K
                    il(I,Jj,1) = Mxl
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
          ENDDO
 150    ENDDO
 200  ENDDO
99999 END SUBROUTINE ORDHKL
!*==ORDHKL1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE ORDHKL1(Kram,Krap,Krbm,Krbp,Krcm,Krcp,K1)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: K1, Kram, Krap, Krbm, Krbp, Krcm, Krcp
      INTENT (IN) K1, Kram, Krap, Krbm, Krbp, Krcm, Krcp
!
! Local variables
!
      INTEGER :: I, J, Jj, K, Kj, Kk, Kqqm, Kqqp, M, Mxl

      nt = 0
      Kk = K1 - 1
      DO I = 1, n
        Jj = irj(I,Kk)
        irj(I,K1) = 0
        DO J = 1, Jj
          M = ih(I,J,Kk)
          K = ik(I,J,Kk)
          Mxl = il(I,J,Kk)
          Kqqp = Mxl*Mxl*Krcm + K*K*Krbm + M*M*Kram
          Kqqm = Mxl*Mxl*Krcp + K*K*Krbp + M*M*Krap
          IF ( (Kqqm-kepsq(I)).LE.kq(I) ) THEN
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
99999 END SUBROUTINE ORDHKL1
!*==MONOC1.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
!     ------------------------------------------------------------------
!     | 		   M O N O C L I N I C 1		       |
!     ------------------------------------------------------------------
      SUBROUTINE MONOC1(Na,Nb,Nc)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER :: Na, Nb, Nc
      INTENT (IN) Na, Nb, Nc
!
! Local variables
!
      REAL :: Aare, Atest, Bbetm, Bbetp, Bbre, Bebere, Beinf1, Beinf2, Beinf3, Beinf4, Beinf5, &
     &        Beinf7, Besup1, Besup2, Besup3, Besup4, Besup5, Besup6, Besup7, Betest, Btest,      &
     &        Ccre,  Csm1, Csm2, Csm3, Csm4, Csm5, Csm6, Csm7, Csp1, Csp2, Csp3,&
     &        Csp4, Csp5, Csp6, Csp7, Ctest, Da
      REAL :: Db, Dc, Pab16, Pab2, Pab32, Pab4, Pab8, Pasbe, Pp, Vap, Vtestm, Vtestp
      INTEGER :: I, Ia, Ia2, Ia3, Ia4, Ia5, Ia6, Ia7, Ib, Ib2, Ib3, Ib4, Ib5, Ib6, Ib7, Ibet1,     &
     &           Ibet2, Ibet3, Ibet4, Ibet5, Ibet6, Ibet7, Ic, Ic2, Ic3, Ic4, Ic5, Ic6, Ic7, Ind, J,&
     &           Kamcp, Kapcm, Kracm, Kracms, Kracp, Kracps, Kram1, Kram2, Kram3, Kram4, Kram5,    &
     &           Kram6, Kram7, Krap1, Krap2, Krap3, Krap4, Krap5, Krap6, Krap7, Krbm1, Krbm2, Krbm3,   &
     &           Krbm4, Krbm5, Krbm6, Krbm7, Krbp1, Krbp2, Krbp3, Krbp4, Krbp5, Krbp6, Krbp7, Krcm,    &
     &           Krcp, Nbeta, Nrind

      DO I = 1, 7
        Ndich(I) = 0
      ENDDO
      aa = 100.
      bb = 100.
      cc = 100.
      bebe = 3.
      Pasbe = pirad*5.00
      Nbeta = ((Bemax-Bemin)/Pasbe)
      Pab2 = Pasbe/2.
      Pab4 = Pasbe/4.
      Pab8 = Pasbe/8.
      Pab16 = Pasbe/16.
      Pab32 = Pasbe/32.
      pab64 = Pasbe/64.
      DO Ibet1 = 0, Nbeta
        Beinf1 = Bemin + (Ibet1*Pasbe)
        IF ( Beinf1.GE.(Bemax-1E-12) ) GOTO 300
        Besup1 = Beinf1 + Pasbe
        Bbetm = Beinf1*pideg
        Bbetp = Besup1*pideg
!        WRITE (iw,99002) Bbetm, Bbetp
        Csm1 = COS(Beinf1)
        Csp1 = COS(Besup1)
        DO Ia = 0, Na
          Amoi1 = Amin + (Ia*pas)
          IF ( (Amoi1/SIN(Beinf1)).GE.petiamax ) GOTO 200
          Aplu1 = Amoi1 + pas
          DO I = 1, 10
            Pp = I
            Da = Aplu1/Pp
            IF ( d(5).GE.Da ) GOTO 20
          ENDDO
 20       mh = I
          DO J = I, 20
            Pp = J
            Da = Aplu1/Pp
            IF ( d(n).GE.Da ) GOTO 40
          ENDDO
 40       mh2 = J
          Krap1 = coeff/(Amoi1*Amoi1)
          Kram1 = coeff/(Aplu1*Aplu1)
          DO Ib = 0, Nb
            Bmoi1 = Bmin + (Ib*pas)
            Bplu1 = Bmoi1 + pas
            IF ( Aplu1.GE.dd1 .OR. Bplu1.GE.dd1 ) THEN
              DO I = 1, 10
                Pp = I
                Db = Bplu1/Pp
                IF ( d(5).GE.Db ) GOTO 45
              ENDDO
 45           mk = I
              DO J = I, 20
                Pp = J
                Db = Bplu1/Pp
                IF ( d(n).GE.Db ) GOTO 50
              ENDDO
 50           mk2 = J
              Krbm1 = coeff/(Bplu1*Bplu1)
              Krbp1 = coeff/(Bmoi1*Bmoi1)
              DO Ic = 0, Nc
                Cmoi1 = Cmin + (Ic*pas)
                IF ( Cmoi1.GE.Aplu1 ) GOTO 100
                Atest = ABS(Amoi1-aa)
                Btest = ABS(Bmoi1-bb)
                Ctest = ABS(Cmoi1-cc)
                Betest = ABS(Beinf1-bebe)
                IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR. Betest.GE.0.009 ) THEN
                  Cplu1 = Cmoi1 + pas
                  IF ( mc.NE.0 .OR. Aplu1.GT.amaxm .OR. Bplu1.GT.bmaxm .OR. Cplu1.GT.cmaxm ) THEN
                    IF ( Cplu1.GT.dd2 .OR. Bplu1.GT.dd2 ) THEN
                      Vtestm = Amoi1*Bmoi1*Cmoi1/SIN(Beinf1)
                      IF ( v.LT.Vtestm ) GOTO 100
                      IF ( vsup.LT.Vtestm ) GOTO 100
                      Vtestp = Aplu1*Bplu1*Cplu1/SIN(Besup1)
                      IF ( vinf.LE.Vtestp ) THEN
                        DO I = 1, 10
                          Pp = I
                          Dc = Cplu1/Pp
                          IF ( d(5).GE.Dc ) GOTO 52
                        ENDDO
 52                     ml = I
                        DO J = I, 20
                          Pp = J
                          Dc = Cplu1/Pp
                          IF ( d(n).GE.Dc ) GOTO 54
                        ENDDO
 54                     ml2 = J
                        Krcm = coeff/(Cplu1*Cplu1)
                        Krcp = coeff/(Cmoi1*Cmoi1)
                        Kracms = 2.*coeff/(Aplu1*Cplu1)
                        Kracps = 2.*coeff/(Amoi1*Cmoi1)
                        Kracm = 2.*coeff*Csm1/(Aplu1*Cplu1)
                        Kracp = 2.*coeff*Csp1/(Amoi1*Cmoi1)
                        Kamcp = 2.*coeff/(Amoi1*Cplu1)
                        Kapcm = 2.*coeff/(Aplu1*Cmoi1)
                        CALL MODHKL(Kram1,Krap1,Krcm,Krcp,Kracm,Kracp,Krbm1,Krbp1,Kamcp,Kapcm,Csm1,Csp1,Amoi1,     &
     &                              Aplu1,Cmoi1,Cplu1,Kracms,Kracps)
                        IF ( nt.NE.-1 ) THEN
                          DO I = 6, n
                            IF ( irj(I,1).EQ.0 ) GOTO 85
                          ENDDO
                          DO I = 1, n - 1
                            J = I + 1
                            IF ( irj(I,1).EQ.1 .AND. irj(J,1).EQ.1 .AND. ih(I,1,1).EQ.ih(J,1,1) .AND. ik(I,1,1)    &
     &                           .EQ.ik(J,1,1) .AND. il(I,1,1).EQ.il(J,1,1) ) GOTO 85
                          ENDDO
                          Ndich(1) = Ndich(1) + 1
                          DO Ibet2 = 0, 1
                            Beinf2 = Beinf1 + (Ibet2*Pab2)
                            Besup2 = Beinf2 + Pab2
                            Csm2 = COS(Beinf2)
                            Csp2 = COS(Besup2)
                            DO Ia2 = 0, 1
                              Amoi2 = Amoi1 + (Ia2*pas2)
                              IF ( (Amoi2/SIN(Beinf2)).GE.petiamax ) GOTO 80
                              Aplu2 = Amoi2 + pas2
                              Kram2 = coeff/(Aplu2*Aplu2)
                              Krap2 = coeff/(Amoi2*Amoi2)
                              DO Ib2 = 0, 1
                                Bmoi2 = Bmoi1 + (Ib2*pas2)
                                Bplu2 = Bmoi2 + pas2
                                Krbm2 = coeff/(Bplu2*Bplu2)
                                Krbp2 = coeff/(Bmoi2*Bmoi2)
                                DO Ic2 = 0, 1
                                  Cmoi2 = Cmoi1 + (Ic2*pas2)
                                  IF ( Cmoi2.GE.Aplu2 ) GOTO 78
                                  Atest = ABS(Amoi2-aa)
                                  Btest = ABS(Bmoi2-bb)
                                  Ctest = ABS(Cmoi2-cc)
                                  Betest = ABS(Beinf2-bebe)
                                  IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR. Betest.GE.0.009 ) THEN
                                    Vtestm = Amoi2*Bmoi2*Cmoi2/SIN(Beinf2)
                                    IF ( Vtestm.GT.vsup ) GOTO 78
                                    Cplu2 = Cmoi2 + pas2
                                    IF ( mc.NE.0 .OR. Aplu2.GT.amaxm .OR. Bplu2.GT.bmaxm .OR. Cplu2.GT.cmaxm ) THEN
                                      Vtestp = Aplu2*Bplu2*Cplu2/SIN(Besup2)
                                      IF ( vinf.LE.Vtestp ) THEN
                                        Krcm = coeff/(Cplu2*Cplu2)
                                        Krcp = coeff/(Cmoi2*Cmoi2)
                                        Kracms = 2.*coeff/(Aplu2*Cplu2)
                                        Kracps = 2.*coeff/(Amoi2*Cmoi2)
                                        Kracm = 2.*coeff*Csm2/(Aplu2*Cplu2)
                                        Kracp = 2.*coeff*Csp2/(Amoi2*Cmoi2)
                                        Kamcp = 2.*coeff/(Amoi2*Cplu2)
                                        Kapcm = 2.*coeff/(Aplu2*Cmoi2)
                                        CALL MODHKL2(Kram2,Krap2,Krcm,Krcp,Kracm,Kracp,Krbm2,Krbp2,Kamcp,Kapcm,    &
     &                                    Csm2,Csp2,Amoi2,Aplu2,Cmoi2,Cplu2,Kracms,Kracps,2)
                                        IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,2).EQ.1 .AND. irj(J,2).EQ.1 .AND. ih(I,1,2).EQ.ih(J,1,2) .AND. &
     &                                     ik(I,1,2).EQ.ik(J,1,2) .AND. il(I,1,2).EQ.il(J,1,2) ) GOTO 76
                                         ENDDO
                                         Ndich(2) = Ndich(2) + 1
                                         DO Ibet3 = 0, 1
                                         Beinf3 = Beinf2 + (Ibet3*Pab4)
                                         Besup3 = Beinf3 + Pab4
                                         Csm3 = COS(Beinf3)
                                         Csp3 = COS(Besup3)
                                         DO Ia3 = 0, 1
                                         Amoi3 = Amoi2 + (Ia3*pas4)
                                         Aplu3 = Amoi3 + pas4
                                         Kram3 = coeff/(Aplu3*Aplu3)
                                         Krap3 = coeff/(Amoi3*Amoi3)
                                         DO Ib3 = 0, 1
                                         Bmoi3 = Bmoi2 + (Ib3*pas4)
                                         Bplu3 = Bmoi3 + pas4
                                         Krbm3 = coeff/(Bplu3*Bplu3)
                                         Krbp3 = coeff/(Bmoi3*Bmoi3)
                                         DO Ic3 = 0, 1
                                         Cmoi3 = Cmoi2 + (Ic3*pas4)
                                         IF ( Cmoi3.GE.Aplu3 ) GOTO 74
                                         Atest = ABS(Amoi3-aa)
                                         Btest = ABS(Bmoi3-bb)
                                         Ctest = ABS(Cmoi3-cc)
                                         Betest = ABS(Beinf3-bebe)
                                         IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR.                &
     &                                     Betest.GE.0.009 ) THEN
                                         Vtestm = Amoi3*Bmoi3*Cmoi3/SIN(Beinf3)
                                         IF ( Vtestm.GT.vsup ) GOTO 74
                                         Cplu3 = Cmoi3 + pas4
                                         IF ( mc.NE.0 .OR. Aplu3.GT.amaxm .OR. Bplu3.GT.bmaxm .OR. Cplu3.GT.cmaxm )&
     &                                     THEN
                                         Vtestp = Aplu3*Bplu3*Cplu3/SIN(Besup3)
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm = coeff/(Cplu3*Cplu3)
                                         Krcp = coeff/(Cmoi3*Cmoi3)
                                         Kracms = 2.*coeff/(Aplu3*Cplu3)
                                         Kracps = 2.*coeff/(Amoi3*Cmoi3)
                                         Kracm = 2.*coeff*Csm3/(Aplu3*Cplu3)
                                         Kracp = 2.*coeff*Csp3/(Amoi3*Cmoi3)
                                         Kamcp = 2.*coeff/(Amoi3*Cplu3)
                                         Kapcm = 2.*coeff/(Aplu3*Cmoi3)
                                         CALL MODHKL2(Kram3,Krap3,Krcm,Krcp,Kracm,Kracp,Krbm3,Krbp3,Kamcp,Kapcm,   &
     &                                     Csm3,Csp3,Amoi3,Aplu3,Cmoi3,Cplu3,Kracms,Kracps,3)
                                         IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,3).EQ.1 .AND. irj(J,3).EQ.1 .AND. ih(I,1,3).EQ.ih(J,1,3) .AND. &
     &                                     ik(I,1,3).EQ.ik(J,1,3) .AND. il(I,1,3).EQ.il(J,1,3) ) GOTO 72
                                         ENDDO
                                         Ndich(3) = Ndich(3) + 1
                                         DO Ibet4 = 0, 1
                                         Beinf4 = Beinf3 + (Ibet4*Pab8)
                                         Besup4 = Beinf4 + Pab8
                                         Csm4 = COS(Beinf4)
                                         Csp4 = COS(Besup4)
                                         DO Ia4 = 0, 1
                                         Amoi4 = Amoi3 + (Ia4*pas8)
                                         Aplu4 = Amoi4 + pas8
                                         Kram4 = coeff/(Aplu4*Aplu4)
                                         Krap4 = coeff/(Amoi4*Amoi4)
                                         DO Ib4 = 0, 1
                                         Bmoi4 = Bmoi3 + (Ib4*pas8)
                                         Bplu4 = Bmoi4 + pas8
                                         Krbm4 = coeff/(Bplu4*Bplu4)
                                         Krbp4 = coeff/(Bmoi4*Bmoi4)
                                         DO Ic4 = 0, 1
                                         Cmoi4 = Cmoi3 + (Ic4*pas8)
                                         IF ( Cmoi4.GE.Aplu4 ) GOTO 70
                                         Atest = ABS(Amoi4-aa)
                                         Btest = ABS(Bmoi4-bb)
                                         Ctest = ABS(Cmoi4-cc)
                                         Betest = ABS(Beinf4-bebe)
                                         IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR.                &
     &                                     Betest.GE.0.009 ) THEN
                                         Vtestm = Amoi4*Bmoi4*Cmoi4/SIN(Beinf4)
                                         IF ( Vtestm.GT.vsup ) GOTO 70
                                         Cplu4 = Cmoi4 + pas8
                                         IF ( mc.NE.0 .OR. Aplu4.GT.amaxm .OR. Bplu4.GT.bmaxm .OR. Cplu4.GT.cmaxm )&
     &                                     THEN
                                         Vtestp = Aplu4*Bplu4*Cplu4/SIN(Besup4)
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm = coeff/(Cplu4*Cplu4)
                                         Krcp = coeff/(Cmoi4*Cmoi4)
                                         Kracms = 2.*coeff/(Aplu4*Cplu4)
                                         Kracps = 2.*coeff/(Amoi4*Cmoi4)
                                         Kracm = 2.*coeff*Csm4/(Aplu4*Cplu4)
                                         Kracp = 2.*coeff*Csp4/(Amoi4*Cmoi4)
                                         Kamcp = 2.*coeff/(Amoi4*Cplu4)
                                         Kapcm = 2.*coeff/(Aplu4*Cmoi4)
                                         CALL MODHKL2(Kram4,Krap4,Krcm,Krcp,Kracm,Kracp,Krbm4,Krbp4,Kamcp,Kapcm,   &
     &                                     Csm4,Csp4,Amoi4,Aplu4,Cmoi4,Cplu4,Kracms,Kracps,4)
                                         IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,4).EQ.1 .AND. irj(J,4).EQ.1 .AND. ih(I,1,4).EQ.ih(J,1,4) .AND. &
     &                                     ik(I,1,4).EQ.ik(J,1,4) .AND. il(I,1,4).EQ.il(J,1,4) ) GOTO 68
                                         ENDDO
                                         Ndich(4) = Ndich(4) + 1
                                         DO Ibet5 = 0, 1
                                         Beinf5 = Beinf4 + (Ibet5*Pab16)
                                         Besup5 = Beinf5 + Pab16
                                         Csm5 = COS(Beinf5)
                                         Csp5 = COS(Besup5)
                                         DO Ia5 = 0, 1
                                         Amoi5 = Amoi4 + (Ia5*pas16)
                                         Aplu5 = Amoi5 + pas16
                                         Kram5 = coeff/(Aplu5*Aplu5)
                                         Krap5 = coeff/(Amoi5*Amoi5)
                                         DO Ib5 = 0, 1
                                         Bmoi5 = Bmoi4 + (Ib5*pas16)
                                         Bplu5 = Bmoi5 + pas16
                                         Krbm5 = coeff/(Bplu5*Bplu5)
                                         Krbp5 = coeff/(Bmoi5*Bmoi5)
                                         DO Ic5 = 0, 1
                                         Cmoi5 = Cmoi4 + (Ic5*pas16)
                                         IF ( Cmoi5.GE.Aplu5 ) GOTO 66
                                         Atest = ABS(Amoi5-aa)
                                         Btest = ABS(Bmoi5-bb)
                                         Ctest = ABS(Cmoi5-cc)
                                         Betest = ABS(Beinf5-bebe)
                                         IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR.                &
     &                                     Betest.GE.0.009 ) THEN
                                         Vtestm = Amoi5*Bmoi5*Cmoi5/SIN(Beinf5)
                                         IF ( Vtestm.GT.vsup ) GOTO 66
                                         Cplu5 = Cmoi5 + pas16
                                         IF ( mc.NE.0 .OR. Aplu5.GT.amaxm .OR. Bplu5.GT.bmaxm .OR. Cplu5.GT.cmaxm )&
     &                                     THEN
                                         Vtestp = Aplu5*Bplu5*Cplu5/SIN(Besup5)
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm = coeff/(Cplu5*Cplu5)
                                         Krcp = coeff/(Cmoi5*Cmoi5)
                                         Kracms = 2.*coeff/(Aplu5*Cplu5)
                                         Kracps = 2.*coeff/(Amoi5*Cmoi5)
                                         Kracm = 2.*coeff*Csm5/(Aplu5*Cplu5)
                                         Kracp = 2.*coeff*Csp5/(Amoi5*Cmoi5)
                                         Kamcp = 2.*coeff/(Amoi5*Cplu5)
                                         Kapcm = 2.*coeff/(Aplu5*Cmoi5)
                                         CALL MODHKL2(Kram5,Krap5,Krcm,Krcp,Kracm,Kracp,Krbm5,Krbp5,Kamcp,Kapcm,   &
     &                                     Csm5,Csp5,Amoi5,Aplu5,Cmoi5,Cplu5,Kracms,Kracps,5)
                                         IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,5).EQ.1 .AND. irj(J,5).EQ.1 .AND. ih(I,1,5).EQ.ih(J,1,5) .AND. &
     &                                     ik(I,1,5).EQ.ik(J,1,5) .AND. il(I,1,5).EQ.il(J,1,5) ) GOTO 64
                                         ENDDO
                                         Ndich(5) = Ndich(5) + 1
                                         DO Ibet6 = 0, 1
                                         beinf6 = Beinf5 + (Ibet6*Pab32)
                                         Besup6 = beinf6 + Pab32
                                         Csm6 = COS(beinf6)
                                         Csp6 = COS(Besup6)
                                         DO Ia6 = 0, 1
                                         amoi6 = Amoi5 + (Ia6*pas32)
                                         Aplu6 = amoi6 + pas32
                                         Kram6 = coeff/(Aplu6*Aplu6)
                                         Krap6 = coeff/(amoi6*amoi6)
                                         DO Ib6 = 0, 1
                                         bmoi6 = Bmoi5 + (Ib6*pas32)
                                         Bplu6 = bmoi6 + pas32
                                         Krbm6 = coeff/(Bplu6*Bplu6)
                                         Krbp6 = coeff/(bmoi6*bmoi6)
                                         DO Ic6 = 0, 1
                                         cmoi6 = Cmoi5 + (Ic6*pas32)
                                         IF ( cmoi6.GE.Aplu6 ) GOTO 62
                                         Atest = ABS(amoi6-aa)
                                         Btest = ABS(bmoi6-bb)
                                         Ctest = ABS(cmoi6-cc)
                                         Betest = ABS(beinf6-bebe)
                                         IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR.                &
     &                                     Betest.GE.0.009 ) THEN
                                         Vtestm = amoi6*bmoi6*cmoi6/SIN(beinf6)
                                         IF ( Vtestm.GT.vsup ) GOTO 62
                                         Cplu6 = cmoi6 + pas32
                                         IF ( mc.NE.0 .OR. Aplu6.GT.amaxm .OR. Bplu6.GT.bmaxm .OR. Cplu6.GT.cmaxm )&
     &                                     THEN
                                         Vtestp = Aplu6*Bplu6*Cplu6/SIN(Besup6)
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm = coeff/(Cplu6*Cplu6)
                                         Krcp = coeff/(cmoi6*cmoi6)
                                         Kracms = 2.*coeff/(Aplu6*Cplu6)
                                         Kracps = 2.*coeff/(amoi6*cmoi6)
                                         Kracm = 2.*coeff*Csm6/(Aplu6*Cplu6)
                                         Kracp = 2.*coeff*Csp6/(amoi6*cmoi6)
                                         Kamcp = 2.*coeff/(amoi6*Cplu6)
                                         Kapcm = 2.*coeff/(Aplu6*cmoi6)
                                         CALL MODHKL2(Kram6,Krap6,Krcm,Krcp,Kracm,Kracp,Krbm6,Krbp6,Kamcp,Kapcm,   &
     &                                     Csm6,Csp6,amoi6,Aplu6,cmoi6,Cplu6,Kracms,Kracps,6)
                                         IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,6).EQ.1 .AND. irj(J,6).EQ.1 .AND. ih(I,1,6).EQ.ih(J,1,6) .AND. &
     &                                     ik(I,1,6).EQ.ik(J,1,6) .AND. il(I,1,6).EQ.il(J,1,6) ) GOTO 60
                                         ENDDO
                                         Ndich(6) = Ndich(6) + 1
                                         DO Ibet7 = 0, 1
                                         Beinf7 = beinf6 + (Ibet7*pab64)
                                         Besup7 = Beinf7 + pab64
                                         Csm7 = COS(Beinf7)
                                         Csp7 = COS(Besup7)
                                         DO Ia7 = 0, 1
                                         Amoi7 = amoi6 + (Ia7*pas64)
                                         Aplu7 = Amoi7 + pas64
                                         Kram7 = coeff/(Aplu7*Aplu7)
                                         Krap7 = coeff/(Amoi7*Amoi7)
                                         DO Ib7 = 0, 1
                                         Bmoi7 = bmoi6 + (Ib7*pas64)
                                         Bplu7 = Bmoi7 + pas64
                                         Krbm7 = coeff/(Bplu7*Bplu7)
                                         Krbp7 = coeff/(Bmoi7*Bmoi7)
                                         DO Ic7 = 0, 1
                                         Cmoi7 = cmoi6 + (Ic7*pas64)
                                         IF ( Cmoi7.GE.Aplu7 ) GOTO 58
                                         Atest = ABS(Amoi7-aa)
                                         Btest = ABS(Bmoi7-bb)
                                         Ctest = ABS(Cmoi7-cc)
                                         Betest = ABS(Beinf7-bebe)
                                         IF ( Atest.GE.0.1 .OR. Btest.GE.0.1 .OR. Ctest.GE.0.1 .OR.                &
     &                                     Betest.GE.0.009 ) THEN
                                         Vtestm = Amoi7*Bmoi7*Cmoi7/SIN(Beinf7)
                                         IF ( Vtestm.GT.vsup ) GOTO 58
                                         Cplu7 = Cmoi7 + pas64
                                         IF ( mc.NE.0 .OR. Aplu7.GT.amaxm .OR. Bplu7.GT.bmaxm .OR. Cplu7.GT.cmaxm )&
     &                                     THEN
                                         Vtestp = Aplu7*Bplu7*Cplu7/SIN(Besup7)
                                         IF ( vinf.LE.Vtestp ) THEN
                                         Krcm = coeff/(Cplu7*Cplu7)
                                         Krcp = coeff/(Cmoi7*Cmoi7)
                                         Kracms = 2.*coeff/(Aplu7*Cplu7)
                                         Kracps = 2.*coeff/(Amoi7*Cmoi7)
                                         Kracm = 2.*coeff*Csm7/(Aplu7*Cplu7)
                                         Kracp = 2.*coeff*Csp7/(Amoi7*Cmoi7)
                                         Kamcp = 2.*coeff/(Amoi7*Cplu7)
                                         Kapcm = 2.*coeff/(Aplu7*Cmoi7)
                                         CALL MODHKL2(Kram7,Krap7,Krcm,Krcp,Kracm,Kracp,Krbm7,Krbp7,Kamcp,Kapcm,   &
     &                                     Csm7,Csp7,Amoi7,Aplu7,Cmoi7,Cplu7,Kracms,Kracps,7)
                                         IF ( nt.NE.-1 ) THEN
                                         DO I = 1, n - 1
                                         J = I + 1
                                         IF ( irj(I,7).EQ.1 .AND. irj(J,7).EQ.1 .AND. ih(I,1,7).EQ.ih(J,1,7) .AND. &
     &                                     ik(I,1,7).EQ.ik(J,1,7) .AND. il(I,1,7).EQ.il(J,1,7) ) GOTO 56
                                         ENDDO
                                         Ndich(7) = Ndich(7) + 1
                                         Aare = aa
                                         Bbre = bb
                                         Ccre = cc
                                         Bebere = bebe
                                         aa = Amoi7 + (pas64/2.)
                                         bb = Bmoi7 + (pas64/2.)
                                         cc = Cmoi7 + (pas64/2.)
                                         bebe = Beinf7 + (pab64/2.)
                                         Vap = aa*bb*cc/SIN(bebe)
                                         beta = pideg*bebe
                                      !   DO I = 1, n
                                        !   Jj = irj(I,7)
                                        !   DO J = 1, Jj
                                        !     Carh  = ih(I,J,7)*ih(I,J,7)
                                        !     Cark  = ik(I,J,7)*ik(I,J,7)
                                        !     Carl  = il(I,J,7)*il(I,J,7)
                                        !     Prohl = il(I,J,7)*ih(I,J,7)
                                        !   ENDDO
                                      !   ENDDO
                                         Ind = 5
                                         Nrind = 4
                                         CALL AFFPAR(Ind,Nrind,Vap)
                                         IF (DICVOL_Error .NE. 0) RETURN
                                         aa = aa*SIN(bebe)
                                         cc = cc*SIN(bebe)
                                         IF ( fwolff.NE.-1000. ) THEN
                                         ELSE
                                         aa = Aare
                                         bb = Bbre
                                         cc = Ccre
                                         bebe = Bebere
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
 56                                      ENDDO
 58                                      ENDDO
                                         ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
 60                                      ENDDO
 62                                      ENDDO
                                         ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
 64                                      ENDDO
 66                                      ENDDO
                                         ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
 68                                      ENDDO
 70                                      ENDDO
                                         ENDDO
                                         ENDDO
                                         ENDIF
                                         ENDIF
                                         ENDIF
                                         ENDIF
 72                                      ENDDO
 74                                      ENDDO
                                         ENDDO
                                         ENDDO
                                        ENDIF
                                      ENDIF
                                    ENDIF
                                  ENDIF
 76                             ENDDO
 78                           ENDDO
                            ENDDO
 80                       ENDDO
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
 85           ENDDO
            ENDIF
 100      ENDDO
        ENDDO
 200  ENDDO
 300  CONTINUE ! WRITE (iw,99003) (Ndich(I),I=1,7)
!99003 FORMAT (//,2X,'ITERATION NUMBER AT EACH DICHOTOMY LEVEL :',7I5)
!99002 FORMAT (' ',2X,'ANGLE RANGE SCANNED :',2X,'BETA MIN=',F7.3,' Deg.  BETA MAX=',F7.3,' Deg.')
      END SUBROUTINE MONOC1
!*==MODHKL.f90  processed by SPAG 6.11Dc at 15:36 on 20 Sep 2001
      SUBROUTINE MODHKL(Kram,Krap,Krcm,Krcp,Kracm,Kracp,Krbm,Krbp,Kamcp,Kapcm,Csm,Csp,Amoi,Aplu,Cmoi,Cplu,Kracms,  &
     &                  Kracps)
      USE DICVAR
      IMPLICIT NONE
!
! Dummy arguments
!
      REAL :: Amoi, Aplu, Cmoi, Cplu, Csm, Csp
      INTEGER :: Kamcp, Kapcm, Kracm, Kracms, Kracp, Kracps, Kram, Krap, Krbm, Krbp, Krcm, Krcp
      INTENT (IN) Amoi, Aplu, Cmoi, Cplu, Csm, Csp, Kamcp, Kapcm, Kracm, Kracms, Kracp, Kracps, Kram, &
     &            Krap, Krbm, Krbp, Krcm, Krcp
!
! Local variables
!
      REAL :: Dd
      REAL :: F
      INTEGER :: I, Jj, K, Kborin, Kborsu, Kqqm, Kqqp, La1, Lc0, M, Mcarh, Mcark, Mcarl, Mdl, Mdl2, &
     &           Mm1, Mm2, Mm3, Mp1, Mp2, Mp3, Mprohl, Mxl, Ne, Ng, Nx, Ny, Nz

      F(Nx,Ny,Nz,Dd,Ne,Ng) = Nx + Ny + Nz - Dd*Ne*Ng
      nt = 0
      DO I = 1, n
        irj(I,1) = 0
      ENDDO
      DO M = 0, mh
        Mdl = ml*MOD(1,M+1)
        Mcarh = M*M
        Mm1 = Mcarh*Kram
        Mp1 = Mcarh*Krap
        DO K = 0, mk
          Mcark = K*K
          Mm2 = (Mcark*Krbm)
          Mp2 = (Mcark*Krbp)
          DO Mxl = -Mdl, ml
            Mcarl = Mxl*Mxl
            Mprohl = M*Mxl
            Mm3 = (Mcarl*Krcm)
            Mp3 = (Mcarl*Krcp)
            IF ( Mprohl.LT.0 ) THEN
              Lc0 = 0
              La1 = 0
              IF ( (M*Cplu/(Amoi*Mxl)).LE.Csp ) THEN
                Lc0 = 1
                IF ( (M*Cplu/(Mxl*Aplu)).GE.Csp ) THEN
                  Kqqm = (coeff*(Mxl/Cplu)**2*(1-Csp**2)) + Mm2
                  GOTO 20
                ENDIF
              ENDIF
              IF ( (M*Cmoi/(Mxl*Amoi)).LE.Csp ) THEN
                IF ( (M*Cmoi/(Mxl*Aplu)).GE.Csp ) THEN
                  Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                  GOTO 20
                ENDIF
              ENDIF
              IF ( (Mxl*Aplu/(M*Cmoi)).LE.Csp ) THEN
                La1 = 1
                IF ( (Mxl*Aplu/(M*Cplu)).GE.Csp ) THEN
                  Kqqm = (coeff*(M/Aplu)**2*(1-Csp**2)) + Mm2
                  GOTO 10
                ENDIF
              ENDIF
              IF ( (Mxl*Amoi/(M*Cmoi)).LE.Csp .AND. (Mxl*Amoi/(M*Cplu)).GE.Csp ) THEN
                Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
              ELSEIF ( Csm.LT.(Mxl*Aplu/(M*Cmoi)) ) THEN
                Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
                Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                GOTO 30
              ELSEIF ( Csm.LE.(Mxl*Amoi/(M*Cplu)) ) THEN
                Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
                GOTO 30
              ELSEIF ( Lc0.EQ.0 ) THEN
                Kqqm = F(Mp1,Mm2,Mm3,Csp,Kamcp,Mprohl)
                GOTO 20
              ELSE
                Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
                IF ( La1.EQ.0 ) THEN
                  Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                  GOTO 30
                ELSE
                  Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                  GOTO 20
                ENDIF
              ENDIF
            ELSE
              Kqqm = Mm1 + Mm2 + Mm3 - (Mprohl*Kracm)
              IF ( (Kqqm-kepsq(5)).LE.kq(5) ) THEN
                Kqqp = Mp1 + Mp2 + Mp3 - (Mprohl*Kracp)
                GOTO 40
              ELSE
                IF ( Mxl.GE.0 ) GOTO 100
                GOTO 60
              ENDIF
            ENDIF
 10         IF ( (1./Cmoi+1./Cplu).GE.((2.*M*Csm)/(Mxl*Amoi)) ) THEN
              Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
            ELSE
              Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
            ENDIF
            GOTO 30
 20         IF ( (1./Amoi+1./Aplu).GE.(2.*Mxl*Csm/(M*Cmoi)) ) THEN
              Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
            ELSE
              Kqqp = F(Mm1,Mp2,Mp3,Csm,Kapcm,Mprohl)
            ENDIF
 30         IF ( (Kqqm-kepsq(5)).GT.kq(5) ) GOTO 60
 40         IF ( (Kqqp+kepsq(1)).GE.kq(1) ) THEN
              DO I = 1, 5
                Kborin = Kqqm - kepsq(I)
                IF ( Kborin.LE.kq(I) ) THEN
                  Kborsu = Kqqp + kepsq(I)
                  IF ( Kborsu.GE.kq(I) ) THEN
                    irj(I,1) = irj(I,1) + 1
                    Jj = irj(I,1)
                    ih(I,Jj,1) = M
                    ik(I,Jj,1) = K
                    il(I,Jj,1) = Mxl
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
 60       ENDDO
 100    ENDDO
      ENDDO
      DO I = 1, 5
        IF ( irj(I,1).EQ.0 ) THEN
          nt = -1
          GOTO 99999
        ENDIF
      ENDDO
      DO M = 0, mh2
        Mdl2 = ml2*MOD(1,M+1)
        Mcarh = M*M
        Mm1 = Mcarh*Kram
        Mp1 = Mcarh*Krap
        DO K = 0, mk2
          Mcark = K*K
          Mm2 = (Mcark*Krbm)
          Mp2 = (Mcark*Krbp)
          DO Mxl = -Mdl2, ml2
            Mcarl = Mxl*Mxl
            Mprohl = M*Mxl
            Mm3 = (Mcarl*Krcm)
            Mp3 = (Mcarl*Krcp)
            IF ( Mprohl.LT.0 ) THEN
              Lc0 = 0
              La1 = 0
              IF ( (M*Cplu/(Amoi*Mxl)).LE.Csp ) THEN
                Lc0 = 1
                IF ( (M*Cplu/(Mxl*Aplu)).GE.Csp ) THEN
                  Kqqm = (coeff*(Mxl/Cplu)**2*(1-Csp**2)) + Mm2
                  GOTO 120
                ENDIF
              ENDIF
              IF ( (M*Cmoi/(Mxl*Amoi)).LE.Csp ) THEN
                IF ( (M*Cmoi/(Mxl*Aplu)).GE.Csp ) THEN
                  Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                  GOTO 110
                ENDIF
              ENDIF
              IF ( (Mxl*Aplu/(M*Cmoi)).LE.Csp ) THEN
                La1 = 1
                IF ( (Mxl*Aplu/(M*Cplu)).GE.Csp ) THEN
                  Kqqm = coeff*(M/Aplu)**2*(1-Csp**2) + Mm2
                  GOTO 120
                ENDIF
              ENDIF
              IF ( (Mxl*Amoi/(M*Cmoi)).LE.Csp .AND. (Mxl*Amoi/(M*Cplu)).GE.Csp ) THEN
                Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
              ELSEIF ( Csm.LT.(Mxl*Aplu/(M*Cmoi)) ) THEN
                Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
                Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                GOTO 130
              ELSEIF ( Csm.LE.(Mxl*Amoi/(M*Cplu)) ) THEN
                Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
                GOTO 130
              ELSEIF ( Lc0.EQ.0 ) THEN
                Kqqm = F(Mp1,Mm2,Mm3,Csp,Kamcp,Mprohl)
                GOTO 120
              ELSE
                Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
                IF ( La1.EQ.0 ) THEN
                  Kqqm = F(Mm1,Mm2,Mp3,Csp,Kapcm,Mprohl)
                  GOTO 130
                ELSE
                  Kqqm = F(Mm1,Mm2,Mm3,Csp,Kracms,Mprohl)
                  GOTO 120
                ENDIF
              ENDIF
            ELSE
              Kqqm = Mm1 + Mm2 + Mm3 - (Mprohl*Kracm)
              IF ( (Kqqm-kepsq(n)).LE.kq(n) ) THEN
                Kqqp = Mp1 + Mp2 + Mp3 - (Mprohl*Kracp)
                GOTO 140
              ELSE
                IF ( Mxl.GE.0 ) GOTO 200
                GOTO 160
              ENDIF
            ENDIF
 110        IF ( (1./Cmoi+1./Cplu).GE.((2.*M*Csm)/(Mxl*Amoi)) ) THEN
              Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
            ELSE
              Kqqp = F(Mp1,Mp2,Mm3,Csm,Kamcp,Mprohl)
            ENDIF
            GOTO 130
 120        IF ( (1./Amoi+1./Aplu).GE.(2.*Mxl*Csm/(M*Cmoi)) ) THEN
              Kqqp = F(Mp1,Mp2,Mp3,Csm,Kracps,Mprohl)
            ELSE
              Kqqp = F(Mm1,Mp2,Mp3,Csm,Kapcm,Mprohl)
            ENDIF
 130        IF ( (Kqqm-kepsq(n)).GT.kq(n) ) GOTO 160
 140        IF ( (Kqqp+kepsq(6)).GE.kq(6) ) THEN
              DO I = 6, n
                Kborin = Kqqm - kepsq(I)
                IF ( Kborin.LE.kq(I) ) THEN
                  Kborsu = Kqqp + kepsq(I)
                  IF ( Kborsu.GE.kq(I) ) THEN
                    irj(I,1) = irj(I,1) + 1
                    Jj = irj(I,1)
                    ih(I,Jj,1) = M
                    ik(I,Jj,1) = K
                    il(I,Jj,1) = Mxl
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
 160      ENDDO
 200    ENDDO
      ENDDO
99999 END SUBROUTINE MODHKL
