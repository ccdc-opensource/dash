!
!*****************************************************************************
!
!     ------------------------------------------------------------------
!     |       A F F I N E M E N T   D E S   P A R A M E T R E S        |
!     ------------------------------------------------------------------
      SUBROUTINE AFFPAR(Ind,Nrind,Vap)
!
! This is where we jump to whenever a solution is found. Ind is the crystal system:
! 1 = Cubic
! 2 = Tetragonal
! 3 = Hexagonal
! 4 = Orthorhombic
! 5 = Monoclinic
! 6 = Triclinic
!
      USE DICVAR
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER, INTENT (IN   ) :: Ind, Nrind
      REAL,    INTENT (IN   ) :: Vap
!
! Local variables
!
      REAL, DIMENSION(7,7) :: A
      REAL :: Ae, Ae2, Be, Be2, Bidon1, Bidon2, Calq, Ce, Ce2, Coa, Cob, Cog, Cosa, Cosb,     &
     &        Cosg, Dcal, Deltaq,Diag, Difd, Difq, Difq1, Dift, Dtet, Dtheta, &
     &        Findex, Fitest, Fvar, Fwtest, Parh, Parhk, Park, Parkl, Parl, Ph, Ph2, Pk, Pk2, Pl,    &
     &        Pl2, Qnaf, Qnc, R, Raizda, Rindep, Sia, Sib, Sig, Svol, Tcal, Vol, Xt1, Xt2
      REAL, DIMENSION(6,6) :: B
      REAL, DIMENSION(6) :: Dir, Ecart, Error, Tem
      REAL, DIMENSION(7) :: E
      INTEGER :: I, I1, Iaf, Im, Indi, Is, Iterat, J, Jaf, Jj, Jjs, Js, K, K1, K2, Kclef, Key,    &
     &           Ktestwolff, L, L1, L2, Ll, Llave, M, M1, Mrj, Mxl, Ncaf, Nh, Nk, Nl, Nraies
      INTEGER, DIMENSION(50) :: Jh, Jk, Jl
      REAL, DIMENSION(50) :: Poids, Ssc, Ssq
      INTEGER ir

      Ncaf = 1
      DO I = 1, 6
        Dir(I) = 0.0
      ENDDO
      jcount = jcount + 1
! The following stopped DICVOL from generating more than 30 solutions
      IF ( jcount.GT.30 ) THEN
        WRITE (iw,99023) n, fom
99023 FORMAT (10X,'W A R N I N G   :'/6X,'THE NUMBER OF SOLUTIONS FOUND,WITH M(',I2,') .GT.',F5.2,                 &
     &        ', IS GREATER THAN 30.'/29X,'THE CALCULATION HAS BEEN STOPPED.'/29X,                                 &
     &        'PLEASE, CHECK YOUR INPUT DATA !')
        DICVOL_Error = cDICVOL_TooManySolutions
        RETURN
      ENDIF
      DICVOL_NumOfSolutions(Ind) = DICVOL_NumOfSolutions(Ind) + 1
! Misnomer...
      CALL DICVOL_FinishedCrystalSystem(LSHIFT(1,Ind-1))
      DO I = 1, 3
        Error(I) = 0.0001
        Error(I+3) = .001
      ENDDO
      IF ( Ind.NE.6 ) THEN
        DO I = 1, n
          irj(I,8) = irj(I,7)
          DO J = 1, irj(I,8)
            ih(I,J,8) = ih(I,J,7)
            ik(I,J,8) = ik(I,J,7)
            il(I,J,8) = il(I,J,7)
          ENDDO
        ENDDO
      ENDIF
      Bidon1 = 1.001
      Bidon2 = 1000./1001.
      Bebe = pirad*beta
      IF ( Ind.EQ.5 ) THEN
        aa = aa/SIN(Bebe)
        cc = cc/SIN(Bebe)
      ENDIF
      par(1) = aa
      par(2) = bb
      par(3) = cc
      par(4) = beta
      IF ( Nrind.EQ.2 ) par(2) = cc
      IF ( Ind.EQ.6 ) THEN
        par(1) = ar
        par(2) = br
        par(3) = cr
        par(4) = dr
        par(5) = er
        par(6) = fr
      ENDIF
!     S E L E C T I O N   D E S   R A I E S   U N I V O Q U E S
 100  Jaf = 0
      Nraies = 0
      DO I = 1, n
        IF ( irj(I,8).LE.1 ) THEN
          Nraies = Nraies + 1
          Jh(Nraies) = ih(I,1,8)
          Jk(Nraies) = ik(I,1,8)
          Jl(Nraies) = il(I,1,8)
          Poids(Nraies) = 1.0 / epsq(I)
          Ssq(Nraies) = q(I)
        ENDIF
      ENDDO
      Nh = 0
      Nk = 0
      Nl = 0
      DO I = 1, Nraies
        IF ( Jh(I).EQ.0 ) Nh = Nh + 1
        IF ( Jk(I).EQ.0 ) Nk = Nk + 1
        IF ( Jl(I).EQ.0 ) Nl = Nl + 1
      ENDDO
      IF ( Nh.NE.Nraies .AND. Nk.NE.Nraies .AND. Nl.NE.Nraies ) THEN
        Rindep = Nraies - Nrind
        IF ( Rindep.LE.0.0 ) THEN
          WRITE (iw,99001)
99001     FORMAT (/10X,'WARNING ! THE FOLLOWING SOLUTION IS NOT REFINED !'///)
          GOTO 500
        ELSE
!     CAMBIO SEGUN HEXAGONAL
!*****-----------------------------------------------
!     TEST N. 1 : NOMBRE DE RAIES (PROVISOIRE)
! SUPPRIMER ?    30 IF (NRAIES.LT.NRIND) GOTO 565
          Llave = 1
!      A F F I N E M E N T
          CALL PASAJE(Ind)
          DO I = 1, Nrind
            par(I) = rec(I)
          ENDDO
          CALL PASAJE(Ind)
          DO I = 1, Nrind
            Dir(I) = rec(I)
          ENDDO
          Iterat = 0
        ENDIF
      ELSE
        fwolff = -1000.
        jcount = jcount - 1
        GOTO 99999
      ENDIF
 200  Iterat = Iterat + 1
      DO J = 1, Nrind + 1
        DO K = 1, Nrind + 1
          A(J,K) = 0.
        ENDDO
      ENDDO
      DO I = 1, Nraies
        Parh = 2.*par(1)*Jh(I)**2
        Park = 2.*par(2)*Jk(I)**2
        Parl = 2.*par(3)*Jl(I)**2
        Parkl = 2.*par(2)*Jl(I)**2
        Parhk = 2.*par(1)*Jk(I)**2
        SELECT CASE (Ind)
        CASE (2)
          E(1) = (Parh+Parhk)*Poids(I)
          E(2) = Parkl*Poids(I)
          Ssc(I) = par(1)**2*(Jh(I)**2+Jk(I)**2) + par(2)**2*Jl(I)**2
        CASE (3)
          E(1) = (Parh+Parhk+2.*par(1)*Jh(I)*Jk(I))*Poids(I)
          E(2) = Parkl*Poids(I)
          Ssc(I) = par(1)**2*(Jh(I)**2+Jk(I)**2+Jh(I)*Jk(I)) + (par(2)*Jl(I))**2
        CASE (4)
          E(1) = Parh*Poids(I)
          E(2) = Park*Poids(I)
          E(3) = Parl*Poids(I)
          Ssc(I) = par(1)**2*Jh(I)**2 + par(2)**2*Jk(I)**2 + par(3)**2*Jl(I)**2
        CASE (5)
          Cosb = 2.*Jh(I)*Jl(I)*COS(par(4)*pirad)
          E(1) = (Parh+par(3)*Cosb)*Poids(I)
          E(2) = Park*Poids(I)
          E(3) = (Parl+par(1)*Cosb)*Poids(I)
          E(4) = (-2.*Jh(I)*Jl(I)*SIN(par(4)*pirad)*par(1)*par(3))*Poids(I)*pirad
          Ssc(I) = (par(1)*Jh(I))**2 + (par(2)*Jk(I))**2 + (par(3)*Jl(I))**2 + Cosb*par(1)*par(3)
        CASE (6)
          Cosa = 2.*Jk(I)*Jl(I)*COS(par(4)*pirad)
          Cosb = 2.*Jh(I)*Jl(I)*COS(par(5)*pirad)
          Cosg = 2.*Jh(I)*Jk(I)*COS(par(6)*pirad)
          E(1) = (Parh+Cosg*par(2)+Cosb*par(3))*Poids(I)
          E(2) = (Park+Cosg*par(1)+Cosa*par(3))*Poids(I)
          E(3) = (Parl+Cosa*par(2)+Cosb*par(1))*Poids(I)
          E(4) = (-2.*Jk(I)*Jl(I)*par(2)*par(3)*SIN(par(4)*pirad))*Poids(I)*pirad
          E(5) = (-2.*Jh(I)*Jl(I)*par(1)*par(3)*SIN(par(5)*pirad))*Poids(I)*pirad
          E(6) = (-2.*Jh(I)*Jk(I)*par(1)*par(2)*SIN(par(6)*pirad))*Poids(I)*pirad
          Ssc(I) = (par(1)*Jh(I))**2 + (par(2)*Jk(I))**2 + (par(3)*Jl(I))**2 + Cosa*par(2)*par(3) + Cosb*par(1)    &
     &             *par(3) + Cosg*par(1)*par(2)
        CASE DEFAULT
          E(1) = (Parh+Parhk+2.*par(1)*Jl(I)**2)*Poids(I)
          Ssc(I) = par(1)**2*(Jh(I)**2+Jk(I)**2+Jl(I)**2)
        END SELECT
        E(Nrind+1) = (Ssq(I)-Ssc(I))*Poids(I)
        DO J = 1, Nrind + 1
          DO K = J, Nrind + 1
            A(J,K) = A(J,K) + E(J)*E(K)
          ENDDO
        ENDDO
      ENDDO
      IF ( Nrind.EQ.1 ) THEN
        Ecart(1) = A(1,2)/A(1,1)
        A(1,1) = 1./A(1,1)
        GOTO 300
      ENDIF
      DO I = 2, Nrind
        L = I - 1
        DO J = 1, L
          A(I,J) = 0.0
        ENDDO
      ENDDO
      A(1,1) = 1.0/SQRT(A(1,1))
      DO I = 2, Nrind
        A(1,I) = A(1,I)*A(1,1)
      ENDDO
      DO I = 2, Nrind
        L = I - 1
        DO J = 1, L
          A(I,I) = A(I,I) - A(J,I)*A(J,I)
          IF ( A(I,I).LE.0. ) THEN
            fwolff = -1000.
            jcount = jcount - 1
            GOTO 99999
          ENDIF
        ENDDO
        Raizda = SQRT(A(I,I))
        A(I,I) = 1./Raizda
        Ll = I + 1
        DO J = 1, L
          DO K = J, L
            A(I,J) = A(I,J) - A(K,I)*A(K,J)
          ENDDO
          A(I,J) = A(I,J)*A(I,I)
        ENDDO
        IF ( I.NE.Nrind ) THEN
          DO J = Ll, Nrind
            DO K = 1, L
              A(I,J) = A(I,J) - A(K,I)*A(K,J)
            ENDDO
            A(I,J) = A(I,J)*A(I,I)
          ENDDO
        ENDIF
      ENDDO
      DO J = 1, Nrind
        A(Nrind,J) = A(Nrind,J)*A(Nrind,Nrind)
      ENDDO
      DO I = 2, Nrind
        Is = Nrind + 1 - I
        Diag = A(Is,Is)
        DO J = 1, Is
          L = I - 1
          DO K = 1, L
            Im = Nrind + 1 - K
            A(Is,J) = A(Is,J) - A(Is,Im)*A(Im,J)
          ENDDO
          A(Is,J) = A(Is,J)*Diag
        ENDDO
      ENDDO
      DO I = 2, Nrind
        L = I - 1
        DO J = 1, L
          A(J,I) = A(I,J)
        ENDDO
      ENDDO
      DO I = 1, Nrind
        Ecart(I) = 0.
        DO J = 1, Nrind
          Ecart(I) = Ecart(I) + A(I,J)*A(J,Nrind+1)
        ENDDO
      ENDDO
 300  DO I = 1, Nrind
        par(I) = par(I) + Ecart(I)
      ENDDO
      CALL PASAJE(Ind)
      DO J = 1, Nrind
        Ecart(J) = rec(J) - Dir(J)
        Dir(J) = rec(J)
      ENDDO
      DO J = 1, Nrind
        IF ( ABS(Ecart(J)).GT.Error(J) ) GOTO 200
      ENDDO
      Indi = 3
      IF ( Nrind.LT.3 ) Indi = Nrind
      DO I = 1, Indi
        par(I) = par(I)*Bidon1
        CALL PASAJE(Ind)
        par(I) = par(I)*Bidon2
        DO J = 1, Nrind
          B(I,J) = (rec(J)-Dir(J))*1000.0/par(I)
        ENDDO
      ENDDO
      IF ( Nrind.GE.4 ) THEN
        DO I = 4, Nrind
          par(I) = par(I) + 0.01
          CALL PASAJE(Ind)
          par(I) = par(I) - 0.01
          DO J = 1, Nrind
            B(I,J) = (rec(J)-Dir(J))*100.0
          ENDDO
        ENDDO
      ENDIF
      Rindep = A(Nrind+1,Nrind+1)/Rindep
      DO I = 1, Nrind
        Ecart(I) = 0.0
        DO J = 1, Nrind
          Tem(J) = 0.0
          DO K = 1, Nrind
            Tem(J) = Tem(J) + A(J,K)*B(K,I)
          ENDDO
          Ecart(I) = Ecart(I) + Tem(J)*B(J,I)
        ENDDO
        Ecart(I) = SQRT(Ecart(I)*Rindep)
      ENDDO
      Ae = par(1)
      Be = par(2)
      Ce = par(3)
      Ae2 = Ae*Ae
      Be2 = Be*Be
      Ce2 = Ce*Ce
      nposs = -1
      Qnc = q(n) + epsq(n)
      SELECT CASE (Ind)
        CASE (6) ! Triclinic
          Cosa = COS(pirad*par(4))
          Cosb = COS(pirad*par(5))
          Cosg = COS(pirad*par(6))
        CASE (5) ! Monoclinic
          Cosb = COS(pirad*par(4))
      END SELECT
      M1 = 0
      K1 = 0
      L1 = 0
      K2 = mk2
      L2 = ml2
      IF ( n.NE.nini ) THEN
        DO ir = n + 1, nini
          irj(ir,8) = 0
        ENDDO
      ENDIF
      IF ( Ind.EQ.1 ) THEN ! Cubic
        nposs = 0
        M1 = 1
      ENDIF 
      DO M = M1, mh2
        IF ( Ind.EQ.6 .AND. M.NE.0 ) K1 = -mk2
        IF ( Ind.LT.4 ) K2 = M
        IF ( Ind.EQ.5 .AND. M.NE.0 ) L1 = -ml2
        DO K = K1, K2
          IF ( Ind.EQ.1 ) L2 = K
          IF ( Ind.EQ.6 .AND. (M.NE.0 .OR. K.NE.0) ) L1 = -ml2
          DO Mxl = L1, L2
            SELECT CASE (Ind)
            CASE (1)
              Calq = (M*M+K*K+Mxl*Mxl)*Ae2
            CASE (2)
              Calq = (M*M+K*K    )*Ae2 + Mxl*Mxl*Be2
            CASE (3)
              Calq = (M*M+K*K+M*K)*Ae2 + Mxl*Mxl*Be2
            CASE (4)
              Calq = M*M*Ae2 + K*K*Be2 + Mxl*Mxl*Ce2
            CASE (5)
              Calq = M*M*Ae2 + K*K*Be2 + Mxl*Mxl*Ce2 + 2.*M*Mxl*Ae*Ce*Cosb
            CASE (6)
              Calq = M*M*Ae2 + K*K*Be2 + Mxl*Mxl*Ce2 + 2.*M*K*Ae*Be*Cosg + 2.*K*Mxl*Be*Ce*Cosa +                   &
     &               2.*M*Mxl*Ae*Ce*Cosb
            END SELECT
            IF ( Calq.LE.Qnc ) THEN
              nposs = nposs + 1
              IF ( M.NE.0 .OR. K.NE.0 .OR. Mxl.NE.0 ) THEN
                IF ( nini.NE.n ) THEN
                  DO Is = nini, n + 1, -1
                    IF ( irj(Is,8).NE.0 ) THEN
                      DO Js = 1, irj(Is,8)
                        IF ( M.EQ.ih(Is,Js,8) .AND. K.EQ.ik(Is,Js,8) .AND. Mxl.EQ.il(Is,Js,8) ) GOTO 302
                      ENDDO
                    ENDIF
                    Xt1 = 2.*pideg*ASIN(wave2*SQRT(Calq))
                    Xt2 = 2.*pideg*ASIN(wave2*SQRT(q(Is)))
                    IF ( ABS(Xt1-Xt2).LE.epst ) THEN
                      irj(Is,8) = irj(Is,8) + 1
                      Jjs = irj(Is,8)
                      ih(Is,Jjs,8) = M
                      ik(Is,Jjs,8) = K
                      il(Is,Jjs,8) = Mxl
                    ENDIF
 302              ENDDO
                ENDIF
                DO I = 1, n
                  Xt1 = 2.*pideg*ASIN(wave2*SQRT(Calq))
                  Xt2 = 2.*pideg*ASIN(wave2*SQRT(q(I)))
                  IF ( ABS(Xt1-Xt2).LE.epst ) THEN
                    Mrj = irj(I,8)
                    DO J = 1, irj(I,8)
                      IF ( M.EQ.ih(I,J,8) .AND. K.EQ.ik(I,J,8) .AND. Mxl.EQ.il(I,J,8) ) GOTO 306
                    ENDDO
                    DO J = 1, irj(I,8)
                      SELECT CASE (Ind)
                      CASE (2)
                        Qnaf = (ih(I,J,8)**2+ik(I,J,8)**2)*Ae2 + il(I,J,8)**2*Be2
                      CASE (3)
                        Qnaf = (ih(I,J,8)**2+ik(I,J,8)**2+ih(I,J,8)*ik(I,J,8))*Ae2 + il(I,J,8)**2*Be2
                      CASE (4)
                        Qnaf = ih(I,J,8)**2*Ae2 + ik(I,J,8)**2*Be2 + il(I,J,8)**2*Ce2
                      CASE (5)
                        Qnaf = ih(I,J,8)**2*Ae2 + ik(I,J,8)**2*Be2 + il(I,J,8)**2*Ce2 + 2.*ih(I,J,8)*il(I,J,8)     &
     &                         *Ae*Ce*Cosb
                      CASE (6)
                        Qnaf = ih(I,J,8)**2*Ae2 + ik(I,J,8)**2*Be2 + il(I,J,8)**2*Ce2 + 2.*ih(I,J,8)*ik(I,J,8)     &
     &                         *Ae*Be*Cosg + 2.*ik(I,J,8)*il(I,J,8)*Be*Ce*Cosa + 2.*ih(I,J,8)*il(I,J,8)*Ae*Ce*Cosb
                      CASE DEFAULT
                        Qnaf = (ih(I,J,8)**2+ik(I,J,8)**2+il(I,J,8)**2)*Ae2
                      END SELECT
                      IF ( ABS(Calq-q(I)).LE.ABS(Qnaf-q(I)) ) THEN
                        Mrj = Mrj + 1
                        ih(I,Mrj,8) = M
                        ik(I,Mrj,8) = K
                        il(I,Mrj,8) = Mxl
                        Jaf = 1
                        GOTO 304
                      ENDIF
                    ENDDO
 304                irj(I,8) = Mrj
                  ENDIF
 306            ENDDO
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF ( Jaf.EQ.1 ) Ncaf = Ncaf + 1
      IF ( Jaf.EQ.1 .AND. Ncaf.LT.4 ) THEN
        DO Iaf = 1, 6
          par(Iaf) = Dir(Iaf)
        ENDDO
        GOTO 100
      ENDIF
      Dtheta = 0.0
      Deltaq = 0.0
      DO I = 1, n
        Jj = irj(I,8)
        Difq = 100.0
        J = 1
 350    Ph = ih(I,J,8)
        Pk = ik(I,J,8)
        Pl = il(I,J,8)
        Ph2 = Ph*Ph
        Pk2 = Pk*Pk
        Pl2 = Pl*Pl
        SELECT CASE (Ind)
        CASE (1)
          Calq = (Ph2+Pk2+Pl2)*Ae2
        CASE (2)
          Calq = (Ph2+Pk2)*Ae2 + Pl2*Be2
        CASE (3)
          Calq = (Ph2+Pk2+Ph*Pk)*Ae2 + Pl2*Be2
        CASE (4)
          Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2
        CASE (5)
          Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2 + 2.*Ph*Pl*Ae*Ce*Cosb
        CASE (6)
          Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2 + 2.*Ph*Pk*Ae*Be*Cosg + 2.*Pk*Pl*Be*Ce*Cosa + 2.*Ph*Pl*Ae*Ce*Cosb
        END SELECT
        Difq1 = ABS(q(I)-Calq)
        Dcal = SQRT(Calq)
        Tcal = 2.*pideg*ASIN(wave2*Dcal)
        Dcal = 1./Dcal
        Difd = d(I) - Dcal
        Dift = th(I) - Tcal
        IF ( ABS(Dift).LE.epst ) THEN
          J = J + 1
          IF ( J.LE.Jj ) GOTO 350
        ELSEIF ( Jj.EQ.J ) THEN
          Jj = Jj - 1
        ELSE
          ih(I,J,8) = ih(I,Jj,8)
          ik(I,J,8) = ik(I,Jj,8)
          il(I,J,8) = il(I,Jj,8)
          Jj = Jj - 1
          GOTO 350
        ENDIF
        irj(I,8) = Jj
        IF ( Jj.LE.0 ) THEN
          fwolff = -1000.
          jcount = jcount - 1
          GOTO 99999
        ENDIF
      ENDDO
!
!     Output of the results
!
      Ktestwolff = 1
      Kclef = 1
 400  IF ( Kclef.NE.1 ) THEN
        Ae = par(1)
        Be = par(2)
        Ce = par(3)
        Ae2 = Ae*Ae
        Be2 = Be*Be
        Ce2 = Ce*Ce
        SELECT CASE (Ind)
          CASE (5)
            Cosb = COS(pirad*par(4))
          CASE (6)
            Cosa = COS(pirad*par(4))
            Cosb = COS(pirad*par(5))
            Cosg = COS(pirad*par(6))
        END SELECT
      ENDIF
      SELECT CASE (Ind)
      CASE (2)
        Vol = Dir(1)**2*Dir(2)
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99002) Dir(1), Dir(2), Vol
99002     FORMAT (//20X,'T E T R A G O N A L   S Y S T E M  '//6X,'DIRECT PARAMETERS :',4X,'A=',F9.5,2X,'C=',F9.5,4X,&
     &          'VOLUME=',F9.2)
          CALL AddDICVOLSolution(6,Dir(1),Dir(1),Dir(2),90.0,90.0,90.0,Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99003) Ecart(1), Ecart(2)
99003   FORMAT (6X,'STANDARD DEVIATIONS :',4X,F9.5,4X,F9.5,11X,F9.2)
      CASE (3)
        Vol = Dir(1)**2*Dir(2)*SQRT(3.)/2.
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99004) Dir(1), Dir(2), Vol
99004     FORMAT (//20X,'H E X A G O N A L    S Y S T E M '//6X,'DIRECT PARAMETERS :',4X,'A=',F9.5,2X,'C=',F9.5,4X,  &
     &          'VOLUME=',F9.2)
          CALL AddDICVOLSolution(9,Dir(1),Dir(1),Dir(2),90.0,90.0,120.0,Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99005) Ecart(1), Ecart(2)
99005   FORMAT (6X,'STANDARD DEVIATIONS :',4X,F9.5,4X,F9.5,11X,F9.2)
      CASE (4)
        Vol = Dir(1)*Dir(2)*Dir(3)
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99006) (Dir(I),I=1,3), Vol
99006     FORMAT (//20X,'O R T H O R H O M B I C    S Y S T E M '//2X,'DIRECT PARAMETERS :',4X,'A=',F9.5,2X,'B=',    &
     &          F9.5,2X,'C=',F9.5,3X,'VOLUME=',F8.2)
          CALL AddDICVOLSolution(5,Dir(1),Dir(2),Dir(3),90.0,90.0,90.0,Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99007) (Ecart(I),I=1,3)
99007   FORMAT (2X,'STANDARD DEVIATIONS :',4X,3(F9.5,4X),6X,F8.2)
      CASE (5)
        Vol = Dir(1)*Dir(2)*Dir(3)*SIN(Dir(4)*pirad)
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99008) (Dir(I),I=1,4), Vol
99008     FORMAT (//20X,'M O N O C L I N I C    S Y S T E M '//1X,'DIRECT PARAMETERS :',1X,'A=',F7.4,' B=',F7.4,     &
     &          ' C=',F7.4,1X,'BETA=',F7.3,1X,'VOLUME=',F7.2)
          CALL AddDICVOLSolution(3,Dir(1),Dir(2),Dir(3),90.0,Dir(4),90.0,Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99009) (Ecart(I),I=1,4)
99009   FORMAT (1X,'STANDARD DEVIATIONS :',1X,F7.4,3X,F7.4,3X,F7.4,6X,F7.3/)
      CASE (6)
        Coa = COS(Dir(4)*pirad)
        Cob = COS(Dir(5)*pirad)
        Cog = COS(Dir(6)*pirad)
        Sia = SIN(Dir(4)*pirad)
        Sib = SIN(Dir(5)*pirad)
        Sig = SIN(Dir(6)*pirad)
        Svol = SQRT(1.-Coa*Coa-Cob*Cob-Cog*Cog+2.*Coa*Cob*Cog)
        Vol = Dir(1)*Dir(2)*Dir(3)*Svol
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99010) (Dir(I),I=1,6), Vol
99010     FORMAT (//20X,'T R I C L I N I C    S Y S T E M '//1X,'DIRECT PARAMETERS AND THEIR STANDARD DEVIATIONS :'/1&
     &          X,'A=',F7.4,' B=',F7.4,' C=',F7.4,2X,'ALP=',F7.3,1X,'BET=',F7.3,' GAM=',F7.3,1X,'VOL=',F7.2)
          CALL AddDICVOLSolution(1,Dir(1),Dir(2),Dir(3),Dir(4),Dir(5),Dir(6),Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99011) (Ecart(I),I=1,6)
99011   FORMAT (3X,F7.4,3X,F7.4,3X,F7.4,6X,F7.3,5X,F7.3,5X,F7.3,6X,F5.2/)
      CASE (1)
        Vol = Dir(1)**3
        IF ( Ktestwolff.EQ.0 ) THEN
          WRITE (iw,99012) Dir(1), Vol
99012     FORMAT (//20X,' C U B I C    S Y S T E M '//6X,'DIRECT PARAMETERS :',4X,'A=',F9.5,4X,'VOLUME=',F9.2)
          CALL AddDICVOLSolution(10,Dir(1),Dir(1),Dir(1),90.0,90.0,90.0,Vol)
        ENDIF
        IF ( Ktestwolff.EQ.0 .AND. Kclef.EQ.1 ) WRITE (iw,99013) Ecart(1)
99013   FORMAT (6X,'STANDARD DEVIATIONS :',4X,F9.5,11X,F9.2)
      END SELECT
      IF ( Ktestwolff.NE.1 ) THEN
        IF ( kdens.NE.1 ) THEN
          WRITE (iw,99014) kz
99014     FORMAT (/6X,'NUMBER OF MOLECULES IN THE UNIT CELL :  ',7X,'Z=',I3/)
        ENDIF
        WRITE (iw,99015)
99015   FORMAT (/3X,'H',4X,'K',4X,'L',4X,'DOBS',5X,'DCAL',3X,'DOBS-DCAL',2X,'2TH.OBS',2X,'2TH.CAL',1X,'DIF.2TH.'/)
      ENDIF
      Dtheta = 0.
      Deltaq = 0.
      DO I1 = 1, nini
        I = I1 + n - nini
        IF ( I1.GT.1 .AND. I1.LT.(nini-n+2) ) I = nini - I1 + 2
        IF ( I1.EQ.1 ) I = 1
        Difq = 100.
        Key = 1
        DO J = 1, irj(I,8)
          Ph = ih(I,J,8)
          Pk = ik(I,J,8)
          Pl = il(I,J,8)
          Ph2 = Ph*Ph
          Pk2 = Pk*Pk
          Pl2 = Pl*Pl
          SELECT CASE (Ind)
          CASE (2)
            Calq = (Ph2+Pk2)*Ae2 + Pl2*Be2
          CASE (3)
            Calq = (Ph2+Pk2+Ph*Pk)*Ae2 + Pl2*Be2
          CASE (4)
            Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2
          CASE (5)
            Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2 + 2.*Ph*Pl*Ae*Ce*Cosb
          CASE (6)
            Calq = Ph2*Ae2 + Pk2*Be2 + Pl2*Ce2 + 2.*Ph*Pk*Ae*Be*Cosg + 2.*Pk*Pl*Be*Ce*Cosa + 2.*Ph*Pl*Ae*Ce*Cosb
          CASE DEFAULT
            Calq = (Ph2+Pk2+Pl2)*Ae2
          END SELECT
          Difq1 = ABS(q(I)-Calq)
          Dcal = SQRT(Calq)
          Tcal = 2.*pideg*ASIN(wave2*Dcal)
          Dcal = 1./Dcal
          Difd = d(I) - Dcal
          Dift = th(I) - Tcal
          IF ( Ktestwolff.NE.1 ) THEN
            IF ( Key.EQ.1 ) THEN
              WRITE (iw,99016) ih(I,J,8), ik(I,J,8), il(I,J,8), d(I), Dcal, Difd, th(I), Tcal, Dift
99016         FORMAT (1X,I3,1X,I4,1X,I4,F9.5,F9.5,1X, F9.5,1X,F8.3,1X,F8.3,1X,F7.3)
            ELSE
              WRITE (iw,99017) ih(I,J,8), ik(I,J,8), il(I,J,8),       Dcal, Difd,        Tcal, Dift
99017         FORMAT (1X,I3,1X,I4,1X,I4,9X,1X,F8.5,1X,F9.5,1X,8X,  1X,F8.3,1X,F7.3)
            ENDIF
          ENDIF
          IF ( Difq1.LT.Difq ) THEN
            Difq = Difq1
            Dtet = ABS(Dift)
            Key = 2
          ENDIF
        ENDDO
        Deltaq = Difq + Deltaq
        Dtheta = Dtet + Dtheta
      ENDDO
! Figures of merit
      Deltaq = Deltaq/nini
      Dtheta = Dtheta/nini
      fwolff = q(n)/(2.*nposs*Deltaq)
      IF ( Ktestwolff.EQ.0 ) THEN
        Findex = nini/(nposs*Dtheta)
        Fvar = 100.*(1.-(nposs*Dtheta*Dtheta/(2.*nini*dth*dth)))
        IF ( Fvar.LE.0.1 ) Fvar = 0.0
        WRITE (iw,99018) nini, nposs
99018   FORMAT (//10X,'* NUMBER OF LINES'/15X,'.- INPUT DATA =',I4/15X,'.- CALCULATED =',I4)
        WRITE (iw,99019) Deltaq, Dtheta, epst
99019   FORMAT (//10X,'* MEAN ABSOLUTE DISCREPANCIES'/46X,'<Q> =',E10.4/33X,'<DELTA(2-THETA)> =',E10.4/15X,        &
     &          'MAX. ERROR ACCEPTED (DEG. 2-THETA) =',E10.4)
        WRITE (iw,99020) nini, fwolff, nini, Findex, Dtheta, nposs
99020   FORMAT (//10X,'* FIGURES OF MERIT'/17X,'1.- M(',I3,') =',F7.1,25X,'(REF. 4)'/17X,'2.- F(',I3,') =',F7.1,   &
     &          '(',F6.4,',',I4,')',12X,'(REF. 5)'/)
        IF ( Llave.EQ.2 ) GOTO 600
! TEST N. 2 : FIGURES DE MERITE (A ENLEVER ?)
        R = 2.*dth*nposs
! DTH= ERREUR SUR 2THETA
        Fwtest = TAN(th(n)/2.)/R
        Fitest = 2.*nini/R
        IF ( Fwtest.LE.fwolff .AND. Fitest.LE.Findex ) GOTO 600
        WRITE (iw,99021)
99021   FORMAT (/10X,'WARNING !'/14X,'THE NEXT SOLUTION IS THE SAME AS THE LAST SOLUTION BUT IT IS NOT REFINED.'///)
      ELSEIF ( fwolff.GT.fom ) THEN
        Ktestwolff = 0
        GOTO 400
      ELSE
        fwolff = -1000.
        jcount = jcount - 1
        GOTO 99999
      ENDIF
 500  Llave = 2
      Kclef = 2
      Dir(1) = aa
      Dir(2) = bb
      Dir(3) = cc
      Dir(4) = beta
      IF ( Nrind.EQ.2 ) Dir(2) = cc
      IF ( Ind.EQ.6 ) THEN
        Dir(1) = ar
        Dir(2) = br
        Dir(3) = cr
        Dir(4) = dr
        Dir(5) = er
        Dir(6) = fr
      ENDIF
      Vol = Vap
      DO I = 1, Nrind
        par(I) = Dir(I)
      ENDDO
      CALL PASAJE(Ind)
      DO I = 1, Nrind
        par(I) = rec(I)
      ENDDO
      Ktestwolff = 0
      GOTO 400
 600  v = Vol
      CALL AddDICVOL_M(fwolff)
      CALL AddDICVOL_F(Findex)
      WRITE (iw,99022) v, nini, fwolff, nini, Findex, Dtheta, nposs
99022 FORMAT (2x,'=====>  CELL VOLUME = ',F7.1,' A**3     M(',I2,')=',F6.1,'  F(',I2,')=',F6.1,'(',F6.4,',',I3,')')

99999 END SUBROUTINE AFFPAR
!
!*****************************************************************************
!
      SUBROUTINE PASAJE(Ind)

      USE DICVAR

      IMPLICIT NONE
!
! Dummy arguments
!
      INTEGER, INTENT (IN   ) :: Ind
!
! Local variables
!
      REAL :: Cosa, Cosb, Cosg, Sina, Sinb, Sing, Svol, Vol

      rec(1) = 1./par(1)
      SELECT CASE (Ind)
      CASE (1) ! Cubic
      CASE (2) ! Tetragonal
        rec(2) = 1./par(2)
      CASE (3) ! Hexagonal
        rec(1) = 2./par(1)/SQRT(3.)
        rec(2) = 1./par(2)
      CASE (4) ! Orthorhombic
        rec(2) = 1./par(2)
        rec(3) = 1./par(3)
      CASE (5) ! Monoclinic
        rec(1) = rec(1)/SIN(par(4)*Pirad)
        rec(2) = 1./par(2)
        rec(3) = 1./par(3)/SIN(par(4)*Pirad)
        rec(4) = 180. - par(4)
      CASE (6) ! Triclinic
        Cosa = COS(par(4)*Pirad)
        Cosb = COS(par(5)*Pirad)
        Cosg = COS(par(6)*Pirad)
        Sina = SIN(par(4)*Pirad)
        Sinb = SIN(par(5)*Pirad)
        Sing = SIN(par(6)*Pirad)
        Svol = SQRT(1.-Cosa*Cosa-Cosb*Cosb-Cosg*Cosg+2.*Cosa*Cosb*Cosg)
        Vol = 1./Svol/par(1)/par(2)/par(3)
        rec(1) = par(2)*par(3)*Sina*Vol
        rec(2) = par(1)*par(3)*Sinb*Vol
        rec(3) = par(1)*par(2)*Sing*Vol
        rec(4) = Pideg*ACOS((Cosb*Cosg-Cosa)/Sinb/Sing)
        rec(5) = Pideg*ACOS((Cosa*Cosg-Cosb)/Sina/Sing)
        rec(6) = Pideg*ACOS((Cosa*Cosb-Cosg)/Sina/Sinb)
      END SELECT

      END SUBROUTINE PASAJE
!
!*****************************************************************************
!
