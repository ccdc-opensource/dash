!
!*****************************************************************************
!
      SUBROUTINE BLKINT(IC,ICORL,NHKL,IB,NB,NHSMAX,NCORL)

      INTEGER IC(*), ICORL(15,*), IB(*)
      LOGICAL ISOLAT

      ICUT = 15
    1 ICUT = ICUT + 5
      IB(1) = 1
      DO K = 1, NHKL
        ICNEXT = 1
        DO J = IB(K), NHKL - 1
          IF (IC(J+1).NE.IC(J)) THEN
            ICNEXT = ICNEXT - 1
            IF (ICNEXT.EQ.0) THEN
              ISOLAT = .TRUE.
              DO I = 1, NCORL
                IF (ABS(ICORL(I,J)).GT.ICUT) THEN
                  ISOLAT = .FALSE.
                  ICNEXT = I
                ENDIF
              ENDDO
              IF (ISOLAT) GOTO 2
            ENDIF
          ENDIF
        ENDDO
    2   IB(K+1) = J + 1
        IF ((IB(K+1)-IB(K)).GT.NHSMAX) GOTO 1
        IF (J.GE.NHKL) GOTO 3
      ENDDO
    3 NB = K

      END SUBROUTINE BLKINT
!
!*****************************************************************************
!
      SUBROUTINE CHECKP(W,H,N,JREF,POSDEF)

      REAL W(N,N), H(N,N), D(50), E(50)
      LOGICAL POSDEF
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
      INTEGER Idummy
      INTEGER PawleyErrorLog

      POSDEF = .TRUE.
      CALL VCOPY(H,W,N*N)
      CALL TRED2(W,N,N,D,E,.TRUE.)
      CALL TQLI(D,E,N,N,W,.TRUE.)
      DMIN = 0.0
      DMAX = -1.0E10
      DO I = 1, N
        DI = D(I)
        IF (DI.LT.DMIN) DMIN = DI
        IF (DI.GT.DMAX) DMAX = DI
      ENDDO
      DTEST = DMIN/(ABS(DMAX)+1.0E-10)
      IF (DTEST.LT.-0.000001) THEN
        POSDEF = .FALSE.
        Idummy = PawleyErrorLog(1)   ! Log the error in the GUI
        WRITE (LPT,100)
  100   FORMAT (' Poor data - try a reduced range!')
        WRITE (LPT,101) JREF, JREF + N - 1, DMIN, DMAX
  101   FORMAT (' -ve intensity covariance eigenvalues for reflections ',I4' to',I4' : ',2F15.5)
      ENDIF
!      DMIN=ABS(DMIN)*1.0001
      DMIN = MAX(ABS(DMAX)*1.0E-6,ABS(DMIN)*1.5)
      DO I = 1, N
        H(I,I) = H(I,I) + DMIN
      ENDDO

      END SUBROUTINE CHECKP
!
!*****************************************************************************
!
      SUBROUTINE CLUMPS(IC,N,NC,NCTOT,MLTP,ALPHA)

      REAL ALPHA(*)
      INTEGER IC(*), NC(*), MLTP(*)

      J = 1
      DO JJ = 1, N
        NCJJ = 1
        MSUM = MLTP(J)
        DO I = J + 1, N
          IF (IC(I).NE.IC(J)) GOTO 1
          NCJJ = NCJJ + 1
          MSUM = MSUM + MLTP(I)
        ENDDO
    1   IF (NCJJ.GT.1) THEN
          DO I = J, J + NCJJ - 1
            ALPHA(I) = FLOAT(MLTP(I))/FLOAT(MSUM)
          ENDDO
        ELSE
          ALPHA(J) = 1.0
        ENDIF
        J = J + NCJJ
        NC(JJ) = NCJJ
        IF (J.GT.N) GOTO 2
      ENDDO
    2 NCTOT = JJ

      END SUBROUTINE CLUMPS
!
!*****************************************************************************
!
      SUBROUTINE COVAR(SIGY,ICORL,NCORL,NC,N,CY)

      REAL SIGY(*), CY(N,N)
      INTEGER ICORL(15,*), NC(*)

      CALL VRFILL(CY,0.0,N*N)
      JJ = 1
      DO J = 1, N
        CY(J,J) = SIGY(JJ)*SIGY(JJ)
        SIGYJ = SIGY(JJ)*0.01
        K = JJ + NC(J)
        DO II = 1, NCORL
          I = J + II
          IF (I.GT.N) GOTO 1
          CY(I,J) = FLOAT(ICORL(II,JJ))*SIGYJ*SIGY(K)
          CY(J,I) = CY(I,J)
          K = K + NC(I)
        ENDDO
    1   JJ = JJ + NC(J)
      ENDDO

      END SUBROUTINE COVAR
!
!*****************************************************************************
!
      SUBROUTINE DCLUMP(HY,NY,IC,H,N,ALPHA)

      REAL HY(NY,NY), H(N,N), ALPHA(*)
      INTEGER IC(*)

      CALL VRFILL(H,0.0,N*N)
      IC0 = IC(1) - 1
      DO J = 1, N
        JJ = IC(J) - IC0
        DO I = J, N
          II = IC(I) - IC0
          H(I,J) = HY(II,JJ)*ALPHA(I)*ALPHA(J)
          H(J,I) = H(I,J)
        ENDDO
      ENDDO
      CALL VCOPY(H,HY,N*N)

      END SUBROUTINE DCLUMP
!
!*****************************************************************************
!
      SUBROUTINE HBLOCK(IC,SIGY,ICORL,CY,HY,N,NCORL,JREF,POSDEF,MLTP)

      INTEGER N, NCORL, JREF
      REAL SIGY(*), CY(N,N), HY(N,N), ALPHA(50)
      INTEGER IC(*), ICORL(15,*), MLTP(*), NC(50)
      LOGICAL POSDEF

      CALL CLUMPS(IC,N,NC,NCTOT,MLTP,ALPHA)
      CALL COVAR(SIGY,ICORL,NCORL,NC,NCTOT,CY)
      CALL XXINVERT(CY,HY,NCTOT,2.0)
      CALL DCLUMP(HY,NCTOT,IC,CY,N,ALPHA)
      CALL CHECKP(CY,HY,N,JREF,POSDEF)

      END SUBROUTINE HBLOCK
!
!*****************************************************************************
!
      SUBROUTINE HCVOUT(HKL,IC,X,HX,N,NCORL,POSDEF)

      IMPLICIT NONE

      REAL HKL(3,*)
      INTEGER IC(*)
      INTEGER N
      REAL    X(*), HX(N,N)
      INTEGER NCORL
      LOGICAL POSDEF

      INTEGER     IREFSM
      PARAMETER ( IREFSM = 2000 )
      INTEGER         LCV, ICORL,            ICLUMP
      COMMON /HCVCMN/ LCV, ICORL(15,IREFSM), ICLUMP(IREFSM)
      INTEGER         IHCOV
      COMMON /CORHES/ IHCOV(30,10000)
      INTEGER         KOUNT
      COMMON /COUNTE/ KOUNT

      INTEGER J
      REAL    SIGX, SIGXX
      INTEGER I, K
      INTEGER NumOfCorrel

      DO J = 1, N
        SIGXX = HX(J,J)
        IF (SIGXX.LE.1.0E-20) THEN
          SIGX = 0.0
        ELSE
          SIGX = SQRT(SIGXX)
          IF (.NOT.POSDEF) SIGX = -SIGX
        ENDIF
        KOUNT = KOUNT + 1
! Starting at the end, count the number of zeros (these needn't be written out)
        NumOfCorrel = NCORL
        DO WHILE ((NumOfCorrel .GE. 2) .AND. (IHCOV(NumOfCorrel,KOUNT) .EQ. 0))
          NumOfCorrel = NumOfCorrel - 1
        ENDDO
        WRITE (LCV,100) (NINT(HKL(I,J)),I=1,3), X(J), SIGX, IC(J), (IHCOV(K,KOUNT),K=1,NumOfCorrel)
  100   FORMAT (3I5,1X,F12.3,1X,F12.4,1X,I5,15I4)
      ENDDO

      END SUBROUTINE HCVOUT
!
!*****************************************************************************
!
      SUBROUTINE HESCOR(ALSQ,MATSZ)

!
! *** HESCOR from HKLOUT ***
!
!H Writes h,k,l list, possibly plus other info, to unit LKH
!P On entry, reflection indices must be in REFH in /REFLNS/
!P IREF, various LOGICALS in /REFIPR give type of refinement -
!P in particular RIET, CAIL, SAPS, APES . .
!
      DIMENSION ALSQ(MATSZ)

      INCLUDE 'PARAMS.INC'

      PARAMETER (IREFSM=2000)
      COMMON /HCVCMN/ LCV, ICORL(15,IREFSM), ICLUMP(IREFSM)

      COMMON /DERBAS/ DERIVB(MaxBVar), LVARB
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6
      COMMON /NEWOLD/ SHIFT, XOLD, XNEW, ESD, IFAM, IGEN, ISPC, NEWIN,  &
     &                KPACK, LKH, SHESD, ISHFT, AVSHFT, AMAXSH

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /POINTS/ LVRBS(500), LVRPR(500), LBSVR(400), LRDVR(300)
      COMMON /PRBLEM/ NFAM, NGENPS(6,9), NSPCPS(6,9), LF1SP(5),         &
     &                LF3SP(10,9,5), LVFST1(6,9,5), LBFST1(6,9,5),      &
     &                NVARF(6,9,5), NBARF(6,9,5), LF6SP(3,5)
      DIMENSION NGENS(6), NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1))
      EQUIVALENCE (NSPC(1),NSPCPS(1,1))
      COMMON /REFINE/ IREF, NCYC, NCYC1, LASTCY, ICYC, MODERR(5),       &
     &                MODEOB(5), IPRNT(20), MAXCOR, IONLY(9), SIMUL,    &
     &                MAG, MPL, FIXED, DONE, CONV
      LOGICAL SIMUL, MAG, MPL, FIXED, DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/ RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC
      LOGICAL RIET, CAIL, SAPS, APES, RAPS, TOF, CN, LX, SR, ED, PRECYC, TIC

      INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/ MESSAG, NAMFIL
      CHARACTER*80 ICARD, MESSAG*100, NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      COMMON /CORHES/ IHCOV(30,10000)

      DIMENSION IH(3), ADIAG(MaxBVar), ICOV(30)
      CHARACTER*80 FMT2

! OUT IF LIST NOT WANTED:
      IF (SIMUL) GOTO 100
      IF (IABS(MODERR(JSOURC)).NE.2 .AND. RIET) GOTO 100
! IF CAIL, GET DIAGONAL LSQ MATRIX INVERSE ELEMENTS:
      IF (CAIL) THEN
        DO I = 1, LVARB
          ADIAG(I) = SQRT(ELEMAT(ALSQ,MATSZ,I,I))
        ENDDO
      ENDIF
      DO I = 1, MAXKK(JPHASE)
        IF (FIXED) CALL INDFIX(rHKL(1,I),IH)
! CAIL:
        IF (CAIL) THEN
          IF (IPRNT(5).EQ.0) THEN
            IF (FIXED) THEN
              WRITE (LKH,FMT2) IH, F4PAR(1,I), F4PESD(1,I)
            ELSE
              WRITE (LKH,FMT2) (rHKL(J,I),J=1,3), F4PAR(1,I), F4PESD(1,I)
            ENDIF
          ENDIF
          IF (IPRNT(5).GT.0) THEN
! SET NO COVARIANCES:
            CALL JGMZER(ICOV,1,30)
! WHICH VARIABLE IS THIS PARAMETER?
            K = KF4PAR(1,I)
! IF FIXED, NO COVARIANCES:
            IF (K.EQ.0) GOTO 89
! WHICH BASIC VARIABLE (IE ROW OF LSQ MATRIX) IS THIS VARIABLE?
            I1 = K
   90       K = LVRBS(I1)
! STARTING INTS MAY BE STRICTLY RELATED TO ONE PREVIOUS:
            IF (K.LT.0) THEN
              I1 = I1 - 1
              GOTO 90
            ENDIF
! IF FIRST INTS, RECORD OFFSET FOR PRINTING CLUMP NUMBER:
            IF (I.EQ.1) KBASE = K - 1
! NOW FIND THE NEXT IPRNT(5) BASICS AFTER K:
            L1 = K + IPRNT(5)
            IF (L1.GT.L4END(JPHASE)) L1 = L4END(JPHASE)
            DO L = K + 1, L1
              ICOV(L-K) = NINT(100.*ELEMAT(ALSQ,MATSZ,K,L)/(ADIAG(K)*ADIAG(L)))
            ENDDO
   89       DO L = 1, IPRNT(5)
              IHCOV(L,I) = ICOV(L)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
  100 RETURN

      END SUBROUTINE HESCOR
!
!*****************************************************************************
!
      SUBROUTINE HKL2HCV(NCORL)

      USE REFVAR

      INCLUDE 'PARAMS.INC'

      INTEGER IB(2001)
      REAL HESSY(50,50), COVARY(50,50)
      COMMON /F4PARS/ NGEN4(9,5), F4VAL(3,MF4PAR), F4PAR(3,MF4PAR),     &
     &                KF4PAR(3,MF4PAR), F4PESD(3,MF4PAR), KOM6

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      INCLUDE 'REFLNS.INC'
      PARAMETER (IREFSM=2000)
      REAL SIGI(IREFSM), FSQV(IREFSM)
      INTEGER MLTP(IREFSM)
      COMMON /HCVCMN/ LCV, ICORL(15,IREFSM), ICLUMP(IREFSM)
      LOGICAL POSDEF
      INTEGER NJ
      DATA NHSMAX/50/
      COMMON /COUNTE/ KOUNT
      COMMON /CORHES/ IHCOV(30,10000)

      KOUNT = 0
      NHKL = MAXKK(JPHASE)
      DO J = NHKL, 2, -1
        IF (ICLUMP(J).EQ.ICLUMP(J-1)) THEN
          IHCOV(1,J-1) = 100
          DO II = 2, NCORL
            IHCOV(II,J-1) = IHCOV(II-1,J)
          ENDDO
        ENDIF
      ENDDO
      DO J = 1, NHKL
        MLTP(J) = AMUL(J)
        SIGI(J) = F4PESD(1,J)
        FSQV(J) = F4PAR(1,J)
        DO K = 1, NCORL
          ICORL(K,J) = NINT(0.99*FLOAT(ICORL(K,J)))
        ENDDO
      ENDDO
      CALL BLKINT(ICLUMP,ICORL,NHKL,IB,NB,NHSMAX,NCORL)
      DO I = 1, NB
        J = IB(I)
        NJ = IB(I+1) - J
        CALL HBLOCK(ICLUMP(J),SIGI(J),ICORL(1,J),COVARY,HESSY,NJ,NCORL,J,POSDEF,MLTP(J))
        CALL HCVOUT(rHKL(1,J),ICLUMP(J),FSQV(J),HESSY,NJ,NCORL,POSDEF)
      ENDDO

      END SUBROUTINE HKL2HCV
!
!*****************************************************************************
!
      SUBROUTINE XXINVERT(A,B,N,X)

      REAL A(N,N), B(N,N)
      INTEGER INDEX(100)

      CALL VRFILL(B,0.0,N*N)
      DO J = 1, N
        B(J,J) = X
      ENDDO
      CALL XXLUDCMP(A,N,N,INDEX,D)
      DO J = 1, N
        CALL XXLUBKSB(A,N,N,INDEX,B(1,J))
      ENDDO

      END SUBROUTINE XXINVERT
!
!*****************************************************************************
!
      SUBROUTINE XXLUBKSB(A,N,NP,INDX,B)

      DIMENSION A(NP,NP), INDX(N), B(N)

      II = 0
      DO I = 1, N
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IF (II.NE.0) THEN
          DO J = II, I - 1
            SUM = SUM - A(I,J)*B(J)
          ENDDO
        ELSEIF (SUM.NE.0.0) THEN
          II = I
        ENDIF
        B(I) = SUM
      ENDDO
      DO I = N, 1, -1
        SUM = B(I)
        IF (I.LT.N) THEN
          DO J = I + 1, N
            SUM = SUM - A(I,J)*B(J)
          ENDDO
        ENDIF
        B(I) = SUM/A(I,I)
      ENDDO

      END SUBROUTINE XXLUBKSB
!
!*****************************************************************************
!
      SUBROUTINE XXLUDCMP(A,N,NP,INDX,D)

      INTEGER    MVAR
      PARAMETER (MVAR = 100)
      PARAMETER (TINY=1.0E-20)
      DIMENSION A(NP,NP), INDX(N), VV(MVAR)

      D = 1.0
      DO I = 1, N
        AAMAX = 0.0
        DO J = 1, N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX = ABS(A(I,J))
        ENDDO
        IF (AAMAX.EQ.0.0) PAUSE ' Singular matrix!'
        VV(I) = 1.0/AAMAX
      ENDDO
      DO J = 1, N
        IF (J.GT.1) THEN
          DO I = 1, J - 1
            SUM = A(I,J)
            IF (I.GT.1) THEN
              DO K = 1, I - 1
                SUM = SUM - A(I,K)*A(K,J)
              ENDDO
              A(I,J) = SUM
            ENDIF
          ENDDO
        ENDIF
        AAMAX = 0.0
        DO I = J, N
          SUM = A(I,J)
          IF (J.GT.1) THEN
            DO K = 1, J - 1
              SUM = SUM - A(I,K)*A(K,J)
            ENDDO
            A(I,J) = SUM
          ENDIF
          DUM = VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
        ENDDO
        IF (J.NE.IMAX) THEN
          DO K = 1, N
            DUM = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K) = DUM
          ENDDO
          D = -D
          VV(IMAX) = VV(J)
        ENDIF
        INDX(J) = IMAX
        IF (J.NE.N) THEN
          IF (A(J,J).EQ.0.0) A(J,J) = TINY
          DUM = 1.0/A(J,J)
          DO I = J + 1, N
            A(I,J) = A(I,J)*DUM
          ENDDO
        ENDIF
      ENDDO
      IF (A(N,N).EQ.0.0) A(N,N) = TINY

      END SUBROUTINE XXLUDCMP
!
!*****************************************************************************
!
      SUBROUTINE XDELPR
! Calculates the value of XPKDEL for each reflection

      INCLUDE 'PARAMS.INC'
      INCLUDE 'REFLNS.INC'

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL XIDEL(MOBS)

      DO I = 1, NPTS - 1
        XIDEL(I) = 0.5*(ZARGI(I+1)-ZARGI(I))
      ENDDO
      XIDEL(NPTS) = XIDEL(NPTS-1)
      IOBS = 1
      DO IR = 1, MAXK
        ZXDEL(IR) = XIDEL(NPTS)
        DO I = IOBS, NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            IOBS = I
            ZXDEL(IR) = XIDEL(I)
            KOBZ(IR) = I
            GOTO 30
          ENDIF
        ENDDO
   30   CONTINUE
      ENDDO

      END SUBROUTINE XDELPR
!
!*****************************************************************************
!
      SUBROUTINE QNUMPP

      INCLUDE 'PARAMS.INC'

      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),    &
     &                DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),         &
     &                NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE,&
     &                CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,            &
     &                NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      LOGICAL         PFNVAR
      COMMON /PFNINF/ PFNVAR(8,9,5)

      INTEGER NUMPFP
!
! FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
      JPHASE = 1
      JSOURC = 1
      DO I = 1, NPKGEN(JPHASE,JSOURC)
        NUMPFP = 0
        DO J = 1, NPKFSP(I,JPHASE,JSOURC)
          IF (KPFNSP(I,J,JPHASE,JSOURC).GT.0) NUMPFP = NUMPFP + 1
        ENDDO
        PFNVAR(I,JPHASE,JSOURC) = (NUMPFP .NE. 0)
      ENDDO

      END SUBROUTINE QNUMPP
!
!*****************************************************************************
!
