      SUBROUTINE BLKINT(IC,ICORL,NHKL,IB,NB,NHSMAX,NCORL)
C     ---------------------------------------------------
C
      INTEGER IC(*),ICORL(15,*),IB(*)
      LOGICAL ISOLAT
C
      ICUT=15
   1  ICUT=ICUT+5
      IB(1)=1
      DO 30 K=1,NHKL
        ICNEXT=1
        DO 20 J=IB(K),NHKL-1
          IF (IC(J+1).NE.IC(J)) THEN
            ICNEXT=ICNEXT-1
            IF (ICNEXT.EQ.0) THEN
              ISOLAT=.TRUE.
              DO 10 I=1,NCORL
                IF (ABS(ICORL(I,J)).GT.ICUT) THEN
                  ISOLAT=.FALSE.
                  ICNEXT=I
                ENDIF
  10          CONTINUE
              IF (ISOLAT) GOTO 2
            ENDIF
          ENDIF
  20    CONTINUE
   2    IB(K+1)=J+1
        IF ((IB(K+1)-IB(K)).GT.NHSMAX) GOTO 1
        IF (J.GE.NHKL) GOTO 3
  30  CONTINUE
   3  NB=K
      END
      SUBROUTINE CHECKP(W,H,N,JREF,POSDEF)
C     ------------------------------------
C
      REAL    W(N,N),H(N,N),D(50),E(50)
      LOGICAL POSDEF
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
	INTEGER Idummy
	INTEGER PawleyErrorLog
C
      POSDEF=.TRUE.
      CALL VCOPY(H,W,N*N)
      CALL TRED2(W,N,N,D,E,.TRUE.)
      CALL TQLI(D,E,N,N,W,.TRUE.)
      DMIN=0.0
      DMAX=-1.0E10
      DO 10 I=1,N
        DI=D(I)
        IF (DI.LT.DMIN) DMIN=DI
        IF (DI.GT.DMAX) DMAX=DI
  10  CONTINUE
      DTEST=DMIN/(ABS(DMAX)+1.0E-10)
      IF (DTEST.LT.-0.000001) THEN
        POSDEF=.FALSE.
	  Idummy = PawleyErrorLog(1) ! Log the error in the GUI
        WRITE(LPT,100)
        WRITE(LPT,101) JREF,JREF+N-1,DMIN,DMAX
 100    FORMAT(' Poor data - try a reduced range!')
 101    FORMAT(' -ve intensity covariance eigenvalues for reflections ',
     &  I4' to',I4' : ',2F15.5)
      ENDIF
C      DMIN=ABS(DMIN)*1.0001
      DMIN=MAX(ABS(DMAX)*1.0E-6,ABS(DMIN)*1.5)
      DO 20 I=1,N
  20    H(I,I)=H(I,I)+DMIN
      END
      SUBROUTINE CLUMPS(IC,N,NC,NCTOT,MLTP,ALPHA)
C     -------------------------------------------
C
      REAL    ALPHA(*)
      INTEGER IC(*),NC(*),MLTP(*)
C
      J=1
      DO 30 JJ=1,N
        NCJJ=1
        MSUM=MLTP(J)
        DO 10 I=J+1,N
          IF (IC(I).NE.IC(J)) GOTO 1
          NCJJ=NCJJ+1
          MSUM=MSUM+MLTP(I)
  10    CONTINUE
   1    IF (NCJJ.GT.1) THEN
          DO 20 I=J,J+NCJJ-1
  20        ALPHA(I)=FLOAT(MLTP(I))/FLOAT(MSUM)
        ELSE
          ALPHA(J)=1.0
        ENDIF
        J=J+NCJJ
        NC(JJ)=NCJJ
        IF (J.GT.N) GOTO 2
  30  CONTINUE
   2  NCTOT=JJ
      END
      SUBROUTINE COVAR(SIGY,ICORL,NCORL,NC,N,CY)
C     ------------------------------------------
C
      REAL    SIGY(*),CY(N,N)
      INTEGER ICORL(15,*),NC(*)
C
      CALL VRFILL(CY,0.0,N*N)
      JJ=1
      DO 20 J=1,N
        CY(J,J)=SIGY(JJ)*SIGY(JJ)
        SIGYJ=SIGY(JJ)*0.01
        K=JJ+NC(J)
        DO 10 II=1,NCORL
          I=J+II
          IF (I.GT.N) GOTO 1
          CY(I,J)=FLOAT(ICORL(II,JJ))*SIGYJ*SIGY(K)
          CY(J,I)=CY(I,J)
          K=K+NC(I)
  10    CONTINUE
   1    JJ=JJ+NC(J)
  20  CONTINUE
      END
      SUBROUTINE DCLUMP(HY,NY,IC,H,N,ALPHA)
C     -------------------------------------
C
      REAL    HY(NY,NY),H(N,N),ALPHA(*)
      INTEGER IC(*)
C
      CALL VRFILL(H,0.0,N*N)
      IC0=IC(1)-1
      DO 20 J=1,N
        JJ=IC(J)-IC0
        DO 10 I=J,N
          II=IC(I)-IC0
          H(I,J)=HY(II,JJ)*ALPHA(I)*ALPHA(J)
          H(J,I)=H(I,J)
  10    CONTINUE
  20  CONTINUE
      CALL VCOPY(H,HY,N*N)
      END
c
c
c
      SUBROUTINE HBLOCK(IC,SIGY,ICORL,CY,HY,N,NCORL,JREF,POSDEF,MLTP)
C     ---------------------------------------------------------------
C
	INTEGER N,NCORL,JREF
      REAL    SIGY(*),CY(N,N),HY(N,N),ALPHA(50)
      INTEGER IC(*),ICORL(15,*),MLTP(*),NC(50)
      LOGICAL POSDEF
C
      CALL CLUMPS(IC,N,NC,NCTOT,MLTP,ALPHA)
      CALL COVAR(SIGY,ICORL,NCORL,NC,NCTOT,CY)
      CALL XXINVERT(CY,HY,NCTOT,2.0)
      CALL DCLUMP(HY,NCTOT,IC,CY,N,ALPHA)
      CALL CHECKP(CY,HY,N,JREF,POSDEF)
      END
      SUBROUTINE HCVOUT(HKL,IC,X,HX,N,NCORL,POSDEF)
C     ---------------------------------------------
C
      REAL    X(*),HX(N,N),HKL(3,*)
      INTEGER IC(*),ICOR(15)
      LOGICAL POSDEF
C
      PARAMETER (IREFSM=2000)
      COMMON /HCVCMN/ LCV,ICORL(15,IREFSM),ICLUMP(IREFSM)
      COMMON /CORHES/ IHCOV(30,10000)
      COMMON /COUNTE/ KOUNT
C
      DO 30 J=1,N
        SIGXX=HX(J,J)
        IF (SIGXX.LE.1.0E-20) THEN
          SIGX=0.0
        ELSE
          SIGX=SQRT(SIGXX)
          IF (.NOT. POSDEF) SIGX=-SIGX
        ENDIF
        DO 10 I=1,NCORL
  10      ICOR(I)=0
        IMAX=N-J
        IF (IMAX.GT.NCORL) IMAX=NCORL
        II=1
        DO 20 I=J+1,J+IMAX
          CXIJ=ABS(SIGXX*HX(I,I))+1.0E-20
          ICOR(II)=NINT(100.0*HX(I,J)/SQRT(CXIJ))
          II=II+1
  20    CONTINUE
        KOUNT=KOUNT+1
        WRITE(LCV,100) (NINT(HKL(I,J)),I=1,3),X(J),SIGX,IC(J),
     *               (IHCOV(K,KOUNT),K=1,NCORL)
 100    FORMAT(3I5,F10.3,F10.4,I5,15I4)
  30  CONTINUE
      END
      SUBROUTINE HESCOR(ALSQ,MATSZ)
C
C *** HESCOR from HKLOUT ***
C
CH Writes h,k,l list, possibly plus other info, to unit LKH
CP On entry, reflection indices must be in REFH in /REFLNS/
CP IREF, various LOGICALS in /REFIPR give type of refinement -
CP in particular RIET, CAIL, SAPS, APES . .
C
      DIMENSION ALSQ(MATSZ)
C%
C      DIMENSION IH(3),ADIAG(%BVAR%),ICOV(30)
      DIMENSION IH(3),ADIAG(400),ICOV(30)
      CHARACTER *80 FMT2
C

      INCLUDE 'PARAMS.INC'

      PARAMETER (IREFSM=2000)
      COMMON /HCVCMN/ LCV,ICORL(15,IREFSM),ICLUMP(IREFSM)
C
      COMMON /DERBAS/DERIVB(400),LVARB
      COMMON /F4PARS/NGEN4(9,5),F4VAL(3,MF4PAR),
     & F4PAR(3,MF4PAR),KF4PAR(3,MF4PAR),F4PESD(3,MF4PAR),KOM6
      COMMON /NEWOLD/SHIFT,XOLD,XNEW,ESD,IFAM,IGEN,ISPC,
     & NEWIN,KPACK,LKH,SHESD,ISHFT,AVSHFT,AMAXSH
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      COMMON /PRPKFN/ARGI,YNORM,PKFNSP(8,6,9,5),
     & KPFNSP(8,6,9,5),
     & DERPFN(8,6),NPKFSP(8,9,5),TOLER(8,9,
     & 5),NPKGEN(9,5),PKFNVA(8),DYNDVQ(8),
     & DYNDKQ,REFUSE,CYC1,NOPKRF,TOLR(2,5),NFFT,AKNOTS,
     & NBASF4(MPRPKF,2,9),L4END(9),L6ST,L6END
      LOGICAL REFUSE,CYC1,NOPKRF
      COMMON /POINTS/LVRBS(500),LVRPR(500),LBSVR(400),LRDVR(300)
      COMMON /PRBLEM/NFAM,NGENPS(6,9),NSPCPS(6,9),
     & LF1SP(5),LF3SP(10,9,5),LVFST1(6,9,5),
     & LBFST1(6,9,5),NVARF(6,9,5),
     & NBARF(6,9,5),LF6SP(3,5)
      DIMENSION NGENS(6),NSPC(6)
      EQUIVALENCE (NGENS(1),NGENPS(1,1)),(NSPC(1),NSPCPS(1,1))
      COMMON /REFINE/IREF,NCYC,NCYC1,LASTCY,ICYC,MODERR(5),
     & MODEOB(5),IPRNT(20),MAXCOR,IONLY(9),SIMUL,MAG,MPL,
     & FIXED,DONE,CONV
      LOGICAL SIMUL,MAG,MPL,FIXED,DONE
      EQUIVALENCE (MODER,MODERR(1))
      COMMON /REFIPR/RIET,CAIL,SAPS,APES,RAPS,TOF,CN,LX,SR,ED,PRECYC,TIC
      LOGICAL RIET,CAIL,SAPS,APES,RAPS,TOF,CN,LX,SR,ED,PRECYC,TIC
C>> JCC Moved to an include file
	INCLUDE 'REFLNS.INC'
      COMMON /SCRACH/MESSAG,NAMFIL
      CHARACTER *80 ICARD,MESSAG*100,NAMFIL*100
      EQUIVALENCE (ICARD,MESSAG)
      COMMON /SOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /CORHES/ IHCOV(30,10000)
C
C OUT IF LIST NOT WANTED:
      IF (SIMUL) GO TO 100
      IF (IABS(MODERR(JSOURC)).NE.2 .AND. RIET) GO TO 100
C IF CAIL, GET DIAGONAL LSQ MATRIX INVERSE ELEMEMTS:
      IF (CAIL) THEN
        DO 88 I=1,LVARB
  88    ADIAG(I)=SQRT(ELEMAT(ALSQ,MATSZ,I,I))
      ENDIF
      DO 5 I=1,MAXKK(JPHASE)
      IF (FIXED) CALL INDFIX(REFH(1,I),IH)
C CAIL:
      IF (CAIL) THEN
        IF (IPRNT(5) .EQ. 0) THEN
          IF (FIXED) THEN
            WRITE (LKH,FMT2) IH,F4PAR(1,I),
     &      F4PESD(1,I)
          ELSE
            WRITE (LKH,FMT2) (REFH(J,I),J=1,3),F4PAR(1,I),
     &      F4PESD(1,I)
          ENDIF
        ENDIF
        IF (IPRNT(5) .GT. 0) THEN
C SET NO COVARIANCES:
          CALL JGMZER(ICOV,1,30)
C WHICH VARIABLE IS THIS PARAMETER?
          K=KF4PAR(1,I)
C IF FIXED, NO COVARIANCES:
          IF (K .EQ. 0) GO TO 89
C WHICH BASIC VARIABLE (IE ROW OF LSQ MATRIX) IS THIS VARIABLE?
          I1=K
  90      K=LVRBS(I1)
C STARTING INTS MAY BE STRICTLY RELATED TO ONE PREVIOUS:
          IF (K .LT. 0) THEN
            I1=I1-1
            GO TO 90
          ENDIF
C
C IF FIRST INTS, RECORD OFFSET FOR PRINTING CLUMP NUMBER:
          IF (I .EQ. 1) KBASE=K-1
C NOW FIND THE NEXT IPRNT(5) BASICS AFTER K:
          L1=K+IPRNT(5)
          IF (L1 .GT. L4END(JPHASE)) L1=L4END(JPHASE)
          DO 91 L=K+1,L1
  91      ICOV(L-K)=NINT(100.*ELEMAT(ALSQ,MATSZ,K,L)/
     &    (ADIAG(K)*ADIAG(L)))
 89       DO L=1,IPRNT(5)
            IHCOV(L,I)=ICOV(L)
          END DO
        ENDIF
      ENDIF
   5  CONTINUE
 100  RETURN
      END
      SUBROUTINE HKL2HCV(NCORL) 
C     ------------------------------------------------------------
C

      INCLUDE 'PARAMS.INC'
      INTEGER IB(2001)
      REAL    HESSY(50,50),COVARY(50,50)
      COMMON /F4PARS/NGEN4(9,5),F4VAL(3,MF4PAR),
     & F4PAR(3,MF4PAR),KF4PAR(3,MF4PAR),F4PESD(3,MF4PAR),KOM6
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
C>> JCC Moved to an include file
	INCLUDE 'REFLNS.INC'
      PARAMETER (IREFSM=2000)
      REAL SIGI(IREFSM),FSQV(IREFSM)
      INTEGER MLTP(IREFSM)
      COMMON /HCVCMN/ LCV,ICORL(15,IREFSM),ICLUMP(IREFSM)
      LOGICAL POSDEF
C>> Declare NJ
	INTEGER NJ
      DATA    NHSMAX /50/
      COMMON /COUNTE/ KOUNT
      COMMON /CORHES/ IHCOV(30,10000)
C
      KOUNT=0
C
      NHKL=MAXKK(JPHASE)
      DO J=NHKL,2,-1
        IF (ICLUMP(J).EQ.ICLUMP(J-1)) THEN
          IHCOV(1,J-1)=100
            DO II=2,NCORL
               IHCOV(II,J-1)=IHCOV(II-1,J)
            END DO
          END IF
      END DO
C
      DO 30 J=1,NHKL
        MLTP(J)=AMUL(J)
        SIGI(J)=F4PESD(1,J)
        FSQV(J)=F4PAR(1,J)
        DO 32 K=1,NCORL
  32      ICORL(K,J)=NINT(0.99*FLOAT(ICORL(K,J)))
  30  CONTINUE
C
      CALL BLKINT(ICLUMP,ICORL,NHKL,IB,NB,NHSMAX,NCORL)
C
      DO 10 I=1,NB
        J=IB(I)
        NJ=IB(I+1)-J
        CALL HBLOCK(ICLUMP(J),SIGI(J),ICORL(1,J),COVARY,HESSY,
     &  NJ,NCORL,J,POSDEF,MLTP(J))
        CALL HCVOUT(REFH(1,J),ICLUMP(J),FSQV(J),
     &  HESSY,NJ,NCORL,POSDEF)
  10  CONTINUE
C
      END
      SUBROUTINE XXINVERT(A,B,N,X)
C     --------------------------
C
      REAL    A(N,N),B(N,N)
      INTEGER INDEX(100)
C
      CALL VRFILL(B,0.0,N*N)
      DO 10 J=1,N
  10    B(J,J)=X
      CALL XXLUDCMP(A,N,N,INDEX,D)
      DO 20 J=1,N
  20    CALL XXLUBKSB(A,N,N,INDEX,B(1,J))
      END
      SUBROUTINE XXLUBKSB(A,N,NP,INDX,B)
C     --------------------------------
C
      DIMENSION A(NP,NP),INDX(N),B(N)
C
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0) THEN
          DO 11 J=II,I-1
  11        SUM=SUM-A(I,J)*B(J)
        ELSEIF (SUM.NE.0.0) THEN
          II=I
        ENDIF
        B(I)=SUM
  12  CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF (I.LT.N) THEN
          DO 13 J=I+1,N
  13        SUM=SUM-A(I,J)*B(J)
        ENDIF
        B(I)=SUM/A(I,I)
  14  CONTINUE
      END
      SUBROUTINE XXLUDCMP(A,N,NP,INDX,D)
C     --------------------------------
C
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION  A(NP,NP),INDX(N),VV(NMAX)
C
      D=1.0
      DO 12 I=1,N
        AAMAX=0.0
        DO 11 J=1,N
  11      IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
        IF (AAMAX.EQ.0.0) PAUSE' Singular matrix!'
        VV(I)=1.0/AAMAX
  12  CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1) THEN
              DO 13 K=1,I-1
  13            SUM=SUM-A(I,K)*A(K,J)
              A(I,J)=SUM
            ENDIF
  14      CONTINUE
        ENDIF
        AAMAX=0.0
	DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1) THEN
            DO 15 K=1,J-1
  15          SUM=SUM-A(I,K)*A(K,J)
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
  16    CONTINUE
        IF (J.NE.IMAX) THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
  17      CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF (J.NE.N) THEN
          IF (A(J,J).EQ.0.0) A(J,J)=TINY
          DUM=1.0/A(J,J)
            DO 18 I=J+1,N
  18          A(I,J)=A(I,J)*DUM
        ENDIF
  19  CONTINUE
      IF (A(N,N).EQ.0.0) A(N,N)=TINY
      END
!U      SUBROUTINE PIKCAL
!UC
!UC.. Calculates the values of p(i,k) and dp(i,k)/dpsf(q)
!UC>> JCC Moved to an include file
!U	INCLUDE 'REFLNS.INC'
!UC.. Note only 3 phases specifically hardwired here
!U      PARAMETER (MPPTS=15000,MKPTS=150000)
!U      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),
!U     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
!UC
!UC
!UC
!U      RETURN
!U      END
      SUBROUTINE XXVCOPY(X,Y,N)
C     -----------------------
C
      REAL X(*),Y(*)
C
      DO 10 I=1,N
  10    Y(I)=X(I)
      END
      SUBROUTINE XXVRFILL(X,A,N)
C     ------------------------
C
      REAL X(*)
C
      DO 10 I=1,N
  10    X(I)=A
      END
      SUBROUTINE XDELPR
C
C.. Calculates the value of XPKDEL for each reflection
C>> JCC Moved to an include file
	INCLUDE 'REFLNS.INC'

	INCLUDE 'params.inc'
C.. Note only 3 phases specifically hardwired here
      COMMON /REFLNZ/ZARGK(MRFLNZ),ZXDEL(MRFLNZ)
      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      REAL XIDEL(MPPTS)
C
      DO I=1,NPTS-1
        XIDEL(I)=0.5*(ZARGI(I+1)-ZARGI(I))
      END DO
      XIDEL(NPTS)=XIDEL(NPTS-1)
C
      IOBS=1
      DO IR=1,MAXK
        ZXDEL(IR)=XIDEL(NPTS)
        DO I=IOBS,NPTS
          IF (ZARGI(I).GT.ZARGK(IR)) THEN
            IOBS=I
            ZXDEL(IR)=XIDEL(I)
	      KOBZ(IR)=I
            GOTO 30
          END IF
        END DO
 30     CONTINUE
      END DO
C
      RETURN
      END
C
C
C
      SUBROUTINE QNUMPP
C

      INCLUDE 'PARAMS.INC'
      COMMON /PRPKFN/ARGI,YNORM,PKFNSP(8,6,9,5),
     & KPFNSP(8,6,9,5),
     & DERPFN(8,6),NPKFSP(8,9,5),TOLER(8,9,
     & 5),NPKGEN(9,5),PKFNVA(8),DYNDVQ(8),
     & DYNDKQ,REFUSE,CYC1,NOPKRF,TOLR(2,5),NFFT,AKNOTS,
     & NBASF4(MPRPKF,2,9),L4END(9),L6ST,L6END
      COMMON /PHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      LOGICAL PFNVAR
      COMMON /PFNINF/ NUMPFP(8,9,5),PFNVAR(8,9,5)
C
C FOR NOW, IMPOSE PHASE 1, SOURCE 1 WHICH WE HOPE STAY THERE:
      JPHASE=1
      JSOURC=1
C
      DO I=1,NPKGEN(JPHASE,JSOURC)
        NUMPFP(I,JPHASE,JSOURC)=0
        DO J=1,NPKFSP(I,JPHASE,JSOURC)
          IF (KPFNSP(I,J,JPHASE,JSOURC).GT.0) THEN
            NUMPFP(I,JPHASE,JSOURC)=NUMPFP(I,JPHASE,JSOURC)+1
          END IF
        END DO
        PFNVAR(I,JPHASE,JSOURC)=NUMPFP(I,JPHASE,JSOURC).NE.0
C        WRITE(76,*) ' Peak descriptor ',I,NUMPFP(I,JPHASE,JSOURC)
      END DO
C
      END