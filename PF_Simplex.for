C
      FUNCTION CHIQUAD(N,P)
      PARAMETER (MPAR=50)
      REAL CHIQUAD,P(MPAR)
      PARAMETER (MVAL=50)
      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)
C
      CHIQUAD=0.
C      
      DO I=1,NVAL
        XI=XVAL(I)
        ZI=P(1)*XI*XI+P(2)*XI+P(3)
        CTEM=(ZI-YVAL(I))/EVAL(I)
        CHIQUAD=CHIQUAD+CTEM*CTEM
      END DO
C
      RETURN
      END
C
C
C***********************************************************************
C
!U      FUNCTION ROBCHI_CREF(DAT,SIG,FIT,NDAT)
!UC     ---------------------------------
!UC
!U      PARAMETER (MPAR=50)
!U      REAL ROBCHI_CREF,P(MPAR)
!U      PARAMETER (MVAL=50)
!U      COMMON /FUNVAL/ NVAL,XVAL(MVAL),YVAL(MVAL),ZVAL(MVAL),EVAL(MVAL)
!U
!U      REAL DAT(*),SIG(*),FIT(*)
!U      DATA RT2,YJ0,YJ1 /0.7071067814,0.120782238,0.333333333/
!U      DATA ERSMAL,ERFBIG,SMALL /0.03,3.0,1.0E-20/
!UC
!U      CHI=0.0
!U      DO 10 I=1,NVAL
!UC.. Include the d*^2 calculated function on the following line
!UC        FITI= ...
!U! JvdS The following line generated a compiler error (Warning: Variable FITI is used before its value has been defined).
!U! JvdS I assume that FITI = FIT(I).
!U! JvdS FIT(*) is one of the arguments to this routine and is not used anywhere else,
!U! JvdS also, it is inside a loop over I.
!U
!U! JvdS Note that due to this typo, ***NONE*** of the arguments passed to this routine was used.
!U
!U! JvdS The good news is: this function isn't used anywhere anyway.
!U
!U!JvdS the line of code was:
!U!        DIF=RT2*ABS(YVAL(I)-FITI)/EVAL(I)
!U! JvdS and now it is:
!U        DIF=RT2*ABS(YVAL(I)-FIT(I))/EVAL(I)
!U        IF (DIF.LT.ERSMAL) THEN
!U          CHI=CHI+YJ0-YJ1*DIF**2
!U        ELSEIF (DIF.GT.ERFBIG) THEN
!U          CHI=CHI-LOG(DIF)
!U        ELSE
!U          CHI=CHI-LOG(DIF)+LOG(ABS(1.0-ERFC_FUN(DIF))+SMALL)
!U        ENDIF
!U        IF (CHI.LE.-1.0E15) GOTO 1
!U  10  CONTINUE
!U   1  ROBCHI_CREF=-2.0*CHI
!U      END
!UC
!UC
!U      FUNCTION ERFC_FUN(X)
!UC     ----------------
!UC
!U      Z=ABS(X)
!U      T=1.0/(1.0+0.5*Z)
!U      ERFC_FUN=T*EXP(-Z*Z-1.26551223+T*(1.00002368+T*(0.37409196+
!U     *     T*(0.09678418+T*(-0.18628806+T*(0.27886807+T*(-1.13520398+
!U     *     T*(1.48851587+T*(-0.82215223+T*0.17087277)))))))))
!U      IF (X.LT.0) ERFC_FUN=2.0-ERFC_FUN
!U      END
!UC
C***********************************************************************

c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SIMOPT(X,DX,COVAR,N,CHIFUN)
C     --------------------------------------
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose
C     A SIMPLEX-based routine which optimises the values of the N 
C parameters pertaining to the components of the vector X; it also 
C estimates the related covariance matrix.
C
C Parameters
C   ARGUMENT  TYPE  I/O  DIMENSION  DESCRIPTION
C    N        I*4    I       -      No. of parameters to be optimised.
C    X        R*4    I       N      Initial guess.
C    X        R*4    O       N      The optimal answer.
C    DX       R*4    I       N      Initial step-lengths for X.
C    COVAR    R*4    O     N x N    The covariance matrix.
C
C Other Requirements
C     The user must provide a FUNCTION CHIFUN(X) which evaluates 
C Chi-squared (un-normalised) given the vector X.
C
C History
C     D. S. Sivia    9 Feb 1995  Initial release.
C
C-----------------------------------------------------------------------
C
      PARAMETER (MAXITR=10)
      REAL     X(N),DX(N),COVAR(N,N)
      REAL     HESS(2500),DELTA(50),C0,C1(2,50),C2(2500)
      REAL     V(2550),EX(150),C(51)
      INTEGER  IR(51),INDX(50)
      EXTERNAL CHIFUN
      LOGICAL  LERANL
      COMMON   /PKCOM3/ LERANL
!O      DATA     NMAX /50/
C
!O      IF (N.GT.NMAX) STOP' Sorry, too many parameters !'
      LERANL=.FALSE.
      CALL VCOPY(X,V,N)
      CHIMIN=CHIFUN(N,V)
      ZERO=0.0
      ITER=0
C.. Zero things ...
      DO I=1,150
        EX(I)=0.
      END DO
      DO I=1,51
        IR(I)=0
        C(I)=0.
      END DO
   1  ITER=ITER+1
      IF (ITER.GT.MAXITR) GOTO 999
      N1000=N*1000
      CALL SIMPLEX(V,N,DX,EX,C,IR,N1000,CHIFUN)
C
      CALL VCOPY(EX,V,N)
      CHI=CHIFUN(N,V)
C      write(76,*) ' chi',iter,chi
C
C      IF ((1.0-CHI/CHIMIN).GT.0.0001) THEN
      IF ((1.0-CHI/CHIMIN).GT.0.01) THEN
        CHIMIN=CHI
        GOTO 1
      ENDIF
 999  CALL VCOPY(V,X,N)
      LERANL=.TRUE.
      CALL CHINIT(V,N,DELTA,C0,C1,C2,CHIFUN)
      CALL HSINT1(C0,C1,DELTA,N,HESS)
      CALL HSINT2(C0,C1,C2,DELTA,N,HESS)
      CALL INVERT(HESS,COVAR,N,INDX)
      LERANL=.FALSE.
      END
C
C***<utilities>*********************************************************
C
      SUBROUTINE VCOPY(X,Y,N)
C     -----------------------
C
      REAL X(*),Y(*)
C
      DO 10 I=1,N
  10    Y(I)=X(I)
      END
C
      SUBROUTINE VRFILL(X,A,N)
C     ------------------------
C
      REAL X(*)
C
      DO 10 I=1,N
  10    X(I)=A
      END
C
C***<Simplex>*****************************************************
C
      SUBROUTINE SIMPLEX(V,N,D,EX,C,IR,MX,CHIFUN)
C     ------------------------------------
C
      EXTERNAL CHIFUN
      REAL    V(N,*),EX(N,*),C(*),D(*)
      INTEGER IR(*)
      LOGICAL SLOW
      DATA    ALPHA,BETA,GAMA/1.0,0.5,2.0/
C.. Original      DATA    ALPHA,BETA,GAMA/1.0,0.5,2.0/
C
      SLOW=.FALSE.
      N3=3*N
      CAIM=0.0
      C(1)=CHIFUN(N,V(1,1))
      CALL SIMP0(V,D,C,IR,N,CHIFUN)
      CMIN=C(IR(N+1))
C
      ITER=0
      NOLUCK=0
      IRSTRT=0
  10  ITER=ITER+1
      NOLUCK=NOLUCK+1
      IF (ITER.GE.MX .OR. NOLUCK.GT.100) THEN
        CALL SSORT(C,IR,N)
        CALL VCOPY(V(1,IR(N+1)),EX,N)
        IF (ITER.LT.250) THEN
          NOLUCK=0
          DO 20 ID=1,N
  20        D(ID)=D(ID)/10.0
          CALL VCOPY(EX,V,N)
          C(1)=CHIFUN(N,V(1,1))
          CALL SIMP0(V,DSMALL,C,IR,N,CHIFUN)
          CMIN=C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL VRFILL(EX,0.,N3)
      CALL XCENT(V,EX(1,3),IR(1),N)
      CALL XZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0=CHIFUN(N,EX)
      CL=C(IR(N+1))
      CH=C(IR(1))
      CS=C(IR(2))
      IF (C0.LT.CL) THEN
        CALL EXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA,CHIFUN)
      ELSEIF (C0.GT.CS) THEN 
        CALL CONTRACT(CH,CS,C0,C00,EX,
     *EX(1,2),EX(1,3),V(1,IR(1)),BETA,N,CHIFUN)
        IF (C00.LT.CH.AND.C00.LT.C0) THEN
           CALL VCOPY(EX(1,2),EX,N)
           C0=C00
        ELSE
           CALL CONT2(IR(N+1),V,C,N,IR,CHIFUN)
           IRSTRT=IRSTRT+1
           IF (C(IR(N+1)).LT.CMIN) CMIN=C(IR(N+1))
           IF (IRSTRT.GE.5) THEN
             NOLUCK=0
             CALL SSORT(C,IR,N)
             CALL VCOPY(V(1,IR(N+1)),EX,N)
             RETURN
          ENDIF
        ENDIF
      ENDIF
C
      CALL VCOPY(EX,V(1,IR(1)),N)
      C(IR(1))=C0
      CALL SSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP=(CMIN-C(IR(N+1)))/CMIN
C        IF (ABS(DROP).LT.1.0E-4) SLOW=.TRUE.
        IF (ABS(DROP).LT.1.0E-4) SLOW=.TRUE.
        NOLUCK=0
        CMIN=C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT. SLOW) GOTO 10
      CALL VCOPY(V(1,IR(N+1)),EX,N)
      END
C
      SUBROUTINE SIMP0(V,D,C,IR,N,CHIFUN)
C     ----------------------------
C
      EXTERNAL CHIFUN
      REAL    V(N,*),C(*),D(*)
      INTEGER IR(*)
C
      DO 1 I=2,N+1
        CALL VCOPY(V,V(1,I),N)
        V(I-1,I)=V(I-1,I)+D(I-1)
        C(I)= CHIFUN(N,V(1,I))
   1  CONTINUE
      CALL SSORT(C,IR,N)
      END
C
      SUBROUTINE SSORT(C,IR,N)
C     ------------------------
C
      REAL    C(*)
      INTEGER IR(*)
C
      IR(1)=1
      DO 20 J=2,N+1
        DO 10 I=1,J-1
          IF (C(J).LT.C(IR(I))) GOTO 10
          DO 1 II=1,J-I
   1        IR(J+1-II)=IR(J-II)
          IR(I)=J
          GOTO 20
  10    CONTINUE
        IR(J)=J
  20  CONTINUE
      END
C
      SUBROUTINE XCENT(V,XC,IH,N)
C     ---------------------------
C
      REAL V(N,*),XC(*)
C
      XNORM=1.0/FLOAT(N)
      DO 20 J=1,N+1
        IF (J.EQ.IH) GOTO 20
        DO 10 I=1,N
  10      XC(I)=XC(I)+V(I,J)
  20  CONTINUE
      DO 30 I=1,N
  30    XC(I)=XC(I)*XNORM
      END
C
      SUBROUTINE XZERO(XH,X0,XC,N,A)
C     ------------------------------
C
      REAL XH(*),X0(*),XC(*)
C
      DO 1 I=1,N
   1    X0(I)=A*(XC(I)-XH(I))+XC(I)
      END
C
      SUBROUTINE EXPAND(CL,X0,X00,XC,N,C0,G,CHIFUN)
C     --------------------------------------
C
      EXTERNAL CHIFUN
      REAL X0(*),X00(*),XC(*)
C
      DO 1 I=1,N
   1    X00(I)=G*(X0(I)-XC(I))+XC(I)
      C00=CHIFUN(N,X00)
      IF (C00.LT.CL) THEN
        CALL VCOPY(X00,X0,N)
        C0=C00
      ENDIF
      END
C
      SUBROUTINE CONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N,CHIFUN)
C     --------------------------------------------------
C
      EXTERNAL CHIFUN
      REAL X0(*),X00(*),XC(*),XH(*)
C
      IF (C0.LT.CH) THEN
        DO 1 I=1,N
   1      X00(I)=B*(X0(I)-XC(I))+XC(I)
      ELSE
        DO 10 I=1,N
  10      X00(I)=B*(XH(I)-XC(I))+XC(I)
      ENDIF
      C00=CHIFUN(N,X00)
      END
C
      SUBROUTINE CONT2(K,V,C,N,IR,CHIFUN)
C     ----------------------------
C
      EXTERNAL CHIFUN
      REAL     V(N,*),C(*)
      INTEGER  IR(*)
C
      DO 20 J=1,N+1
        IF (J.EQ.K) GOTO 20
        DO 10 I=1,N
          V(I,J)=0.5*(V(I,J)+V(I,K))
          C(J)=CHIFUN(N,V(1,J))
  10    CONTINUE
  20  CONTINUE
      CALL SSORT(C,IR,N)
      END
C
C***<error analysis>****************************************************
C
      SUBROUTINE CHINIT(V,N,DELTA,C0,C1,C2,CHIFUN)
C     -------------------------------------
C
      REAL     V(*),DELTA(*),C0,C1(2,*),C2(N,*)
      EXTERNAL CHIFUN
C
      CALL VRFILL(C2,0.0,N*N)
      C0=CHIFUN(N,V)
      CDELTA=C0/2000.0
      IF (CDELTA.LT.0.5) CDELTA=0.5
      DO 10 I=1,N
  10    CALL DLINIT(V,I,C1(1,I),C1(2,I),C0,CDELTA,DELTA(I),CHIFUN)
      DO 30 J=1,N
        V(J)=V(J)+DELTA(J)
        DO 20 I=J+1,N
          V(I)=V(I)+DELTA(I)
          C2(I,J)=CHIFUN(N,V)
          C2(J,I)=C2(I,J)
          V(I)=V(I)-DELTA(I)
  20    CONTINUE
        V(J)=V(J)-DELTA(J)
  30  CONTINUE
      END
C
      SUBROUTINE DLINIT(X,J,CMINUS,CPLUS,C0,CDELTA,DELTA,CHIFUN)
C     ----------------------------------------------------
C
      EXTERNAL CHIFUN
      REAL X(*)
      DATA FRAC,SMALL,XLARGE /1.0E-6,1.0E-20,1.0E20/
C
      X0=X(J)
      D=FRAC*ABS(X0)+SMALL
      DO 10 I=1,100
        D=D+D
        X(J)=X0-D
        CMINUS=CHIFUN(N,X)
        X(J)=X0+D
        CPLUS=CHIFUN(N,X)
        IF (X(J).GT.XLARGE) GOTO 1
        IF (ABS(CMINUS-C0).GT.CDELTA) GOTO 1
        IF (ABS(CPLUS-C0).GT.CDELTA) GOTO 1
  10  CONTINUE
   1  DELTA=D
      X(J)=X0
      END
C
      SUBROUTINE HSINT1(C0,C1,D,N,HS)
C     -------------------------------
C
      REAL C0,C1(2,*),D(*),HS(N,*)
C
      DO 10 I=1,N
  10    HS(I,I)=(C1(2,I)-C0-C0+C1(1,I))/(D(I)*D(I))
      END
C
      SUBROUTINE HSINT2(C0,C1,C2,D,N,HS)
C     ----------------------------------
C
      REAL C0,C1(2,*),C2(N,*),D(*),HS(N,*)
C
      DO 20 J=1,N
        DO 10 I=J+1,N
          HS(I,J)=(C2(I,J)-C1(2,I)-C1(2,J)+C0)/(D(I)*D(J))
          HS(J,I)=HS(I,J)
  10    CONTINUE
  20  CONTINUE
      END
C
      SUBROUTINE INVERT(HESS,COVAR,N,INDX)
C     ------------------------------------
C
      REAL    HESS(N,*),COVAR(N,*)
      INTEGER INDX(*)
C>> JCC This is for testing for mathematical errors used to PAUSE the program: THe pause seemed to#
C>> be causing a repeated CMD window to appear on screen ....
	INTEGER IBMBER
	COMMON / CCSLER / IBMBER 
C
      CALL VRFILL(COVAR,0.0,N*N)
      DO 10 I=1,N
  10    COVAR(I,I)=2.0
      CALL LUDCMP(HESS,N,N,INDX,D)
C>> JCC Trap for singular matrices
	IF ( IBMBER .EQ. 1) RETURN 
      DO 20 I=1,N
  20    CALL LUBKSB(HESS,N,N,INDX,COVAR(1,I))
      END
C
C----------------------------------------------------------------------C
C  Two subroutines, for Cholesky decomposition of a matrix, copied out C
C  of Numerical Recipes.                                               C
C----------------------------------------------------------------------C
C
      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
C     --------------------------------
C
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION  A(NP,NP),INDX(N),VV(NMAX)
C>> JCC This is for testing for mathematical errors used to PAUSE the program: THe pause seemed to#
C>> be causing a repeated CMD window to appear on screen ....
	INTEGER IBMBER
	COMMON / CCSLER / IBMBER 
C
      D=1.0
      DO 12 I=1,N
        AAMAX=0.0
        DO 11 J=1,N
  11      IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
C>> JCC Removed the PAUSE
        IF (AAMAX.EQ.0.0) THEN
		IBMBER = 1
		RETURN
	  ENDIF
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
C
      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
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
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
