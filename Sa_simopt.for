C
      FUNCTION SACCHI(N,P)
      PARAMETER (MPAR=50)
      REAL*8 CHIANS,DBLEP(MPAR)
      REAL SACCHI,P(MPAR)
      COMMON /SIMSTO/ NPAR,IP(100),PSTORE(100)
      common /sapars/ nvar,ns,nt,neps,maxevl,iprint,iseed1,iseed2
C
      DO I=1,NVAR
        DBLEP(I)=DBLE(PSTORE(I))
c	write(6,*) i,pstore(i)
      END DO
      DO II=1,N
        I=IP(II)
        DBLEP(I)=DBLE(P(II))
c	write(6,*) ii,i,p(ii)
      END DO
      CALL FCN(NPAR,DBLEP,CHIANS)
      SACCHI=SNGL(CHIANS)
      CALL PutSimplexChisq(SACCHI)
C
      RETURN
      END
c
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SA_SIMOPT(X,DX,COVAR,N)
C     -------------------------------
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
C     The user must provide a FUNCTION SACCHI(X) which evalutes 
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
      EXTERNAL SACCHI
      DATA     NMAX /50/
C
C...      IF (N.GT.NMAX) STOP' Sorry, too many parameters !'
      CALL SAVCOPY(X,V,N)
      CHIMIN=SACCHI(N,V)
c      WRITE(*,*)
c      WRITE(*,100) CHIMIN
c 100  FORMAT('  New chi-squared = ',F12.4)
      ZERO=0.0
!      WRITE(63,*) ZERO,CHIMIN,ZERO,ZERO
      ITER=0
   1  ITER=ITER+1
      IF (ITER.GT.MAXITR) GOTO 999
      CALL SASIMPLEX(V,N,DX,EX,C,IR,N*1000)
c
C
c      WRITE(*,986)
c 986  FORMAT('  Current parameter listing (Simplex minimisation):')
c      WRITE(*,988)
c 988  FORMAT('    #    Current value  ')
C             '123123451234567890121234512345678902121234567890212123456789012'
c	DO I=1,N
c        WRITE(*,991) I,SNGL(V(I))
c 991    FORMAT(I5,3X,F12.5)
c 	END DO
C
c
c	write(6,*) (v(ii),ii=1,n)
      CALL SAVCOPY(EX,V,N)
      CHI=SACCHI(N,V)
c      WRITE(*,100) CHI
      IF ((1.0-CHI/CHIMIN).GT.0.0001) THEN
        CHIMIN=CHI
        GOTO 1
      ENDIF
 999  CALL SAVCOPY(V,X,N)
c      CALL CHINIT(V,N,DELTA,C0,C1,C2)
c      CALL HSINT1(C0,C1,DELTA,N,HESS)
c      CALL HSINT2(C0,C1,C2,DELTA,N,HESS)
c      CALL INVERT(HESS,COVAR,N,INDX)
      END
C
C***<utilities>*********************************************************
C
      SUBROUTINE SAVCOPY(X,Y,N)
C     -----------------------
C
      REAL X(*),Y(*)

      DO 10 I=1,N
  10    Y(I)=X(I)
      END
C
      SUBROUTINE SAVRFILL(X,A,N)
C     ------------------------
C
      REAL X(*)

      DO 10 I=1,N
 10    X(I)=A
      END
C
C***<Simplex>*****************************************************
C
      SUBROUTINE SASIMPLEX(V,N,D,EX,C,IR,MX)
C     ------------------------------------
C
      REAL    V(N,*),EX(N,*),C(*),D(*)
      INTEGER IR(*)
      LOGICAL RESTART,SLOW
      DATA    ALPHA,BETA,GAMA/1.0,0.5,2.0/
C
      SLOW=.FALSE.
      N3=3*N
      CAIM=0.0
      C(1)=SACCHI(N,V(1,1))
      CALL SASIMP0(V,D,C,IR,N)
      CMIN=C(IR(N+1))
C
      ITER=0
      NOLUCK=0
      IRSTRT=0
  10  ITER=ITER+1
      NOLUCK=NOLUCK+1
      IF (ITER.GE.MX .OR. NOLUCK.GT.100) THEN
        CALL SASSORT(C,IR,N)
        CALL SAVCOPY(V(1,IR(N+1)),EX,N)
        IF (ITER.LT.250) THEN
          NOLUCK=0
          DO 20 ID=1,N
  20        D(ID)=D(ID)/10.0
          CALL SAVCOPY(EX,V,N)
          C(1)=SACCHI(N,V(1,1))
          CALL SASIMP0(V,DSMALL,C,IR,N)
          CMIN=C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL SAVRFILL(EX,0.,N3)
      CALL SAXCENT(V,EX(1,3),IR(1),N)
      CALL SAXZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0=SACCHI(N,EX)
      CL=C(IR(N+1))
      CH=C(IR(1))
      CS=C(IR(2))
      IF (C0.LT.CL) THEN
        CALL SAEXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA)
      ELSEIF (C0.GT.CS) THEN 
      CALL SACONTRACT(CH,CS,C0,C00,EX,EX(1,2),EX(1,3),V(1,IR(1)),BETA,N)
        IF (C00.LT.CH.AND.C00.LT.C0) THEN
           CALL SAVCOPY(EX(1,2),EX,N)
           C0=C00
        ELSE
           CALL SACONT2(IR(N+1),V,C,N,IR)
           IRSTRT=IRSTRT+1
           IF (C(IR(N+1)).LT.CMIN) CMIN=C(IR(N+1))
           IF (IRSTRT.GE.5) THEN
             NOLUCK=0
             CALL SASSORT(C,IR,N)
             CALL SAVCOPY(V(1,IR(N+1)),EX,N)
             RETURN
          ENDIF
        ENDIF
      ENDIF
C
      CALL SAVCOPY(EX,V(1,IR(1)),N)
      C(IR(1))=C0
      CALL SASSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP=(CMIN-C(IR(N+1)))/CMIN
        IF (ABS(DROP).LT.1.0E-4) SLOW=.TRUE.
        NOLUCK=0
        CMIN=C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT. SLOW) GOTO 10
      CALL SAVCOPY(V(1,IR(N+1)),EX,N)
      END
C
      SUBROUTINE SASIMP0(V,D,C,IR,N)
C     ----------------------------
C
      REAL    V(N,*),C(*),D(*)
      INTEGER IR(*)
C
      DO 1 I=2,N+1
        CALL SAVCOPY(V,V(1,I),N)
        V(I-1,I)=V(I-1,I)+D(I-1)
        C(I)= SACCHI(N,V(1,I))
   1  CONTINUE
      CALL SASSORT(C,IR,N)
      END
C
      SUBROUTINE SASSORT(C,IR,N)
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
      SUBROUTINE SAXCENT(V,XC,IH,N)
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
      SUBROUTINE SAXZERO(XH,X0,XC,N,A)
C     ------------------------------
C
      REAL XH(*),X0(*),XC(*)
C
      DO 1 I=1,N
   1    X0(I)=A*(XC(I)-XH(I))+XC(I)
      END
C
      SUBROUTINE SAEXPAND(CL,X0,X00,XC,N,C0,G)
C     --------------------------------------
C
      REAL X0(*),X00(*),XC(*)
C
      DO 1 I=1,N
   1    X00(I)=G*(X0(I)-XC(I))+XC(I)
      C00=SACCHI(N,X00)
      IF (C00.LT.CL) THEN
        CALL SAVCOPY(X00,X0,N)
        C0=C00
      ENDIF
      END
C
      SUBROUTINE SACONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N)
C     --------------------------------------------------
C
      REAL X0(*),X00(*),XC(*),XH(*)
C
      IF (C0.LT.CH) THEN
        DO 1 I=1,N
   1      X00(I)=B*(X0(I)-XC(I))+XC(I)
      ELSE
        DO 10 I=1,N
  10      X00(I)=B*(XH(I)-XC(I))+XC(I)
      ENDIF
      C00=SACCHI(N,X00)
      END
C
      SUBROUTINE SACONT2(K,V,C,N,IR)
C     ----------------------------
C
      REAL     V(N,*),C(*)
      INTEGER  IR(*)
C
      DO 20 J=1,N+1
        IF (J.EQ.K) GOTO 20
        DO 10 I=1,N
          V(I,J)=0.5*(V(I,J)+V(I,K))
          C(J)=SACCHI(N,V(1,J))
  10    CONTINUE
  20  CONTINUE
      CALL SASSORT(C,IR,N)
      END
C
C***<error analysis>****************************************************
C
C      SUBROUTINE CHINIT(V,N,DELTA,C0,C1,C2)
C     -------------------------------------
C
C      REAL     V(*),DELTA(*),C0,C1(2,*),C2(N,*)
C      EXTERNAL SACCHI
C
C      CALL VRFILL(C2,0.0,N*N)
C      C0=SACCHI(N,V)
C      CDELTA=C0/2000.0
C      IF (CDELTA.LT.0.5) CDELTA=0.5
C      DO 10 I=1,N
C  10    CALL SADLINIT(V,I,C1(1,I),C1(2,I),C0,CDELTA,DELTA(I))
C      DO 30 J=1,N
C        V(J)=V(J)+DELTA(J)
C        DO 20 I=J+1,N
C          V(I)=V(I)+DELTA(I)
C          C2(I,J)=SACCHI(N,V)
C          C2(J,I)=C2(I,J)
C          V(I)=V(I)-DELTA(I)
C  20    CONTINUE
C        V(J)=V(J)-DELTA(J)
C  30  CONTINUE
C      END
C
C      SUBROUTINE SADLINIT(X,J,CMINUS,CPLUS,C0,CDELTA,DELTA)
C     ----------------------------------------------------
C
C      REAL X(*)
C      DATA FRAC,SMALL,XLARGE /1.0E-6,1.0E-20,1.0E20/
CC
C      X0=X(J)
C      D=FRAC*ABS(X0)+SMALL
C      DO 10 I=1,100
C        D=D+D
C        X(J)=X0-D
C        CMINUS=SACCHI(N,X)
C        X(J)=X0+D
C        CPLUS=SACCHI(N,X)
C        IF (X(J).GT.XLARGE) GOTO 1
C        IF (ABS(CMINUS-C0).GT.CDELTA) GOTO 1
C        IF (ABS(CPLUS-C0).GT.CDELTA) GOTO 1
C  10  CONTINUE
C   1  DELTA=D
C      X(J)=X0
C      END
C