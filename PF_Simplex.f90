!***********************************************************************
!
!
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
      SUBROUTINE SIMOPT(X,DX,COVAR,N,CHIFUN)
!     --------------------------------------
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Purpose
!     A SIMPLEX-based routine which optimises the values of the N
! parameters pertaining to the components of the vector X; it also
! estimates the related covariance matrix.
!
! Parameters
!   ARGUMENT  TYPE  I/O  DIMENSION  DESCRIPTION
!    N        I*4    I       -      No. of parameters to be optimised.
!    X        R*4    I       N      Initial guess.
!    X        R*4    O       N      The optimal answer.
!    DX       R*4    I       N      Initial step-lengths for X.
!    COVAR    R*4    O     N x N    The covariance matrix.
!
! Other Requirements
!     The user must provide a FUNCTION CHIFUN(X) which evaluates
! Chi-squared (un-normalised) given the vector X.
!
! History
!     D. S. Sivia    9 Feb 1995  Initial release.
!
!-----------------------------------------------------------------------
!
      PARAMETER (MAXITR=10)
      REAL X(N), DX(N), COVAR(N,N)
      REAL HESS(2500), DELTA(50), C0, C1(2,50), C2(2500)
      REAL V(2550), EX(150), C(51)
      INTEGER IR(51), INDX(50)
      EXTERNAL CHIFUN
      LOGICAL LERANL
      COMMON /PKCOM3/ LERANL
!O      DATA     NMAX /50/
!
!O      IF (N.GT.NMAX) STOP' Sorry, too many parameters !'
      LERANL = .FALSE.
      CALL VCOPY(X,V,N)
      CHIMIN = CHIFUN(N,V)
      ZERO = 0.0
      ITER = 0
!.. Zero things ...
      DO I = 1, 150
        EX(I) = 0.
      ENDDO
      DO I = 1, 51
        IR(I) = 0
        C(I) = 0.
      ENDDO
    1 ITER = ITER + 1
      IF (ITER.GT.MAXITR) GOTO 999
      N1000 = N*1000
      CALL SIMPLEX(V,N,DX,EX,C,IR,N1000,CHIFUN)
!
      CALL VCOPY(EX,V,N)
      CHI = CHIFUN(N,V)
!
!      IF ((1.0-CHI/CHIMIN).GT.0.0001) THEN
      IF ((1.0-CHI/CHIMIN).GT.0.01) THEN
        CHIMIN = CHI
        GOTO 1
      ENDIF
  999 CALL VCOPY(V,X,N)
      LERANL = .TRUE.
      CALL CHINIT(V,N,DELTA,C0,C1,C2,CHIFUN)
      CALL HSINT1(C0,C1,DELTA,N,HESS)
      CALL HSINT2(C0,C1,C2,DELTA,N,HESS)
      CALL INVERT(HESS,COVAR,N,INDX)
      LERANL = .FALSE.
      END SUBROUTINE SIMOPT
!*==VCOPY.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!***<utilities>*********************************************************
!
      SUBROUTINE VCOPY(X,Y,N)
!     -----------------------
!
      REAL X(*), Y(*)
!
      DO I = 1, N
        Y(I) = X(I)
      ENDDO
      END SUBROUTINE VCOPY
!*==VRFILL.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE VRFILL(X,A,N)
!     ------------------------
!
      REAL X(*)
!
      DO I = 1, N
        X(I) = A
      ENDDO
      END SUBROUTINE VRFILL
!*==SIMPLEX.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!***<Simplex>*****************************************************
!
      SUBROUTINE SIMPLEX(V,N,D,EX,C,IR,MX,CHIFUN)
!     ------------------------------------
!
      EXTERNAL CHIFUN
      REAL V(N,*), EX(N,*), C(*), D(*)
      INTEGER IR(*)
      LOGICAL SLOW
      DATA ALPHA, BETA, GAMA/1.0, 0.5, 2.0/
!.. Original      DATA    ALPHA,BETA,GAMA/1.0,0.5,2.0/
!
      SLOW = .FALSE.
      N3 = 3*N
      CAIM = 0.0
      C(1) = CHIFUN(N,V(1,1))
      CALL SIMP0(V,D,C,IR,N,CHIFUN)
      CMIN = C(IR(N+1))
!
      ITER = 0
      NOLUCK = 0
      IRSTRT = 0
   10 ITER = ITER + 1
      NOLUCK = NOLUCK + 1
      IF (ITER.GE.MX .OR. NOLUCK.GT.100) THEN
        CALL SSORT(C,IR,N)
        CALL VCOPY(V(1,IR(N+1)),EX,N)
        IF (ITER.LT.250) THEN
          NOLUCK = 0
          DO ID = 1, N
            D(ID) = D(ID)/10.0
          ENDDO
          CALL VCOPY(EX,V,N)
          C(1) = CHIFUN(N,V(1,1))
          CALL SIMP0(V,DSMALL,C,IR,N,CHIFUN)
          CMIN = C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL VRFILL(EX,0.,N3)
      CALL XCENT(V,EX(1,3),IR(1),N)
      CALL XZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0 = CHIFUN(N,EX)
      CL = C(IR(N+1))
      CH = C(IR(1))
      CS = C(IR(2))
      IF (C0.LT.CL) THEN
        CALL EXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA,CHIFUN)
      ELSEIF (C0.GT.CS) THEN
        CALL CONTRACT(CH,CS,C0,C00,EX,EX(1,2),EX(1,3),V(1,IR(1)),BETA,N,&
     &                CHIFUN)
        IF (C00.LT.CH .AND. C00.LT.C0) THEN
          CALL VCOPY(EX(1,2),EX,N)
          C0 = C00
        ELSE
          CALL CONT2(IR(N+1),V,C,N,IR,CHIFUN)
          IRSTRT = IRSTRT + 1
          IF (C(IR(N+1)).LT.CMIN) CMIN = C(IR(N+1))
          IF (IRSTRT.GE.5) THEN
            NOLUCK = 0
            CALL SSORT(C,IR,N)
            CALL VCOPY(V(1,IR(N+1)),EX,N)
            RETURN
          ENDIF
        ENDIF
      ENDIF
!
      CALL VCOPY(EX,V(1,IR(1)),N)
      C(IR(1)) = C0
      CALL SSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP = (CMIN-C(IR(N+1)))/CMIN
!        IF (ABS(DROP).LT.1.0E-4) SLOW=.TRUE.
        IF (ABS(DROP).LT.1.0E-4) SLOW = .TRUE.
        NOLUCK = 0
        CMIN = C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT.SLOW) GOTO 10
      CALL VCOPY(V(1,IR(N+1)),EX,N)
      END SUBROUTINE SIMPLEX
!*==SIMP0.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE SIMP0(V,D,C,IR,N,CHIFUN)
!     ----------------------------
!
      EXTERNAL CHIFUN
      REAL V(N,*), C(*), D(*)
      INTEGER IR(*)
!
      DO I = 2, N + 1
        CALL VCOPY(V,V(1,I),N)
        V(I-1,I) = V(I-1,I) + D(I-1)
        C(I) = CHIFUN(N,V(1,I))
      ENDDO
      CALL SSORT(C,IR,N)
      END SUBROUTINE SIMP0
!*==SSORT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE SSORT(C,IR,N)
!     ------------------------
!
      REAL C(*)
      INTEGER IR(*)
!
      IR(1) = 1
      DO J = 2, N + 1
        DO I = 1, J - 1
          IF (C(J).LT.C(IR(I))) GOTO 10
          DO II = 1, J - I
            IR(J+1-II) = IR(J-II)
          ENDDO
          IR(I) = J
          GOTO 20
   10   ENDDO
        IR(J) = J
   20 ENDDO
      END SUBROUTINE SSORT
!*==XCENT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE XCENT(V,XC,IH,N)
!     ---------------------------
!
      REAL V(N,*), XC(*)
!
      XNORM = 1.0/FLOAT(N)
      DO J = 1, N + 1
        IF (J.EQ.IH) GOTO 20
        DO I = 1, N
          XC(I) = XC(I) + V(I,J)
        ENDDO
   20 ENDDO
      DO I = 1, N
        XC(I) = XC(I)*XNORM
      ENDDO
      END SUBROUTINE XCENT
!*==XZERO.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE XZERO(XH,X0,XC,N,A)
!     ------------------------------
!
      REAL XH(*), X0(*), XC(*)
!
      DO I = 1, N
        X0(I) = A*(XC(I)-XH(I)) + XC(I)
      ENDDO
      END SUBROUTINE XZERO
!*==EXPAND.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE EXPAND(CL,X0,X00,XC,N,C0,G,CHIFUN)
!     --------------------------------------
!
      EXTERNAL CHIFUN
      REAL X0(*), X00(*), XC(*)
!
      DO I = 1, N
        X00(I) = G*(X0(I)-XC(I)) + XC(I)
      ENDDO
      C00 = CHIFUN(N,X00)
      IF (C00.LT.CL) THEN
        CALL VCOPY(X00,X0,N)
        C0 = C00
      ENDIF
      END SUBROUTINE EXPAND
!*==CONTRACT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE CONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N,CHIFUN)
!     --------------------------------------------------
!
      EXTERNAL CHIFUN
      REAL X0(*), X00(*), XC(*), XH(*)
!
      IF (C0.LT.CH) THEN
        DO I = 1, N
          X00(I) = B*(X0(I)-XC(I)) + XC(I)
        ENDDO
      ELSE
        DO I = 1, N
          X00(I) = B*(XH(I)-XC(I)) + XC(I)
        ENDDO
      ENDIF
      C00 = CHIFUN(N,X00)
      END SUBROUTINE CONTRACT
!*==CONT2.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE CONT2(K,V,C,N,IR,CHIFUN)
!     ----------------------------
!
      EXTERNAL CHIFUN
      REAL V(N,*), C(*)
      INTEGER IR(*)
!
      DO J = 1, N + 1
        IF (J.EQ.K) GOTO 20
        DO I = 1, N
          V(I,J) = 0.5*(V(I,J)+V(I,K))
          C(J) = CHIFUN(N,V(1,J))
        ENDDO
   20 ENDDO
      CALL SSORT(C,IR,N)
      END SUBROUTINE CONT2
!*==CHINIT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!***<error analysis>****************************************************
!
      SUBROUTINE CHINIT(V,N,DELTA,C0,C1,C2,CHIFUN)
!     -------------------------------------
!
      REAL V(*), DELTA(*), C0, C1(2,*), C2(N,*)
      EXTERNAL CHIFUN
!
      CALL VRFILL(C2,0.0,N*N)
      C0 = CHIFUN(N,V)
      CDELTA = C0/2000.0
      IF (CDELTA.LT.0.5) CDELTA = 0.5
      DO I = 1, N
        CALL DLINIT(V,I,C1(1,I),C1(2,I),C0,CDELTA,DELTA(I),CHIFUN)
      ENDDO
      DO J = 1, N
        V(J) = V(J) + DELTA(J)
        DO I = J + 1, N
          V(I) = V(I) + DELTA(I)
          C2(I,J) = CHIFUN(N,V)
          C2(J,I) = C2(I,J)
          V(I) = V(I) - DELTA(I)
        ENDDO
        V(J) = V(J) - DELTA(J)
      ENDDO
      END SUBROUTINE CHINIT
!*==DLINIT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE DLINIT(X,J,CMINUS,CPLUS,C0,CDELTA,DELTA,CHIFUN)
!     ----------------------------------------------------
!
      EXTERNAL CHIFUN
      REAL X(*)
      DATA FRAC, SMALL, XLARGE/1.0E-6, 1.0E-20, 1.0E20/
!
      X0 = X(J)
      D = FRAC*ABS(X0) + SMALL
      DO I = 1, 100
        D = D + D
        X(J) = X0 - D
        CMINUS = CHIFUN(N,X)
        X(J) = X0 + D
        CPLUS = CHIFUN(N,X)
        IF (X(J).GT.XLARGE) GOTO 1
        IF (ABS(CMINUS-C0).GT.CDELTA) GOTO 1
        IF (ABS(CPLUS-C0).GT.CDELTA) GOTO 1
      ENDDO
    1 DELTA = D
      X(J) = X0
      END SUBROUTINE DLINIT
!*==HSINT1.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE HSINT1(C0,C1,D,N,HS)
!     -------------------------------
!
      REAL C0, C1(2,*), D(*), HS(N,*)
!
      DO I = 1, N
        HS(I,I) = (C1(2,I)-C0-C0+C1(1,I))/(D(I)*D(I))
      ENDDO
      END SUBROUTINE HSINT1
!*==HSINT2.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE HSINT2(C0,C1,C2,D,N,HS)
!     ----------------------------------
!
      REAL C0, C1(2,*), C2(N,*), D(*), HS(N,*)
!
      DO J = 1, N
        DO I = J + 1, N
          HS(I,J) = (C2(I,J)-C1(2,I)-C1(2,J)+C0)/(D(I)*D(J))
          HS(J,I) = HS(I,J)
        ENDDO
      ENDDO
      END SUBROUTINE HSINT2
!*==INVERT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE INVERT(HESS,COVAR,N,INDX)
!     ------------------------------------
!
      REAL HESS(N,*), COVAR(N,*)
      INTEGER INDX(*)
!>> JCC This is for testing for mathematical errors used to PAUSE the program: THe pause seemed to#
!>> be causing a repeated CMD window to appear on screen ....
      INTEGER IBMBER
      COMMON /CCSLER/ IBMBER
!
      CALL VRFILL(COVAR,0.0,N*N)
      DO I = 1, N
        COVAR(I,I) = 2.0
      ENDDO
      CALL LUDCMP(HESS,N,N,INDX,D)
!>> JCC Trap for singular matrices
      IF (IBMBER.EQ.1) RETURN
      DO I = 1, N
        CALL LUBKSB(HESS,N,N,INDX,COVAR(1,I))
      ENDDO
      END SUBROUTINE INVERT
!*==LUDCMP.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!----------------------------------------------------------------------C
!  Two subroutines, for Cholesky decomposition of a matrix, copied out C
!  of Numerical Recipes.                                               C
!----------------------------------------------------------------------C
!
      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
!     --------------------------------
!
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP), INDX(N), VV(NMAX)
!>> JCC This is for testing for mathematical errors used to PAUSE the program: THe pause seemed to#
!>> be causing a repeated CMD window to appear on screen ....
      INTEGER IBMBER
      COMMON /CCSLER/ IBMBER
!
      D = 1.0
      DO I = 1, N
        AAMAX = 0.0
        DO J = 1, N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX = ABS(A(I,J))
        ENDDO
!>> JCC Removed the PAUSE
        IF (AAMAX.EQ.0.0) THEN
          IBMBER = 1
          RETURN
        ENDIF
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
      END SUBROUTINE LUDCMP
!*==LUBKSB.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
!     --------------------------------
!
      DIMENSION A(NP,NP), INDX(N), B(N)
!
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
      END SUBROUTINE LUBKSB
