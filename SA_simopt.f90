!
!*****************************************************************************
!
      REAL FUNCTION SA_FCN(N,P)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL P(MVAR)
      INTEGER N

      DOUBLE PRECISION XOPT,       C,       FOPT
      COMMON /sacmn /  XOPT(MVAR), C(MVAR), FOPT

      INTEGER         NPAR, IP
      COMMON /SIMSTO/ NPAR, IP(MVAR)

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      DOUBLE PRECISION CHIANS, DBLEP(MVAR)
      INTEGER I, II

      DO I = 1, NVAR
        DBLEP(I) = XOPT(I)
      ENDDO
      DO II = 1, NPAR
        I = IP(II)
        DBLEP(I) = DBLE(P(II))
      ENDDO
!O      CALL PO_PRECFC
      CALL FCN(DBLEP,CHIANS,0)
      SA_FCN = SNGL(CHIANS)

      END FUNCTION SA_FCN
!
!*****************************************************************************
!
      SUBROUTINE SA_SIMOPT(X,DX,N,TheChiSqd)
!
! Purpose
!     A SIMPLEX-based routine which optimises the values of the N
! parameters pertaining to the components of the vector X
!
! Parameters
!   ARGUMENT  TYPE  I/O  DIMENSION  DESCRIPTION
!    N        I*4    I       -      No. of parameters to be optimised.
!    X        R*4    I       N      Initial guess.
!    X        R*4    O       N      The optimal answer.
!    DX       R*4    I       N      Initial step-lengths for X.
!
! Other Requirements
!     The user must provide a FUNCTION SA_FCN(X) which evalutes
! Chi-squared (un-normalised) given the vector X.
!
! History
!     D. S. Sivia    9 Feb 1995  Initial release.
!
      INCLUDE 'PARAMS.INC'

      REAL, INTENT (  OUT) :: TheChiSqd

      INTEGER     MAXITR
      PARAMETER ( MAXITR = MVAR )
      REAL X(*), DX(*)
      REAL V((MVAR+1)*MAXITR), EX(3*MVAR), C(MVAR+1)
      INTEGER IR(MVAR+1) ! Maximum index I could find: N+1. Maximum value for N should be MVAR
                         ! IR seems to hold a sorted list of pointers into C
      EXTERNAL SA_FCN

      CALL VCOPY(X,V,N)
      CHIMIN = SA_FCN(N,V)
      ZERO = 0.0
      ITER = 0
    1 ITER = ITER + 1
      IF (ITER.GT.MAXITR) THEN
   !     CALL DebugErrorMessage('ITER.GT.MAXITR in SA_SIMOPT')
        GOTO 999
      ENDIF
      CALL SASIMPLEX(V,N,DX,EX,C,IR,N*1000)
      CALL VCOPY(EX,V,N)
      CHI = SA_FCN(N,V)
! Convergence criterion
      IF (CHI .GT. CHIMIN) THEN
        CALL DebugErrorMessage('CHI .GT. CHIMIN in SA_SIMOPT')
        GOTO 1
      ENDIF
      IF ((1.0-CHI/CHIMIN) .GT. 0.00001) THEN
        CHIMIN = CHI
        GOTO 1
      ENDIF
  999 CALL VCOPY(V,X,N)
      TheChiSqd = CHI

      END SUBROUTINE SA_SIMOPT
!
!*****************************************************************************
!
      SUBROUTINE SASIMPLEX(V,N,D,EX,C,IR,MX)

      REAL V(N,*), EX(N,*), C(*), D(*)
      INTEGER IR(*)
      LOGICAL SLOW
      DATA ALPHA, BETA, GAMA/1.0, 0.5, 2.0/

      SLOW = .FALSE.
      N3 = 3*N
      CAIM = 0.0
      C(1) = SA_FCN(N,V(1,1))
      CALL SASIMP0(V,D,C,IR,N)
      CMIN = C(IR(N+1))
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
!O            D(ID) = D(ID)/10.0
            D(ID) = D(ID)/2.0
          ENDDO
          CALL VCOPY(EX,V,N)
          C(1) = SA_FCN(N,V(1,1))
          CALL SASIMP0(V,DSMALL,C,IR,N)
          CMIN = C(IR(N+1))
        ELSE
          RETURN
        ENDIF
      ENDIF
      CALL VRFILL(EX,0.,N3)
      CALL XCENT(V,EX(1,3),IR(1),N)
      CALL XZERO(V(1,IR(1)),EX,EX(1,3),N,ALPHA)
      C0 = SA_FCN(N,EX)
      CL = C(IR(N+1))
      CH = C(IR(1))
      CS = C(IR(2))
      IF (C0.LT.CL) THEN
        CALL SAEXPAND(CL,EX,EX(1,2),EX(1,3),N,C0,GAMA)
      ELSEIF (C0.GT.CS) THEN
        CALL SACONTRACT(CH,CS,C0,C00,EX,EX(1,2),EX(1,3),V(1,IR(1)),BETA,N)
        IF (C00.LT.CH .AND. C00.LT.C0) THEN
          CALL VCOPY(EX(1,2),EX,N)
          C0 = C00
        ELSE
          CALL SACONT2(IR(N+1),V,C,N,IR)
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
      CALL VCOPY(EX,V(1,IR(1)),N)
      C(IR(1)) = C0
      CALL SSORT(C,IR,N)
      IF (C(IR(N+1)).LT.CMIN) THEN
        DROP = (CMIN-C(IR(N+1)))/CMIN
        IF (ABS(DROP).LT.1.0E-4) SLOW = .TRUE.
        NOLUCK = 0
        CMIN = C(IR(N+1))
      ENDIF
      IF (CMIN.GT.CAIM .AND. .NOT.SLOW) GOTO 10
      CALL VCOPY(V(1,IR(N+1)),EX,N)

      END SUBROUTINE SASIMPLEX
!
!*****************************************************************************
!
      SUBROUTINE SASIMP0(V,D,C,IR,N)

      REAL V(N,*), C(*), D(*)
      INTEGER IR(*)

      DO I = 2, N + 1
        CALL VCOPY(V,V(1,I),N)
        V(I-1,I) = V(I-1,I) + D(I-1)
        C(I) = SA_FCN(N,V(1,I))
      ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE SASIMP0
!
!*****************************************************************************
!
      SUBROUTINE SAEXPAND(CL,X0,X00,XC,N,C0,G)

      REAL X0(*), X00(*), XC(*)

      DO I = 1, N
        X00(I) = G*(X0(I)-XC(I)) + XC(I)
      ENDDO
      C00 = SA_FCN(N,X00)
      IF (C00.LT.CL) THEN
        CALL VCOPY(X00,X0,N)
        C0 = C00
      ENDIF

      END SUBROUTINE SAEXPAND
!
!*****************************************************************************
!
      SUBROUTINE SACONTRACT(CH,CS,C0,C00,X0,X00,XC,XH,B,N)

      REAL X0(*), X00(*), XC(*), XH(*)

      IF (C0.LT.CH) THEN
        DO I = 1, N
          X00(I) = B*(X0(I)-XC(I)) + XC(I)
        ENDDO
      ELSE
        DO I = 1, N
          X00(I) = B*(XH(I)-XC(I)) + XC(I)
        ENDDO
      ENDIF
      C00 = SA_FCN(N,X00)

      END SUBROUTINE SACONTRACT
!
!*****************************************************************************
!
      SUBROUTINE SACONT2(K,V,C,N,IR)

      REAL V(N,*), C(*)
      INTEGER IR(*)

      DO J = 1, N + 1
        IF (J.EQ.K) GOTO 20
        DO I = 1, N
          V(I,J) = 0.5*(V(I,J)+V(I,K))
          C(J) = SA_FCN(N,V(1,J))
        ENDDO
   20 ENDDO
      CALL SSORT(C,IR,N)

      END SUBROUTINE SACONT2
!
!*****************************************************************************
!
