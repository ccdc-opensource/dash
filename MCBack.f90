!
!*****************************************************************************
!
      SUBROUTINE BackMCBruckner(nbruckwin,mbruckiter,UseMC,UseSpline)

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INTEGER  NBIN, LBIN
      REAL     XBIN, YOBIN, YCBIN, YBBIN, EBIN
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc, UseSpline

      INTEGER MAXSSPL
      PARAMETER (MAXSSPL=5000)
      REAL,    DIMENSION(MAXSSPL)         :: xkt
      INTEGER, DIMENSION(MAXSSPL)         :: ikt
      INTEGER, DIMENSION(MAXSSPL)         :: ipartem
      REAL,    DIMENSION(MOBS)            :: es
      REAL,    DIMENSION(-200:MOBS+200)   :: ys
      INTEGER, DIMENSION(MOBS)            :: jft
      REAL                                tRandomNumber
      INTEGER I, II, IIII, I1, I2, KK, jf1, jf0, jfp1, jfn, n0, ndiv
      INTEGER iter, IILO, IIHI, nsep, ninsep, ngood
      INTEGER knotem, npartem
      INTRINSIC MOD
      REAL rat, stem

!
!  This subroutine determines the background using a smoothing
!  procedure published by Sergio Brueckner in J. Appl. Cryst. (2000) 33, 977-979
!  Modified by WIFD to smooth residual noise using SplineSmooth 
!  and raise background to correct value using a Monte Carlo sampling procedure)
!
      CALL WCursorShape(CurHourGlass)
      DO I = -nbruckwin, 0
        ys(I) = yobin(1)
      END DO
      DO I = 1, NBIN
        ys(I) = yobin(I)
      END DO
      DO I = 1, nbruckwin
        ii = NBIN + I
        ys(ii) = yobin(NBIN)
      END DO
      IIII = 0

!U      DO I = 1, NBIN
!U        ybbin(I) = 0.0
!U      END DO

      DO iter = 1, mbruckiter
! Loop over data points
        DO I = 1, NBIN
          iilo = I - nbruckwin
          iihi = I + nbruckwin
          ybbin(I) = 0.0
! Loop over window around current data point
          DO II = iilo, iihi
            ybbin(I) = ybbin(I) + ys(II)
          END DO
          ybbin(I) = (ybbin(I)-ys(I))/FLOAT(2 * nbruckwin)
        END DO
! Use a Monte Carlo algorithm to find the correct height of the background
        IF (UseMC) THEN
          DO I = 1, NBIN
! rat = ratio?
            rat = (ybbin(I)-ys(I))/ebin(I)
            stem = 1.0 / (1.0 + EXP(MIN(20.0,-rat)))  
            CALL RANDOM_NUMBER(tRandomNumber)
            IF (tRandomNumber .LT. stem) ybbin(I) = ys(I)
            ys(I) = ybbin(I)
          END DO
        ELSE
          DO I = 1, NBIN
            ys(I) = ybbin(I)
          END DO
        ENDIF
      END DO
!.. Now we should do some spline smoothing to remove the noise 
      IF (UseSpline) THEN
        nsep    =  5
        ninsep  = 10
        ngood   =  0
        knotem  =  0  
        npartem =  0
        DO i = 1, NBIN
          IF (ybbin(i) .EQ. yobin(i)) THEN
            es(i) = 1.E6 * ebin(i)
          ELSE
            es(i) = ebin(i)
            IF (MOD(ngood,nsep) .EQ. 0) THEN
              IF (MOD(knotem,ninsep) .EQ. 0) THEN
                npartem = npartem + 1
                ipartem(npartem) = knotem + 1
              END IF
              knotem = knotem + 1
              ikt(knotem) = i
              xkt(knotem) = xbin(i)
            END IF
            ngood = ngood + 1
          END IF
        END DO
        ikt(knotem) = NBIN
        ipartem(npartem) = knotem
        jft(1) = 1
        jft(NBIN) = knotem - 1
        DO kk = 1, knotem-1
          i1 = ikt(kk)
          i2 = ikt(kk+1) - 1
          DO i = i1, i2
            jft(i) = kk
          END DO
        END DO
        DO i = 1, npartem-1
          jf1 = ipartem(i)
          jf0 = jf1-1
          jfp1 = ipartem(i+1)
          jfn = 1 + jfp1 - ipartem(i)
          n0 = ikt(jf1)
          ndiv = 1 + ikt(jfp1) - n0
          CALL SplineSmooth(xbin(n0),ys(n0),es(n0),ndiv,jf0,jft(n0),xkt(jf1),jfn,ybbin(n0))
        END DO
      ENDIF
      CALL WCursorShape(CurArrow)

      END SUBROUTINE BackMCBruckner
!
!*****************************************************************************
!
      SUBROUTINE SplineSmooth(x,y,e,NDAT,jf0,jfs,xkk,nkn,smo)

      REAL    x(NDAT),y(NDAT),e(NDAT)
      INTEGER NDAT
      INTEGER jfs(NDAT)
      REAL    xkk(nkn)
      REAL    smo(NDAT)

      REAL*8  xdel(nkn),u(nkn,nkn)
      REAL*8  bvec(nkn),hess(nkn,nkn),covar(nkn,nkn)

      REAL*8  xdd
      REAL*8  a(NDAT),b(NDAT),c(NDAT),d(NDAT)

      REAL*8  deri(nkn),ans(nkn)
      REAL*8  w
      REAL*8  qj, qj1
      REAL*8  TempResult

      DO J = 1, nkn-1
        xdel(J) = DBLE(xkk(J+1)-xkk(J))
      END DO
      CALL SplVal(xdel,u,nkn)
      nd1 = ndat-1
      nk1 = nkn-1
      DO j = 1, nkn
        bvec(j) = 0.0 
        DO k = 1, nkn
          hess(j,k) = 0.0 
        END DO
      END DO
      DO i = 1, ndat
        j0 = MIN(nkn-1,jfs(i)-jf0)
        j1 = j0 + 1
        w = DBLE(e(i))**-2
!       b(i)=(dble(x(i))-xkk(j0))/xdel(j0)
        b(i) = ( DBLE( x(i)-xkk(j0) ) )/xdel(j0)
        a(i) = 1.0-b(i)
        ab = -a(i)*b(i)/6.0
        xdd = xdel(j0)**2
        c(i) = ab*(1.0 + a(i))*xdd
        d(i) = ab*(1.0 + b(i))*xdd
        DO j = 1, nkn
          deri(j) = 0.0
        END DO
        deri(j0) = a(i)
        deri(j1) = b(i)
        DO j = 1, nkn
          deri(j) = deri(j)+c(i)*u(j0,j)+d(i)*u(j1,j)
        END DO
        DO j = 1, nkn
          bvec(j) = bvec(j)+w*DBLE(y(i))*deri(j)
          DO k = 1, nkn
            hess(j,k) = hess(j,k)+w*deri(j)*deri(k)
          END DO
        END DO
      END DO
      CALL DGMINV(hess,covar,nkn)
      DO I = 1, nkn
        ans(i) = 0.0
        DO j = 1, nkn
          ans(i) = ans(i) + covar(i,j) * bvec(j)
        END DO
      END DO
      DO I = 1, ndat
        j0 = jfs(i) - jf0
        j1 = j0 + 1
        qj  = 0.0
        qj1 = 0.0
        DO k = 1, nkn
          qj  = qj  + u(j0,k)*ans(k)
          qj1 = qj1 + u(j1,k)*ans(k)
        END DO
! JvdS Was:
!O        smo(i) = SNGL(a(i)*ans(j0)+b(i)*ans(j1)+c(i)*qj+d(i)*qj1)
! This caused an underflow error
        TempResult = a(i)*ans(j0)+b(i)*ans(j1)+c(i)*qj+d(i)*qj1
        IF (ABS(TempResult) .LT. 0.000001) TempResult = 0.0
        smo(i) = SNGL(TempResult)
      END DO
!
      END SUBROUTINE SplineSmooth
!
!*****************************************************************************
!
      SUBROUTINE SplVal(XDEL,U,M)

      INTEGER M
      REAL*8  xdel(m)
      REAL*8  u(m,m)

      REAL*8  A(m,m),b(m,m),c(m,m)
      INTEGER I, J

! Initialise all entries of A, b and c to 0.0
      DO J = 1, m
        DO I = 1, m
          A(I,J) = 0.0
          b(I,J) = 0.0
          c(I,J) = 0.0
        END DO
      END DO
      A(1,1) = 1.0
      A(m,m) = 1.0
      DO I = 2, m-1
!        Im1 = I - 1
!        Ip1 = I + 1
        A(I,I-1) = xdel(I-1) / 6.0
        A(I,I+1) = xdel(I  ) / 6.0
        A(I,I  ) = 2.0 * (A(I,I-1)+A(I,I+1))
        c(I,I-1) = 1.0 / xdel(I-1)
        c(I,I+1) = 1.0 / xdel(I)
        c(I,I)   = -(c(I,I-1)+c(I,I+1))
      END DO
! Invert matrix A. Answer is in b
      CALL DGMINV(A,b,m)
! Multiply matrix b by matrix c. Result is in u
      CALL MultiplyMatrices(b,c,u,m,m,m)

      END SUBROUTINE SplVal
!
!*****************************************************************************
!
! LEVEL 3      SUBROUTINE DGMINV(A,B,N)
      SUBROUTINE DGMINV(A,B,N)
!
! *** GMINV by JCM from SID 11 Oct 88 ***
!
!X
!C 12C
!H Inverts matrix A into matrix B.
!A On entry A is a square NxN real matrix
!A On exit  B is its inverse
!D Based on SID
!
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER         II(500),IL(500),IG(500)
      REAL*8          A(N,N),B(N,N)
      INTEGER         I, J

! Initialise b with values from a
      DO I = 1, N
        DO J = 1, N
          B(I,J) = A(I,J)
        ENDDO
      ENDDO
      D = 1.0
      IS = N - 1
      DO K = 1, N
        IL(K) = 0
        IG(K) = K
      ENDDO
      DO 150 K = 1, N
        R = 0.0
        DO 40 I = 1, N
          IF (IL(I) .NE. 0) GOTO 40
          W = B(I,K)
          X = ABS(W)
          IF (R .GT. X) GOTO 40
          R  = X
          P  = W
          KF = I
   40   CONTINUE
        II(K) = KF
        IL(KF) = KF
        D = D * P
        IF (D .EQ. 0.0) D = 1.0E-6
        DO 80 I = 1, N
          IF (I .EQ. KF) THEN
            B(I,K) = 1.0/P
          ELSE
            B(I,K) = -B(I,K)/P
          ENDIF
   80   CONTINUE
        DO 140 J = 1, N
          IF (J .EQ. K) GOTO 140
          W = B(KF,J)
          IF (W .EQ. 0.0) GOTO 140
          DO 130 I = 1, N
            IF (I .EQ. KF) THEN
              B(I,J) = W/P
            ELSE
              B(I,J) = B(I,J)+W*B(I,K)
            ENDIF
  130     CONTINUE
  140   CONTINUE
  150 CONTINUE
      DO 190 K = 1, IS
        KF = II(K)
        KL = IL(KF)
        KG = IG(K)
        IF(KF .EQ. KG) GOTO 190
        DO 170 I = 1, N
          R = B(I,KF)
          B(I,KF) = B(I,KG)
  170     B(I,KG) = R
        DO 180 J = 1, N
          R = B(K,J)
          B(K,J) = B(KL,J)
  180     B(KL,J) = R
        IL(KF) = K
        IL(KG) = KL
        IG(KL) = IG(K)
        IG(K) = KF
        D = -D
  190 CONTINUE
      RETURN
      END SUBROUTINE DGMINV
!
!*****************************************************************************
!
      SUBROUTINE MultiplyMatrices(A,B,C,Dim1,Dim2,Dim3)
!
! This subroutine multiplies two matrices A and B
!
! INPUT  : A, B = the matrices to be multiplied
!          Dim1, Dim2, Dim3 : Dimensions of the matrices.
!          A is a Dim1 x Dim2 matrix
!          B is a Dim2 x Dim3 matrix
!
! OUTPUT : C = A x B
!
      IMPLICIT NONE

      INTEGER Dim1, Dim2, Dim3
      REAL*8  A(Dim1,Dim2), B(Dim2,Dim3), C(Dim1,Dim3)
      INTEGER I, J, K

      DO I = 1, Dim1
        DO J = 1, Dim3
          C(I,J) = 0.0
          DO K = 1, Dim2
            C(I,J) =  C(I,J) + A(I,K)*B(K,J)
          ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE MultiplyMatrices
!
!*****************************************************************************
!
