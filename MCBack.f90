!
!*****************************************************************************
!
      SUBROUTINE SubtractBackground(nbruckwin,mbruckiter,UseMC)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER I

! Calculate the background
      CALL CalculateBackground(nbruckwin,mbruckiter,UseMC)
! Subtract the background
      DO I = 1, NBIN
        YOBIN(I) = YOBIN(I) - YBBIN(I)
      ENDDO
      CALL Clear_BackGround
      CALL GetProfileLimits
      BackRef = .FALSE.

      END SUBROUTINE SubtractBackground
!
!*****************************************************************************
!
      SUBROUTINE Background_Fit

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER tInt1, tInt2
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Background_Fit)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_Preview)
              CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
              CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
              CALL CalculateBackground(tInt1,tInt2,WDialogGetCheckBoxLogical(IDF_UseMCYN))
              CALL Profile_Plot
            CASE (IDOK)
              CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
              CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
              CALL SubtractBackground(tInt1,tInt2,WDialogGetCheckBoxLogical(IDF_UseMCYN))
              CALL WDialogHide
              CALL Profile_Plot
            CASE (IDCANCEL)
! If user Cancels, assume no knowledge on background
              CALL Clear_BackGround
              CALL WDialogHide
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE Background_Fit
!
!*****************************************************************************
!
      SUBROUTINE CalculateBackground(nbruckwin,mbruckiter,UseMC)

      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER     MAXSSPL
      PARAMETER ( MAXSSPL = 5000 )

      REAL    xkt(MAXSSPL)
      INTEGER ikt(MAXSSPL),ipartem(MAXSSPL)
      REAL    es(MOBS)
      REAL    ys(-200:MOBS+200)
      INTEGER jft(MOBS)
      REAL    tRandomNumber
      INTEGER I, II, I1, I2, KK, jf1, jf0, jfp1, jfn, n0, ndiv
      INTEGER iter, IILO, IIHI, nsep, ninsep, ngood
      INTEGER knotem, npartem
      INTRINSIC MOD
      REAL    rat, stem
      REAL AvgBeg
      INTEGER Nbeg
!
!  This subroutine determines the background using a smoothing
!  procedure published by Sergio Brueckner in J. Appl. Cryst. (2000) 33, 977-979
!  Modified by WIFD to smooth residual noise using SplineSmooth 
!  and raise background to correct value using a Monte Carlo sampling procedure
!
      CALL WCursorShape(CurHourGlass)
      !C Calculate the average of the first Nbeg points
      AvgBeg = 0.0
      Nbeg = 1
      DO I = 1, Nbeg
        AvgBeg = AvgBeg + YOBIN(I)
      ENDDO
      AvgBeg = AvgBeg / Nbeg
      DO I = -nbruckwin, 0
        ys(I) = AvgBeg
      ENDDO
      DO I = 1, NBIN
        ys(I) = YOBIN(I)
      ENDDO
      DO I = 1, nbruckwin
        ii = NBIN + I
        ys(ii) = YOBIN(NBIN)
      ENDDO
      DO iter = 1, mbruckiter
! Loop over data points
        DO I = 1, NBIN
          iilo = I - nbruckwin
          iihi = I + nbruckwin
          YBBIN(I) = 0.0
! Loop over window around current data point
          DO II = iilo, iihi
            YBBIN(I) = YBBIN(I) + ys(II)
          ENDDO
          YBBIN(I) = (YBBIN(I)-ys(I))/FLOAT(2 * nbruckwin)
        ENDDO
        IF (UseMC) THEN
          DO I = 1, NBIN
! Use a Monte Carlo algorithm to find the correct height of the background
! rat = ratio?
            rat = (YBBIN(I)-ys(I))/EBIN(I)
            stem = 1.0 / (1.0 + EXP(MIN(20.0,-rat)))  
            CALL RANDOM_NUMBER(tRandomNumber)
            IF (tRandomNumber .LT. stem) YBBIN(I) = ys(I)
            ys(I) = YBBIN(I)
          ENDDO
        ELSE
          DO I = 1, NBIN
            ys(I) = MIN(YBBIN(I),ys(I))
          ENDDO
        ENDIF
      ENDDO
! Now we should do some spline smoothing to remove the noise 
      IF (UseMc) THEN
        nsep    =  5
        ninsep  = 10
        ngood   =  0
        knotem  =  0  
        npartem =  0
        DO i = 1, NBIN
          IF (YBBIN(i) .EQ. YOBIN(i)) THEN
            es(i) = 1.E6 * EBIN(i)
          ELSE
            es(i) = EBIN(i)
            IF (MOD(ngood,nsep) .EQ. 0) THEN
              IF (MOD(knotem,ninsep) .EQ. 0) THEN
                npartem = npartem + 1
                ipartem(npartem) = knotem + 1
              ENDIF
              knotem = knotem + 1
              ikt(knotem) = i
              xkt(knotem) = XBIN(i)
            ENDIF
            ngood = ngood + 1
          ENDIF
        ENDDO
        ikt(knotem) = NBIN
        ipartem(npartem) = knotem
        jft(1) = 1
        jft(NBIN) = knotem - 1
        DO kk = 1, knotem-1
          i1 = ikt(kk)
          i2 = ikt(kk+1) - 1
          DO i = i1, i2
            jft(i) = kk
          ENDDO
        ENDDO
        DO i = 1, npartem-1
          jf1 = ipartem(i)
          jf0 = jf1-1
          jfp1 = ipartem(i+1)
          jfn = 1 + jfp1 - ipartem(i)
          n0 = ikt(jf1)
          ndiv = 1 + ikt(jfp1) - n0
          CALL SplineSmooth(XBIN(n0),ys(n0),es(n0),ndiv,jf0,jft(n0),xkt(jf1),jfn,YBBIN(n0))
        ENDDO
      ENDIF
      CALL WCursorShape(CurCrossHair)

      END SUBROUTINE CalculateBackground
!
!*****************************************************************************
!
      SUBROUTINE SplineSmooth(x,y,e,NDAT,jf0,jfs,xkk,nkn,smo)

      IMPLICIT NONE

      INTEGER nkn
      INTEGER NDAT
      REAL    x(NDAT),y(NDAT),e(NDAT)
      INTEGER jfs(NDAT)
      REAL    xkk(nkn)
      REAL    smo(NDAT)
      REAL  xdel(nkn),u(nkn,nkn)
      REAL  bvec(nkn),hess(nkn,nkn),covar(nkn,nkn)
      REAL  xdd
      REAL  a(NDAT),b(NDAT),c(NDAT),d(NDAT)
      REAL  deri(nkn),ans(nkn)
      REAL  w, ab
      REAL  qj, qj1
      REAL  TempResult
      INTEGER J, ND1, NK1, I, J0, J1, JF0, K

      DO J = 1, nkn-1
        xdel(J) = xkk(J+1)-xkk(J)
      ENDDO
      CALL SplVal(xdel,u,nkn)
      nd1 = ndat-1
      nk1 = nkn-1
      bvec = 0.0
      hess = 0.0
      DO i = 1, ndat
        j0 = MIN(nkn-1,jfs(i)-jf0)
        j1 = j0 + 1
        w = e(i)**-2
        b(i) = (x(i)-xkk(j0)) / xdel(j0)
        a(i) = 1.0-b(i)
        ab = -a(i)*b(i)/6.0
        xdd = xdel(j0)**2
        c(i) = ab*(1.0 + a(i))*xdd
        d(i) = ab*(1.0 + b(i))*xdd
        DO j = 1, nkn
          deri(j) = 0.0
        ENDDO
        deri(j0) = a(i)
        deri(j1) = b(i)
        DO j = 1, nkn
          deri(j) = deri(j)+c(i)*u(j0,j)+d(i)*u(j1,j)
        ENDDO
        DO j = 1, nkn
          bvec(j) = bvec(j)+w*y(i)*deri(j)
          DO k = 1, nkn
            hess(j,k) = hess(j,k)+w*deri(j)*deri(k)
          ENDDO
        ENDDO
      ENDDO
      CALL DGMINV(hess,covar,nkn)
      DO I = 1, nkn
        ans(i) = 0.0
        DO j = 1, nkn
          ans(i) = ans(i) + covar(i,j) * bvec(j)
        ENDDO
      ENDDO
      DO I = 1, ndat
        j0 = MIN(nkn-1,jfs(i)-jf0)
        j1 = j0 + 1
        qj  = 0.0
        qj1 = 0.0
        DO k = 1, nkn
          qj  = qj  + u(j0,k)*ans(k)
          qj1 = qj1 + u(j1,k)*ans(k)
        ENDDO
        TempResult = a(i)*ans(j0)+b(i)*ans(j1)+c(i)*qj+d(i)*qj1
        IF (ABS(TempResult) .LT. 0.000001) TempResult = 0.0
        smo(i) = TempResult
      ENDDO

      END SUBROUTINE SplineSmooth
!
!*****************************************************************************
!
      SUBROUTINE SplVal(XDEL,U,m)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: m
      REAL,    INTENT (IN   ) :: xdel(m)
      REAL,    INTENT (  OUT) :: u(m,m)

      REAL  A(m,m),b(m,m),c(m,m)
      INTEGER I

! Initialise all entries of A, b and c to 0.0
      A = 0.0
      b = 0.0
      c = 0.0
      A(1,1) = 1.0
      A(m,m) = 1.0
      DO I = 2, m-1
        A(I,I-1) = xdel(I-1) / 6.0
        A(I,I+1) = xdel(I  ) / 6.0
        A(I,I  ) = 2.0 * (A(I,I-1)+A(I,I+1))
        c(I,I-1) = 1.0 / xdel(I-1)
        c(I,I+1) = 1.0 / xdel(I)
        c(I,I)   = -(c(I,I-1)+c(I,I+1))
      ENDDO
! Invert matrix A. Answer is in b
      CALL DGMINV(A,b,m)
! Multiply matrix b by matrix c. Result is in u
      CALL MultiplyMatrices(b,c,u,m,m,m)

      END SUBROUTINE SplVal
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
      REAL    A(Dim1,Dim2), B(Dim2,Dim3), C(Dim1,Dim3)
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
