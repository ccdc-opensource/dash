!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE FourierPattern(Xbegin, Xeind)
!U!
!U! This subroutine calculates the Fourier transform of the pattern, allowing application of
!U! low and high pass filters for removing background and noise.
!U!
!U! This routine assumes that the two theta range has a uniform stepsize.
!U!
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U      USE VARIABLES
!U
!U      IMPLICIT NONE
!U
!U      INTEGER, INTENT (IN   ) :: Xbegin, Xeind
!U
!U      INCLUDE 'PARAMS.INC'
!U      INCLUDE 'GLBVAR.INC'
!U      INCLUDE 'Poly_Colours.inc'
!U
!U      INTEGER          NBIN, LBIN
!U      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
!U      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
!U
!U      INTEGER        FT_N
!U      DOUBLE PRECISION                 HulpData
!U      COMMON /FTPRO/ HulpData(1:MOBS,1:2), FT_N
!U
!U      REAL            Smoothed
!U      COMMON /SMOOTH/ Smoothed(MOBS)
!U! COMMON block holding the Fourier transform of the profile
!U! HulpData(1:MOBS,1) = magnitudes
!U! HulpData(1:MOBS,2) = phases
!U
!U      INTEGER Re, Im
!U      PARAMETER ( Re = 1, Im = 2 )
!U
!U      INTEGER     :: N
!U      DOUBLE PRECISION :: F(Re:Im)
!U      INTEGER     :: Nu, Tau, Tau2
!U      DOUBLE PRECISION        :: Fx
!U      DOUBLE PRECISION        :: NxPi
!U      INTEGER     :: tXbegin, tXeind ! to emulate call by value
!U      DOUBLE PRECISION           Amplitude
!U      REAL           MinX, MinY, MaxX, MaxY
!U      INTEGER        IHANDLE
!U      REAL           Step ! Step size in 2 theta
!U      INTEGER        I
!U      DOUBLE PRECISION           Phase
!U      REAL           PlotData(1:MOBS)
!U
!U      RETURN
!U      CALL WCursorShape(CurHourGlass)
!U      tXbegin = Xbegin
!U      tXeind  = Xeind
!U      tXbegin = 1
!U      tXeind  = NBIN
!U      N = 1 + tXeind - tXbegin
!U! If necessary, make number of points even by omitting last point
!U      IF (MOD(N,2) .EQ. 1) THEN
!U        N = N -1
!U        tXeind = tXeind - 1
!U      ENDIF
!U      FT_N = N
!U! Assuming that the powder pattern has been recorded with a fixed step size, we can calculate it
!U      Step = (XBIN(N)-XBIN(1)) / (N-1)
!U      MinX = 1.0
!U      MaxX = SNGL(FT_N)
!U      HulpData = 0.0D0
!U!      NxPi = 2.0D0 * 3.14159265358979323846D0 / DBLE(N)
!U      NxPi = 8.0D0 * DATAN(1.0D0) / DBLE(N)
!U      MinY = 0.0
!U      MaxY = 0.0
!U      DO Nu = 0, N-1
!U        F(Re) = 0.0D0
!U        F(Im) = 0.0D0
!U        DO Tau = (-N / 2), (N / 2)-1
!U          IF (Tau .LT. 0) THEN
!U            Tau2 = Tau + N 
!U          ELSE 
!U            Tau2 = Tau
!U          ENDIF
!U          Tau2 = Tau2 + tXbegin
!U          Fx = DBLE(YOBIN(Tau2))
!U          F(Re) = F(Re) + (Fx*DCOS(NxPi*Nu*Tau))
!U          F(Im) = F(Im) - (Fx*DSIN(NxPi*Nu*Tau))
!U        ENDDO
!U
!U        F(Re) = F(Re) / DBLE(N)
!U        F(Im) = F(Im) / DBLE(N)
!U        Amplitude = DSQRT(F(Re)**2 + F(Im)**2)
!U        MinY = MIN(MinY,SNGL(Amplitude))
!U        MaxY = MAX(MaxY,SNGL(Amplitude))
!U        HulpData(tXbegin+Nu,1) = Amplitude          ! Amplitude
!U        HulpData(tXbegin+Nu,2) = DATAN2(F(Im),F(Re)) ! Phase
!U      ENDDO
!U      Smoothed = 0.0D0
!U      DO Tau = (-N / 2), (N / 2)-1
!U        IF (Tau .LT. 0) THEN
!U          Tau2 = Tau + N 
!U        ELSE 
!U          Tau2 = Tau
!U        ENDIF
!U        Tau2 = Tau2 + tXbegin
!U        DO Nu = 0, N-1
!U          Amplitude = HulpData(tXbegin+Nu,1) 
!U          Phase     = HulpData(tXbegin+Nu,2)
!U!          IF ((DABS(Nu/(DBLE(N)*Tau)) .GT. ) .AND. (DABS(Nu/(DBLE(N)*Tau)) .LT. )) 
!U          Smoothed(Tau2) = Smoothed(Tau2) + Amplitude * DCOS(NxPi*Nu*Tau+Phase)
!U        ENDDO
!U      ENDDO
!U      CALL WCursorShape(CurCrossHair)
!U      OPEN(10,FILE='Fourier.xye')
!U      WRITE(10,*) ALambda
!U      DO I = 1, FT_N
!U        WRITE(10,*) XBIN(I), Smoothed(I), EBIN(I)
!U      ENDDO
!U      CLOSE(10)
!U      RETURN
!U! The coefficients are now in HulpData
!U	CALL WindowOpenChild(IHANDLE)
!U!      CALL IGrArea(0.0, 0.0, 1.0, 1.0)
!U!	CALL IGrUnits(0.0, 0.0, 1.0, 1.0)		
!U!
!U!  Start new presentation graphics plot
!U!!
!U!  Set marker number for data sets not using default marker
!U!
!U!
!U!  Set units for plot
!U!
!U      CALL IPgUnits(MinX*0.9, MinY*0.9, MaxX*1.1, MaxY*1.1)
!U!  Draw axes
!U!
!U      CALL IGrColourN(KolNumMain)
!U      CALL IPgBorder()
!U!
!U!  Draw graph.
!U!
!U      CALL IPgNewPlot(PgLinePlot,1,FT_N)
!U      CALL IPgStyle(1,0,0,0,KolNumBack,0)
!U      DO I = 1, FT_N
!U        PlotData(I) = SNGL(HulpData(I,1))
!U      ENDDO
!U      CALL IPgLinePlot(PlotData(1))
!U
!U      END SUBROUTINE FourierPattern
!
!*****************************************************************************
!
      SUBROUTINE SubtractBackground(nbruckwin,mbruckiter,UseMC)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER IOBS, I, J

! Calculate the background
      CALL CalculateBackground(nbruckwin,mbruckiter,UseMC)
! Subtract the background
      IOBS = 0
      YPMIN = YOBS(1) - YBBIN(1)
      YPMAX = YPMIN
      DO I = 1, NBIN
        DO J = 1, LBIN
          IOBS = IOBS + 1
          YOBS(IOBS) = YOBS(IOBS) - YBBIN(I)
          YPMIN = MIN(YOBS(IOBS),YPMIN)
          YPMAX = MAX(YOBS(IOBS),YPMAX)
        ENDDO
        YOBIN(I) = YOBIN(I) - YBBIN(I)
      ENDDO
      CALL Clear_BackGround
      CALL GetProfileLimits
      BACKREF = .FALSE.

      END SUBROUTINE SubtractBackground
!
!*****************************************************************************
!
      SUBROUTINE Background_Fit

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER IBpass

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Background_Fit)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Background_Apply)
              CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
              CALL CalculateBackground(IBpass,20,.TRUE.)
              CALL Profile_Plot
            CASE (IDF_Background_Accept)
              CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
              CALL SubtractBackground(IBpass,20,.TRUE.)
              CALL WDialogHide()
              CALL Profile_Plot
            CASE (IDCANCEL)
! If user Cancels, assume no knowledge on background
              CALL Clear_BackGround
              CALL WDialogHide()
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
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER MAXSSPL
      PARAMETER (MAXSSPL=5000)
      REAL,    DIMENSION(MAXSSPL)         :: xkt
      INTEGER, DIMENSION(MAXSSPL)         :: ikt
      INTEGER, DIMENSION(MAXSSPL)         :: ipartem
      REAL,    DIMENSION(MOBS)            :: es
      REAL,    DIMENSION(-200:MOBS+200)   :: ys
      INTEGER, DIMENSION(MOBS)            :: jft
      REAL                                tRandomNumber
      INTEGER I, II, I1, I2, KK, jf1, jf0, jfp1, jfn, n0, ndiv
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
        ys(I) = YOBIN(1)
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
      ENDDO
      CALL SplVal(xdel,u,nkn)
      nd1 = ndat-1
      nk1 = nkn-1
      bvec = 0.0
      hess = 0.0
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
        ENDDO
        deri(j0) = a(i)
        deri(j1) = b(i)
        DO j = 1, nkn
          deri(j) = deri(j)+c(i)*u(j0,j)+d(i)*u(j1,j)
        ENDDO
        DO j = 1, nkn
          bvec(j) = bvec(j)+w*DBLE(y(i))*deri(j)
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
        j0 = jfs(i) - jf0
        j1 = j0 + 1
        qj  = 0.0
        qj1 = 0.0
        DO k = 1, nkn
          qj  = qj  + u(j0,k)*ans(k)
          qj1 = qj1 + u(j1,k)*ans(k)
        ENDDO
        TempResult = a(i)*ans(j0)+b(i)*ans(j1)+c(i)*qj+d(i)*qj1
        IF (ABS(TempResult) .LT. 0.000001) TempResult = 0.0
        smo(i) = SNGL(TempResult)
      ENDDO

      END SUBROUTINE SplineSmooth
!
!*****************************************************************************
!
      SUBROUTINE SplVal(XDEL,U,M)

      INTEGER M
      REAL*8  xdel(m)
      REAL*8  u(m,m)

      REAL*8  A(m,m),b(m,m),c(m,m)
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
      END DO
! Invert matrix A. Answer is in b
      CALL DGMINV(A,b,m)
! Multiply matrix b by matrix c. Result is in u
      CALL MultiplyMatrices(b,c,u,m,m,m)

      END SUBROUTINE SplVal
!
!*****************************************************************************
!
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
      B = A
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
