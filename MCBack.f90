!
!*****************************************************************************
!
      SUBROUTINE FourierPattern(Xbegin, Xeind)
!
! This subroutine calculates the Fourier transform of the pattern, allowing application of
! low and high pass filters for removing background and noise.
!
! This routine assumes that the two theta range has a uniform stepsize.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Xbegin, Xeind

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER        FT_N
      DOUBLE PRECISION                 HulpData
      COMMON /FTPRO/ HulpData(1:MOBS,1:2), FT_N

      REAL            Smoothed
      COMMON /SMOOTH/ Smoothed(MOBS)
! COMMON block holding the Fourier transform of the profile
! HulpData(1:MOBS,1) = magnitudes
! HulpData(1:MOBS,2) = phases

      INTEGER Re, Im
      PARAMETER ( Re = 1, Im = 2 )

      INTEGER     :: N
      DOUBLE PRECISION :: F(Re:Im)
      INTEGER     :: Nu, Tau, Tau2
      DOUBLE PRECISION        :: Fx
      DOUBLE PRECISION        :: NxPi
      INTEGER     :: tXbegin, tXeind ! to emulate call by value
      DOUBLE PRECISION           Amplitude
      REAL           MinX, MinY, MaxX, MaxY
      INTEGER        IHANDLE
      REAL           Step ! Step size in 2 theta
      INTEGER        I
      DOUBLE PRECISION           Phase
      REAL           PlotData(1:MOBS)

      RETURN
      CALL WCursorShape(CurHourGlass)
      tXbegin = Xbegin
      tXeind  = Xeind
      tXbegin = 1
      tXeind  = NBIN
      N = 1 + tXeind - tXbegin
! If necessary, make number of points even by omitting last point
      IF (MOD(N,2) .EQ. 1) THEN
        N = N -1
        tXeind = tXeind - 1
      ENDIF
      FT_N = N
! Assuming that the powder pattern has been recorded with a fixed step size, we can calculate it
      Step = (XBIN(N)-XBIN(1)) / (N-1)
      MinX = 1.0
      MaxX = SNGL(FT_N)
      HulpData = 0.0D0
!      NxPi = 2.0D0 * 3.14159265358979323846D0 / DBLE(N)
      NxPi = 8.0D0 * DATAN(1.0D0) / DBLE(N)
      MinY = 0.0
      MaxY = 0.0
      DO Nu = 0, N-1
        F(Re) = 0.0D0
        F(Im) = 0.0D0
        DO Tau = (-N / 2), (N / 2)-1
          IF (Tau .LT. 0) THEN
            Tau2 = Tau + N 
          ELSE 
            Tau2 = Tau
          ENDIF
          Tau2 = Tau2 + tXbegin
          Fx = DBLE(YOBIN(Tau2))
          F(Re) = F(Re) + (Fx*DCOS(NxPi*Nu*Tau))
          F(Im) = F(Im) - (Fx*DSIN(NxPi*Nu*Tau))
        ENDDO

        F(Re) = F(Re) / DBLE(N)
        F(Im) = F(Im) / DBLE(N)
        Amplitude = DSQRT(F(Re)**2 + F(Im)**2)
        MinY = MIN(MinY,SNGL(Amplitude))
        MaxY = MAX(MaxY,SNGL(Amplitude))
        HulpData(tXbegin+Nu,1) = Amplitude          ! Amplitude
        HulpData(tXbegin+Nu,2) = DATAN2(F(Im),F(Re)) ! Phase
      ENDDO
      Smoothed = 0.0D0
      DO Tau = (-N / 2), (N / 2)-1
        IF (Tau .LT. 0) THEN
          Tau2 = Tau + N 
        ELSE 
          Tau2 = Tau
        ENDIF
        Tau2 = Tau2 + tXbegin
        DO Nu = 0, N-1
          Amplitude = HulpData(tXbegin+Nu,1) 
          Phase     = HulpData(tXbegin+Nu,2)
!          IF ((DABS(Nu/(DBLE(N)*Tau)) .GT. ) .AND. (DABS(Nu/(DBLE(N)*Tau)) .LT. )) 
          Smoothed(Tau2) = Smoothed(Tau2) + Amplitude * DCOS(NxPi*Nu*Tau+Phase)
        ENDDO
      ENDDO
      CALL WCursorShape(CurCrossHair)
      OPEN(10,FILE='Fourier.xye')
      WRITE(10,*) ALambda
      DO I = 1, FT_N
        WRITE(10,*) XBIN(I), Smoothed(I), EBIN(I)
      ENDDO
      CLOSE(10)
      RETURN
! The coefficients are now in HulpData
	CALL WindowOpenChild(IHANDLE)
!      CALL IGrArea(0.0, 0.0, 1.0, 1.0)
!	CALL IGrUnits(0.0, 0.0, 1.0, 1.0)		
!
!  Start new presentation graphics plot
!!
!  Set marker number for data sets not using default marker
!
!
!  Set units for plot
!
      CALL IPgUnits(MinX*0.9, MinY*0.9, MaxX*1.1, MaxY*1.1)
!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgBorder()
!
!  Draw graph.
!
      CALL IPgNewPlot(PgLinePlot,1,FT_N)
      CALL IPgStyle(1,0,0,0,KolNumBack,0)
      DO I = 1, FT_N
        PlotData(I) = SNGL(HulpData(I,1))
      ENDDO
      CALL IPgLinePlot(PlotData(1))

      END SUBROUTINE FourierPattern
!
!*****************************************************************************
!
      SUBROUTINE SubtractBackground(nbruckwin,mbruckiter,UseMC,UseSpline)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc, UseSpline

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

      REAL              XPF_Range
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit

      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                                   &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)
      INTEGER IOBS, I, J

! Calculate the background
      CALL CalculateBackground(nbruckwin,mbruckiter,UseMC,UseSpline)
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
        END DO
        YOBIN(I) = YOBIN(I) - YBBIN(I)
        YBBIN(I) = 0.0
      END DO
      XPGMIN = XPMIN
      XPGMAX = XPMAX                       
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      CALL UPLOAD_RANGE()
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      IPMIN = 1
      IPMAX = NBIN
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX
      BACKREF = .FALSE.

      END SUBROUTINE SubtractBackground
!
!*****************************************************************************
!
      SUBROUTINE Background_Fit

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER I, IBpass
      LOGICAL QUIT

! Remember current dialogue window
      CALL PushActiveWindowID
! The reason behind this greying out seems to be that the next dialogue
! window cannot be made modal because it needs to draw the calculated background 
! to the main screen. And the user should be able to zoom in on parts of the graph
! to decide whether or not to accept the background.
      CALL WDialogSelect(IDD_Background_Fit)
! Initialise the background
      CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
      CALL CalculateBackground(IBpass,20,.TRUE.,.TRUE.)
      CALL Profile_Plot(IPTYPE)
      CALL WDialogShow(-1,-1,0,Modeless)
      QUIT = .FALSE.
      DO WHILE(.NOT. QUIT)
        CALL GetEvent
        IF (EventType .EQ. PushButton) THEN
! Process it
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Background_Apply)
              CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
              CALL CalculateBackground(IBpass,20,.TRUE.,.TRUE.)
              CALL Profile_Plot(IPTYPE)
            CASE (IDF_Background_Accept)
              CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
              CALL SubtractBackground(IBpass,20,.TRUE.,.TRUE.)
              QUIT = .TRUE.
            CASE (IDCANCEL)
! If user Cancels, assume no knowledge on background
              DO I = 1, NBIN
                YBBIN(I) = 0.0
              END DO
              QUIT = .TRUE.
          END SELECT
        END IF
      END DO
      CALL WDialogSelect(IDD_Background_Fit)
      CALL WDialogHide()
      CALL Profile_Plot(IPTYPE)
      CALL PopActiveWindowID

      END SUBROUTINE Background_Fit
!
!*****************************************************************************
!
      SUBROUTINE CalculateBackground(nbruckwin,mbruckiter,UseMC,UseSpline)

      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nbruckwin, mbruckiter
      LOGICAL, INTENT (IN   ) :: UseMc, UseSpline

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
        ys(I) = yobin(1)
      END DO
      DO I = 1, NBIN
        ys(I) = yobin(I)
      END DO
      DO I = 1, nbruckwin
        ii = NBIN + I
        ys(ii) = yobin(NBIN)
      END DO
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
        IF (UseMC) THEN
          DO I = 1, NBIN
! Use a Monte Carlo algorithm to find the correct height of the background
! rat = ratio?
            rat = (ybbin(I)-ys(I))/ebin(I)
            stem = 1.0 / (1.0 + EXP(MIN(20.0,-rat)))  
            CALL RANDOM_NUMBER(tRandomNumber)
            IF (tRandomNumber .LT. stem) ybbin(I) = ys(I)
            ys(I) = ybbin(I)
          END DO
        ELSE
          DO I = 1, NBIN
            ys(I) = MIN(ybbin(I),ys(I))
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
      END DO
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
      INTEGER I

! Initialise all entries of A, b and c to 0.0
      A = 0.0
      b = 0.0
      c = 0.0
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
