
      SUBROUTINE BackFit(FilterWidth)
      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)

      INTEGER FilterWidth

! Calculate values in common array
      CALL BackMCBruckner(XBIN,YOBIN,EBIN,NBIN,FilterWidth,YBBIN)

      END SUBROUTINE BackFit
!
!*****************************************************************************
!
      SUBROUTINE BackMCBruckner(xbin,yobin,ebin,ndat,nbruckwin,ybbin)
!
      INCLUDE 'PARAMS.INC'

      PARAMETER (MAXSSPL=5000)
      PARAMETER (MXRAN=10000)
!
      REAL,    DIMENSION(MAXSSPL)                 :: xkt
      INTEGER, DIMENSION(MAXSSPL)                 :: ikt
      INTEGER, DIMENSION(MAXSSPL)                 :: ipartem
!
      REAL,    DIMENSION(MOBS), INTENT(IN)      :: xbin,yobin,ebin
      REAL,    DIMENSION(MOBS)            :: ybbin
! JvdS New, local copies of ybbin etc. are created here, but that's not necessary
      REAL,    DIMENSION(MOBS)            :: yst,es
      REAL,    DIMENSION(-200:MOBS+200)   :: ys
      INTEGER, DIMENSION(MOBS)            :: jft
!
      INTEGER                             :: IRAN
      REAL, DIMENSION(MXRAN)              :: RANVAL
!
!  This subroutine determines the background using a smoothing
!  procedure published by Sergio Bruckner in J. Appl. Cryst. (2000) 33, 977-979
!  Modified by WIFD to smooth residual noise using SplineSmooth 
!  and raise background to correct value using a Monte Carlo sampling procedure)
!
      mbruckiter = 20
      iran = 1
      CALL Random_Number(RanVal)
      DO i = -nbruckwin, 0
        ii = ndat + i
        ys(i) = yobin(1)
      END DO
      DO i = 1, ndat
        ys(i) = yobin(i)
      END DO
      DO i = 1, nbruckwin
        ii = ndat + i
        ys(ii) = yobin(ndat)
      END DO
      item = 2 * nbruckwin
      DO iter = 1, mbruckiter
        DO i = 1, ndat
          iilo = i - nbruckwin
          iihi = i + nbruckwin
          ybbin(i) = 0.0
          DO ii = iilo, iihi
            ybbin(i) = ybbin(i) + ys(ii)
          END DO
          ybbin(i) = (ybbin(i)-ys(i))/FLOAT(item)
        END DO
        DO i = 1, ndat
          rat = (ybbin(i)-ys(i))/ebin(i)
          stem=1./(1.+EXP(MIN(20.,-rat)))  
          iran=1+MOD(iran+1,mxran)
          IF (ranval(iran) .LT. stem) THEN
            ybbin(i)=ys(i)
          END IF
          ys(i)=ybbin(i)
        END DO
      END DO
!
!.. Now we should do some spline smoothing to remove the noise 
!   
      nsep    =  5
      ninsep  = 10
      ngood   =  0
      knotem  =  0
      npartem =  0
      DO i = 1, ndat
        IF (ybbin(i) .EQ. yobin(i)) THEN
          es(i)=1.e6*ebin(i)
        ELSE
          es(i) = ebin(i)
          IF (ngood.eq.nsep*(ngood/nsep)) THEN
            IF (knotem.eq.ninsep*(knotem/ninsep)) THEN
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
      ikt(knotem) = ndat
      ipartem(npartem) = knotem
      jft(1) = 1
      jft(ndat) = knotem - 1
      DO kk=1,knotem-1
        i1=ikt(kk)
        i2=ikt(kk+1)-1
        DO i=i1,i2
          jft(i)=kk
        END DO
      END DO
      DO i = 1, npartem - 1
        jf1 = ipartem(i)
        jf0 = jf1-1
        jfp1 = ipartem(i+1)
        jfn = 1 + jfp1 - ipartem(i)
        n0 = ikt(jf1)
        ndiv = 1 + ikt(jfp1) - n0
        CALL SplineSmooth(xbin(n0),ys(n0),es(n0),ndiv,jf0,jft(n0),xkt(jf1),jfn,ybbin(n0))
      END DO
!
      END SUBROUTINE BackMCBruckner
!
!*****************************************************************************
!
      SUBROUTINE SplineSmooth(x,y,e,ndat,jf0,jfs,xkk,nkn,smo)
!
      REAL xkk(nkn)
      REAL x(ndat),y(ndat),e(ndat)
      INTEGER jfs(ndat)
!
      REAL*8 xdel(nkn),u(nkn,nkn)
      REAL*8 bvec(nkn),hess(nkn,nkn),covar(nkn,nkn)
!
      REAL*8 xdd
      REAL*8 a(ndat),b(ndat),c(ndat),d(ndat)
!
      REAL*8 deri(nkn),ans(nkn)
      REAL   smo(ndat)
      REAL*8 w
      REAL*8 qj, qj1
!
      DO j=1,nkn-1
        xdel(j)=DBLE(xkk(j+1)-xkk(j))
      END DO
      CALL SplVal(xdel,u,nkn)
      nd1=ndat-1
      nk1=nkn-1
      DO j=1,nkn
        bvec(j)=0. 
        DO k=1,nkn
          hess(j,k)=0. 
        END DO
      END DO
      DO i=1,ndat
        j0=MIN(nkn-1,jfs(i)-jf0)
        j1=j0+1
        w= DBLE(e(i))**-2
!       b(i)=(dble(x(i))-xkk(j0))/xdel(j0)
        b(i)= ( DBLE( x(i)-xkk(j0) ) )/xdel(j0)
        a(i)=1.-b(i)
        ab=-a(i)*b(i)/6.
        xdd=xdel(j0)**2
        c(i)=ab*(1.+a(i))*xdd
        d(i)=ab*(1.+b(i))*xdd
        DO j=1,nkn
          deri(j)=0.
        END DO
        deri(j0)=a(i)
        deri(j1)=b(i)
        DO j=1,nkn
          deri(j)=deri(j)+c(i)*u(j0,j)+d(i)*u(j1,j)
        END DO
        do j=1,nkn
          bvec(j)=bvec(j)+w*dble(y(i))*deri(j)
          do k=1,nkn
            hess(j,k)=hess(j,k)+w*deri(j)*deri(k)
          end do
        end do
      end do
      call DGMINV(hess,covar,nkn)
      do i=1,nkn
        ans(i)=0.
        do j=1,nkn
          ans(i)=ans(i)+covar(i,j)*bvec(j)
        end do
      end do
      do i=1,ndat
        j0=jfs(i)-jf0
        j1=j0+1
        qj=0.
        qj1=0.
        do k=1,nkn
          qj=qj+u(j0,k)*ans(k)
          qj1=qj1+u(j1,k)*ans(k)
        end do
        smo(i)=sngl(a(i)*ans(j0)+b(i)*ans(j1)+c(i)*qj+d(i)*qj1)
      END DO
!
      END SUBROUTINE SplineSmooth
!
!*****************************************************************************
!
      SUBROUTINE SplVal(XDEL,U,M)
!
      REAL*8 xdel(m)
      REAL*8 u(m,m)
      REAL*8 a(m,m),b(m,m),c(m,m)
!
      do j=1,m
        do i=1,m
          a(i,j)=0.
          b(i,j)=0.
          c(i,j)=0.
        end do
      end do
      a(1,1)=1.
      a(m,m)=1.
      DO i=2,m-1
        im1=i-1
        ip1=i+1
        a(i,im1)=xdel(im1)/6.0
        a(i,ip1)=xdel(i)/6.0
        a(i,i)=2.*(a(i,im1)+a(i,ip1))
        c(i,im1)=1./xdel(im1)
        c(i,ip1)=1./xdel(i)
        c(i,i)=-(c(i,im1)+c(i,ip1))
      end do
!
      call DGMINV(a,b,m)
      call DGMPRD(b,c,u,m,m,m)
!
      end subroutine SplVal
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
      INTEGER           II(500),IL(500),IG(500)
      REAL*8            A(N,N),B(N,N)
!
      CALL DGMEQ(A,B,N,N)
      D=1.
      IS=N-1
      DO 10 K=1,N
      IL(K)=0
   10 IG(K)=K
!
      DO 150 K=1,N
      R=0.
      DO 40 I=1,N
      IF (IL(I) .NE. 0) GO TO 40
      W=B(I,K)
      X=ABS(W)
      IF (R .GT. X) GO TO 40
      R=X
      P=W
      KF=I
   40 CONTINUE
      II(K)=KF
      IL(KF)=KF
      D=D*P
      IF (D .EQ. 0.) D=1.e-6
!
      DO 80 I=1,N
      IF (I .EQ. KF) THEN
      B(I,K)=1./P
      ELSE
      B(I,K)=-B(I,K)/P
      ENDIF
   80 CONTINUE
!
      DO 140 J=1,N
        IF (J .EQ. K) GO TO 140
        W=B(KF,J)
        IF (W .EQ. 0.) GO TO 140
        DO 130 I=1,N
          IF (I .EQ. KF) THEN
            B(I,J)=W/P
          ELSE
            B(I,J)=B(I,J)+W*B(I,K)
          ENDIF
  130   CONTINUE
  140 CONTINUE
!
  150 CONTINUE
!.....
!
      DO 190 K=1,IS
      KF=II(K)
      KL=IL(KF)
      KG=IG(K)
      IF(KF .EQ. KG) GO TO 190
      DO 170 I=1,N
      R=B(I,KF)
      B(I,KF)=B(I,KG)
  170 B(I,KG)=R
      DO 180 J=1,N
      R=B(K,J)
      B(K,J)=B(KL,J)
  180 B(KL,J)=R
      IL(KF)=K
      IL(KG)=KL
      IG(KL)=IG(K)
      IG(K)=KF
      D=-D
  190 CONTINUE
      RETURN
      END
!
!
! LEVEL 1      SUBROUTINE DGMEQ(A,B,NI,NJ)
      SUBROUTINE DGMEQ(A,B,NI,NJ)
!
! *** GMEQ by PJB/JCM 28 Jun 83 ***
!
!X
!C 12C
!H Sets matrix B = matrix A.
!A On exit  B is a real matrix equal to A
!N NI and NJ must be at least 1
!
      REAL*8 A(NI,NJ),B(NI,NJ)

      DO 1 I=1,NI
        DO 1 J=1,NJ
    1 B(I,J)=A(I,J)
      RETURN
      END
!
!*****************************************************************************
!
! LEVEL 1      SUBROUTINE DGMPRD(A,B,C,NI,NJ,NK)
      SUBROUTINE DGMPRD(A,B,C,NI,NJ,NK)
!
! *** GMPRD by JCM ***
!
!X
!
!H Sets matrix C = matrix A times matrix B.
!A On entry A is a real NIxNJ matrix
!A          B is a real NJxNK matrix
!A On exit  C is a real NIxNK matrix holding A times B
!
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(*),B(*),C(*)
      DO 2 I = 1, NI
      IK = I
      JK = 1
      DO 2 K = 1, NK
        IJ = I
        C(IK) = 0.
        DO 1 J = 1,NJ
          C(IK) = C(IK) + A(IJ)*B(JK)
          IJ = IJ + NI
    1   JK = JK + 1
    2 IK = IK + NI
      RETURN
      END
