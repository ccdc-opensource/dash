
  subroutine BackFit(Mode, FilterWidth)
   INCLUDE 'PARAMS.INC'

! Temporary local copies 
   REAL,    DIMENSION(MOBS)		:: ybbin_backup
   SAVE ybbin_backup

   COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
   COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
   COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
   YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
   XGGMIN,XGGMAX,YGGMIN,YGGMAX
   COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD


   INTEGER Mode,FilterWidth
   INTEGER I

   IF (Mode .EQ. 0) THEN
    ! make back up
     DO I = 1, NBIN
	  ybbin_backup(I) = ybbin(I)
	 END DO

   ELSE IF (Mode .EQ. 1) THEN ! Calculate values in common array

	CALL BackMCBruckner(xbin,yobin,ebin,nbin,FilterWidth,ybbin)	

   ELSE IF (Mode .EQ. 2) THEN
    ! recover backup
     DO I = 1, NBIN
	  ybbin(I) = ybbin_backup(I)
	 END DO
	 
   END IF

  end subroutine BackFit
!
!
  subroutine BackMCBruckner(xbin,yobin,ebin,ndat,nbruckwin,ybbin)
!
!
   INCLUDE 'PARAMS.INC'

	PARAMETER (MAXSSPL=5000)
	PARAMETER (MXRAN=10000)
!
	REAL,    DIMENSION(MAXSSPL)		        :: xkt
	INTEGER, DIMENSION(MAXSSPL)		        :: ikt
	INTEGER, DIMENSION(MAXSSPL)		        :: ipartem
!
	REAL,    DIMENSION(MOBS), INTENT(IN)	:: xbin,yobin,ebin
	REAL,    DIMENSION(MOBS)		:: ybbin
	REAL,    DIMENSION(MOBS)		:: yst,es
	REAL,    DIMENSION(-200:MOBS+200)	:: ys
	INTEGER, DIMENSION(MOBS)		:: jft
!
	INTEGER					:: IRAN
	REAL, DIMENSION(MXRAN)			:: RANVAL
!
!  This subroutine determines the background using a smoothing
!  procedure published by Sergio Bruckner in J. Appl. Cryst. (2000) 33, 977-979
!  Modified by WIFD to smooth residual noise using SplineSmooth 
!  and raise background to correct value using a Monte Carlo sampling procedure)
!
	mbruckiter=20
	iran=1
      call Random_Number(RanVal)
!
	do i=-nbruckwin,0
	  ii=ndat+i
	  ys(i)=yobin(1)
	end do
	do i=1,ndat
	  ys(i)=yobin(i)
	end do
	do i=1,nbruckwin
	  ii=ndat+i
	  ys(ii)=yobin(ndat)
	end do
!
    item= 2*nbruckwin
	do iter=1,mbruckiter
	 do i=1,ndat
	  iilo=i-nbruckwin
	  iihi=i+nbruckwin
	  ybbin(i)=0.
	  do ii=iilo,iihi
	    ybbin(i)=ybbin(i)+ys(ii)
	  end do
	  ybbin(i)= (ybbin(i)-ys(i))/float(item)
	 end do
!
	 do i=1,ndat
	  rat=(ybbin(i)-ys(i))/ebin(i)
	  stem=1./(1.+exp(min(20.,-rat)))  
	  iran=1+mod(iran+1,mxran)
	  if (ranval(iran).lt.stem) then
		ybbin(i)=ys(i)
	  end if
	  ys(i)=ybbin(i)
	 end do
!
	end do
!	return
!
!.. Now we should do some spline smoothing to remove the noise 
!   
	nsep=5
	ninsep=10
	ngood=0
	knotem=0
	npartem=0
	do i=1,ndat
	  if (ybbin(i).eq.yobin(i)) then
 	    es(i)=1.e6*ebin(i)
	  else
	    es(i)=ebin(i)
	    if (ngood.eq.nsep*(ngood/nsep)) then
	      if (knotem.eq.ninsep*(knotem/ninsep)) then
	        npartem=npartem+1
	        ipartem(npartem)=knotem+1
	      end if
	      knotem=knotem+1
	      ikt(knotem)=i
	      xkt(knotem)=xbin(i)
	    end if
	    ngood=ngood+1
	  end if
	end do
	ikt(knotem)=ndat
	ipartem(npartem)=knotem

!
	jft(1)=1
	jft(ndat)=knotem-1
	do kk=1,knotem-1
	  i1=ikt(kk)
	  i2=ikt(kk+1)-1
	  do i=i1,i2
	    jft(i)=kk
	  end do
	end do
!
	do i=1,npartem-1
	  jf1=ipartem(i)
	  jf0=jf1-1
	  jfp1=ipartem(i+1)
	  jfn=1+jfp1-ipartem(i)
	  n0=ikt(jf1)
	  ndiv=1+ikt(jfp1)-n0
	  call SplineSmooth(xbin(n0),ys(n0),es(n0),ndiv,jf0,jft(n0),xkt(jf1),jfn,ybbin(n0))
	end do
!
!
  end subroutine BackMCBruckner
!
!
	subroutine SplineSmooth(x,y,e,ndat,jf0,jfs,xkk,nkn,smo)
!
	real xkk(nkn)
	real x(ndat),y(ndat),e(ndat)
	integer jfs(ndat)
!
	real*8 xdel(nkn),u(nkn,nkn)
	real*8 bvec(nkn),hess(nkn,nkn),covar(nkn,nkn)
!
	real*8 xdd
	real*8 a(ndat),b(ndat),c(ndat),d(ndat)
!
	real*8 deri(nkn),ans(nkn)
	real   smo(ndat)
	real*8 w
	real*8 qj, qj1
!
	do j=1,nkn-1
	  xdel(j)=dble(xkk(j+1)-xkk(j))
	end do
!
	call SplVal(xdel,u,nkn)
!
	nd1=ndat-1
	nk1=nkn-1
!
	do j=1,nkn
	  bvec(j)=0. 
	  do k=1,nkn
	    hess(j,k)=0. 
	  end do
	end do
!
!
	do i=1,ndat
	  j0=min(nkn-1,jfs(i)-jf0)
	  j1=j0+1
	  w= dble(e(i))**-2
!	  b(i)=(dble(x(i))-xkk(j0))/xdel(j0)
	  b(i)= ( dble( x(i)-xkk(j0) ) )/xdel(j0)
	  a(i)=1.-b(i)
	  ab=-a(i)*b(i)/6.
	  xdd=xdel(j0)**2
	  c(i)=ab*(1.+a(i))*xdd
	  d(i)=ab*(1.+b(i))*xdd
	  do j=1,nkn
	    deri(j)=0.
	  end do
	  deri(j0)=a(i)
	  deri(j1)=b(i)
	  do j=1,nkn
	    deri(j)=deri(j)+c(i)*u(j0,j)+d(i)*u(j1,j)
	  end do
	  do j=1,nkn
	    bvec(j)=bvec(j)+w*dble(y(i))*deri(j)
	    do k=1,nkn
	      hess(j,k)=hess(j,k)+w*deri(j)*deri(k)
	    end do
	  end do
	end do
!
	call DGMINV(hess,covar,nkn)
!
	do i=1,nkn
	  ans(i)=0.
	  do j=1,nkn
	    ans(i)=ans(i)+covar(i,j)*bvec(j)
	  end do
	end do
!
!
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
! 
	end do
!
	end subroutine SplineSmooth
!
!
!
	subroutine SplVal(XDEL,U,M)
!
	real*8 xdel(m)
	real*8 u(m,m)
	real*8 a(m,m),b(m,m),c(m,m)
!
	do j=1,m
	  do i=1,m
	    a(i,j)=0.
	    b(i,j)=0.
	    c(i,j)=0.
	  end do
	end do
!
	a(1,1)=1.
	a(m,m)=1.
!
	do i=2,m-1
	  im1=i-1
	  ip1=i+1
	  a(i,im1)=xdel(im1)/6.
	  a(i,ip1)=xdel(i)/6.
	  a(i,i)=2.*(a(i,im1)+a(i,ip1))
	  c(i,im1)=1./xdel(im1)
	  c(i,ip1)=1./xdel(i)
	  c(i,i)=-(c(i,im1)+c(i,ip1))
	end do
!
	call DGMINV(a,b,m)
	call DGMPRD(b,c,u,m,m,m)
!
	end
!
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
      INTEGER		II(500),IL(500),IG(500)
	REAL*8		A(N,N),B(N,N)
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
  130 CONTINUE
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
      DO 2 I = 1,NI
      IK = I
      JK = 1
      DO 2 K = 1,NK
      IJ = I
      C(IK) = 0.
      DO 1 J = 1,NJ
      C(IK) = C(IK) + A(IJ)*B(JK)
      IJ = IJ + NI
    1 JK = JK + 1
    2 IK = IK + NI
      RETURN
      END