      SUBROUTINE MULTIPEAK_FITTER()
      REAL MULTIPEAK_CHISQ
      EXTERNAL MULTIPEAK_CHISQ
      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL X(MPAR),DX(MPAR),COV(MMPAR)
C
C.. Do all the initialisation
      CALL INITSP(N,X,DX)
C.. Get variables
      CALL GETVAR(N,X,DX)
C.. Perform simplex
      CALL SIMOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
C      CALL SUBPLXOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
C
      DO I=1,N
        II=I+(I-1)*N
        DX(I)=SQRT(AMAX1(0.,COV(II)))
      END DO
C
      CALL OUTPUT_PRO(N,X,DX)
C
      END
C
C
C
      SUBROUTINE GETVAR(N,V,D)

      INCLUDE 'PARAMS.INC'

      PARAMETER (MPAR=50)
      REAL V(MPAR),D(MPAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT,X(MPT),Y(MPT),E(MPT)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      PARAMETER (MPeak=10)
      COMMON /MULTPK/NPEAK,AREA(MPEAK),XPOS(MPEAK),IPOS(MPEAK)
      REAL YHITE(MPEAK)
C
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), 
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
C
      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),
     &PkPosAv(MAX_NPFR)
C
      PI=4.*ATAN(1.)
      RAD=PI/180.
      DEG=1./RAD
      TWOPI=2.*PI
      FOURPI=4.*PI
      PIBY2=0.5*PI
      ALOG2=LOG(2.)
C
      YMAX=Y(1)
      IMAX=1
      DO I=1,NPT
        IF (Y(I).GE.YMAX) THEN
          YMAX=Y(I)
          IMAX=I
        END IF
      END DO
C
      XMIN=X(1)
      XMAX=X(NPT)
      XDIF=XMAX-XMIN
      V(1)=Y(1)
      V(2)=(Y(NPT)-Y(1))/XDIF
      YMXB=V(1)+V(2)*(X(IMAX)-X(1))
      YPMX=YMAX-YMXB
      YPMX50=0.5*YPMX
C
C.. Estimate GAMM & SIGM
      DO I=IMAX,NPT
        YBT=V(1)+V(2)*(X(I)-X(1))
        YPT=Y(I)-YBT
        IF (YPT.LT.YPMX50) THEN
          XHWHMU=X(I)-X(IMAX)
          GOTO 15
        END IF
      END DO
C
 15   GAMM=2.*XHWHMU
      SIGM=0.2*GAMM
C
C.. Now estimate HPSL & HMSL
      YPMX20=0.2*YPMX
      DO I=IMAX,1,-1
        YBT=V(1)+V(2)*(X(I)-X(1))
        YPT=Y(I)-YBT
        IF (YPT.LT.YPMX20) THEN
          XTEM=X(IMAX)-X(I)
          GOTO 20
        END IF
      END DO
 20   HTEM=2.*ABS(XTEM-GAMM)
      HPSL=MAX(0.001,SQRT(2.*RAD*HTEM*TAN(RAD*X(IMAX))))
      HMSL=0.5*HPSL
C
      TOTAREA=0.
      DO I=1,NPT
        YB=V(1)+V(2)*(X(I)-X(1))
        TOTAREA=TOTAREA+Y(I)-YB
      END DO
      XDELT=X(IMAX)-X(IMAX-1)
      ZXDELT=0.25*XDELT
      TOTAREA=TOTAREA*XDELT
C
      YHSUM=0.
      DO IP=1,NPEAK
        ATEM=ABS(X(1)-XPOS(IP))
        IPOS(IP)=1
        DO I=1,NPT
          ANEW=ABS(X(I)-XPOS(IP))
          IF (ANEW.LT.ATEM) THEN
            ATEM=ANEW
            IPOS(IP)=I
          END IF
        END DO
        YB=V(1)+V(2)*(XPOS(IP)-X(1))
        YHITE(IP)=Y(IPOS(IP))-YB
        YHSUM=YHSUM+YHITE(IP)
      END DO
C
      DO IP=1,NPEAK
        AREA(IP)=TOTAREA*YHITE(IP)/YHSUM
      END DO
C
C      IF (CurrentRange.eq.1) Then
        V(3)=SIGM
        V(4)=GAMM
        V(5)=MIN(HPSL,0.02)
        V(6)=MIN(HMSL,0.01)
C      ELSE
C.. Use the previous results
c        XPosAvTem=0.
c        Do I=1,NPeak
c          XPosAvTem=XPosAvTem+XPos(I)
c        End Do
c        XPosAvTem=XPosAvTem/Float(NPeak)
c        Do I=1,4
c          I2=I+2
c          vi2=v(i2)
c          V(I2)=PeakFnValue(I,XPosAvTem)
c          write(76,*) 'Old & New PeakFnVal ',vi2,v(i2)
c        End Do
c      END IF
      DO I=1,NPEAK
        KK=4+2*I
        V(KK+1)=AREA(I)
        V(KK+2)=XPOS(I)
      END DO
C
      DO I=1,N
        D(I)=0.1*abs(V(I))
      END DO
C
      DO IP=1,NPEAK
        KK=6+2*IP
        D(KK)=0.002!(X(IMAX)-X(IMAX-1))
      END DO
C
!      do i=1,n
!        write(76,*) i,v(i),d(i)
!      end do
C
      RETURN
      END
C
C
C
C LEVEL 1    SUBROUTINE WWFT01A(IT,INV,TR,TI)
      SUBROUTINE WWFT01A(IT,INV,TR,TI)
C
C *** FT01A updated by JCM FROM HARWELL ROUTINE 9 Sep 91 ***
C
CX
CC 9C
CH Modification of Harwell Fast Fourier Transform.
C
C%
C      DIMENSION TR(%FFT2%),TI(%FFT2%)
      DIMENSION TR(1024),TI(1024)
      EXTERNAL WWFFTADD
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /WWFFTDA/KJUMP,UR(15),UI(15)
C
      GO TO (1,2),KJUMP
   1  UM=0.5
      DO 6 I=1,15
      UM=0.5*UM
      TH=TWOPI*UM
      UR(I)=COS(TH)
   6  UI(I)=SIN(TH)
      KJUMP=2
C
C SECOND AND SUBSEQUENT ENTRIES:
   2  UM=1.
      IF (INV .EQ. 1) UM=-1.
      IO=2
      DO 3 I=2,16
      IO=IO+IO
      IF(IO-IT)3,4,99
   3  CONTINUE
C ERROR EXIT - IT NOT A POWER OF 2, OR TOO BIG:
  99  INV=-1
      GO TO 100
C
   4  IO=I
      II=IO
      I1=IT/2
      I3=1
  10  K=0
      I2=I1+I1
  11  WR=1.
      WI=0.
      KK=K
      JO=IO
C
  12  IF (KK .EQ. 0) GO TO 13
  14  JO=JO-1
      KK1=KK
      KK=KK/2
      IF (KK1 .EQ. 2*KK) GO TO 14
      WS=WR*UR(JO)-WI*UI(JO)
      WI=WR*UI(JO)+WI*UR(JO)
      WR=WS
      GO TO 12
C
  13  WI=WI*UM
      J=0
C
   9  L=J*I2+K
      L1=L+I1
      ZR=TR(L+1)+TR(L1+1)
      ZI=TI(L+1)+TI(L1+1)
      Z=WR*(TR(L+1)-TR(L1+1))-WI*(TI(L+1)-TI(L1+1))
      TI(L1+1)=WR*(TI(L+1)-TI(L1+1))+WI*(TR(L+1)-TR(L1+1))
      TR(L+1)=ZR
      TR(L1+1)=Z
      TI(L+1)=ZI
      J=J+1
      IF (J.LT. I3) GO TO 9
      K=K+1
      IF (K .LT. I1) GO TO 11
      I3=I3+I3
      IO=IO-1
      I1=I1/2
      IF (I1 .GT. 0) GO TO 10
      J=1
      UM=1.
      IF (INV .EQ. 1) UM=1./FLOAT(IT)
C
   7  K=0
      J1=J
      DO 8 I=1,II
      J2=J1/2
      K=2*(K-J2)+J1
   8  J1=J2
C
      IF (K .GE. J) THEN
        IF (K .EQ. J) THEN
          TR(J+1)=TR(J+1)*UM
          TI(J+1)=TI(J+1)*UM
        ELSE
          ZR=TR(J+1)
          ZI=TI(J+1)
          TR(J+1)=TR(K+1)*UM
          TI(J+1)=TI(K+1)*UM
          TR(K+1)=ZR*UM
          TI(K+1)=ZI*UM
        ENDIF
      ENDIF
      J=J+1
      IF (J .LT. IT-1) GO TO 7
      TR(1)=TR(1)*UM
      TI(1)=TI(1)*UM
      TR(IT)=TR(IT)*UM
      TI(IT)=TI(IT)*UM
 100  RETURN
      END
C
      BLOCK DATA WWFFTADD
      COMMON /WWFFTDA/KJUMP,UR(15),UI(15)
      DATA KJUMP/1/
      END
C
C
      SUBROUTINE INITSP(N,V,D)
C.. Do all the initialisation

	INCLUDE 'PARAMS.INC'

      PARAMETER (MPAR=50)
      REAL V(MPAR),D(MPAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT,X(MPT),Y(MPT),E(MPT)
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /WWREFLNZ/ZARGK(MRFLNZ),ZXDEL(MRFLNZ)

      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT
      COMMON /ZSTOR2/ MN,MN2
      COMMON /WWPRPKFN/ARGI,YNORM,PKFNSP(8,6,9,5),
     & KPFNSP(8,6,9,5),
     & DERPFN(8,6),NPKFSP(8,9,5),TOLER(8,9,
     & 5),NPKGEN(9,5),PKFNVA(8),DYNDVQ(8),
     & DYNDKQ,REFUSE,CYC1,NOPKRF,TOLR(2,5),NFFT,AKNOTS,
     & NBASF4(MPRPKF,2,9),L4END(9),L6ST,L6END

      LOGICAL REFUSE,CYC1,NOPKRF
      COMMON /WWREFLNS/REFH(3,WWREFDIM),AMUL(WWREFDIM),AICALC(WWREFDIM),
     & AIOBS(WWREFDIM),ESDOBS(WWREFDIM),SOMEGA(WWREFDIM),GGCALC(300),
     & MAXKK(9),KMIN,KMAX,KMOD,KNOW,DSTAR(WWREFDIM),ISMAG(WWREFDIM),
     & DKDDS,KOM23
      COMMON /WWSOURCE/NSOURC,JSOURC,KSOURC,NDASOU(5),METHOD(
     & 9),NPFSOU(9,5),NSOBS(5),SCALES(5),
     & KSCALS(5),NPCSOU(9,5)
      COMMON /WWPHASE/NPHASE,IPHASE,JPHASE,KPHASE,NPHUNI(9),
     & SCALEP(9),KSCALP(9),PHMAG(9)
      LOGICAL PHMAG
      PARAMETER (MPeak=10)
      COMMON /MULTPK/NPEAK,AREA(MPEAK),XPOS(MPEAK),IPOS(MPEAK)
C
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), 
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),
     &YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),
     &YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
c.. speedup attempt
	real ixres
C
      PI=4.*ATAN(1.)
      RAD=PI/180.
      DEG=1./RAD
      TWOPI=2.*PI
      FOURPI=4.*PI
      PIBY2=0.5*PI
      ALOG2=LOG(2.)
C.. Get the number of peaks and their positions
      Ntem=NumInPFR(CurrentRange)
C
      If (Ntem.eq.0) THEN
        NumInPFR(CurrentRange)=1
        YMaxTem=YOBin(IPF_Lo(CurrentRange))
        Do I=IPF_Lo(CurrentRange),IPF_Hi(CurrentRange)
          If (YOBin(I).gt.YMaxTem) Then
            XPF_Pos(1,CurrentRange)=XBin(I)
            YPF_Pos(1,CurrentRange)=YOBin(I)
            YMaxTem=YOBin(i)
          End If
        End Do
      End If
C        
      NPeak=NumInPFR(CurrentRange)
C
      N=6+2*NPeak
C
      DO I=1,NPeak
        XPos(I)=XPF_Pos(I,CurrentRange)
      END DO
C
      Npt=IPF_Range(CurrentRange)
      ITEM=IPF_Lo(CurrentRange)-1
      DO I=1,NPT
        J=ITEM+I
        X(I)=XBIN(J)
        Y(I)=YOBIN(J)
        E(I)=EBIN(J)
      END DO
C
      IXRES=1.2
C      DXRES=1./FLOAT(IXRES)
      DXRES=1./IXRES
C
      NPTS=NPT
      MN=1
 10   MN=2*MN
      MNTEM=NINT(2.*IXRES*FLOAT(NPTS))
C      MNTEM=2*IXRES*NPTS
      IF (MN.LT.MNTEM) GOTO 10
      MN=MIN(2048,MN)
!	WRITE(76,*) ' MN & NPTS ',MN,NPTS
      MN2=MN/2
C
      KNOW=1
      JPHASE=1
      JSOURC=1
      NPKGEN(JPHASE,JSOURC)=4
      ISPMIN=1
      ISPMAX=NPT
      NPT2=NPT/2
      ZXDELT=DXRES*(X(NPT2)-X(NPT2-1))
      ZXDEL(KNOW)=ZXDELT
      DO I=1,NPT
        ZOBS(I)=Y(I)
        ZDOBS(I)=E(I)
        ZARGI(I)=X(I)
      END DO
      KOBZ(1)=ISPMIN
      ZOBSMAX=ZOBS(ISPMIN)
      DO I=ISPMIN,ISPMAX
        IF (ZOBS(I).GT.ZOBSMAX) THEN
          ZOBSMAX=ZOBS(I)
          KOBZ(1)=I
        END IF
      END DO
C
      IIMIN=1
      IIMAX=NPTS
      XDIFT=ZARGI(IIMAX)-ZARGI(IIMIN)
      XMINT=ZARGI(IIMIN)
C
      RETURN
      END
c
c
c
      subroutine OUTPUT_PRO(NVAR,VARVAL,VARESD)
c

      INCLUDE 'PARAMS.INC'
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
c
      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),
     &ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
C
      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),
     &PkPosAv(MAX_NPFR)
c
      COMMON /WWPRPKFN/ARGI,YNORM,PKFNSP(8,6,9,5),
     & KPFNSP(8,6,9,5),
     & DERPFN(8,6),NPKFSP(8,9,5),TOLER(8,9,
     & 5),NPKGEN(9,5),PKFNVA(8),DYNDVQ(8),
     & DYNDKQ,REFUSE,CYC1,NOPKRF,TOLR(2,5),NFFT,AKNOTS,
     & NBASF4(MPRPKF,2,9),L4END(9),L6ST,L6END
      LOGICAL REFUSE,CYC1,NOPKRF
c
      PARAMETER (MPeak=10)
      COMMON /MULTPK/NPEAK,AREA(MPEAK),XPOS(MPEAK),IPOS(MPEAK)
c
      PARAMETER (MPAR=50)
      REAL VARVAL(MPAR),VARESD(MPAR)
c

C ... Set the ranges correctly
	IPF_RPt(1)=0
	DO II = 1, NumPeakFitRange
		IPF_RPt(II+1) = IPF_RPt(II) + IPF_Range(II)
	END DO


      IF (CurrentRange.eq.NumPeakFitRange) THEN
         IPF_RPt(1)=0
c.. We just need to add on to the end of the list
         IPF_RPt(CurrentRange+1)=
     &       IPF_RPt(CurrentRange)+IPF_Range(CurrentRange)
         ITEM=IPF_Lo(CurrentRange)-1
         DO I=1,NPTS
           II=IPF_RPt(CurrentRange)+I
c..           JJ=ITEM+I
           XPeakFit(II)=ZARGI(I)
           YPeakFit(II)=ZCAL(I)
         END DO
         DO IP=1,NPKGEN(1,1)
           jp=ip+2
           PkFnVal(ip,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-6) varesd(jp)=0.02
           PkFnEsd(ip,CurrentRange)=varesd(jp)
         END DO
         DO I=1,NumInPFR(CurrentRange)
           jp=1+NPKGEN(1,1)+2*I
           PkAreaVal(i,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-6) 
     &      varesd(jp)=max(0.001,0.1*abs(varval(jp)))
           PkAreaEsd(i,CurrentRange)=varesd(jp)
           jp=jp+1
           PkPosVal(i,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-5) varesd(jp)=0.003
           PkPosEsd(i,CurrentRange)=varesd(jp)
           XPF_Pos(i,CurrentRange)=varval(jp)
           atem=abs(varval(jp)-zargi(1))
           do ii=1,npts
             anewt=abs(varval(jp)-zargi(ii))
             if (anewt.le.atem) then
               atem=anewt
               item=ii
             end if
           end do	
           YPF_Pos(i,CurrentRange)=zcal(item)	
         END DO
      ELSE ! It's an old range that we are refitting
         IPF_RPt(1)=0

C JCC Hmm - problem here. What if we have not fitted the range, only swept it out?
C attempt to solve by re-calculating the next range's value every time rather than asuuming it is set
C (it wont be set if we've not already got into this routine before for this peak)

         IPF_RPt(CurrentRange+1)=
     &       IPF_RPt(CurrentRange)+IPF_Range(CurrentRange)          
C 
c.. We just need to modify a few variables
         DO I=1,NPTS
           II=IPF_RPt(CurrentRange)+I
           XPeakFit(II)=ZARGI(I)
           YPeakFit(II)=ZCAL(I)
         END DO
         DO IP=1,NPKGEN(1,1)
           jp=ip+2
           PkFnVal(ip,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-5) varesd(jp)=0.02
           PkFnEsd(ip,CurrentRange)=varesd(jp)
         END DO
         DO I=1,NumInPFR(CurrentRange)
           jp=1+NPKGEN(1,1)+2*I
           PkAreaVal(i,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-5) varesd(jp)=0.1*abs(varval(jp))
           PkAreaEsd(i,CurrentRange)=varesd(jp)
           jp=jp+1
           PkPosVal(i,CurrentRange)=varval(jp)
           if (varesd(jp).lt.1.e-5) varesd(jp)=0.003
           PkPosEsd(i,CurrentRange)=varesd(jp)
           XPF_Pos(i,CurrentRange)=varval(jp)
           atem=abs(varval(jp)-zargi(1))
           do ii=1,npts
             anewt=abs(varval(jp)-zargi(ii))
             if (anewt.le.atem) then
               atem=anewt
               item=ii
             end if
           end do	
           YPF_Pos(i,CurrentRange)=zcal(item)	
         END DO
      END IF
c
!      do i=1,nvar
!        write(76,*) i,varval(i),varesd(i)
!      end do
c
!      write(85,*) ' '
!      write(85,*) ' Current range is number ',CurrentRange
!      do i=1,npeak
!        write(85,*) ' Peak position ',i,
!     &  PkPosVal(i,CurrentRange),PkPosEsd(i,CurrentRange)
!        write(85,*) ' Peak area ',i,
!     &  PkAreaVal(i,CurrentRange),PkAreaEsd(i,CurrentRange)   
!      end do
c
!      write(85,*) ' Peak shape values are:'
!      do ip=1,npkgen(1,1)
!        write(85,*) PkFnVal(ip,CurrentRange),
!     &  PkFnEsd(ip,CurrentRange)
!      end do
c
      xranav=0.5*(XPF_Range(1,CurrentRange)
     &        +XPF_Range(2,CurrentRange))
      PkPosAv(CurrentRange)=xranav
!      do ip=1,npkgen(1,1)
!        iunit=90+ip
!        write(iunit,*) xranav,PkFnVal(ip,CurrentRange),
!     &  PkFnEsd(ip,CurrentRange)
!      end do
c
      call Upload_Widths()
      call Upload_Positions()
c
      open(83,file='multi.pro',status='unknown')
      do i=1,npts
        write(83,*) zargi(i),zbak(i),zobs(i),zcal(i),zdobs(i)
      end do
      close(83)
c
      end
c
c
c
      Function PeakFnValue(IPFnV,XIn)
c
c.. Evaluate the parameters for the peak shape variables

      INCLUDE 'PARAMS.INC'

      INTEGER CurrentRange
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT) 

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),
     &PkFnEsd(MPkDes,Max_NPFR),PkFnCal(MPkDes,Max_NPFR),
     &PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes),
     &PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR),
     &PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),
     &PkPosAv(MAX_NPFR)
c
      COMMON /WWPRPKFN/ARGI,YNORM,PKFNSP(8,6,9,5),
     & KPFNSP(8,6,9,5),
     & DERPFN(8,6),NPKFSP(8,9,5),TOLER(8,9,
     & 5),NPKGEN(9,5),PKFNVA(8),DYNDVQ(8),
     & DYNDKQ,REFUSE,CYC1,NOPKRF,TOLR(2,5),NFFT,AKNOTS,
     & NBASF4(MPRPKF,2,9),L4END(9),L6ST,L6END
      LOGICAL REFUSE,CYC1,NOPKRF
c
c.. just do averages for the moment
      etem=-1.
      NTem=NumPeakFitRange-1
      do i=1,NTem
        etem=max(etem,PkFnEsd(IPFnV,i))
      end do
c
      if (etem.eq.0.) then
        do i=1,NTem
          PkFnEsd(IPFnV,i)=10.
        end do
      end if
c
      atem=0.
      btem=0.
      do i=1,NTem
        wt=1./PkFnEsd(IPFnV,i)**2
        atem=atem+wt*PkFnVal(IPFnV,i)
        btem=btem+wt
      end do
c
      PeakFnValue=atem/btem
!	 write(76,*) IPFnV,NTem,PeakFnValue
c
c
c      IF (I.eq.1) THEN
c.. Sigma
c      ELSE IF (I.eq.2) THEN
c.. Gamma
c      ELSE IF (I.eq.3) THEN
c.. HPSL
c      ELSE IF (I.eq.4) THEN
c.. HMSL
c      END IF
c
      End