!*==MULTIPEAK_FITTER.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE MULTIPEAK_FITTER()
      REAL MULTIPEAK_CHISQ
      EXTERNAL MULTIPEAK_CHISQ
      PARAMETER (MPAR=50,MMPAR=MPAR*MPAR)
      REAL X(MPAR), DX(MPAR), COV(MMPAR)
!
!.. Do all the initialisation
      CALL INITSP(N,X,DX)
!.. Get variables
      CALL GETVAR(N,X,DX)
!.. Perform simplex
      CALL SIMOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
!      CALL SUBPLXOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
!
      DO I = 1, N
        II = I + (I-1)*N
        DX(I) = SQRT(AMAX1(0.,COV(II)))
      ENDDO
!
      CALL OUTPUT_PRO(N,X,DX)
!
      END SUBROUTINE MULTIPEAK_FITTER
!*==GETVAR.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
      SUBROUTINE GETVAR(N,V,D)
!
      INCLUDE 'PARAMS.INC'
!
      PARAMETER (MPAR=50)
      REAL V(MPAR), D(MPAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT, X(MPT), Y(MPT), E(MPT)
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)
      REAL YHITE(MPEAK)
!
      INTEGER CurrentRange
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), IPF_Lo(MAX_NPFR),        &
     &                  IPF_Hi(MAX_NPFR), NumPeakFitRange, CurrentRange,&
     &                  IPF_Range(MAX_NPFR), NumInPFR(MAX_NPFR),        &
     &                  XPF_Pos(MAX_NPPR,MAX_NPFR),                     &
     &                  YPF_Pos(MAX_NPPR,MAX_NPFR), IPF_RPt(MAX_NPFR),  &
     &                  XPeakFit(MAX_FITPT), YPeakFit(MAX_FITPT)
!
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),                       &
     &                  PkFnEsd(MPkDes,Max_NPFR),                       &
     &                  PkFnCal(MPkDes,Max_NPFR), PkFnVarVal(3,MPkDes), &
     &                  PkFnVarEsd(3,MPkDes),                           &
     &                  PkAreaVal(MAX_NPPR,MAX_NPFR),                   &
     &                  PkAreaEsd(MAX_NPPR,MAX_NPFR),                   &
     &                  PkPosVal(MAX_NPPR,MAX_NPFR),                    &
     &                  PkPosEsd(MAX_NPPR,MAX_NPFR), PkPosAv(MAX_NPFR)
!
      PI = 4.*ATAN(1.)
      RAD = PI/180.
      DEG = 1./RAD
      TWOPI = 2.*PI
      FOURPI = 4.*PI
      PIBY2 = 0.5*PI
      ALOG2 = LOG(2.)
!
      YMAX = Y(1)
      IMAX = 1
      DO I = 1, NPT
        IF (Y(I).GE.YMAX) THEN
          YMAX = Y(I)
          IMAX = I
        ENDIF
      ENDDO
!
      XMIN = X(1)
      XMAX = X(NPT)
      XDIF = XMAX - XMIN
      V(1) = Y(1)
      V(2) = (Y(NPT)-Y(1))/XDIF
      YMXB = V(1) + V(2)*(X(IMAX)-X(1))
      YPMX = YMAX - YMXB
      YPMX50 = 0.5*YPMX
!
!.. Estimate GAMM & SIGM
      DO I = IMAX, NPT
        YBT = V(1) + V(2)*(X(I)-X(1))
        YPT = Y(I) - YBT
        IF (YPT.LT.YPMX50) THEN
          XHWHMU = X(I) - X(IMAX)
          GOTO 15
        ENDIF
      ENDDO
!
   15 GAMM = 2.*XHWHMU
      SIGM = 0.2*GAMM
!
!.. Now estimate HPSL & HMSL
      YPMX20 = 0.2*YPMX
      DO I = IMAX, 1, -1
        YBT = V(1) + V(2)*(X(I)-X(1))
        YPT = Y(I) - YBT
        IF (YPT.LT.YPMX20) THEN
          XTEM = X(IMAX) - X(I)
          GOTO 20
        ENDIF
      ENDDO
   20 HTEM = 2.*ABS(XTEM-GAMM)
      HPSL = MAX(0.001,SQRT(2.*RAD*HTEM*TAN(RAD*X(IMAX))))
      HMSL = 0.5*HPSL
!
      TOTAREA = 0.
      DO I = 1, NPT
        YB = V(1) + V(2)*(X(I)-X(1))
        TOTAREA = TOTAREA + Y(I) - YB
      ENDDO
      XDELT = X(IMAX) - X(IMAX-1)
      ZXDELT = 0.25*XDELT
      TOTAREA = TOTAREA*XDELT
!
      YHSUM = 0.
      DO IP = 1, NPEAK
        ATEM = ABS(X(1)-XPOS(IP))
        IPOS(IP) = 1
        DO I = 1, NPT
          ANEW = ABS(X(I)-XPOS(IP))
          IF (ANEW.LT.ATEM) THEN
            ATEM = ANEW
            IPOS(IP) = I
          ENDIF
        ENDDO
        YB = V(1) + V(2)*(XPOS(IP)-X(1))
        YHITE(IP) = Y(IPOS(IP)) - YB
        YHSUM = YHSUM + YHITE(IP)
      ENDDO
!
      DO IP = 1, NPEAK
        AREA(IP) = TOTAREA*YHITE(IP)/YHSUM
      ENDDO
!
!      IF (CurrentRange.eq.1) Then
      V(3) = SIGM
      V(4) = GAMM
      V(5) = MIN(HPSL,0.02)
      V(6) = MIN(HMSL,0.01)
!      ELSE
!.. Use the previous results
!        XPosAvTem=0.
!        Do I=1,NPeak
!          XPosAvTem=XPosAvTem+XPos(I)
!        End Do
!        XPosAvTem=XPosAvTem/Float(NPeak)
!        Do I=1,4
!          I2=I+2
!          vi2=v(i2)
!          V(I2)=PeakFnValue(I,XPosAvTem)
!          write(76,*) 'Old & New PeakFnVal ',vi2,v(i2)
!        End Do
!      END IF
      DO I = 1, NPEAK
        KK = 4 + 2*I
        V(KK+1) = AREA(I)
        V(KK+2) = XPOS(I)
      ENDDO
!
      DO I = 1, N
        D(I) = 0.1*abs(V(I))
      ENDDO
!
      DO IP = 1, NPEAK
        KK = 6 + 2*IP
        D(KK) = 0.002
                   !(X(IMAX)-X(IMAX-1))
      ENDDO
!
!      do i=1,n
!        write(76,*) i,v(i),d(i)
!      end do
!
      RETURN
      END SUBROUTINE GETVAR
!*==WWFT01A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
! LEVEL 1    SUBROUTINE WWFT01A(IT,INV,TR,TI)
      SUBROUTINE WWFT01A(IT,INV,TR,TI)
!
! *** FT01A updated by JCM FROM HARWELL ROUTINE 9 Sep 91 ***
!
!X
!C 9C
!H Modification of Harwell Fast Fourier Transform.
!
!%
!      DIMENSION TR(%FFT2%),TI(%FFT2%)
      DIMENSION TR(1024), TI(1024)
      EXTERNAL WWFFTADD
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      COMMON /WWFFTDA/ KJUMP, UR(15), UI(15)
!
      GOTO (1,2), KJUMP
    1 UM = 0.5
      DO I = 1, 15
        UM = 0.5*UM
        TH = TWOPI*UM
        UR(I) = COS(TH)
        UI(I) = SIN(TH)
      ENDDO
      KJUMP = 2
!
! SECOND AND SUBSEQUENT ENTRIES:
    2 UM = 1.
      IF (INV.EQ.1) UM = -1.
      IO = 2
      DO I = 2, 16
        IO = IO + IO
        IF (IO-IT) 3, 4, 99
    3 ENDDO
! ERROR EXIT - IT NOT A POWER OF 2, OR TOO BIG:
   99 INV = -1
      GOTO 100
!
    4 IO = I
      II = IO
      I1 = IT/2
      I3 = 1
   10 K = 0
      I2 = I1 + I1
   11 WR = 1.
      WI = 0.
      KK = K
      JO = IO
!
   12 IF (KK.EQ.0) GOTO 13
   14 JO = JO - 1
      KK1 = KK
      KK = KK/2
      IF (KK1.EQ.2*KK) GOTO 14
      WS = WR*UR(JO) - WI*UI(JO)
      WI = WR*UI(JO) + WI*UR(JO)
      WR = WS
      GOTO 12
!
   13 WI = WI*UM
      J = 0
!
    9 L = J*I2 + K
      L1 = L + I1
      ZR = TR(L+1) + TR(L1+1)
      ZI = TI(L+1) + TI(L1+1)
      Z = WR*(TR(L+1)-TR(L1+1)) - WI*(TI(L+1)-TI(L1+1))
      TI(L1+1) = WR*(TI(L+1)-TI(L1+1)) + WI*(TR(L+1)-TR(L1+1))
      TR(L+1) = ZR
      TR(L1+1) = Z
      TI(L+1) = ZI
      J = J + 1
      IF (J.LT.I3) GOTO 9
      K = K + 1
      IF (K.LT.I1) GOTO 11
      I3 = I3 + I3
      IO = IO - 1
      I1 = I1/2
      IF (I1.GT.0) GOTO 10
      J = 1
      UM = 1.
      IF (INV.EQ.1) UM = 1./FLOAT(IT)
!
    7 K = 0
      J1 = J
      DO I = 1, II
        J2 = J1/2
        K = 2*(K-J2) + J1
        J1 = J2
      ENDDO
!
      IF (K.GE.J) THEN
        IF (K.EQ.J) THEN
          TR(J+1) = TR(J+1)*UM
          TI(J+1) = TI(J+1)*UM
        ELSE
          ZR = TR(J+1)
          ZI = TI(J+1)
          TR(J+1) = TR(K+1)*UM
          TI(J+1) = TI(K+1)*UM
          TR(K+1) = ZR*UM
          TI(K+1) = ZI*UM
        ENDIF
      ENDIF
      J = J + 1
      IF (J.LT.IT-1) GOTO 7
      TR(1) = TR(1)*UM
      TI(1) = TI(1)*UM
      TR(IT) = TR(IT)*UM
      TI(IT) = TI(IT)*UM
  100 RETURN
      END SUBROUTINE WWFT01A
!*==WWFFTADD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      BLOCKDATA WWFFTADD
      COMMON /WWFFTDA/ KJUMP, UR(15), UI(15)
      DATA KJUMP/1/
      END BLOCKDATA WWFFTADD
!*==INITSP.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
      SUBROUTINE INITSP(N,V,D)
!.. Do all the initialisation
!
      INCLUDE 'PARAMS.INC'
!
      PARAMETER (MPAR=50)
      REAL V(MPAR), D(MPAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT, X(MPT), Y(MPT), E(MPT)
!
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
      COMMON /WWREFLNZ/ ZARGK(MRFLNZ), ZXDEL(MRFLNZ)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT
      COMMON /ZSTOR2/ MN, MN2
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
     &                  DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
     &                  NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
     &                  REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
     &                  NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
!
      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /WWREFLNS/ REFH(3,WWREFDIM), AMUL(WWREFDIM),               &
     &                  AICALC(WWREFDIM), AIOBS(WWREFDIM),              &
     &                  ESDOBS(WWREFDIM), SOMEGA(WWREFDIM), GGCALC(300),&
     &                  MAXKK(9), KMIN, KMAX, KMOD, KNOW,               &
     &                  DSTAR(WWREFDIM), ISMAG(WWREFDIM), DKDDS, KOM23
      COMMON /WWSOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),   &
     &                  NPFSOU(9,5), NSOBS(5), SCALES(5), KSCALS(5),    &
     &                  NPCSOU(9,5)
      COMMON /WWPHASE/ NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9),       &
     &                 SCALEP(9), KSCALP(9), PHMAG(9)
      LOGICAL PHMAG
      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)
!
      INTEGER CurrentRange
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), IPF_Lo(MAX_NPFR),        &
     &                  IPF_Hi(MAX_NPFR), NumPeakFitRange, CurrentRange,&
     &                  IPF_Range(MAX_NPFR), NumInPFR(MAX_NPFR),        &
     &                  XPF_Pos(MAX_NPPR,MAX_NPFR),                     &
     &                  YPF_Pos(MAX_NPPR,MAX_NPFR), IPF_RPt(MAX_NPFR),  &
     &                  XPeakFit(MAX_FITPT), YPeakFit(MAX_FITPT)
!
      COMMON /PROFOBS/ NOBS, XOBS(MOBS), YOBS(MOBS), YCAL(MOBS),        &
     &                 YBAK(MOBS), EOBS(MOBS)
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS),&
     &                 YBBIN(MOBS), EBIN(MOBS)
!.. speedup attempt
      REAL ixres
!
      PI = 4.*ATAN(1.)
      RAD = PI/180.
      DEG = 1./RAD
      TWOPI = 2.*PI
      FOURPI = 4.*PI
      PIBY2 = 0.5*PI
      ALOG2 = LOG(2.)
!.. Get the number of peaks and their positions
      Ntem = NumInPFR(CurrentRange)
!
      IF (Ntem.EQ.0) THEN
        NumInPFR(CurrentRange) = 1
        YMaxTem = YOBin(IPF_Lo(CurrentRange))
        DO I = IPF_Lo(CurrentRange), IPF_Hi(CurrentRange)
          IF (YOBin(I).GT.YMaxTem) THEN
            XPF_Pos(1,CurrentRange) = XBin(I)
            YPF_Pos(1,CurrentRange) = YOBin(I)
            YMaxTem = YOBin(i)
          ENDIF
        ENDDO
      ENDIF
!
      NPeak = NumInPFR(CurrentRange)
!
      N = 6 + 2*NPeak
!
      DO I = 1, NPeak
        XPos(I) = XPF_Pos(I,CurrentRange)
      ENDDO
!
      Npt = IPF_Range(CurrentRange)
      ITEM = IPF_Lo(CurrentRange) - 1
      DO I = 1, NPT
        J = ITEM + I
        X(I) = XBIN(J)
        Y(I) = YOBIN(J)
        E(I) = EBIN(J)
      ENDDO
!
      IXRES = 1.2
!      DXRES=1./FLOAT(IXRES)
      DXRES = 1./IXRES
!
      NPTS = NPT
      MN = 1
   10 MN = 2*MN
      MNTEM = NINT(2.*IXRES*FLOAT(NPTS))
!      MNTEM=2*IXRES*NPTS
      IF (MN.LT.MNTEM) GOTO 10
      MN = MIN(2048,MN)
!	WRITE(76,*) ' MN & NPTS ',MN,NPTS
      MN2 = MN/2
!
      KNOW = 1
      JPHASE = 1
      JSOURC = 1
      NPKGEN(JPHASE,JSOURC) = 4
      ISPMIN = 1
      ISPMAX = NPT
      NPT2 = NPT/2
      ZXDELT = DXRES*(X(NPT2)-X(NPT2-1))
      ZXDEL(KNOW) = ZXDELT
      DO I = 1, NPT
        ZOBS(I) = Y(I)
        ZDOBS(I) = E(I)
        ZARGI(I) = X(I)
      ENDDO
      KOBZ(1) = ISPMIN
      ZOBSMAX = ZOBS(ISPMIN)
      DO I = ISPMIN, ISPMAX
        IF (ZOBS(I).GT.ZOBSMAX) THEN
          ZOBSMAX = ZOBS(I)
          KOBZ(1) = I
        ENDIF
      ENDDO
!
      IIMIN = 1
      IIMAX = NPTS
      XDIFT = ZARGI(IIMAX) - ZARGI(IIMIN)
      XMINT = ZARGI(IIMIN)
!
      RETURN
      END SUBROUTINE INITSP
!*==OUTPUT_PRO.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
      SUBROUTINE OUTPUT_PRO(NVAR,VARVAL,VARESD)
!
!
      INCLUDE 'PARAMS.INC'
      INTEGER CurrentRange
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), IPF_Lo(MAX_NPFR),        &
     &                  IPF_Hi(MAX_NPFR), NumPeakFitRange, CurrentRange,&
     &                  IPF_Range(MAX_NPFR), NumInPFR(MAX_NPFR),        &
     &                  XPF_Pos(MAX_NPPR,MAX_NPFR),                     &
     &                  YPF_Pos(MAX_NPPR,MAX_NPFR), IPF_RPt(MAX_NPFR),  &
     &                  XPeakFit(MAX_FITPT), YPeakFit(MAX_FITPT)
!
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS),    &
     &                ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)
!
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),                       &
     &                  PkFnEsd(MPkDes,Max_NPFR),                       &
     &                  PkFnCal(MPkDes,Max_NPFR), PkFnVarVal(3,MPkDes), &
     &                  PkFnVarEsd(3,MPkDes),                           &
     &                  PkAreaVal(MAX_NPPR,MAX_NPFR),                   &
     &                  PkAreaEsd(MAX_NPPR,MAX_NPFR),                   &
     &                  PkPosVal(MAX_NPPR,MAX_NPFR),                    &
     &                  PkPosEsd(MAX_NPPR,MAX_NPFR), PkPosAv(MAX_NPFR)
!
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
     &                  DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
     &                  NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
     &                  REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
     &                  NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END
      LOGICAL REFUSE, CYC1, NOPKRF
!
      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)
!
      PARAMETER (MPAR=50)
      REAL VARVAL(MPAR), VARESD(MPAR)
!
!
! ... Set the ranges correctly
      IPF_RPt(1) = 0
      DO II = 1, NumPeakFitRange
        IPF_RPt(II+1) = IPF_RPt(II) + IPF_Range(II)
      ENDDO
!
!
      IF (CurrentRange.EQ.NumPeakFitRange) THEN
        IPF_RPt(1) = 0
!.. We just need to add on to the end of the list
        IPF_RPt(CurrentRange+1) = IPF_RPt(CurrentRange)                 &
     &                            + IPF_Range(CurrentRange)
        ITEM = IPF_Lo(CurrentRange) - 1
        DO I = 1, NPTS
          II = IPF_RPt(CurrentRange) + I
!..           JJ=ITEM+I
          XPeakFit(II) = ZARGI(I)
          YPeakFit(II) = ZCAL(I)
        ENDDO
        DO IP = 1, NPKGEN(1,1)
          jp = ip + 2
          PkFnVal(ip,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-6) varesd(jp) = 0.02
          PkFnEsd(ip,CurrentRange) = varesd(jp)
        ENDDO
        DO I = 1, NumInPFR(CurrentRange)
          jp = 1 + NPKGEN(1,1) + 2*I
          PkAreaVal(i,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-6) varesd(jp)                           &
     &        = max(0.001,0.1*abs(varval(jp)))
          PkAreaEsd(i,CurrentRange) = varesd(jp)
          jp = jp + 1
          PkPosVal(i,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-5) varesd(jp) = 0.003
          PkPosEsd(i,CurrentRange) = varesd(jp)
          XPF_Pos(i,CurrentRange) = varval(jp)
          atem = abs(varval(jp)-zargi(1))
          DO ii = 1, npts
            anewt = abs(varval(jp)-zargi(ii))
            IF (anewt.LE.atem) THEN
              atem = anewt
              item = ii
            ENDIF
          ENDDO
          YPF_Pos(i,CurrentRange) = zcal(item)
        ENDDO
      ELSE ! It's an old range that we are refitting
        IPF_RPt(1) = 0
!
! JCC Hmm - problem here. What if we have not fitted the range, only swept it out?
! attempt to solve by re-calculating the next range's value every time rather than asuuming it is set
! (it wont be set if we've not already got into this routine before for this peak)
!
        IPF_RPt(CurrentRange+1) = IPF_RPt(CurrentRange)                 &
     &                            + IPF_Range(CurrentRange)
!
!.. We just need to modify a few variables
        DO I = 1, NPTS
          II = IPF_RPt(CurrentRange) + I
          XPeakFit(II) = ZARGI(I)
          YPeakFit(II) = ZCAL(I)
        ENDDO
        DO IP = 1, NPKGEN(1,1)
          jp = ip + 2
          PkFnVal(ip,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-5) varesd(jp) = 0.02
          PkFnEsd(ip,CurrentRange) = varesd(jp)
        ENDDO
        DO I = 1, NumInPFR(CurrentRange)
          jp = 1 + NPKGEN(1,1) + 2*I
          PkAreaVal(i,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-5) varesd(jp) = 0.1*abs(varval(jp))
          PkAreaEsd(i,CurrentRange) = varesd(jp)
          jp = jp + 1
          PkPosVal(i,CurrentRange) = varval(jp)
          IF (varesd(jp).LT.1.E-5) varesd(jp) = 0.003
          PkPosEsd(i,CurrentRange) = varesd(jp)
          XPF_Pos(i,CurrentRange) = varval(jp)
          atem = abs(varval(jp)-zargi(1))
          DO ii = 1, npts
            anewt = abs(varval(jp)-zargi(ii))
            IF (anewt.LE.atem) THEN
              atem = anewt
              item = ii
            ENDIF
          ENDDO
          YPF_Pos(i,CurrentRange) = zcal(item)
        ENDDO
      ENDIF
!
!      do i=1,nvar
!        write(76,*) i,varval(i),varesd(i)
!      end do
!
!      write(85,*) ' '
!      write(85,*) ' Current range is number ',CurrentRange
!      do i=1,npeak
!        write(85,*) ' Peak position ',i,
!     &  PkPosVal(i,CurrentRange),PkPosEsd(i,CurrentRange)
!        write(85,*) ' Peak area ',i,
!     &  PkAreaVal(i,CurrentRange),PkAreaEsd(i,CurrentRange)
!      end do
!
!      write(85,*) ' Peak shape values are:'
!      do ip=1,npkgen(1,1)
!        write(85,*) PkFnVal(ip,CurrentRange),
!     &  PkFnEsd(ip,CurrentRange)
!      end do
!
      xranav = 0.5*(XPF_Range(1,CurrentRange)+XPF_Range(2,CurrentRange))
      PkPosAv(CurrentRange) = xranav
!      do ip=1,npkgen(1,1)
!        iunit=90+ip
!        write(iunit,*) xranav,PkFnVal(ip,CurrentRange),
!     &  PkFnEsd(ip,CurrentRange)
!      end do
!
      CALL Upload_Widths()
      CALL Upload_Positions()
!
      OPEN (83,FILE='multi.pro',STATUS='unknown')
      DO i = 1, npts
        WRITE (83,*) zargi(i), zbak(i), zobs(i), zcal(i), zdobs(i)
      ENDDO
      CLOSE (83)
!
      END SUBROUTINE OUTPUT_PRO
