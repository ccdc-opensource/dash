!
!*****************************************************************************
!
      SUBROUTINE MULTIPEAK_FITTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      REAL, EXTERNAL :: MULTIPEAK_CHISQ
      INTEGER     MMPAR
      PARAMETER ( MMPAR = MVAR * MVAR )
      REAL X(MVAR), DX(MVAR), COV(MMPAR)
      INTEGER N, I, II

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! Do all the initialisation
      CALL INITSP(N,X,DX)
! Get variables
      CALL GETVAR(N,X,DX)
! Perform simplex
      IBMBER = 0
      CALL SIMOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
      IF (IBMBER .NE. 0) THEN
        CALL DebugErrorMessage('Error fitting peak.')
        RETURN
      ENDIF
      RangeFitYN(CurrentRange) = .TRUE.
      DO I = 1, N
        II = I + (I-1)*N
        DX(I) = SQRT(AMAX1(0.0,COV(II)))
      ENDDO
      CALL OUTPUT_PRO(N,X,DX)

      END SUBROUTINE MULTIPEAK_FITTER
!
!*****************************************************************************
!
      SUBROUTINE GETVAR(N,V,D)

      INCLUDE 'PARAMS.INC'

      REAL V(MVAR), D(MVAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT, X(MPT), Y(MPT), E(MPT)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)
      REAL YHITE(MPEAK)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      YMAX = Y(1)
      IMAX = 1
      DO I = 1, NPT
        IF (Y(I).GE.YMAX) THEN
          YMAX = Y(I)
          IMAX = I
        ENDIF
      ENDDO
      XMIN = X(1)
      XMAX = X(NPT)
      XDIF = XMAX - XMIN
      V(1) = Y(1)
      V(2) = (Y(NPT)-Y(1))/XDIF
      YMXB = V(1) + V(2)*(X(IMAX)-X(1))
      YPMX = YMAX - YMXB
      YPMX50 = 0.5*YPMX
! Estimate GAMM & SIGM
      DO I = IMAX, NPT
        YBT = V(1) + V(2)*(X(I)-X(1))
        YPT = Y(I) - YBT
        IF (YPT.LT.YPMX50) THEN
          XHWHMU = X(I) - X(IMAX)
          GOTO 15
        ENDIF
      ENDDO
   15 GAMM = 2.*XHWHMU
      SIGM = 0.2*GAMM
! Now estimate HPSL & HMSL
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
      TOTAREA = 0.
      DO I = 1, NPT
        YB = V(1) + V(2)*(X(I)-X(1))
        TOTAREA = TOTAREA + Y(I) - YB
      ENDDO
      XDELT = X(IMAX) - X(IMAX-1)
      TOTAREA = TOTAREA*XDELT
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
      DO IP = 1, NPEAK
        AREA(IP) = TOTAREA*YHITE(IP)/YHSUM
      ENDDO
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
!        End Do
!      END IF
      DO I = 1, NPEAK
        KK = 4 + 2*I
        V(KK+1) = AREA(I)
        V(KK+2) = XPOS(I)
      ENDDO
      DO I = 1, N
        D(I) = 0.1*ABS(V(I))
      ENDDO
      DO IP = 1, NPEAK
        KK = 6 + 2*IP
        D(KK) = 0.002
                   !(X(IMAX)-X(IMAX-1))
      ENDDO

      END SUBROUTINE GETVAR
!
!*****************************************************************************
!
      SUBROUTINE INITSP(N,V,D)
! Do all the initialisation

      INCLUDE 'PARAMS.INC'

      REAL V(MVAR), D(MVAR)
      PARAMETER (MPT=2000)
      COMMON /LSQDAT/ NPT, X(MPT), Y(MPT), E(MPT)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XMINT
      COMMON /ZSTOR2/ MN, MN2
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
     &                  DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
     &                  NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
     &                  REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
     &                  NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END

      LOGICAL REFUSE, CYC1, NOPKRF

      INCLUDE 'REFLNS.INC'

      INTEGER         NSOURC, JSOURC, KSOURC, NDASOU,    METHOD
      INTEGER         NPFSOU
      REAL                         SCALES
      INTEGER                                 KSCALS,    NPCSOU
      COMMON /SOURCE/ NSOURC, JSOURC, KSOURC, NDASOU(5), METHOD(9),     &
                      NPFSOU(9,5), SCALES(5), KSCALS(5), NPCSOU(9,5)

      INTEGER         NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI
      REAL                                                       SCALEP
      INTEGER                                                               KSCALP
      LOGICAL                                                                          PHMAG
      COMMON /PHASE / NPHASE, IPHASE, JPHASE, KPHASE, NPHUNI(9), SCALEP(9), KSCALP(9), PHMAG(9)

      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
!.. speedup attempt
      REAL ixres

! If there are no peaks specified, assume there is one. Estimate its position by 
! by finding the 2 theta value with the highest number of counts
      IF (NumInPFR(CurrentRange) .EQ. 0) THEN
        NumInPFR(CurrentRange) = 1
        YMaxTem = YOBIN(IPF_Lo(CurrentRange))
        XPF_Pos(1,CurrentRange) = XBIN(IPF_Lo(CurrentRange))
        YPF_Pos(1,CurrentRange) = YOBIN(IPF_Lo(CurrentRange))
        DO I = IPF_Lo(CurrentRange), IPF_Hi(CurrentRange)
          IF (YOBIN(I) .GT. YMaxTem) THEN
            XPF_Pos(1,CurrentRange) = XBIN(I)
            YPF_Pos(1,CurrentRange) = YOBIN(I)
            YMaxTem = YOBIN(I)
          ENDIF
        ENDDO
      ENDIF
      NPeak = NumInPFR(CurrentRange)
      N = 6 + 2*NPeak
      DO I = 1, NPeak
        XPos(I) = XPF_Pos(I,CurrentRange)
      ENDDO
      Npt = IPF_Range(CurrentRange)
      ITEM = IPF_Lo(CurrentRange) - 1
      DO I = 1, NPT
        J = ITEM + I
        X(I) = XBIN(J)
        Y(I) = YOBIN(J)
        E(I) = EBIN(J)
      ENDDO
      IXRES = 1.2
!      DXRES=1./FLOAT(IXRES)
      DXRES = 1./IXRES
      NPTS = NPT
      MN = 1
   10 MN = 2*MN
      MNTEM = NINT(2.*IXRES*FLOAT(NPTS))
!      MNTEM=2*IXRES*NPTS
      IF (MN.LT.MNTEM) GOTO 10
      MN = MIN(2048,MN)
      MN2 = MN/2
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
      IIMIN = 1
      IIMAX = NPTS
      XMINT = ZARGI(IIMIN)

      END SUBROUTINE INITSP
!
!*****************************************************************************
!
      SUBROUTINE OUTPUT_PRO(NVAR,VARVAL,VARESD)

      INTEGER NVAR

      INCLUDE 'PARAMS.INC'

      REAL VARVAL(MVAR), VARESD(MVAR)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      LOGICAL REFUSE, CYC1, NOPKRF
      COMMON /WWPRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),  &
                        DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),       &
                        NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ,      &
                        REFUSE, CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,  &
                        NBASF4(MPRPKF,2,9), L4END(9), L6ST, L6END

      PARAMETER (MPeak=10)
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), IPOS(MPEAK)

! Set the ranges correctly
      xranav = 0.0
      IPF_RPt(1) = 0
      DO II = 1, NumPeakFitRange
        IPF_RPt(II+1) = IPF_RPt(II) + IPF_Range(II)
      ENDDO
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
        IF (varesd(jp).LT.1.E-5) varesd(jp) = MAX(0.001,0.1*ABS(varval(jp)))
        PkAreaEsd(i,CurrentRange) = varesd(jp)
        jp = jp + 1
        PkPosVal(i,CurrentRange) = varval(jp)
        XPF_Pos(i,CurrentRange) = varval(jp)
        xranav = xranav + PkPosVal(i,CurrentRange)
        IF (varesd(jp).LT.1.E-5) varesd(jp) = 0.003
        PkPosEsd(i,CurrentRange) = varesd(jp)
        atem = ABS(varval(jp)-zargi(1))
        DO ii = 1, NPTS
          anewt = ABS(varval(jp)-zargi(ii))
          IF (anewt.LE.atem) THEN
            atem = anewt
            item = ii
          ENDIF
        ENDDO
        YPF_Pos(i,CurrentRange) = zcal(item)
      ENDDO
!O      xranav = 0.5*(XPF_Range(1,CurrentRange)+XPF_Range(2,CurrentRange))
      xranav = xranav / SNGL(NumInPFR(CurrentRange))
      PkPosAv(CurrentRange) = xranav
      CALL Upload_Positions()
      CALL Upload_Widths()

      END SUBROUTINE OUTPUT_PRO
!
!*****************************************************************************
!
