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
      REAL              PF_FWHM
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR)
      
      INTEGER     MPeak
      PARAMETER ( MPeak = 10 )

      INTEGER         NPEAK
      REAL                   AREA,        XPOS       , P2
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), P2(MVAR)

      REAL, EXTERNAL :: MULTIPEAK_CHISQ, FWHM
      INTEGER     MMPAR
      PARAMETER ( MMPAR = MVAR * MVAR )
      REAL X(MVAR), DX(MVAR), COV(MMPAR)
      INTEGER N, I, II

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

! Do all the initialisation
      CALL INITSP(N)
! Get variables
      CALL GETVAR(N,X,DX)
! Perform simplex
      IBMBER = 0
      CALL SIMOPT(X,DX,COV,N,MULTIPEAK_CHISQ)
      IF (IBMBER .NE. 0) THEN
        CALL DebugErrorMessage('Error fitting peak.')
        RETURN
      ENDIF
      DO I = 1, N
        P2(I) = X(I)
      ENDDO
      PF_FWHM(CurrentRange) = FWHM()
      RangeFitYN(CurrentRange) = .TRUE.
      DO I = 1, N
        II = I + (I-1)*N
        DX(I) = SQRT(AMAX1(0.0,COV(II)))
      ENDDO
      CALL OUTPUT_PRO(X,DX)

      END SUBROUTINE MULTIPEAK_FITTER
!
!*****************************************************************************
!
      SUBROUTINE GETVAR(N,V,D)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER, INTENT (IN   ) :: N
      REAL,    INTENT (  OUT) :: V(MVAR), D(MVAR)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      INTEGER     MPeak
      PARAMETER ( MPeak = 10 )

      INTEGER         NPEAK
      REAL                   AREA,        XPOS       , P2
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), P2(MVAR)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR)

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

      INTEGER I, IMAX, IP, KK, iPos
      REAL YMAX, XMIN, XMAX, XDIF, XDELT, YMXB, YPMX, YPMX50, YBT, YPT, XHWHMU, GAMM
      REAL SIGM, YPMX20, XTEM, HTEM, HPSL, HMSL, TOTAREA, YB, YHSUM, ATEM, ANEW
      REAL YHITE(MPEAK)

      YMAX = ZOBS(1)
      IMAX = 1
      DO I = 1, NPTS
        IF (ZOBS(I) .GE. YMAX) THEN
          YMAX = ZOBS(I)
          IMAX = I
        ENDIF
      ENDDO
      XMIN = ZARGI(1)
      XMAX = ZARGI(NPTS)
      XDIF = XMAX - XMIN
      XDELT = XDIF / (FLOAT(NPTS)-1.0)
      V(1) = ZOBS(1)
      V(2) = (ZOBS(NPTS)-ZOBS(1))/XDIF
      YMXB = V(1) + V(2)*(ZARGI(IMAX)-ZARGI(1))
      YPMX = YMAX - YMXB
      YPMX50 = 0.5 * YPMX
! Estimate GAMM & SIGM
      DO I = IMAX, NPTS
        YBT = V(1) + V(2)*(ZARGI(I)-ZARGI(1))
        YPT = ZOBS(I) - YBT
        IF (YPT .LT. YPMX50) THEN
          XHWHMU = ZARGI(I) - ZARGI(IMAX)
          GOTO 15
        ENDIF
      ENDDO
   15 GAMM = 2.0 * XHWHMU
      SIGM = 0.2 * GAMM
! Now estimate HPSL & HMSL
      YPMX20 = 0.2*YPMX
      DO I = IMAX, 1, -1
        YBT = V(1) + V(2) * (ZARGI(I)-ZARGI(1))
        YPT = ZOBS(I) - YBT
        IF (YPT .LT. YPMX20) THEN
          XTEM = ZARGI(IMAX) - ZARGI(I)
          GOTO 20
        ENDIF
      ENDDO
   20 HTEM = 2.0 * ABS(XTEM-GAMM)
      HPSL = MAX(0.001,SQRT(2.0*RAD*HTEM*TAN(RAD*ZARGI(IMAX))))
      HMSL = 0.5 * HPSL
      TOTAREA = 0.0
      DO I = 1, NPTS
        YB = V(1) + V(2) * (ZARGI(I)-ZARGI(1))
        TOTAREA = TOTAREA + ZOBS(I) - YB
      ENDDO
      TOTAREA = TOTAREA * XDELT
      YHSUM = 0.0
      DO IP = 1, NPEAK
        ATEM = ABS(ZARGI(1)-XPOS(IP))
        iPos = 1
        DO I = 2, NPTS
          ANEW = ABS(ZARGI(I)-XPOS(IP))
          IF (ANEW .LT. ATEM) THEN
            ATEM = ANEW
            iPos = I
          ENDIF
        ENDDO
        YB = V(1) + V(2) * (XPOS(IP)-ZARGI(1))
        YHITE(IP) = ZOBS(iPos) - YB
        YHSUM = YHSUM + YHITE(IP)
      ENDDO
      DO IP = 1, NPEAK
        AREA(IP) = TOTAREA * YHITE(IP) / YHSUM
      ENDDO
!      IF (CurrentRange .EQ. 1) Then
      V(3) = SIGM
      V(4) = GAMM
      V(5) = MIN(HPSL,0.02)
      V(6) = MIN(HMSL,0.01)
!      ELSE
! Use the previous results
!        XPosAvTem = 0.0
!        DO I = 1, NPeak
!          XPosAvTem = XPosAvTem + XPos(I)
!        ENDDO
!        XPosAvTem = XPosAvTem / FLOAT(NPeak)
!        DO I = 1, 4
!          V(I+2) = PeakFnValue(I,XPosAvTem)
!        ENDDO
!      ENDIF
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
      SUBROUTINE INITSP(N)
! Do all the initialisation

      IMPLICIT NONE

      INTEGER, INTENT (  OUT) :: N

      INCLUDE 'PARAMS.INC'

      REAL            ZARGK,         ZXDEL
      COMMON /REFLNZ/ ZARGK(MFCSTO), ZXDEL(MFCSTO)

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL            ZCAL
      COMMON /YSTORE/ ZCAL(MOBS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XMINT

      INTEGER         MN, MN2
      COMMON /ZSTOR2/ MN, MN2

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

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

      INTEGER     MPeak
      PARAMETER ( MPeak = 10 )

      INTEGER         NPEAK
      REAL                   AREA,        XPOS       , P2
      COMMON /MULTPK/ NPEAK, AREA(MPEAK), XPOS(MPEAK), P2(MVAR)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER I, iTem, J, MNTEM, ISPMIN, ISPMAX, NPT2
      REAL    YMAXTEM, ZOBSMAX

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
        XPOS(I) = XPF_Pos(I,CurrentRange)
      ENDDO
      NPTS = IPF_Range(CurrentRange)
      iTem = IPF_Lo(CurrentRange) - 1
      DO I = 1, NPTS
        J = iTem + I
        ZARGI(I) = XBIN(J)
        ZOBS(I)  = YOBIN(J)
        ZDOBS(I) = EBIN(J)
      ENDDO
      MNTEM = NINT(2.0 * 1.2 * FLOAT(NPTS))
      MN = 1
   10 MN = 2 * MN
      IF (MN .LT. MNTEM) GOTO 10
      MN = MIN(2048,MN)
      MN2 = MN/2
      KNOW = 1
      JPHASE = 1
      JSOURC = 1
      NPKGEN(JPHASE,JSOURC) = 4
      ISPMIN = 1
      ISPMAX = NPTS
      NPT2 = NPTS/2
! ######### @@ JvdS 21 July 2003 ####################
! Where does the 1.0/1.2 come from? What's wrong with 1.0 ?
!O      ZXDELT = (1.0 / 1.2)*(ZARGI(NPT2)-ZARGI(NPT2-1))
      ZXDELT = ZARGI(NPT2)-ZARGI(NPT2-1)
      ZXDEL(KNOW) = ZXDELT
      KOBZ(1) = ISPMIN
      ZOBSMAX = ZOBS(ISPMIN)
      DO I = ISPMIN, ISPMAX
        IF (ZOBS(I) .GT. ZOBSMAX) THEN
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
      SUBROUTINE OUTPUT_PRO(VARVAL,VARESD)

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL, INTENT (INOUT) :: VARVAL(MVAR), VARESD(MVAR)

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR)

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

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

      REAL    xranav, atem, anewt
      INTEGER I, II, IP, JP, iTem

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
        JP = IP + 2
        PkFnVal(IP,CurrentRange) = varval(JP)
        IF (varesd(JP).LT.1.E-5) varesd(JP) = 0.02
        PkFnEsd(IP,CurrentRange) = varesd(JP)
      ENDDO
      DO I = 1, NumInPFR(CurrentRange)
        JP = 1 + NPKGEN(1,1) + 2*I
        PkAreaVal(I,CurrentRange) = varval(JP)
        IF (varesd(JP).LT.1.E-5) varesd(jp) = MAX(0.001,0.1*ABS(varval(JP)))
        PkAreaEsd(I,CurrentRange) = varesd(JP)
        JP = JP + 1
        PkPosVal(I,CurrentRange) = varval(JP)
        XPF_Pos(I,CurrentRange) = varval(JP)
        xranav = xranav + PkPosVal(I,CurrentRange)
        IF (varesd(JP).LT.1.E-5) varesd(JP) = 0.003
        PkPosEsd(I,CurrentRange) = varesd(JP)
        atem = ABS(varval(JP)-ZARGI(1))
        iTem = 1
        DO II = 2, NPTS
          anewt = ABS(varval(JP)-ZARGI(II))
          IF (anewt .LE. atem) THEN
            atem = anewt
            iTem = II
          ENDIF
        ENDDO
        YPF_Pos(I,CurrentRange) = ZCAL(iTem)
      ENDDO
      xranav = xranav / FLOAT(NumInPFR(CurrentRange))
      PkPosAv(CurrentRange) = xranav
      CALL Upload_Positions
! Now do a refinement ...
      CALL RefineLattice
      CALL Upload_Widths

      END SUBROUTINE OUTPUT_PRO
!
!*****************************************************************************
!
